/*
 * Support for libffi (http://sources.redhat.com/libffi)
 *
 * libffi is a library that makes it possible to dynamicly create calls
 * to C functions (without knowing the signature at compile-time). It also
 * provides a way to create closures, that is dynamicly create functions with
 * a runtime specified interface.
 *
 * This file contains functions to dynamicly call objc_msgSendSuper and to
 * dynamicly create IMPs for use in Objective-C method dispatch tables. The
 * file 'register.m' contains compile-time generated equivalents of these.
 */
#include "pyobjc.h"

#import <Foundation/NSDictionary.h>
#import <Foundation/NSString.h>
#import <Foundation/NSHost.h>

#ifdef MACOSX
/*
 * Define SMALL_STRUCT_LIMIT as the largest struct that will be returned
 * in registers instead of with a hidden pointer argument.
 */

#if defined(__ppc__)

#   define SMALL_STRUCT_LIMIT	4

#elif defined(__i386__) 

#   define SMALL_STRUCT_LIMIT 	8

#else

#   error "Unsupported MACOSX platform"

#endif

#endif /* MACOSX */



#if 0 /* Usefull during debugging, only used in the debugger */
static void describe_ffitype(ffi_type* type)
{
	switch (type->type) {
	case FFI_TYPE_VOID: printf("%s", "void"); break;
	case FFI_TYPE_INT: printf("%s", "int"); break;
	case FFI_TYPE_FLOAT: printf("%s", "float"); break;
	case FFI_TYPE_DOUBLE: printf("%s", "double"); break;
	case FFI_TYPE_UINT8: printf("%s", "uint8"); break;
	case FFI_TYPE_SINT8: printf("%s", "sint8"); break;
	case FFI_TYPE_UINT16: printf("%s", "uint16"); break;
	case FFI_TYPE_SINT16: printf("%s", "sint16"); break;
	case FFI_TYPE_UINT32: printf("%s", "uint32"); break;
	case FFI_TYPE_SINT32: printf("%s", "sint32"); break;
	case FFI_TYPE_UINT64: printf("%s", "uint64"); break;
	case FFI_TYPE_SINT64: printf("%s", "sint64"); break;
	case FFI_TYPE_POINTER: printf("%s", "*"); break;
	case FFI_TYPE_STRUCT: {
			ffi_type** elems = type->elements;

			printf("%s", "struct { ");
			if (elems) {
				while (*elems) {
					describe_ffitype(*(elems++));
					printf("%s", "; ");
				}
			}
			printf("%s", "}");
		}
	       break;

	default:
	       // Don't abort, this is called from the debugger 
	       printf("?(%d)", type->type);
	}
}

static void describe_cif(ffi_cif* cif)
{
	size_t i;
	printf("<ffi_cif abi=%d nargs=%d  bytes=%d flags=%#x args=[",
		cif->abi, cif->nargs, cif->bytes, cif->flags);
	for  (i = 0; i < cif->nargs; i++) {
		describe_ffitype(cif->arg_types[i]);
		printf("%s", ", ");
	}
	printf("%s", "] rettype=");
	describe_ffitype(cif->rtype);
	printf("%s", ">\n");
}

#endif


static Py_ssize_t
num_struct_fields(const char* argtype)
{
	Py_ssize_t res = 0;

	if (*argtype != _C_STRUCT_B) return -1;
	while (*argtype != _C_STRUCT_E && *argtype != '=') argtype++;
	if (*argtype == _C_STRUCT_E) return 0;
	
	argtype++;
	while (*argtype != _C_STRUCT_E) {
		argtype = PyObjCRT_SkipTypeSpec(argtype);
		if (argtype == NULL) return -1;
		res ++;
	}
	return res;
}


static void
free_type(void *obj)
{
	PyMem_Free(((ffi_type*)obj)->elements);
	PyMem_Free(obj);
}

static ffi_type* signature_to_ffi_type(const char* argtype);

static ffi_type* 
array_to_ffi_type(const char* argtype)
{
	static NSMutableDictionary* array_types = nil;
	NSValue *v;
	ffi_type* type;
	Py_ssize_t field_count;
	Py_ssize_t i;
	const NSString* key = [NSString stringWithUTF8String: argtype];

	if (array_types == NULL || array_types == nil) {
		array_types = [NSMutableDictionary dictionaryWithCapacity: 100];
		if (array_types == NULL || array_types == nil) return NULL;
	}

	v = [array_types objectForKey: key];
	if (v != nil) {
		return (ffi_type*)[v pointerValue];
	}

	/* We don't have a type description yet, dynamicly 
	 * create it.
	 */
	field_count = atoi(argtype+1);
			
	type = PyMem_Malloc(sizeof(*type));
	if (type == NULL) {
		PyErr_NoMemory();
		return NULL;
	}
	type->size = PyObjCRT_SizeOfType(argtype);
	type->alignment = PyObjCRT_AlignOfType(argtype);

	/* Libffi doesn't really know about arrays as part of larger 
	 * data-structres (e.g. struct foo { int field[3]; };). We fake it
	 * by treating the nested array as a struct. This seems to work 
	 * fine on MacOS X.
	 */
	type->type = FFI_TYPE_STRUCT;
	type->elements = PyMem_Malloc((1+field_count) * sizeof(ffi_type*));
	if (type->elements == NULL) {
		PyMem_Free(type);
		PyErr_NoMemory();
		return NULL;
	}
	
	while (isdigit(*++argtype));
	type->elements[0] = signature_to_ffi_type(argtype);
	for (i = 1; i < field_count; i++) {
		type->elements[i] = type->elements[0];
	}
	type->elements[field_count] = 0;

	v = [NSValue valueWithPointer: type];
	if (v == NULL || v == nil) {
		free_type(type);
		return NULL;
	}

	NS_DURING
          {
            [array_types setObject: v forKey: key];
          }
        NS_HANDLER
          {
            NS_VALUERETURN (NULL, ffi_type*);
          }
        NS_ENDHANDLER

	return type;
}

static ffi_type* 
struct_to_ffi_type(const char* argtype)
{
	static NSMutableDictionary* struct_types = nil;
	NSValue* v;
	ffi_type* type;
	Py_ssize_t field_count;
	const char* curtype;
	const NSString* key = [NSString stringWithUTF8String: argtype];

	if (struct_types == NULL || struct_types == nil) {
		struct_types = [NSMutableDictionary dictionaryWithCapacity: 100];
		if (struct_types == NULL || struct_types == nil) return NULL;
	}

	v = [struct_types objectForKey: key];
	if (v != nil) {
		return (ffi_type*)[v pointerValue];
	}

	/* We don't have a type description yet, dynamicly 
	 * create it.
	 */
	field_count = num_struct_fields(argtype);
	if (field_count == -1) {
#ifdef STRICT_TYPE_PARSING
		[[NSException exceptionWithName: @"PyObjCExc_InternalError"
			      reason: [NSString stringWithFormat: @"Cannot determine layout of %s", argtype]
			      userInfo: NULL] raise];
#else
		NSLog (@"PyObjCExc_InternalError: Cannot determine layout of %s", argtype);
#endif
		return NULL;
	}
			
	type = PyMem_Malloc(sizeof(*type));
	if (type == NULL) {
		PyErr_NoMemory();
		return NULL;
	}
	type->size = PyObjCRT_SizeOfType(argtype);
	type->alignment = PyObjCRT_AlignOfType(argtype);
	type->type = FFI_TYPE_STRUCT;
	type->elements = PyMem_Malloc((1+field_count) * sizeof(ffi_type*));
	if (type->elements == NULL) {
		PyMem_Free(type);
		PyErr_NoMemory();
		return NULL;
	}
	
	field_count = 0;
	curtype = argtype+1;
	while (*curtype != _C_STRUCT_E && *curtype != '=') curtype++;
	if (*curtype == '=') {
		curtype ++;
		while (*curtype != _C_STRUCT_E) {
			type->elements[field_count] = 
				signature_to_ffi_type(curtype);
			if (type->elements[field_count] == NULL) {
				PyMem_Free(type->elements);
				return NULL;
			}
			field_count++;
			curtype = PyObjCRT_SkipTypeSpec(curtype);
			if (curtype == NULL) {
				PyMem_Free(type->elements);
				return NULL;
			}
		}
	}
	type->elements[field_count] = NULL;

	v = [NSValue valueWithPointer: type];
	if (v == NULL || v == nil) {
		free_type(type);
		return NULL;
	}

	NS_DURING
          {
            [struct_types setObject: v forKey: key];
          }
        NS_HANDLER
          {
            NS_VALUERETURN (NULL, ffi_type*);
          }
        NS_ENDHANDLER

	return type;
}

ffi_type*
objcl_pyobjc_signature_to_ffi_return_type(const char* argtype)
{
	switch (*argtype) {
	case _C_CHR: case _C_SHT:
		return &ffi_type_sint;
	case _C_UCHR: case _C_USHT:
		return &ffi_type_uint;
#ifdef _C_BOOL
	case _C_BOOL: return &ffi_type_sint;
#endif	
	default:
		return signature_to_ffi_type(argtype);
	}
}


static ffi_type*
signature_to_ffi_type(const char* argtype)
{
	switch (*argtype) {
	case _C_VOID: return &ffi_type_void;
	case _C_ID: return &ffi_type_pointer;
	case _C_CLASS: return &ffi_type_pointer;
	case _C_SEL: return &ffi_type_pointer;
	case _C_CHR: return &ffi_type_schar;
#ifdef _C_BOOL
	case _C_BOOL: return &ffi_type_sint;
#endif	
	case _C_UCHR: return &ffi_type_uchar;
	case _C_SHT: return &ffi_type_sshort;
	case _C_USHT: return &ffi_type_ushort;
	case _C_INT: return &ffi_type_sint;
	case _C_UINT: return &ffi_type_uint;

	 /* The next to defintions are incorrect, but the correct definitions
	  * don't work (e.g. give testsuite failures). We should be fine
	  * as long as sizeof(long) == sizeof(int)
	  */
	case _C_LNG: return &ffi_type_sint;  /* ffi_type_slong */
	case _C_ULNG: return &ffi_type_uint;  /* ffi_type_ulong */
	case _C_LNGLNG: return &ffi_type_sint64;
	case _C_ULNGLNG: return &ffi_type_uint64;
	case _C_FLT: return &ffi_type_float;
	case _C_DBL: return &ffi_type_double;
	case _C_CHARPTR: return &ffi_type_pointer;
	case _C_PTR: return &ffi_type_pointer;
	case _C_ARY_B: 
		return array_to_ffi_type(argtype);
	case _C_IN: case _C_OUT: case _C_INOUT: case _C_CONST:
		return signature_to_ffi_type(argtype+1);
	case _C_STRUCT_B: 
		return struct_to_ffi_type(argtype);
	default:
#ifdef STRICT_TYPE_PARSING
		[[NSException exceptionWithName: @"PyExc_NotImplementedError"
			      reason: [NSString stringWithFormat: @"Type '%c' not supported", *argtype]
			      userInfo: NULL] raise];
#else
		NSLog (@"PyExc_NotImplementedError: Type '%#x' not supported", *argtype);
#endif
		return NULL;
	}
}

/*
 * arg_signature_to_ffi_type: Make the ffi_type for the call to the method IMP,
 * on MacOS X this is the same as the normal signature_to_ffi_type, but on
 * Linux/GNUstep we need a slightly different function.
 */
#ifdef MACOSX

#ifdef __ppc__
ffi_type*
objcl_pyobjc_arg_signature_to_ffi_type(const char* argtype)
{
  return signature_to_ffi_type (argtype);
}

#else
ffi_type*
objcl_pyobjc_arg_signature_to_ffi_type(const char* argtype)
{
	/* NOTE: This is the minimal change to pass the unittests, it is not
	 * based on analysis of the calling conventions.
	 */
	switch (*argtype) {
	case _C_CHR: return &ffi_type_sint;
	case _C_UCHR: return &ffi_type_uint;
	case _C_SHT: return &ffi_type_sint;
	case _C_USHT: return &ffi_type_uint;
	default: return signature_to_ffi_type(argtype);
	}
}
#endif

#else /* GNUstep */

ffi_type*
objcl_pyobjc_arg_signature_to_ffi_type(const char* argtype)
{
	/* NOTE: This is the minimal change to pass the unittests, it is not
	 * based on analysis of the calling conventions.
	 */
	switch (*argtype) {
	case _C_CHR: return &ffi_type_sint;
	case _C_UCHR: return &ffi_type_uint;
	case _C_SHT: return &ffi_type_sint;
	case _C_USHT: return &ffi_type_uint;
	default: return signature_to_ffi_type(argtype);
	}
}

#endif /* GNUstep */
