/* Copyright (c) 1996,97,98 by Lele Gaifax.  All Rights Reserved
 * Copyright (c) 2002, 2003 Ronald Oussoren
 *
 * This software may be used and distributed freely for any purpose
 * provided that this notice is included unchanged on any and all
 * copies. The author does not warrant or guarantee this software in
 * any way.
 *
 * This file is part of the PyObjC package.
 *
 * RCSfile: objc_support.m,v
 * Revision: 1.24
 * Date: 1998/08/18 15:35:58
 *
 * Created Tue Sep 10 14:16:02 1996.
 */

#include <objc/Protocol.h>

#include <unistd.h>

#ifdef MACOSX
/* OSX 10.1 doesn't define LLONG_MIN, LLONG_MAX and ULLONG_MAX */
#ifndef LLONG_MIN
#error "Mac OS X 10.1 not supported"
#endif
#endif

#import <Foundation/NSInvocation.h>
#import <Foundation/NSData.h> 
#import <Foundation/NSValue.h> 
#import <Foundation/NSDecimalNumber.h> 

#ifdef MACOSX
#include <CoreFoundation/CFNumber.h>
#endif /* MACOSX */


#ifndef MAX
static inline ssize_t
MAX(ssize_t x, ssize_t y)
{
	return x > y ? x : y;
}
#endif

static inline ssize_t 
ROUND(ssize_t v, ssize_t a)
{
	if (v % a == 0) {
		return v;
	} else {
		return v + a - (v % a);
	}
}


const char*
PyObjCRT_SkipTypeQualifiers (const char* type)
{
	while (
			*type == _C_CONST ||
			*type == _C_IN ||
			*type == _C_INOUT ||
			*type == _C_OUT ||
			*type == _C_BYCOPY ||
			*type == _C_ONEWAY) {
		type++;
	}
	while (*type && isdigit(*type)) type++;
	return type;
}


const char * 
PyObjCRT_SkipTypeSpec (const char *type)
{
	type = PyObjCRT_SkipTypeQualifiers (type);

	switch (*type) {
	/* The following are one character type codes */
	case _C_UNDEF:
	case _C_CLASS:
	case _C_SEL:
	case _C_CHR:
	case _C_UCHR:
	case _C_CHARPTR:
#ifdef _C_ATOM
	case _C_ATOM:
#endif
#ifdef _C_BOOL
	case _C_BOOL:
#endif
	case _C_SHT:
	case _C_USHT:
	case _C_INT:
	case _C_UINT:
	case _C_LNG:
	case _C_ULNG:
	case _C_FLT:
	case _C_DBL:
	case _C_VOID:
	case _C_LNGLNG:
	case _C_ULNGLNG:
	case _C_BFLD: /* Not really 1 character, but close enough  */
		++type;
		break;

	case _C_ID:
		++type;
#ifdef MACOSX
		if (*type == '"') {
			/* embedded field name in an ivar_type */
			type=strchr(type+1, '"');
			if (type != NULL) {
				type++;
			}
		}
#endif
		break;

	case _C_ARY_B:
		/* skip digits, typespec and closing ']' */

		while (isdigit (*++type));
		type = PyObjCRT_SkipTypeSpec (type);
		assert (type == NULL || *type == _C_ARY_E);
		if (type) type++;
		break;
  
	case _C_STRUCT_B:
		/* skip name, and elements until closing '}'  */
		while (*type != _C_STRUCT_E && *type++ != '='); 
		while (type && *type != _C_STRUCT_E) {
			if (*type == '"') {
				/* embedded field names */
				type = strchr(type+1, '"');
				if (type != NULL) {
					type++;
				} else {
					return NULL;
				}
			}
			type = PyObjCRT_SkipTypeSpec (type);
		}
		if (type) type++;
		break;

	case _C_UNION_B:
		/* skip name, and elements until closing ')'  */
		while (*type != _C_UNION_E && *type++ != '='); 
		while (type && *type != _C_UNION_E) { 
			type = PyObjCRT_SkipTypeSpec (type); 
		}
		if (type) type++;
		break;
  
	case _C_PTR:
	case _C_CONST:
	case _C_IN:
	case _C_INOUT:
	case _C_OUT:
	case _C_BYCOPY:
	case _C_ONEWAY:

		/* Just skip the following typespec */
		type = PyObjCRT_SkipTypeSpec (type+1);
		break;


	default:
		PyErr_Format(PyObjCExc_InternalError,
			"PyObjCRT_SkipTypeSpec: Unhandled type '%#x'", *type); 
		return NULL;
	}

	/* The compiler inserts a number after the actual signature, 
	 * this number may or may not be usefull depending on the compiler
	 * version. We never use it.
	 */
	while (type && *type && isdigit(*type)) type++;
	return type;
}

/*
Return the alignment of an object specified by type 
*/

/*
*  On MacOS X, the elements of a struct are aligned differently inside the
*  struct than outside. That is, the maximum alignment of any struct field
*  (except the first) is 4, doubles outside of a struct have an alignment of
*  8.
*
*  Other platform don't seem to have this inconsistency.
*  
*  XXX: sizeof_struct, alignof_struct and {de,}pythonify_c_struct should
*  probably be moved to platform dependend files. As long as this is the
*  only platform dependent code this isn't worth the effort.
*/
#ifdef MACOSX

static inline ssize_t
PyObjC_EmbeddedAlignOfType (const char*  type)
{
	ssize_t align = PyObjCRT_AlignOfType(type);

#ifdef __i386__
	return align;

#else
	if (align < 4 || align == 16) {
		return align;
	} else {
		return 4;
	}
#endif
}

#else

static inline int
PyObjC_EmbeddedAlignOfType (const char*  type)
{
	ssize_t align =  PyObjCRT_AlignOfType(type);

	/* GNUstep/ix86 seems to behave like this: */
	if (align < 4) {
		return align;
	} else {
		return 4;
	}
}

#endif

ssize_t
PyObjCRT_AlignOfType (const char *type)
{
	switch (*type) {
	case _C_ID:    return __alignof__ (id);
	case _C_CLASS: return __alignof__ (Class);
	case _C_SEL:   return __alignof__ (SEL);
	case _C_CHR:   return __alignof__ (char);
	case _C_UCHR:  return __alignof__ (unsigned char);
	case _C_SHT:   return __alignof__ (short);
	case _C_USHT:  return __alignof__ (unsigned short);
#ifdef _C_BOOL
	case _C_BOOL:   return __alignof__ (bool);
#endif
	case _C_INT:   return __alignof__ (int);
	case _C_UINT:  return __alignof__ (unsigned int);
	case _C_LNG:   return __alignof__ (long);
	case _C_ULNG:  return __alignof__ (unsigned long);
	case _C_FLT:   return __alignof__ (float);
	case _C_DBL:   
#if defined(__APPLE__) && defined(__i386__)
		/* The ABI says natural alignment is 4 bytes, but 
		 * GCC's __alignof__ says 8. The latter is wrong.
		 */
		return 4;
#else
		return __alignof__ (double);
#endif

	case _C_CHARPTR: return __alignof__ (char *);
#ifdef _C_ATOM
	case _C_ATOM: return __alignof__ (char *);
#endif
	case _C_PTR:   return __alignof__ (void *);
#if defined(__APPLE__) && defined(__i386__)
		/* The ABI says natural alignment is 4 bytes, but 
		 * GCC's __alignof__ says 8. The latter is wrong.
		 */
	case _C_LNGLNG: return 4;
	case _C_ULNGLNG: return 4;
#else
	case _C_LNGLNG: return __alignof__(long long);
	case _C_ULNGLNG: return __alignof__(unsigned long long);
#endif

	case _C_ARY_B:
		while (isdigit(*++type)) /* do nothing */;
		return PyObjCRT_AlignOfType (type);
  
	case _C_STRUCT_B:
	{
		struct { int x; double y; } fooalign;
		while(*type != _C_STRUCT_E && *type++ != '=') /* do nothing */;
		if (*type != _C_STRUCT_E) {
			int have_align = 0;
			ssize_t align = 0;

			while (type != NULL && *type != _C_STRUCT_E) {
				if (*type == '"') {
					type = strchr(type+1, '"');
					if (type) type++;
				}
				if (have_align) {
					align = MAX(align, 
					   PyObjC_EmbeddedAlignOfType(type));
				} else {
					align = PyObjCRT_AlignOfType(type);
					have_align = 1;
				}
				type = PyObjCRT_SkipTypeSpec(type);
			}
			if (type == NULL) return -1;
			return align;
		} else {
			return __alignof__ (fooalign);
		}
	}

	case _C_UNION_B:
	{
		int maxalign = 0;
		type++;
		while (*type != _C_UNION_E)
		{
			int item_align = PyObjCRT_AlignOfType(type);
			if (item_align == -1) return -1;
			maxalign = MAX (maxalign, item_align);
			type = PyObjCRT_SkipTypeSpec (type);
		}
		return maxalign;
	}

	case _C_CONST:
	case _C_IN:
	case _C_INOUT:
	case _C_OUT:
	case _C_BYCOPY:
	case _C_ONEWAY:
		return PyObjCRT_AlignOfType(type+1);

	default:
		PyErr_Format(PyObjCExc_InternalError, 
			"PyObjCRT_AlignOfType: Unhandled type '%#x'", *type);
		return -1;
	}
}

/*
The aligned size if the size rounded up to the nearest alignment.
*/

static ssize_t
PyObjCRT_AlignedSize (const char *type)
{
	ssize_t size = PyObjCRT_SizeOfType (type);
	ssize_t align = PyObjCRT_AlignOfType (type);

	if (size == -1 || align == -1) return -1;
	return ROUND(size, align);
}

/*
return the size of an object specified by type 
*/

ssize_t
PyObjCRT_SizeOfType (const char *type)
{
	ssize_t itemSize;
	switch (*type) {
	case _C_VOID:    return 0;
	case _C_ID:      return sizeof(id);
	case _C_CLASS:   return sizeof(Class);
	case _C_SEL:     return sizeof(SEL);
	case _C_CHR:     return sizeof(char);
	case _C_UCHR:    return sizeof(unsigned char);
	case _C_SHT:     return sizeof(short);
	case _C_USHT:    return sizeof(unsigned short);
#ifdef _C_BOOL
	case _C_BOOL:    return sizeof(bool);
#endif
	case _C_INT:     return sizeof(int);
	case _C_UINT:    return sizeof(unsigned int);
	case _C_LNG:     return sizeof(long);
	case _C_ULNG:    return sizeof(unsigned long);
	case _C_FLT:     return sizeof(float);
	case _C_DBL:     return sizeof(double);
	case _C_LNGLNG:  return sizeof(long long);
	case _C_ULNGLNG: return sizeof(unsigned long long);

	case _C_PTR:
	case _C_CHARPTR:
#ifdef _C_ATOM
	case _C_ATOM:
#endif
		return sizeof(char*);
  
	case _C_ARY_B:
	{
		ssize_t len = atoi(type+1);
		ssize_t item_align;
		while (isdigit(*++type))
			;
		item_align = PyObjCRT_AlignedSize(type);
		if (item_align == -1) return -1;
		return len*item_align;
	}
	break; 

	case _C_STRUCT_B:
	{
		ssize_t acc_size = 0;
		int have_align =  0;
		ssize_t align;
		ssize_t max_align = 0;

		while (*type != _C_STRUCT_E && *type++ != '=')
			; /* skip "<name>=" */
		while (*type != _C_STRUCT_E) {
			if (*type == '"') {
				type = strchr(type+1, '"');
				if (type) type++;
			}
			if (have_align) {
				align = PyObjC_EmbeddedAlignOfType(type);
				if (align == -1) return -1;
			} else {
				align = PyObjCRT_AlignOfType(type);
				if (align == -1) return -1;
				have_align = 1;
			}
			max_align = MAX(align, max_align);
			acc_size = ROUND (acc_size, align);

			itemSize = PyObjCRT_SizeOfType (type); 
			if (itemSize == -1) return -1;
			acc_size += itemSize;
			type = PyObjCRT_SkipTypeSpec (type);
		}
		if (max_align) {
			acc_size = ROUND(acc_size, max_align);
		}
		return acc_size;
	}

	case _C_UNION_B:
	{
		ssize_t max_size = 0;
		type++;
		while (*type != _C_UNION_E) {
			itemSize = PyObjCRT_SizeOfType (type);
			if (itemSize == -1) return -1;
			max_size = MAX (max_size, itemSize);
			type = PyObjCRT_SkipTypeSpec (type);
		}
		return max_size;
	}

	case _C_CONST:
	case _C_IN:
	case _C_INOUT:
	case _C_OUT:
	case _C_BYCOPY:
	case _C_ONEWAY:
		return PyObjCRT_SizeOfType(type+1);

	default:
		PyErr_Format(PyObjCExc_InternalError, 
			"PyObjCRT_SizeOfType: Unhandled type '%#x", *type);
		return -1;
	}
}


/*#F Returns a tuple of objects representing the content of a C array
of type @var{type} pointed by @var{datum}. */
static PyObject *
pythonify_c_array (const char *type, void *datum)
{
	PyObject *ret;
	ssize_t nitems, itemidx, sizeofitem;
	unsigned char* curdatum;

	nitems = atoi (type+1);
	while (isdigit (*++type))
		;
	sizeofitem = PyObjCRT_SizeOfType (type);
	if (sizeofitem == -1) return NULL;

	ret = PyTuple_New (nitems);
	if (!ret) return NULL;

	curdatum = datum;
	for (itemidx=0; itemidx < nitems; itemidx++) {
		PyObject *pyitem = NULL;

		pyitem = pythonify_c_value (type, curdatum);

		if (pyitem) {
			PyTuple_SET_ITEM (ret, itemidx, pyitem);
		} else {
			Py_DECREF(ret);
			return NULL;
		}

		curdatum += sizeofitem;
	}

	return ret;
}


ssize_t 
PyObjCRT_SizeOfReturnType(const char* type)
{
	switch(*type) {
	case _C_CHR:
	case _C_UCHR:
	case _C_SHT:
	case _C_USHT:
		return sizeof(int);
	default:
		return PyObjCRT_SizeOfType(type);
	}
}
