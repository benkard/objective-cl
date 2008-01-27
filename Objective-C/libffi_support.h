#ifndef PyObjC_FFI_SUPPORT_H
#define PyObjC_FFI_SUPPORT_H

#ifdef USE_LIBFFI
#ifdef HAVE_FFI_H
#include <ffi.h>
#elif HAVE_FFI_FFI_H
#include <ffi/ffi.h>
#else
/* We are using our own build of libffi. */
#include <ffi.h>
#endif
#endif

ffi_type*
objcl_pyobjc_signature_to_ffi_return_type (const char* argtype);

ffi_type*
objcl_pyobjc_arg_signature_to_ffi_type (const char* argtype);

#endif /* PyObjC_FFI_SUPPORT_H */
