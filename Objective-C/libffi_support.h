#ifndef PyObjC_FFI_SUPPORT_H
#define PyObjC_FFI_SUPPORT_H

#include "ffi.h"

ffi_type*
objcl_pyobjc_signature_to_ffi_return_type (const char* argtype);

ffi_type*
objcl_pyobjc_arg_signature_to_ffi_type (const char* argtype);

#endif /* PyObjC_FFI_SUPPORT_H */
