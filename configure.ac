## Objective-CL, an Objective-C bridge for Common Lisp.
## Copyright (C) 2008  Matthias Andreas Benkard.
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.

AC_PREREQ(2.61)
AC_INIT(Objective-CL, 0.1.1, objcl-bugs@mail.matthias.benkard.de)
AC_CONFIG_SRCDIR([Objective-C/libobjcl.h])

AC_PROG_CC
AC_PROG_OBJC
#AC_PROG_INSTALL
#AC_PROG_LN_S
AC_PROG_MAKE_SET

AC_SEARCH_LIBS([ffi_call], [ffi], [HAVE_LIBFFI=1; break], [HAVE_LIBFFI=0])

HAVE_ANY_FFI_H=0
if test x$HAVE_LIBFFI = x1; then
  AC_CHECK_HEADERS([ffi.h ffi/ffi.h], [HAVE_ANY_FFI_H=1; break])
fi

AC_SUBST(HAVE_ANY_FFI_H)
AC_SUBST(HAVE_LIBFFI)
AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([config.make])
AC_OUTPUT
