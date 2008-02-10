## Objective-CL, an Objective-C bridge for Common Lisp.
## Copyright (C) 2007  Matthias Andreas Benkard.
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


-include $(GNUSTEP_MAKEFILES)/common.make

include ../version.make
-include ../config.make

.PHONY: all clean distclean install

ifeq ($(CONFIG_MAKE_INCLUDED_P),)
all clean install:
	@echo "Please run ./configure before running make."
else # CONFIG_MAKE_INCLUDED_P
USE_LIBFFI = 1

LIBRARY_NAME = libobjcl

RPM_DISABLE_RELOCATABLE = YES
ADDITIONAL_OBJCFLAGS = -Wall -g -DVERSION=\"$(VERSION)\" -I/usr/local/include

libobjcl_OBJC_FILES = JIGS/ObjcRuntimeUtilities2.m libobjcl.m NSObject-ObjectiveCLWrapperLink.m PyObjC/objc_support.m PyObjC/objc-runtime-apple.m PyObjC/objc-runtime-gnu.m PyObjC/objc-runtime-compat.m
libobjcl_C_FILES = JIGS/ObjcRuntimeUtilities.c
LIBRARIES_DEPEND_UPON = $(FND_LIBS) $(GUI_LIBS) $(OBJC_LIBS) $(SYSTEM_LIBS) $(CONFIG_SYSTEM_LIBS)

ADDITIONAL_LDFLAGS = $(LIBS)

ifdef USE_LIBFFI
# FIXME: For now, we statically link against libffi.  We are not certain
# that this is a good idea.  Linking dynamically, on the other hand, is
# hard in general (that's one of the reasons libtool exists, after all).
ADDITIONAL_OBJCFLAGS += -DUSE_LIBFFI
ifneq ($(HAVE_ANY_FFI_H),1)
ADDITIONAL_LDFLAGS += ../libffi/.libs/libffi.a
ADDITIONAL_OBJCFLAGS += -I../libffi/include
FFI_DEPS += ../libffi/.libs/libffi.a
FFI_CLEAN += libffi_clean
endif
libobjcl_OBJC_FILES += PyObjC/libffi_support.m
endif

ifneq ($(COMMON_MAKE_LOADED),)
-include $(GNUSTEP_MAKEFILES)/library.make

before-all:: $(FFI_DEPS)
	mkdir -p $(GNUSTEP_OBJ_DIR)/PyObjC
	mkdir -p $(GNUSTEP_OBJ_DIR)/JIGS

after-clean:: $(FFI_CLEAN)

else  # Mac OS X
libobjcl_OBJ_FILES = $(patsubst %.m,obj/%.o,$(libobjcl_OBJC_FILES))

all: obj/libobjcl.dylib

clean: $(FFI_CLEAN)
	rm -f obj/libobjcl.dylib obj/libobjcl.dylib.$(VERSION) $(libobjcl_OBJ_FILES)

distclean: clean

obj/libobjcl.dylib: $(FFI_DEPS) obj/libobjcl.dylib.$(VERSION)
	ln -s libobjcl.dylib.$(VERSION) $@

obj/libobjcl.dylib.$(VERSION): $(libobjcl_OBJ_FILES)
	make -p obj
	gcc -dynamiclib -current_version $(VERSION) -flat_namespace -undefined warning -install_name $(CURDIR)/obj/libobjcl.dylib.$(VERSION) -o $@ $(libobjcl_OBJ_FILES) -fnext-runtime -framework AppKit -framework Foundation -lm $(ADDITIONAL_LDFLAGS) $(ADDITIONAL_OBJCFLAGS)

obj/%.o: %.m
	mkdir -p $(dir $@)
	gcc -o $@ -c $< -MMD -MP -DNeXT_Foundation_LIBRARY=1 -DNeXT_GUI_LIBRARY=1 -DNeXT_RUNTIME=1 -dynamic -fno-common -fno-omit-frame-pointer -Wno-import -fno-strict-aliasing -fnext-runtime -I. $(ADDITIONAL_OBJCFLAGS)
endif

../libffi/.libs/libffi.a:
	(cd ../libffi && chmod +x ltconfig && sh ./configure && make)

libffi_clean:
	-make -C ../libffi distclean
endif # CONFIG_MAKE_INCLUDED_P
