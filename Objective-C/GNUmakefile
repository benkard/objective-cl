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

USE_LIBFFI = 1

LIBRARY_NAME = libobjcl

RPM_DISABLE_RELOCATABLE = YES
ADDITIONAL_OBJCFLAGS = -Wall -g -DVERSION=\"$(VERSION)\" -I/usr/local/include

libobjcl_OBJC_FILES = libobjcl.m objc_support.m objc-runtime-apple.m objc-runtime-gnu.m
LIBRARIES_DEPEND_UPON = $(FND_LIBS) $(GUI_LIBS) $(OBJC_LIBS) $(SYSTEM_LIBS) $(CONFIG_SYSTEM_LIBS)

ifdef USE_LIBFFI
ADDITIONAL_LDFLAGS = -lffi
ADDITIONAL_OBJCFLAGS += -DUSE_LIBFFI
libobjcl_OBJC_FILES += libffi_support.m
endif

ifneq ($(COMMON_MAKE_LOADED),)
-include $(GNUSTEP_MAKEFILES)/library.make
else  # Mac OS X
libobjcl_OBJ_FILES = $(patsubst %.m,obj/%.o,$(libobjcl_OBJC_FILES))

all: obj/libobjcl.dylib

clean:
	rm -f obj/libobjcl.dylib obj/libobjcl.dylib.$(VERSION) $(libobjcl_OBJ_FILES)

obj/libobjcl.dylib: obj/libobjcl.dylib.$(VERSION)
	ln -s libobjcl.dylib.$(VERSION) $@

obj/libobjcl.dylib.$(VERSION): $(libobjcl_OBJ_FILES)
	make -p obj
	gcc -dynamiclib -current_version $(VERSION) -flat_namespace -undefined warning -install_name libobjcl.dylib.$(VERSION) -o $@ $(libobjcl_OBJ_FILES) -fnext-runtime -framework AppKit -framework Foundation -lm $(ADDITIONAL_LDFLAGS) $(ADDITIONAL_OBJCFLAGS)

obj/%.o: %.m
	mkdir -p obj
	gcc -o $@ -c $< -MMD -MP -DNeXT_Foundation_LIBRARY=1 -DNeXT_GUI_LIBRARY=1 -DNeXT_RUNTIME=1 -dynamic -fno-common -fno-omit-frame-pointer -Wno-import -fno-strict-aliasing -fnext-runtime -I. $(ADDITIONAL_OBJCFLAGS)
endif

