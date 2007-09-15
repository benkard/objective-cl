include $(GNUSTEP_MAKEFILES)/common.make

include ../version.make

LIBRARY_NAME = libobjcl

RPM_DISABLE_RELOCATABLE = YES
ADDITIONAL_OBJCFLAGS = -Wall -g -DVERSION=\"$(VERSION)\" -I/usr/local/include

ifdef USE_LIBFFI
ADDITIONAL_LDFLAGS = -lffi
ADDITIONAL_OBJCFLAGS += -DUSE_LIBFFI
else
ADDITIONAL_LDFLAGS = -lavcall
endif

libobjcl_OBJC_FILES = libobjcl.m objc_support.m objc-runtime-apple.m objc-runtime-gnu.m
LIBRARIES_DEPEND_UPON = $(FND_LIBS) $(GUI_LIBS) $(OBJC_LIBS) $(SYSTEM_LIBS) $(CONFIG_SYSTEM_LIBS)

include $(GNUSTEP_MAKEFILES)/library.make
