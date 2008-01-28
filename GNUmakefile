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

include version.make
-include config.make

.PHONY: all clean distclean install

ifeq ($(CONFIG_MAKE_INCLUDED_P),)
all clean install: config.make
	@echo "Please run ./configure before running make."
else # CONFIG_MAKE_INCLUDED_P
-include $(GNUSTEP_MAKEFILES)/common.make

PACKAGE_NAME = Objective-CL
RPM_DISABLE_RELOCATABLE = YES

SUBPROJECTS = Objective-C

ifneq ($(COMMON_MAKE_LOADED),)
include $(GNUSTEP_MAKEFILES)/aggregate.make
before-all before-clean before-install before-distclean:: config.make

after-distclean::
	rm -f config.make
	rm -f config.h
	rm -f config.status config.log
else  # Mac OS X
all:
	$(MAKE) -C Objective-C all

clean:
	$(MAKE) -C Objective-C clean

distclean:
	$(MAKE) -C Objective-C distclean
	rm -f config.make
	rm -f config.h
	rm -f config.status config.log

install:
	$(MAKE) -C Objective-C install
endif # Mac OS X
endif # CONFIG_MAKE_INCLUDED_P

config.make: configure config.make.in
	sh ./configure

configure: configure.ac
	autoreconf || touch configure
