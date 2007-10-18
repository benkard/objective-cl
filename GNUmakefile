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

include version.make

PACKAGE_NAME = Objective-CL
RPM_DISABLE_RELOCATABLE = YES

SUBPROJECTS = Objective-C

ifneq ($(COMMON_MAKE_LOADED),)
include $(GNUSTEP_MAKEFILES)/aggregate.make
else  # Mac OS X
all:
	make -C Objective-C all

clean:
	make -C Objective-C clean

install:
	make -C Objective-C install
endif

