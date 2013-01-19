# targets:
#   all         building drake runtime (default)
#   import_objs compiling only translated headers for nesting
#   c-archive   archiving translated headers to tar.bz2
# variables:
#   HOST=       host platform (default:gcc -dumpmachine)
#   TARGET=     target platform (default:HOST)
#   RTSDIR=     output directory (default:.)
#   BUILDDIR=   temporary directory (default:RTSDIR/adalib/build)
#   IMPORTDIR=  pre-translated headers (default:generating on make)
#   WITHA=      if 0, building minimum runtime (default:1)
#   WITHC=      if 0, no including translated headers to libgnat.a (default:1)
#   WITHD=      debug info (default:if TARGET=HOST then 1 else 0)
#   WITHF=      -ffast-math (default:0)
#   WITHI=      if 0, symbolic link instead of install (default:1)
#   WITHL=      link time optimization (default:0)
#   WITHS=      assembly source (default:0)
#   CFLAGS=     additional compiler options (default:empty)

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
ifeq ($(TARGET),$(HOST))
GCC=$(dir $(shell which gnatmake))gcc
AR=ar
else
GCC=$(TARGET)-gcc
AR=$(TARGET)-ar
endif

# if WITHA=1, all files be compiled
ifeq ($(WITHA),)
WITHA=1
endif

# if WITHC=1, including c-*.o to libgnat.a
ifeq ($(WITHC),)
WITHC=1
endif

# if WITHD=1, debug info
ifeq ($(WITHD),)
ifeq ($(TARGET),$(HOST))
WITHD=1
else
WITHD=0
endif
endif

# if WITHF=1, -ffast-math
ifeq ($(WITHF),)
WITHF=0
endif

# if WITHL=1, install
ifeq ($(WITHI),)
WITHI=1
endif

# if WITHL=1, link-time optimization be enabled
ifeq ($(WITHL),)
WITHL=0
endif

# if WITHS=1, assembly sources be outputed
ifeq ($(WITHS),)
WITHS=0
endif

# if the format is ELF, ld may accept --gc-sections
ifneq ($(findstring freebsd,$(TARGET)),)
ELF=1
else
ELF=0
endif

PHASE=

RTSDIR=.
IMPORTDIR=$(RTSDIR)/adainclude
GCCLIBDIR=$(dir $(shell $(GCC) -print-libgcc-file-name))

BUILDDIR=$(RTSDIR)/adalib/build

ifneq ($(shell grep -i "ZCX_By_Default.*True" $(GCCLIBDIR)/adainclude/system.ads),)
EH=zcx
else
EH=sjlj
endif

SUBDIRS=ada83 calendar containers directories image interfaces numerics primitives streams strings tasking textio $(EH)

ifneq ($(findstring darwin,$(TARGET)),)
TARGETSUBDIR=univ-apple-darwin
else
ifneq ($(findstring freebsd,$(TARGET)),)
TARGETSUBDIR=univ-pc-freebsd
else
ifneq ($(findstring mingw,$(TARGET)),)
TARGETSUBDIR=univ-w64-mingw
else
TARGETSUBDIR=$(TARGET)
endif
endif
endif

# relation of sub directoris
# (root) + primitives + zcx or sjlj -- minimum runtime
# ada83       : calendar stream text_io        # Ada 83 names
# calendar    :                                # support for delay statement, Ada.Calendars, Ada.Execution_Time and Ada.Real_Time
# containers  :                                # Ada.Containers
# directories : calendar strings               # Ada.Directories
# image       :                                # support for 'Image and 'Value attributes
# interfaces  :                                # Interfaces.C and Interfaces.COBOL
# numerics    :                                # support for float attributes, Ada.Numerics and Interfaces.Fortran
# streams     :                                # all *_IO packages (including Text_IO)
# strings     :                                # Ada.Characters and Ada.Strings
# tasking     : calendar                       # support for task and protected syntax and all tasking packages
# text_io     : image numerics streams strings # child packages of Text_IO

SRC=$(wildcard *.ad?) $(wildcard $(TARGETSUBDIR)/*.ad?) \
	$(foreach i,$(SUBDIRS),$(wildcard $(i)/*.ad?) $(wildcard $(i)/$(TARGETSUBDIR)/*.ad?))

SEPARATED=$(EH)/s-unrase.adb $(EH)/s-untrse.adb

LIBRARY_SRCS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(SRC)))

ifeq ($(EH),zcx)
EH_OBJS=c-unwind_pe.o
else
EH_OBJS=
endif

COMPILER_OBJS= $(addprefix $(BUILDDIR)/, \
	$(EH_OBJS) \
	system.o \
	s-debug.o \
	s-exctab.o \
	s-format.o \
	s-memory.o \
	s-natsta.o \
	s-shaloc.o \
	s-soflin.o \
	s-stalib.o \
	s-termin.o \
	s-unstyp.o \
	s-unwhan.o \
	s-unwind.o \
	s-unwrai.o \
	s-unwsta.o \
	s-unwtra.o)

ifneq ($(WITHA),0)
ALL_OBJS=$(addprefix $(BUILDDIR)/,\
	system.o \
	$(sort $(addsuffix .o,$(basename $(notdir $(filter-out $(SEPARATED),$(SRC)))))))
else
ALL_OBJS=$(COMPILER_OBJS)
endif

ALL_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(basename $(notdir $(ALL_OBJS)))))

ifneq ($(WITHC),0)
IMPORT_HEADERS=import_objs
else
IMPORT_HEADERS=$(IMPORTDIR)/c.ads
endif

ifneq ($(WITHI),0)
INSTALL=install -m 444
else
INSTALL=ln -s
endif

ifneq ($(WITHD),0)
CFLAGS_DEBUG=-g -gnata
CFLAGS_OPTIMIZE=-Os -momit-leaf-frame-pointer
else
CFLAGS_DEBUG=
ifneq ($(ELF),0)
CFLAGS_OPTIMIZE=-Os -momit-leaf-frame-pointer -gnatn -ffunction-sections -fdata-sections
else
CFLAGS_OPTIMIZE=-Os -momit-leaf-frame-pointer -gnatn
endif
endif

ifneq ($(WITHF),0)
CFLAGS_MATH=-ffast-math
else
CFLAGS_MATH=
endif

ifneq ($(WITHL),0)
CFLAGS_LTO=-flto -fwhole-program
else
CFLAGS_LTO=
endif

CFLAGS=

CFLAGS_FOR_COMPILER_UNIT=-nostdinc -I$(RTSDIR)/adainclude -gnatef -gnatg -gnatwP $(CFLAGS_OPTIMIZE) $(CFLAGS_DEBUG) $(CFLAGS_MATH) $(CFLAGS_LTO) $(CFLAGS)

.PHONY: all import_objs c-archive

all: $(RTSDIR)/adalib/libgnat.a $(ALL_ALI)

$(RTSDIR)/adalib/libgnat.a: \
		$(RTSDIR)/adainclude/ \
		$(RTSDIR)/adalib/ \
		$(BUILDDIR) \
		$(RTSDIR)/adainclude/system.ads \
		$(LIBRARY_SRCS) \
		$(IMPORT_HEADERS) \
		$(ALL_OBJS)
	-rm $@ 2> /dev/null
	$(AR) -q $@ $(BUILDDIR)/*.o

$(RTSDIR)/adainclude:
	mkdir -p $@

$(RTSDIR)/adalib:
	mkdir $@

$(BUILDDIR):
	mkdir $@

$(RTSDIR)/adainclude/system.ads:
	ln -s $(GCCLIBDIR)/adainclude/system.ads $@

$(RTSDIR)/adainclude/%.ads: %.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: %.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: $(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: ada83/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: ada83/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: calendar/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: calendar/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: calendar/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: calendar/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: containers/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: containers/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: directories/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: directories/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: image/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: image/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: primitives/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: primitives/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: streams/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: streams/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: streams/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: streams/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: strings/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: strings/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: tasking/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: tasking/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: tasking/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: tasking/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: textio/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: textio/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: $(EH)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(EH)/%.adb
	$(INSTALL) $(abspath $<) $@

$(BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.adb $(RTSDIR)/adainclude/%.ads
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<
ifneq ($(WITHS),0)
	cd $(dir $@) && $(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -S $<
endif

$(BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.ads
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<
ifneq ($(WITHS),0)
	cd $(dir $@) && $(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -S $<
endif

$(RTSDIR)/adalib/%.ali: $(BUILDDIR)/%.o
	install -m 444 $(basename $<).ali $@

ifeq ($(IMPORTDIR),$(RTSDIR)/adainclude)
$(IMPORTDIR)/c.ads: import.h
	headmaster -t ada -p -D $(IMPORTDIR) -gcc="$(GCC)" $<
endif

# nested

ifeq ($(PHASE),2)

IMPLIB_SRC=$(wildcard $(IMPORTDIR)/c-*.ad?) $(IMPORTDIR)/c.ads
IMPLIB_ITEMS=$(filter-out c-unwind_pe c-hfs_casetables,$(sort $(basename $(notdir $(IMPLIB_SRC)))))
IMPLIB_OBJS=$(addsuffix .o,$(addprefix $(BUILDDIR)/,$(IMPLIB_ITEMS)))
IMPLIB_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(IMPLIB_ITEMS)))

ifeq ($(IMPORTDIR),$(RTSDIR)/adainclude)
IMPLIB_LINKS=
else

IMPLIB_LINKS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(IMPLIB_SRC)))

$(RTSDIR)/adainclude/%.ads: $(IMPORTDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(IMPORTDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

endif

import_objs: $(IMPLIB_LINKS) $(IMPLIB_OBJS) $(IMPLIB_ALI)

else

# make will expand wildcard at first, generated file may not be included

import_objs: $(IMPORTDIR)/c.ads
	make import_objs PHASE=2

endif

# archiving import headers

c-archive: $(IMPORTDIR)/c.ads
ifneq ($(DESTDIR),)
	-rm $(DESTDIR)/import-$(TARGET).tar.bz2 2> /dev/null
	cd $(IMPORTDIR) && tar -cjf $(abspath $(DESTDIR)/import-$(TARGET).tar.bz2) c.ads c-*
else
	@echo please tell DESTDIR=[DIRECTORY]
endif
