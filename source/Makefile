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
#   WITHF=      -ffast-math (default:0)
#   WITHI=      if 0, symbolic link instead of install (default:1)
#   WITHL=      link time optimization (default:0)
#   WITHS=      assembly source (default:0)
#   DEBUG=      debug level (default:if TARGET=HOST then 2 else 1)
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

# if DEBUG>0, debug info
ifeq ($(DEBUG),)
ifeq ($(TARGET),$(HOST))
DEBUG=2
else
DEBUG=1
endif
endif

PHASE=

RTSDIR=.
IMPORTDIR=$(RTSDIR)/adainclude
GCCLIBDIR=$(dir $(shell $(GCC) -print-libgcc-file-name))

BUILDDIR=$(RTSDIR)/adalib/build

SUBDIRS=ada83 calendar containers directories distributed environment hash image interfaces nls numerics pools streams strings tasking textio

ifneq ($(findstring darwin,$(TARGET)),) # *-apple-darwin*
EH=zcx
TARGETSUBDIR=machine-apple-darwin
OBJFMT=MACHO
else
ifneq ($(findstring freebsd,$(TARGET)),) # *-pc-freebsd*
EH=zcx
TARGETSUBDIR=machine-pc-freebsd
OBJFMT=ELF
else
ifneq ($(findstring linux-gnu,$(TARGET)),) # *-pc-linux-gnu
EH=zcx
TARGETSUBDIR=machine-pc-linux-gnu
OBJFMT=ELF
else
ifneq ($(findstring mingw,$(TARGET)),) # *-w64-mingw32
ifneq ($(findstring x86_64,$(TARGET)),)
EH=machine-w64-mingw32/x86_64/seh
else
EH=sjlj
endif
TARGETSUBDIR=machine-w64-mingw32
OBJFMT=COFF
else # others
EH=zcx
TARGETSUBDIR=$(TARGET)
OBJFMT=
endif
endif
endif
endif

ifneq ($(findstring x86_64,$(TARGET))$(findstring amd64,$(TARGET)),) # x86_64-*-*
CPUSUBDIR=x86_64
SIMDSUBDIR=sse2
else
ifneq ($(findstring i386,$(TARGET))$(findstring i686,$(TARGET)),) # i386-*-*
CPUSUBDIR=i386
ifneq ($(findstring darwin,$(TARGET)),) # i686-apple-darwin*
SIMDSUBDIR=sse2
else
SIMDSUBDIR=nosimd
endif
else
CPUSUBDIR=generic
SIMDSUBDIR=nosimd
endif
endif

# relation of sub directoris
# (root) + zcx or sjlj -- minimum runtime
# ada83       : calendar stream text_io        # Ada 83 names
# calendar    :                                # support for delay statement, Ada.Calendars, Ada.Execution_Time and Ada.Real_Time
# containers  :                                # Ada.Containers
# directories : calendar                       # Ada.Directories
# distributed :                                # Annex E, Distributed Systems
# hash        : strings                        # Ada.Strings.Hash and Hash_Case_Insensitive
# image       :                                # support for 'Image and 'Value attributes
# interfaces  :                                # Interfaces.C and Interfaces.COBOL
# nls         :                                # System.Native_Encoding
# numerics    :                                # support for float attributes, Ada.Numerics and Interfaces.Fortran
# pools       :                                # support for 'Storage_Size and custom Storage_Pools
# streams     :                                # all *_IO packages (including Text_IO)
# strings     :                                # Ada.Characters and Ada.Strings
# tasking     : calendar                       # support for task and protected syntax and all tasking packages
# text_io     : image numerics streams strings # child packages of Text_IO

SRC=$(wildcard *.ad[sb]) $(wildcard $(TARGETSUBDIR)/*.ad[sb]) $(wildcard $(TARGETSUBDIR)/$(CPUSUBDIR)/*.ad[sb]) $(wildcard required/*.ad[sb]) \
	$(foreach i,$(SUBDIRS),$(wildcard $(i)/*.ad[sb]) $(wildcard $(i)/$(CPUSUBDIR)/*.ad[sb]) $(wildcard $(i)/$(CPUSUBDIR)/$(SIMDSUBDIR)/*.ad[sb]) $(wildcard $(i)/$(TARGETSUBDIR)/*.ad[sb]) $(wildcard $(i)/required/*.ad[sb]) \
	$(wildcard $(EH)/*.ad[sb]))

SEPARATED=$(EH)/s-unrase.adb $(EH)/s-untrse.adb

LIBRARY_SRCS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(SRC)))

ifdef EH # all mechanisms (zcx, sjlj, seh)
EH_OBJS=c-unwind_pe.o
else # unknown mechanism
EH_OBJS=
endif

COMPILER_OBJS= $(addprefix $(BUILDDIR)/, \
	$(EH_OBJS) \
	system.o \
	s-debug.o \
	s-exctab.o \
	s-foadim.o \
	s-format.o \
	s-maccod.o \
	s-natsta.o \
	s-runcon.o \
	s-runinf.o \
	s-shaloc.o \
	s-staall.o \
	s-stalib.o \
	s-startu.o \
	s-stoele.o \
	s-syncon.o \
	s-termin.o \
	s-unstyp.o \
	s-unwfor.o \
	s-unwhan.o \
	s-unwind.o \
	s-unwmap.o \
	s-unwrai.o \
	s-unwrep.o \
	s-unwsta.o \
	s-unwtra.o)

# s-maccod.o and s-stoele.o are also standard library

ifneq ($(WITHA),0)
ALL_OBJS=$(addprefix $(BUILDDIR)/,\
	system.o \
	$(sort $(addsuffix .o,$(basename $(notdir $(filter-out $(SEPARATED),$(SRC)))))))
else
ALL_OBJS=$(COMPILER_OBJS)
endif

ALL_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(basename $(notdir $(ALL_OBJS)))))

ifneq ($(WITHC),0)
ifneq ($(wildcard $(BUILDDIR)/c.o),)
IMPORT_HEADERS=
else
IMPORT_HEADERS=import_objs
endif
else
IMPORT_HEADERS=$(IMPORTDIR)/c.ads
endif

ifneq ($(WITHI),0)
INSTALL=install -m 444
else
INSTALL=ln -s
endif

ifeq ($(DEBUG),0)
CFLAGS_DEBUG=
OPTIMIZE=1
else
ifeq ($(DEBUG),1)
CFLAGS_DEBUG=-ggdb1
OPTIMIZE=1
else
CFLAGS_DEBUG=-ggdb -gnata
OPTIMIZE=0
endif
endif

ifeq ($(OPTIMIZE),0)
CFLAGS_OPTIMIZE=-Og
else
ifeq ($(OBJFMT),ELF)
CFLAGS_OPTIMIZE=-Os -momit-leaf-frame-pointer -gnatB -gnatVn -gnatn -ffunction-sections -fdata-sections
else
CFLAGS_OPTIMIZE=-Os -momit-leaf-frame-pointer -gnatB -gnatVn -gnatn
endif
endif

ifneq ($(WITHF),0)
CFLAGS_MATH=-ffast-math
ADC=fastmath.adc
else
CFLAGS_MATH=
ADC=$(wildcard $(RTSDIR)/adainclude/*.adc)
endif

ifneq ($(WITHL),0)
CFLAGS_LTO=-flto -fwhole-program
else
CFLAGS_LTO=
endif

ifneq ($(ADC),)
CFLAGS_ADC=-gnatec=$(ADC)
else
CFLAGS_ADC=
endif

CFLAGS=

CFLAGS_FOR_COMPILER_UNIT=-pipe -nostdinc -I$(RTSDIR)/adainclude -gnatef -gnatg -gnatwP $(CFLAGS_OPTIMIZE) $(CFLAGS_DEBUG) $(CFLAGS_MATH) $(CFLAGS_ADC) $(CFLAGS_LTO) $(CFLAGS)

.PHONY: all tree import_objs c-archive

all: $(RTSDIR)/adalib/libgnat.a $(ALL_ALI)

tree: \
		$(RTSDIR)/adainclude \
		$(RTSDIR)/adalib \
		$(BUILDDIR) \
		$(RTSDIR)/adainclude/system.ads \
		$(LIBRARY_SRCS) \
		$(IMPORT_HEADERS)

$(RTSDIR)/adalib/libgnat.a: \
		$(RTSDIR)/adainclude \
		$(RTSDIR)/adalib \
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

$(RTSDIR)/adainclude/%.ads: $(TARGETSUBDIR)/$(CPUSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(TARGETSUBDIR)/$(CPUSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: required/%.adb
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

$(RTSDIR)/adainclude/%.ads: calendar/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: calendar/required/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: containers/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: containers/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: directories/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: directories/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: directories/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: directories/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: distributed/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: distributed/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: distributed/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: distributed/required/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: environment/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: environment/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: environment/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: environment/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: hash/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: hash/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: image/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: image/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: image/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: image/required/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: nls/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: nls/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: nls/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: nls/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/$(CPUSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/$(CPUSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/$(CPUSUBDIR)/$(SIMDSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/$(CPUSUBDIR)/$(SIMDSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/required/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: pools/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: pools/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: pools/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: pools/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: pools/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: pools/required/%.adb
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

$(RTSDIR)/adainclude/%.ads: tasking/required/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: tasking/required/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: textio/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: textio/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: $(EH)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(EH)/%.adb
	$(INSTALL) $(abspath $<) $@

ifneq ($(findstring darwin,$(TARGET)),)
ifneq ($(filter -g,$(CFLAGS_DEBUG))$(filter -ggdb,$(CFLAGS_DEBUG)),)
# Apple ld64 can't add line info to anonymous symbol .lsda from following .o
$(BUILDDIR)/a-finali.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder
$(BUILDDIR)/a-stream.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder
endif
endif

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

# separated

$(BUILDDIR)/s-unwrai.o : $(EH)/s-unrase.adb
$(BUILDDIR)/s-unwtra.o : $(EH)/s-untrse.adb

# nested

ifeq ($(PHASE),2)

IMPLIB_SRC=$(wildcard $(IMPORTDIR)/c-*.ad[sb]) $(IMPORTDIR)/c.ads
IMPLIB_ITEMS=$(filter-out c-unwind_pe c-hfs_casetables c-vfs_utfconvdata c-icucore,$(sort $(basename $(notdir $(IMPLIB_SRC)))))
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
	$(MAKE) import_objs PHASE=2

endif

# archiving import headers

c-archive: $(IMPORTDIR)/c.ads
ifneq ($(DESTDIR),)
	-rm $(DESTDIR)/$(TARGET).tar.bz2 2> /dev/null
	cd $(IMPORTDIR) && tar -cjf $(abspath $(DESTDIR)/$(TARGET).tar.bz2) c.ads c-*
else
	@echo please tell DESTDIR=[DIRECTORY]
endif
