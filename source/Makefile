# targets:
#   all         building drake runtime (default target)
#   tree        building only adainclude
#   import_objs compiling only translated headers for nesting
#   c-archive   archiving translated headers to tar.bz2
# variables:
#   HOST=       host platform (default:`gcc -dumpmachine`)
#   TARGET=     target platform (default:$HOST)
#   IMPORTDIR=  pre-translated headers (default:empty, generating on make)
#   RTSDIR=     output directory (default:.)
#   BUILDDIR=   temporary directory (default:$RTSDIR/adalib)
#   GCC=        compiler (default:$TARGET-gcc)
#   INSTALL=    copy source files to adainclude (default:install -m 444)
#   EH=         mechanism of exception handling (default:selected by $TARGET)
#   WITHA=      if 0, building minimum runtime (default:1)
#   WITHC=      if 0, no including translated headers to libgnat.a (default:1)
#   WITHF=      -ffast-math (default:0)
#   WITHL=      link time optimization (default:0)
#   DEBUG=      debug level (default:if TARGET=HOST then 2 else 1)
#   CFLAGS=     additional options for gcc (default:empty)

export MAKEFLAGS+=r

HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
ifeq ($(TARGET),$(HOST))
GCC=gcc
AR=ar
else
GCC=$(TARGET)-gcc
AR=$(TARGET)-ar
endif

LIBGCCDIR=$(dir $(shell $(GCC) -print-libgcc-file-name))

ifeq ($(INSTALL),)
INSTALL=install -m 444
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

# if WITHL=1, link-time optimization be enabled
ifeq ($(WITHL),)
WITHL=0
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

BUILDDIR=$(RTSDIR)/adalib
LIBGNAT_BUILDDIR=$(BUILDDIR)/libgnat.build
NOSIG_BUILDDIR=$(BUILDDIR)/nosig.build
NOTB_BUILDDIR=$(BUILDDIR)/notb.build

SUBDIRS=ada83 calendar containers directories distributed environment hash image interfaces nls numerics pools streams strings tasking textio

ifneq ($(findstring darwin,$(TARGET)),) # *-apple-darwin*
EH=zcx
TARGETSUBDIR=machine-apple-darwin
OBJFMT=MACHO
else ifneq ($(findstring freebsd,$(TARGET)),) # *-pc-freebsd*
EH=zcx
TARGETSUBDIR=machine-pc-freebsd
OBJFMT=ELF
else ifneq ($(findstring linux-gnu,$(TARGET)),) # *-pc-linux-gnu
EH=zcx
TARGETSUBDIR=machine-pc-linux-gnu
OBJFMT=ELF
else ifneq ($(findstring mingw,$(TARGET)),) # *-w64-mingw32
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

ifneq ($(findstring x86_64,$(TARGET))$(findstring amd64,$(TARGET)),) # x86_64-*-*
CPUSUBDIR=x86_64
SIMDSUBDIR=sse2
else ifneq ($(findstring i386,$(TARGET))$(findstring i686,$(TARGET)),) # i386-*-*
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

# relation of sub directoris
# (root) + zcx or sjlj -- minimum runtime
# ada83       : calendar stream text_io        # Ada 83 names
# calendar    :                                # support for delay statement, Ada.Calendars, Ada.Execution_Time and Ada.Real_Time
# containers  :                                # Ada.Containers
# directories : calendar                       # Ada.Directories
# distributed : environment streams            # Annex E, Distributed Systems
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

LIBRARY_SRCS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(SRC)))

ifdef EH # all mechanisms (zcx, sjlj, seh)
EH_OBJS=c-unwind_pe.o
else # unknown mechanism
EH_OBJS=
endif

COMPILER_OBJS=$(addprefix $(LIBGNAT_BUILDDIR)/, \
	$(EH_OBJS) \
	s-debug.o \
	s-exctab.o \
	s-finall.o \
	s-foradd.o \
	s-format.o \
	s-maccod.o \
	s-natall.o \
	s-natsta.o \
	s-runcon.o \
	s-staall.o \
	s-stalib.o \
	s-startu.o \
	s-stoele.o \
	s-stomap.o \
	s-syncon.o \
	s-termin.o \
	s-unstyp.o \
	s-unwbac.o \
	s-unwfor.o \
	s-unwhan.o \
	s-unwind.o \
	s-unwmap.o \
	s-unwocc.o \
	s-unwrai.o \
	s-unwrep.o \
	s-unwsea.o \
	s-unwsta.o \
	system.o)

EXTRA_OBJS=$(addprefix $(RTSDIR)/adalib/, \
	nosig.o)
ifneq ($(findstring darwin,$(TARGET)),)
EXTRA_OBJS+=$(RTSDIR)/adalib/notb.o
endif

# s-maccod.o and s-stoele.o are also standard library

ifneq ($(WITHA),0)
ALL_OBJS=$(addprefix $(LIBGNAT_BUILDDIR)/, \
	$(sort $(addsuffix .o,$(basename $(notdir $(filter %.ads,$(SRC)))))) \
	system.o)
else
ALL_OBJS=$(COMPILER_OBJS)
endif

ALL_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(basename $(notdir $(ALL_OBJS)))))

ifneq ($(WITHC),0)
ifneq ($(wildcard $(LIBGNAT_BUILDDIR)/c.o),)
IMPORT_HEADERS=
else
IMPORT_HEADERS=import_objs
endif
else
IMPORT_HEADERS=$(IMPORTDIR)/c.ads
endif

# files to be compiled with -O2 instead of -Os
SPEED_OBJS=$(addprefix $(LIBGNAT_BUILDDIR)/, \
	s-arit64.o \
	s-boarop.o \
	s-carsi8.o \
	s-carun8.o \
	s-casi16.o \
	s-casi32.o \
	s-casi64.o \
	s-caun16.o \
	s-caun32.o \
	s-caun64.o \
	s-exnint.o \
	s-exnlli.o \
	s-expint.o \
	s-explli.o \
	s-expllu.o \
	s-expmod.o \
	s-expuns.o)

# in numerics
SPEED_OBJS+=$(addprefix $(LIBGNAT_BUILDDIR)/, \
	a-nlcoar.o \
	a-nllcar.o \
	a-nllrar.o \
	a-nlrear.o \
	a-nscoar.o \
	a-nsrear.o \
	a-nucoar.o \
	a-nummt1.o \
	a-nurear.o \
	a-nusf19.o \
	a-nusf21.o)

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
ifeq ($(EH),sjlj)
CFLAGS_DEBUG+=-fno-omit-frame-pointer
endif
endif
ifeq ($(OBJFMT),ELF)
CFLAGS_DEBUG+=-Wa,--compress-debug-sections
endif
endif

ifeq ($(OPTIMIZE),0)
CFLAGS_OPTIMIZE=-Og
else
ifeq ($(OBJFMT),ELF)
CFLAGS_OPTIMIZE=-Os -gnatB -gnatVn -gnatn -gnatp -ffunction-sections -fdata-sections
else
CFLAGS_OPTIMIZE=-Os -gnatB -gnatVn -gnatn -gnatp
endif
endif

ADC=$(wildcard $(RTSDIR)/adainclude/*.adc)
ifneq ($(WITHF),0)
CFLAGS_MATH=-ffast-math
ADC+=fastmath.adc
else
CFLAGS_MATH=
endif

ifneq ($(WITHL),0)
CFLAGS_LTO=-flto -fwhole-program
else
CFLAGS_LTO=
endif

ifneq ($(ADC),)
CFLAGS_ADC=$(addprefix -gnatec=,$(ADC))
else
CFLAGS_ADC=
endif

CFLAGS=

CFLAGS_FOR_COMPILER_UNIT=$(strip \
	-pipe -nostdinc -I$(RTSDIR)/adainclude -gnatef -gnatg -gnatwP \
	$(CFLAGS_OPTIMIZE) $(CFLAGS_DEBUG) $(CFLAGS_MATH) $(CFLAGS_ADC) $(CFLAGS_LTO) $(CFLAGS))

TREE= \
	$(RTSDIR)/adainclude/system.ads \
	$(LIBRARY_SRCS) \
	$(IMPORT_HEADERS)

.PHONY: all tree import_objs c-archive

all: $(EXTRA_OBJS) $(RTSDIR)/adalib/libgnat.a $(ALL_ALI)

tree: $(TREE)

$(RTSDIR)/adalib/libgnat.a: $(TREE) $(ALL_OBJS) | $(RTSDIR)/adalib/
	-rm $@ 2> /dev/null
	$(AR) -q $@ $(LIBGNAT_BUILDDIR)/*.o

$(RTSDIR)/adalib/nosig.o: $(TREE) $(NOSIG_BUILDDIR)/nosig.o | $(RTSDIR)/adalib/
	install -m 444 $(NOSIG_BUILDDIR)/nosig.o $@

$(RTSDIR)/adalib/notb.o: $(TREE) $(NOTB_BUILDDIR)/notb.o | $(RTSDIR)/adalib/
	install -m 444 $(NOTB_BUILDDIR)/notb.o $@

$(RTSDIR)/adainclude:
	mkdir -p $@

$(RTSDIR)/adalib:
	mkdir -p $@

$(LIBGNAT_BUILDDIR):
	mkdir -p $@

$(NOSIG_BUILDDIR):
	mkdir -p $@

$(NOTB_BUILDDIR):
	mkdir -p $@

$(RTSDIR)/adainclude/system.ads: | $(LIBGCCDIR)/adainclude/system.ads $(RTSDIR)/adainclude
	ln -s $(LIBGCCDIR)/adainclude/system.ads $@

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

$(RTSDIR)/adainclude/%.ads: distributed/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: distributed/$(TARGETSUBDIR)/%.adb
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

$(RTSDIR)/adainclude/%.adb: textio/$(TARGETSUBDIR)/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: textio/$(TARGETSUBDIR)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: textio/%.adb
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: $(EH)/%.ads
	$(INSTALL) $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(EH)/%.adb
	$(INSTALL) $(abspath $<) $@

# optimize for speed instead of size
$(SPEED_OBJS): CFLAGS_OPTIMIZE:=$(subst -Os,-O2,$(CFLAGS_OPTIMIZE))

# keep the order from AAA to ZZZ
$(LIBGNAT_BUILDDIR)/s-unwrai.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder

ifneq ($(findstring darwin,$(TARGET)),)
ifneq ($(filter -g,$(CFLAGS_DEBUG))$(filter -ggdb,$(CFLAGS_DEBUG)),)
# Apple ld64 can't add line info to anonymous symbol .lsda from following .o
$(LIBGNAT_BUILDDIR)/a-excfin.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder
$(LIBGNAT_BUILDDIR)/a-finali.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder
$(LIBGNAT_BUILDDIR)/a-stream.o: CFLAGS_OPTIMIZE+=-fno-toplevel-reorder
endif
endif

$(LIBGNAT_BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.adb $(RTSDIR)/adainclude/%.ads | $(LIBGNAT_BUILDDIR)
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<

$(LIBGNAT_BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.ads | $(LIBGNAT_BUILDDIR)
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<

$(NOSIG_BUILDDIR)/nosig.o: obj/nosig.ads | $(NOSIG_BUILDDIR)
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $(abspath $<)

$(NOTB_BUILDDIR)/notb.o: obj/notb.ads | $(NOTB_BUILDDIR)
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $(abspath $<)

$(RTSDIR)/adalib/%.ali: $(LIBGNAT_BUILDDIR)/%.o | $(RTSDIR)/adalib
	install -m 444 $(basename $<).ali $@

ifeq ($(IMPORTDIR),$(RTSDIR)/adainclude)
$(IMPORTDIR)/c.ads: import.h
	headmaster -t ada -p -D $(IMPORTDIR) -gcc="$(GCC)" $<
endif

# separated

$(LIBGNAT_BUILDDIR)/s-unwbac.o : $(EH)/s-unbase.adb
$(LIBGNAT_BUILDDIR)/s-unwocc.o : $(EH)/s-unocse.adb
$(LIBGNAT_BUILDDIR)/s-unwrai.o : $(EH)/s-unrase.adb

# nested

ifeq ($(PHASE),2)

IMPLIB_SRC=$(wildcard $(IMPORTDIR)/c-*.ad[sb]) $(IMPORTDIR)/c.ads
IMPLIB_ITEMS=$(filter-out c-unwind_pe c-hfs_casetables c-vfs_utfconvdata c-icucore,$(basename $(notdir $(filter %.ads,$(IMPLIB_SRC)))))
IMPLIB_OBJS=$(addsuffix .o,$(addprefix $(LIBGNAT_BUILDDIR)/,$(IMPLIB_ITEMS)))
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
	cd $(IMPORTDIR) && tar -cjf $(abspath $(DESTDIR)/$(TARGET).tar.bz2) c-*.ad[sb] c.ads
else
	@echo please tell DESTDIR=[DIRECTORY]
endif
