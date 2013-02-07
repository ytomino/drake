export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
else
GNATMAKE=$(TARGET)-gnatmake
endif

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

ifeq ($(findstring darwin,$(TARGET)),)
EXAMPLES:=$(filter-out diff_ucd_hfs,$(EXAMPLES))
ifneq ($(findstring linux,$(TARGET)),)
EXAMPLES:=$(filter-out cal charset cntnr_% cpu directories ext_check ext_doc fixed input2forward mappedio pkgversion process random_mt19937 random_sfmt socket_client stackoverflow storagestream streamcat tasking% textio textionum times tty typedio ucd_%,$(EXAMPLES))
endif
ifneq ($(findstring mingw,$(TARGET)),)
EXAMPLES:=$(filter-out cal charset cntnr_% cpu directories ext_check ext_doc fixed input2forward mappedio pkgversion process random_mt19937 random_sfmt socket_client stackoverflow storagestream streamcat tasking% textio textionum times tty typedio ucd_%,$(EXAMPLES))
endif
endif

ALLSOURCES=$(wildcard ../source/*) $(wildcard ../source/*/*) $(wildcard ../source/*/*/*)

ifeq ($(TARGET),$(HOST))
TARGETSUFFIX=
else
TARGETSUFFIX=-$(TARGET)
endif

BUILDDIR=build$(TARGETSUFFIX)

ifneq ($(wildcard import$(TARGETSUFFIX)),)
IMPORTDIR=$(abspath import$(TARGETSUFFIX))
endif

ifneq ($(IMPORTDIR),)
DRAKEVARS+=IMPORTDIR=$(abspath $(IMPORTDIR))
endif

export WITHA=0
export WITHD=1
export WITHF=0
export WITHI=0
export WITHS=0

ifneq ($(WITHA),0)
MFLAGS=
else
MFLAGS=-a
endif

ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lm -lpthread -lgcc_eh
endif
ifneq ($(findstring linux-gnu,$(TARGET)),)
LFLAGS=-lm -lpthread
endif

ifneq ($(WITHD),0)
CFLAGS=-gnatf -gnatwIj -g -gnata
BFLAGS=-E
else
CFLAGS=-gnatf -gnatwIj
BFLAGS=
ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS=-Wl,-dead_strip
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS+=-Wl,--gc-sections
endif
endif

ifneq ($(WITHF),0)
CFLAGS+=-ffast-math -gnatec=$(abspath ../source/fastmath.adc)
endif

.PHONY: all clean

all: $(BUILDDIR)/adalib/libgnat.a $(EXAMPLES)

$(BUILDDIR)/adalib/libgnat.a: $(ALLSOURCES)
	make -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	touch $@
	-rm $(EXAMPLES) 2> /dev/null

$(BUILDDIR)/%$(EXEEXT): %.adb $(BUILDDIR)/adalib/libgnat.a
	cd $(BUILDDIR) && $(GNATMAKE) --RTS=. -D . $(MFLAGS) $(CFLAGS) -o ../$@ ../$< -bargs $(BFLAGS) -largs $(LFLAGS)

$(EXAMPLES): %: $(BUILDDIR)/%$(EXEEXT)

clean:
	-rm -rf build*
