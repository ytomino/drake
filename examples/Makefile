# targets:
#   all      building all examples (default)
#   tree     building only adainclude
#   clean    deleting all built files
#   xfind    finding the declaration of the symbol $(X)
#   xfindall finding all references of the symbol $(X)
# variables:
#   GFLAGS= global
#   CFLAGS= gcc (compiling only)
#   MFLAGS= gnatmake
#   BFLAGS= gnatbind
#   LFLAGS= gnatlink
#   FFLAGS= gnatfind

export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATPREFIX=
else
GNATPREFIX=$(TARGET)-
endif

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

ifneq ($(TARGET),$(HOST))
EXAMPLES:=$(filter-out exception_cpp,$(EXAMPLES))
endif
ifeq ($(findstring darwin,$(TARGET)),)
EXAMPLES:=$(filter-out diff_ucd_hfs,$(EXAMPLES))
endif

ifeq ($(TARGET),$(HOST))
BINLN=bin
else
BINLN=
endif

BUILDDIR=$(TARGET).build

ifneq ($(wildcard import/$(TARGET)),)
IMPORTDIR=$(abspath import/$(TARGET))
endif

ifneq ($(IMPORTDIR),)
DRAKEVARS+=IMPORTDIR=$(abspath $(IMPORTDIR))
endif

export WITHA=0
export WITHF=0
export WITHI=0
export DEBUG=2

GFLAGS=--RTS=$(abspath $(BUILDDIR))
MFLAGS=-D $(BUILDDIR)

ifeq ($(WITHA),0)
MFLAGS+=-a
endif

CFLAGS=
CFLAGS_FOR_EXAMPLE=-pipe -gnatef -gnatf -gnatwI
LFLAGS=
LFLAGS_FOR_EXAMPLE=

ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS_FOR_EXAMPLE+=-licucore
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS_FOR_EXAMPLE+=-lm -lpthread -liconv -lgcc_eh
endif
ifneq ($(findstring linux-gnu,$(TARGET)),)
LFLAGS_FOR_EXAMPLE+=-lm -lpthread -ldl
endif

ifneq ($(GCC),)
MFLAGS+=--GCC="$(GCC)"
LFLAGS_FOR_EXAMPLE+=--GCC="$(GCC)"
endif

ifneq ($(DEBUG),0)
CFLAGS_FOR_EXAMPLE+=-ggdb -gnata -gnat-p
BFLAGS=-E
LFLAGS_FOR_EXAMPLE+=-ggdb
else
BFLAGS=
ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS_FOR_EXAMPLE=-Wl,-dead_strip
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS_FOR_EXAMPLE+=-Wl,--gc-sections
endif
endif

ifneq ($(WITHF),0)
CFLAGS_FOR_EXAMPLE+=-ffast-math -gnatec=$(abspath ../source/fastmath.adc)
endif

CFLAGS_FOR_EXAMPLE+=$(CFLAGS)
LFLAGS_FOR_EXAMPLE+=$(LFLAGS)

.PHONY: all tree clean xfind xfindall

all: $(BUILDDIR)/adalib/libgnat.a $(EXAMPLES)

tree:
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS) tree

$(BINLN):
	ln -s $(BUILDDIR) $(BINLN)

$(BUILDDIR)/adalib/libgnat.a: $(wildcard $(BUILDDIR)/adainclude/*.ad?)
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	touch $@

$(BUILDDIR)/%$(EXEEXT): %.adb $(BUILDDIR)/adalib/libgnat.a
	$(GNATPREFIX)gnatmake -c $< $(GFLAGS) $(MFLAGS) -cargs $(CFLAGS_FOR_EXAMPLE)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatbind $(basename $(notdir $<)).ali $(GFLAGS) $(BFLAGS)
	cd $(BUILDDIR) && $(GNATPREFIX)gnatlink -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS) $(LFLAGS_FOR_EXAMPLE)

$(EXAMPLES): %: $(BUILDDIR)/%$(EXEEXT) $(BINLN)

clean:
	-rm -rf $(BINLN) *.build

xfind:
	gnatfind -f -aO$(BUILDDIR) $(X) $(GFLAGS) $(FFLAGS) | sed 's/^$(subst /,\/,$(PWD))\///'

xfindall: FFLAGS+=-r
xfindall: xfind
