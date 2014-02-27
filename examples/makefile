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
endif

ALLSOURCES=$(wildcard ../source/*) $(wildcard ../source/*/*) $(wildcard ../source/*/*/*)

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
export WITHS=0
export DEBUG=2

ifneq ($(WITHA),0)
MFLAGS=
else
MFLAGS=-a
endif

ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS=-licucore
endif
ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS=-lm -lpthread -liconv -lgcc_eh
endif
ifneq ($(findstring linux-gnu,$(TARGET)),)
LFLAGS=-lm -lpthread -ldl
endif

ifneq ($(DEBUG),0)
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

all: $(BINLN) $(BUILDDIR)/adalib/libgnat.a $(EXAMPLES)

$(BINLN):
	ln -s $(BUILDDIR) $(BINLN)

$(BUILDDIR)/adalib/libgnat.a: $(ALLSOURCES)
	make -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	touch $@
	-rm $(EXAMPLES) 2> /dev/null

$(BUILDDIR)/%$(EXEEXT): %.adb $(BUILDDIR)/adalib/libgnat.a
	cd $(BUILDDIR) && $(GNATMAKE) --RTS=. -D . $(MFLAGS) $(CFLAGS) -o ../$@ ../$< -bargs $(BFLAGS) -largs $(LFLAGS)

$(EXAMPLES): %: $(BUILDDIR)/%$(EXEEXT)

clean:
	-rm -rf $(BINLN) *.build
