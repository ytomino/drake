export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
else
GNATMAKE=$(TARGET)-gnatmake
endif

EXAMPLES=$(basename $(filter-out b~%,$(wildcard *.adb)))

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

.PHONY: all clean

all: $(BUILDDIR)/adalib/libgnat.a $(EXAMPLES)

$(BUILDDIR)/adalib/libgnat.a: $(ALLSOURCES)
	make -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	-rm $(EXAMPLES) 2> /dev/null

$(BUILDDIR)/%: %.adb $(BUILDDIR)/adalib/libgnat.a
	cd $(BUILDDIR) && $(GNATMAKE) --RTS=. -D . $(MFLAGS) $(CFLAGS) -o ../$@ ../$< -bargs $(BFLAGS) -largs $(LFLAGS)

$(EXAMPLES): %: $(BUILDDIR)/%

clean:
	-rm -rf build*
