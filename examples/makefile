HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GNATMAKE=gnatmake
else
GNATMAKE=$(TARGET)-gnatmake
endif

TEST=$(addprefix $(BUILDDIR)/,$(patsubst %.adb,%,$(filter-out b~%,$(wildcard *.adb))))

SOURCES=$(wildcard ../source/*.ad?) $(wildcard ../source/*/*.ad?) $(wildcard ../source/*/*/*.ad?)

ifeq ($(TARGET),$(HOST))
BUILDDIR=build
else
BUILDDIR=build-$(TARGET)
endif

export WITHA=0
export WITHD=1
export WITHS=0

ifneq ($(WITHA),0)
MFLAGS=
else
MFLAGS=-a
endif

ifneq ($(WITHD),0)
CFLAGS=-gnatf -gnatwI -g -gnata
BFLAGS=-E
else
CFLAGS=-gnatf -gnatwI
BFLAGS=
endif

LFLAGS=-lm

.PHONY: all clean

all: $(BUILDDIR)/adalib/libgnat.a $(TEST)

$(BUILDDIR)/adalib/libgnat.a: $(SOURCES)
	make -C ../source RTSDIR=$(abspath $(BUILDDIR))

$(BUILDDIR)/%: %.adb $(BUILDDIR)/adalib/libgnat.a
	-rm $@
	cd $(BUILDDIR) && $(GNATMAKE) --RTS=. -D . $(MFLAGS) $(CFLAGS) -o ../$@ ../$< -bargs $(BFLAGS) -largs $(LFLAGS)

clean:
	-rm -rf build*
