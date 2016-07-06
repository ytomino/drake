# targets:
#   all      building all examples (default target)
#   tree     building only adainclude
#   clean    deleting all built files
#   xfind    finding the declaration of the symbol $X
#   xfindall finding all references of the symbol $X
# variables:
#   HOST, TARGET, IMPORTDIR, GCC are based on ../source/Makefile
#   GFLAGS= additional options for all gnattools (default:empty)
#   CFLAGS= additional options for gcc (default:empty)
#   MFLAGS= additional options for gnatmake (default:empty)
#   BFLAGS= additional options for gnatbind (default:empty)
#   LFLAGS= additional options for gnatlink (default:empty)
#   FFLAGS= additional options for gnatfind (default:empty)

export MAKEFLAGS+=r

export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GCCPREFIX=
else
GCCPREFIX=$(TARGET)-
endif
GCCSUFFIX=

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
IMPORTDIR=import/$(TARGET)
endif

ifneq ($(IMPORTDIR),)
DRAKEVARS+=IMPORTDIR=$(abspath $(IMPORTDIR))
endif

export INSTALL=ln -s
export WITHA=0
export WITHF=0
export DEBUG=2

GFLAGS=
GFLAGS_ALL=--RTS=$(abspath $(BUILDDIR))
CFLAGS=
CFLAGS_ALL=-pipe -gnatef -gnatf -gnatwI
MFLAGS=
MFLAGS_ALL=-D $(BUILDDIR)
BFLAGS=
BFLAGS_ALL=
LFLAGS=
LFLAGS_ALL=
FFLAGS=
FFLAGS_ALL=

ifeq ($(WITHA),0)
MFLAGS_ALL+=-a
endif

ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS_ALL+=-licucore
else ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS_ALL+=-lm -lpthread -liconv -lgcc_eh
else ifneq ($(findstring linux-gnu,$(TARGET)),)
LFLAGS_ALL+=-lm -lpthread -ldl -lrt
endif

GCC=
ifneq ($(GCC),)
MFLAGS_ALL+=--GCC="$(GCC)"
LFLAGS_ALL+=--GCC="$(GCC)"
endif

ifneq ($(DEBUG),0)
CFLAGS_ALL+=-ggdb -gnata -gnat-p
BFLAGS_ALL+=-E
LFLAGS_ALL+=-ggdb
else
ifneq ($(findstring darwin,$(TARGET)),)
LFLAGS_ALL+=-Wl,-dead_strip
else ifneq ($(findstring freebsd,$(TARGET)),)
LFLAGS_ALL+=-Wl,--gc-sections
endif
endif

ifneq ($(WITHF),0)
CFLAGS_ALL+=-ffast-math -gnatec=../source/fastmath.adc
endif

GFLAGS_ALL:=$(strip $(GFLAGS_ALL) $(GFLAGS))
CFLAGS_ALL:=$(strip $(CFLAGS_ALL) $(CFLAGS))
MFLAGS_ALL:=$(strip $(MFLAGS_ALL) $(MFLAGS))
BFLAGS_ALL:=$(strip $(BFLAGS_ALL) $(BFLAGS))
LFLAGS_ALL:=$(strip $(LFLAGS_ALL) $(LFLAGS))
FFLAGS_ALL:=$(strip $(FFLAGS_ALL) $(FFLAGS))

.PHONY: all tree cleanup-rts clean xfind xfindall

all: $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a $(EXAMPLES)

tree:
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS) tree

cleanup-rts: # rm duplicated object files in $(BUILDDIR) and $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a
	@bash -c 'for I in $$(ls $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).build) ; do \
		if [[ -f $(BUILDDIR)/$$I ]] ; then \
			echo rm $$I ; \
			rm $(BUILDDIR)/$$I ; \
			rm $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).build/$$I ; \
			if [[ -f $(BUILDDIR)/adalib/$$I ]] ; then \
				rm -f $(BUILDDIR)/adalib/$$I ; \
			fi ; \
			REMOVED_A=1 ; \
			if [[ $$I =~ ^c- ]] ; then \
				REMOVED_C=1 ; \
			fi ; \
		fi ; \
	done ; \
	if [[ $$REMOVED_C = 1 && -f $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).build/c.o ]] ; then \
		echo rm c.o ; \
		rm $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).build/c.o ; \
	fi ; \
	if [[ $$REMOVED_A = 1 && -f $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a ]] ; then \
		echo rm libgnat$(GCCSUFFIX).a ; \
		rm $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a ; \
	fi'

$(BINLN):
	ln -s $(BUILDDIR) $(BINLN)

$(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a: $(wildcard $(BUILDDIR)/adainclude/*.ad?) $(wildcard ../source/obj/*.ad?)
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	touch $@

$(BUILDDIR)/%$(EXEEXT): %.adb $(BUILDDIR)/adalib/libgnat$(GCCSUFFIX).a
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c $< $(GFLAGS_ALL) $(MFLAGS_ALL) -cargs $(CFLAGS_ALL)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $(basename $(notdir $<)).ali $(GFLAGS_ALL) $(BFLAGS_ALL)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $(basename $(notdir $<)).ali $(GFLAGS_ALL) $(LFLAGS_ALL)

$(BUILDDIR)/nop$(EXEEXT): BFLAGS_ALL:=$(filter-out -E,$(BFLAGS_ALL))
$(BUILDDIR)/nosignal$(EXEEXT): LFLAGS_ALL+=$(abspath $(BUILDDIR)/adalib/nosig.o)

$(EXAMPLES): %: $(BUILDDIR)/%$(EXEEXT) $(BINLN)

clean:
	-rm -rf $(BINLN) *.build

xfind:
	$(GCCPREFIX)gnatfind$(GCCSUFFIX) -f -aO$(BUILDDIR) $(X) $(GFLAGS_ALL) $(FFLAGS_ALL) | sed 's|^$(PWD)/||'

xfindall: xfind
xfindall: FFLAGS_ALL+=-r
