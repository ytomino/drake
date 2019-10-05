# targets:
#   all      building all examples, default target
#   tree     building adainclude only
#   clean    deleting all build directories
#   xfind    finding the declaration of the symbol $X
#   xfindall finding all references of the symbol $X
# variables:
#   GFLAGS=  additional options for all gnattools (default:empty)
#   CFLAGS=  additional options for gcc (default:empty)
#   MFLAGS=  additional options for gnatmake (default:empty)
#   BFLAGS=  additional options for gnatbind (default:empty)
#   LFLAGS=  additional options for gnatlink (default:empty)
#   FFLAGS=  additional options for gnatfind (default:empty)
#   other variables are based on ../source/Makefile

export MAKEFLAGS+=r

export HOST:=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
GCCPREFIX=
else
GCCPREFIX=$(TARGET)-
endif
GCCSUFFIX=
GCC=$(GCCPREFIX)gcc$(GCCSUFFIX)

LIBGCC:=$(shell $(GCC) -print-libgcc-file-name)
LIBGCCDIR=$(patsubst %/,%,$(dir $(LIBGCC)))
LIBVERSION=$(basename $(basename $(notdir $(LIBGCCDIR))))

ifneq ($(and \
	$(findstring linux-gnu,$(HOST)), \
	$(findstring linux-gnu,$(TARGET)), \
	$(filter /usr/lib/gcc/$(TARGET)/%,$(LIBGCCDIR)), \
	$(wildcard /usr/lib/$(TARGET)/libgnat-$(LIBVERSION).so)),) # Debian
LIBGNATBASE=libgnat-$(LIBVERSION)
else
LIBGNATBASE=libgnat
endif

ifneq ($(findstring mingw,$(TARGET)),)
EXEEXT=.exe
else
EXEEXT=
endif

EXAMPLESPREFIX=
EXAMPLES=$(basename $(filter-out b~%,$(notdir $(wildcard $(EXAMPLESPREFIX)*.adb))))

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
export DEBUG=2

ifneq ($(findstring linux-gnu,$(TARGET)),)
LIBGNAT=$(patsubst libgnat%,libgnat_drake%,$(LIBGNATBASE)).a
else
LIBGNAT=$(LIBGNATBASE).a
endif

GFLAGS=
GFLAGS_ALL=--RTS=$(abspath $(BUILDDIR))
CFLAGS=
CFLAGS_ALL=-gnatef -gnatf -gnatwIue
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
endif

ifneq ($(GCC),$(GCCPREFIX)gcc$(GCCSUFFIX))
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

ifeq ($(filter -save-temps%,$(CFLAGS)),)
CFLAGS_ALL+=-pipe
endif

ifneq ($(filter -ffast-math,$(CFLAGS)),)
CFLAGS_ALL+=-gnatec=../source/fastmath.adc
endif

GFLAGS_ALL:=$(strip $(GFLAGS_ALL) $(GFLAGS))
CFLAGS_ALL:=$(strip $(CFLAGS_ALL) $(CFLAGS))
MFLAGS_ALL:=$(strip $(MFLAGS_ALL) $(MFLAGS))
BFLAGS_ALL:=$(strip $(BFLAGS_ALL) $(BFLAGS))
LFLAGS_ALL:=$(strip $(LFLAGS_ALL) $(LFLAGS))
FFLAGS_ALL:=$(strip $(FFLAGS_ALL) $(FFLAGS))

.PHONY: all tree depend cleanup-rts clean cleanall xfind xfindall

all: $(BUILDDIR)/adalib/$(LIBGNAT) $(EXAMPLES)

tree:
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS) tree

depend:
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS) depend

cleanup-rts: # rm duplicated object files in $BUILDDIR and $BUILDDIR/adalib/$LIBGNATBASE.build
	@bash -c 'for I in $$(ls $(BUILDDIR)/adalib/$(LIBGNATBASE).build) ; do \
		B=$${I%.*} ; \
		if [[ -f $(BUILDDIR)/$$B.o ]] ; then \
			echo rm $$B.\* ; \
			rm $(BUILDDIR)/$$B.* ; \
			rm $(BUILDDIR)/adalib/$(LIBGNATBASE).build/$$B.* ; \
			if [[ -f $(BUILDDIR)/adalib/$$B.ali ]] ; then \
				rm -f $(BUILDDIR)/adalib/$$B.ali ; \
			fi ; \
			REMOVED_A=1 ; \
			if [[ $$I =~ ^c- ]] ; then \
				REMOVED_C=1 ; \
			fi ; \
		fi ; \
	done ; \
	if [[ $$REMOVED_C = 1 && -f $(BUILDDIR)/adalib/$(LIBGNATBASE).build/c.o ]] ; then \
		echo rm c.\* ; \
		rm $(BUILDDIR)/adalib/$(LIBGNATBASE).build/c.* ; \
	fi ; \
	if [[ $$REMOVED_A = 1 && -f $(BUILDDIR)/adalib/$(LIBGNAT) ]] ; then \
		echo rm $(LIBGNAT) ; \
		rm $(BUILDDIR)/adalib/$(LIBGNAT) ; \
	fi'

$(BINLN):
	ln -s $(BUILDDIR) $(BINLN)

$(BUILDDIR)/adalib/$(LIBGNAT): $(wildcard $(BUILDDIR)/adainclude/*.ad?) $(wildcard ../source/obj/*.ad?)
	$(MAKE) -C ../source RTSDIR=$(abspath $(BUILDDIR)) $(DRAKEVARS)
	touch $@

$(BUILDDIR)/%$(EXEEXT): $(EXAMPLESPREFIX)%.adb $(BUILDDIR)/adalib/$(LIBGNAT)
	$(GCCPREFIX)gnatmake$(GCCSUFFIX) -c $(EXAMPLESPREFIX)$*.adb $(GFLAGS_ALL) $(MFLAGS_ALL) -cargs $(CFLAGS_ALL)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatbind$(GCCSUFFIX) $*.ali $(GFLAGS_ALL) $(BFLAGS_ALL)
	cd $(BUILDDIR) && $(GCCPREFIX)gnatlink$(GCCSUFFIX) -o ../$@ $*.ali $(GFLAGS_ALL) $(LFLAGS_ALL)

$(BUILDDIR)/bindtimevars$(EXEEXT): BFLAGS_ALL+=-VKEY1=VALUE1 -VKAGI2=ATAI2
$(BUILDDIR)/nop$(EXEEXT): BFLAGS_ALL:=$(filter-out -E,$(BFLAGS_ALL))
$(BUILDDIR)/nosignal$(EXEEXT): LFLAGS_ALL+=$(abspath $(BUILDDIR)/adalib/nosig.o)

$(EXAMPLES): %: $(BUILDDIR)/%$(EXEEXT) $(BINLN)

clean:
	-[ -h "$(BINLN)" ] && rm "$(BINLN)"
	-rm -rf $(BUILDDIR)/adalib
	-find $(BUILDDIR) -mindepth 1 -maxdepth 1 -not -name adainclude | xargs rm -r

cleanall:
	-[ -h bin ] && rm bin
	-rm -rf *.build/adalib
	-rm -r *.build

xfind:
	$(GCCPREFIX)gnatfind$(GCCSUFFIX) -f -aO$(BUILDDIR) $(X) $(GFLAGS_ALL) $(FFLAGS_ALL) | sed 's|^$(PWD)/||'

xfindall: xfind
xfindall: FFLAGS_ALL+=-r
