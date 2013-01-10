# example for continue:
#   make CONTINUE=aa2010a.ada

export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

ifeq ($(TARGET),$(HOST))
BUILDDIR_NAME=build
else
BUILDDIR_NAME=build-$(TARGET)
endif
BUILDDIR_REL=../$(BUILDDIR_NAME)

SOURCES=$(wildcard ../../source/*.ad?) $(wildcard ../../source/*/*.ad?) $(wildcard ../../source/*/*/*.ad?)

# if RTSDIR is empty, test with GNAT runtime
export RTSDIR=$(abspath $(BUILDDIR_REL))

export ACATSDIR=acats_30
export SUPPORTDIR=support
export TESTDIR=test

MACROSUB=$(SUPPORTDIR)/macrosub
RUN_ACATS=$(SUPPORTDIR)/run_acats

SUPPORT_SOURCES=\
	$(SUPPORTDIR)/check_file.adb \
	$(SUPPORTDIR)/enum_check.adb \
	$(SUPPORTDIR)/b730006_0-child1.adb \
	$(SUPPORTDIR)/f340a000.ads \
	$(SUPPORTDIR)/f340a001.ads \
	$(SUPPORTDIR)/f341a00_0.ads \
	$(SUPPORTDIR)/f390a00.ads \
	$(SUPPORTDIR)/f392a00.ads \
	$(SUPPORTDIR)/f392c00_1.ads \
	$(SUPPORTDIR)/f392d00.ads \
	$(SUPPORTDIR)/f393a00_0.ads \
	$(SUPPORTDIR)/f393b00.ads \
	$(SUPPORTDIR)/f394a00.ads \
	$(SUPPORTDIR)/f3a2a00.ads \
	$(SUPPORTDIR)/f431a00.ads \
	$(SUPPORTDIR)/f433a00.ads \
	$(SUPPORTDIR)/f460a00.ads \
	$(SUPPORTDIR)/f730a000.ads \
	$(SUPPORTDIR)/f730a001.ads \
	$(SUPPORTDIR)/f731a00.ads \
	$(SUPPORTDIR)/f750a00.ads \
	$(SUPPORTDIR)/f750b00.ads \
	$(SUPPORTDIR)/f940a00.ads \
	$(SUPPORTDIR)/f954a00.ads \
	$(SUPPORTDIR)/fa11a00.ads \
	$(SUPPORTDIR)/fa11b00.ads \
	$(SUPPORTDIR)/fa11c00_0.ads \
	$(SUPPORTDIR)/fa11d00.ads \
	$(SUPPORTDIR)/fa13a00_0.ads \
	$(SUPPORTDIR)/fa13b00_0.ads \
	$(SUPPORTDIR)/fa21a00.ads \
	$(SUPPORTDIR)/fb20a00.ads \
	$(SUPPORTDIR)/fb40a00.ads \
	$(SUPPORTDIR)/fc50a00.ads \
	$(SUPPORTDIR)/fc51a00.ads \
	$(SUPPORTDIR)/fc51b00.ads \
	$(SUPPORTDIR)/fc51c00.ads \
	$(SUPPORTDIR)/fc51d00.ads \
	$(SUPPORTDIR)/fc54a00.ads \
	$(SUPPORTDIR)/fc70a00.ads \
	$(SUPPORTDIR)/fc70b00.ads \
	$(SUPPORTDIR)/fc70c00_0.ads \
	$(SUPPORTDIR)/fcndecl.ads \
	$(SUPPORTDIR)/fd72a00.ads \
	$(SUPPORTDIR)/fdb0a00.ads \
	$(SUPPORTDIR)/fdd2a00.ads \
	$(SUPPORTDIR)/fxa5a00.ads \
	$(SUPPORTDIR)/fxaca00.ads \
	$(SUPPORTDIR)/fxacb00.ads \
	$(SUPPORTDIR)/fxacc00.ads \
	$(SUPPORTDIR)/fxc6a00.ads \
	$(SUPPORTDIR)/fxe2a00_0.ads \
	$(SUPPORTDIR)/fxf2a00.ads \
	$(SUPPORTDIR)/fxf3a00.ads \
	$(SUPPORTDIR)/impdef.adb \
	$(SUPPORTDIR)/impdef-annex_c.adb \
	$(SUPPORTDIR)/impdef-annex_d.ads \
	$(SUPPORTDIR)/impdef-annex_e.ads \
	$(SUPPORTDIR)/impdef-annex_g.adb \
	$(SUPPORTDIR)/impdef-annex_h.ads \
	$(SUPPORTDIR)/length_check.adb \
	$(SUPPORTDIR)/macrosub.adb \
	$(SUPPORTDIR)/report.ads \
	$(SUPPORTDIR)/report.adb \
	$(SUPPORTDIR)/spprt13.ads \
	$(SUPPORTDIR)/tctouch.adb \
	$(SUPPORTDIR)/cd300051.o

CONTINUE=
ONCE=
OPTION:=--trial
ifneq ($(CONTINUE),)
OPTION:=$(CONTINUE) ..
endif
ifneq ($(ONCE),)
OPTION:=$(ONCE)
endif

MARGS:=
CARGS:=
BARGS:=
ifneq ($(RTSDIR),)
LIBGNAT=$(BUILDDIR_REL)/adalib/libgnat.a
MARGS:=-a
CARGS:=--RTS=$(RTSDIR)
BARGS:=
endif

.PHONY: test

test: $(LIBGNAT) $(ACATSDIR) $(SUPPORTDIR) $(SUPPORT_SOURCES) $(MACROSUB) $(RUN_ACATS)
	$(RUN_ACATS) $(OPTION)

$(BUILDDIR_REL)/adalib/libgnat.a: $(SOURCES)
	make -C .. $(BUILDDIR_NAME)/adalib/libgnat.a HOST=$(HOST) TARGET=$(TARGET) WITHA=1

$(MACROSUB): $(SUPPORTDIR)/macrosub.adb
	gnatmake -o $@ -D $(SUPPORTDIR) -I$(SUPPORTDIR) $(MARGS) $(CARGS) $< $(BARGS)

$(RUN_ACATS): run_acats.adb
	gnatmake -o $@ -D $(SUPPORTDIR) $(MARGS) $(CARGS) $< $(BARGS)

$(ACATSDIR): acats_30.tar.Z
	mkdir $(ACATSDIR)
	cd $(ACATSDIR) && tar -zxf ../$<

acats_30.tar.Z:
	wget http://www.ada-auth.org/acats-files/3.0/acats_30.tar.Z

$(SUPPORTDIR)/b730006_0-child1.adb: b730006_0-child1.adb
	ln -s ../$< $@

$(SUPPORTDIR)/%.ads: $(ACATSDIR)/support/%.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/f341a00_0.ads: $(ACATSDIR)/support/f341a00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/f392c00_1.ads: $(ACATSDIR)/support/f392c00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/f393a00_0.ads: $(ACATSDIR)/support/f393a00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/fa11c00_0.ads: $(ACATSDIR)/support/fa11c00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/fa13a00_0.ads: $(ACATSDIR)/support/fa13a00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/fa13b00_0.ads: $(ACATSDIR)/support/fa13b00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/fc70c00_0.ads: $(ACATSDIR)/support/fc70c00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/fxe2a00_0.ads: $(ACATSDIR)/support/fxe2a00.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/check_file.adb: $(ACATSDIR)/support/checkfil.ada $(SUPPORTDIR)/report.ads
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/enum_check.adb: $(ACATSDIR)/support/enumchek.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/fcndecl.ads: gcc_support/fcndecl.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef.adb: gcc_support/impdef.a $(SUPPORTDIR)/report.ads
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef-annex_c.adb: $(ACATSDIR)/support/impdefc.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef-annex_d.ads: gcc_support/impdefd.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef-annex_e.ads: gcc_support/impdefe.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef-annex_g.adb: gcc_support/impdefg.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/impdef-annex_h.ads: gcc_support/impdefh.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/length_check.adb: $(ACATSDIR)/support/lencheck.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/macrosub.adb: $(ACATSDIR)/support/macrosub.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/report.ads: $(ACATSDIR)/support/repspec.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/report.adb: $(ACATSDIR)/support/repbody.ada $(SUPPORTDIR)/report.ads
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	cp $@ $@.orig
	patch -p0 < report.adb.patch
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) -gnato $@

# "-gnato" means overflow check required for C43206A

$(SUPPORTDIR)/tctouch.adb: $(ACATSDIR)/support/tctouch.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/spprt13.ads: $(ACATSDIR)/support/spprt13s.tst $(MACROSUB)
	ln -s ../gcc_support/MACRO.DFS $(SUPPORTDIR)/MACRO.DFS
	ln -s ../$(ACATSDIR)/support/spprt13s.tst $(SUPPORTDIR)/spprt13s.tst
	echo spprt13s.tst > $(SUPPORTDIR)/TSTTESTS.DAT
	cd $(SUPPORTDIR) && ./macrosub
	gnatchop --GCC=$(TARGET)-gcc $(SUPPORTDIR)/spprt13s.adt $(SUPPORTDIR)
	rm $(SUPPORTDIR)/TSTTESTS.DAT
	$(TARGET)-gcc -o $(basename $@).o -c $(CARGS) $@

$(SUPPORTDIR)/cd300051.o: $(ACATSDIR)/cd/cd300051.c
	$(TARGET)-gcc -c -o $@ $<

$(SUPPORTDIR):
	mkdir $@

clean:
	-rm -r $(SUPPORTDIR)
	-rm -r $(TESTDIR)
