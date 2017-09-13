# targets:
#   test      run the tests, default target
#   setup     building support files only
# variables:
#   CONTINUE= continuing from it
#   ONCE=     testing it only
#   RTSDIR=   runtime, use GNAT runtime if empty (default:../$TARGET.build)
# examples:
#   make                  # run the all tests
#   make CONTINUE=aa2010a # continue the tests from aa2010a
#   make ONCE=aa2010a     # run aa2010a only
#   make RTSDIR=          # run the all tests with GNAT runtime

export HOST=$(shell gcc -dumpmachine)
export TARGET=$(HOST)

SOURCES=$(wildcard ../../source/*.ad?) $(wildcard ../../source/*/*.ad?) $(wildcard ../../source/*/*/*.ad?)

# if $RTSDIR is empty, test with GNAT runtime
export RTSDIR=$(abspath ../$(TARGET).build)

ACATSARCHIVE=acats_31.tar.Z

export ACATSDIR=acats_31
export SUPPORTDIR=support.build
export TESTDIR=test.build

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
	$(SUPPORTDIR)/f3a1a00.ads \
	$(SUPPORTDIR)/f3a2a00.ads \
	$(SUPPORTDIR)/f431a00.ads \
	$(SUPPORTDIR)/f433a00.ads \
	$(SUPPORTDIR)/f460a00.ads \
	$(SUPPORTDIR)/f650a00.ads \
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

ifneq ($(RTSDIR),)
LIBGNAT=../$(TARGET).build/adalib/libgnat.a
GFLAGS=--RTS=../$(TARGET).build
CFLAGS=-pipe -ggdb -gnata
H_GFLAGS=--RTS=../$(HOST).build
H_CFLAGS=-pipe -ggdb -gnata -gnatp-p
H_MFLAGS=-a -D ../$(HOST).build -aI$(SUPPORTDIR)
H_BFLAGS=
H_LFLAGS=
else # for GNAT runtime
LIBGNAT=
GFLAGS=
CFLAGS=-pipe -ggdb -gnata
H_GFLAGS=
H_CFLAGS=-pipe
H_MFLAGS=-D $(SUPPORTDIR) -aI$(SUPPORTDIR)
H_BFLAGS=
H_LFLAGS=
endif

.PHONY: test setup

test: setup $(LIBGNAT) $(RUN_ACATS)
	$(RUN_ACATS) $(OPTION)

setup: $(ACATSDIR) $(SUPPORTDIR) $(SUPPORT_SOURCES) $(MACROSUB)

../$(TARGET).build/adalib/libgnat.a: $(SOURCES)
	make -C .. $(TARGET).build/adalib/libgnat.a HOST=$(HOST) TARGET=$(TARGET) WITHA=1

$(MACROSUB): $(SUPPORTDIR)/macrosub.adb
	gnatmake -o $@ $(H_GFLAGS) $(H_MFLAGS) $< -cargs $(H_CFLAGS) -bargs $(H_BFLAGS) -largs $(H_LFLAGS)

$(RUN_ACATS): run_acats.adb | $(SUPPORTDIR)
	gnatmake -o $@ $(H_GFLAGS) $(H_MFLAGS) $< -cargs $(H_CFLAGS) -bargs $(H_BFLAGS) -largs $(H_LFLAGS)

$(ACATSDIR): $(ACATSARCHIVE)
	mkdir $(ACATSDIR)
	cd $(ACATSDIR) && tar -zxf ../$<

$(ACATSARCHIVE):
	wget http://www.ada-auth.org/acats-files/3.1/$(ACATSARCHIVE)

$(SUPPORTDIR)/b730006_0-child1.adb: b730006_0-child1.adb
	ln -s ../$< $@

$(SUPPORTDIR)/%.ads: $(ACATSDIR)/support/%.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/f340a000.ads: $(ACATSDIR)/support/f340a000.a
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
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/enum_check.adb: $(ACATSDIR)/support/enumchek.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/fcndecl.ads: support/fcndecl.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef.adb: support/impdef.a $(SUPPORTDIR)/report.ads
	gnatchop -w --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	sed -i -e 's|\"cd300051\.o\"|\"$(abspath $(SUPPORTDIR))/cd300051\.o\"|' $(SUPPORTDIR)/impdef.ads
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef-annex_c.adb: support/impdefc.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef-annex_d.ads: support/impdefd.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef-annex_e.ads: support/impdefe.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef-annex_g.adb: support/impdefg.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/impdef-annex_h.ads: support/impdefh.a
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/length_check.adb: $(ACATSDIR)/support/lencheck.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/macrosub.adb: $(ACATSDIR)/support/macrosub.ada | $(SUPPORTDIR)
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/report.ads: $(ACATSDIR)/support/repspec.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)

$(SUPPORTDIR)/report.adb: $(ACATSDIR)/support/repbody.ada $(SUPPORTDIR)/report.ads
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	cp $@ $@.orig
	patch -p0 < report.adb.diff
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) -gnato $@

# "-gnato" means overflow check required for C43206A

$(SUPPORTDIR)/tctouch.adb: $(ACATSDIR)/support/tctouch.ada
	gnatchop --GCC=$(TARGET)-gcc $< $(SUPPORTDIR)
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/spprt13.ads: $(ACATSDIR)/support/spprt13s.tst $(MACROSUB)
	ln -s ../support/MACRO.DFS $(SUPPORTDIR)/MACRO.DFS
	ln -s ../$(ACATSDIR)/support/spprt13s.tst $(SUPPORTDIR)/spprt13s.tst
	echo spprt13s.tst > $(SUPPORTDIR)/TSTTESTS.DAT
	cd $(SUPPORTDIR) && ./macrosub
	gnatchop --GCC=$(TARGET)-gcc $(SUPPORTDIR)/spprt13s.adt $(SUPPORTDIR)
	rm $(SUPPORTDIR)/TSTTESTS.DAT
	$(TARGET)-gcc -o $(basename $@).o -c $(GFLAGS) $(CFLAGS) $@

$(SUPPORTDIR)/cd300051.o: $(ACATSDIR)/cd/cd300051.c
	$(TARGET)-gcc -c -o $@ $<

$(SUPPORTDIR):
	mkdir $@

clean:
	-rm -r $(SUPPORTDIR)
	-rm -r $(TESTDIR)
