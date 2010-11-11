HOST=$(shell gcc -dumpmachine)
TARGET=$(HOST)
ifeq ($(TARGET),$(HOST))
GCC=$(dir $(shell which gnatmake))gcc
AR=ar
else
GCC=$(TARGET)-gcc
AR=$(TARGET)-ar
endif

# if WITHA=1, all files be compiled
WITHA=0

# if WITHC=1, creating libc-gnat.a
WITHC=1

# if WITHD=1, debug info
WITHD=0

# if WITHS=1, assembly sources be outputed
WITHS=0

RTSDIR=.
IMPORTDIR=$(RTSDIR)/adainclude
GCCLIBDIR=$(dir $(shell $(GCC) -print-libgcc-file-name))

BUILDDIR=$(RTSDIR)/adalib/build

SUBDIRS=ada83 containers directories image interfaces numerics streams strings tasking textio zcx

SRC=$(wildcard *.ad?) $(wildcard $(TARGET)/*.ad?) \
	$(foreach i,$(SUBDIRS),$(wildcard $(i)/*.ad?) $(wildcard $(i)/$(TARGET)/*.ad?))

SEPARATED=zcx/s-unrase.adb zcx/s-untrse.adb

LIBRARY_SRCS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(SRC)))

COMPILER_OBJS= $(addprefix $(BUILDDIR)/, \
	c-unwind_pe.o \
	system.o \
	s-debug.o \
	s-exctab.o \
	s-format.o \
	s-memory.o \
	s-soflin.o \
	s-stalib.o \
	s-termin.o \
	s-unstyp.o \
	s-unwhan.o \
	s-unwind.o \
	s-unwrai.o \
	s-unwsta.o \
	s-unwtra.o)

ifneq ($(WITHA),0)
ALL_OBJS=$(addprefix $(BUILDDIR)/,\
	system.o \
	$(sort $(addsuffix .o,$(basename $(notdir $(filter-out $(SEPARATED),$(SRC)))))))
else
ALL_OBJS=$(COMPILER_OBJS)
endif

ALL_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(basename $(notdir $(ALL_OBJS)))))

ifneq ($(WITHC),0)
LIB_C_GNAT=$(RTSDIR)/adalib/libc-gnat.a
else
LIB_C_GNAT=$(IMPORTDIR)/c.ads
endif

ifneq ($(WITHD),0)
CFLAGS_FOR_COMPILER_UNIT=-nostdinc -I$(RTSDIR)/adainclude -gnatg -gnatwP -Os -g -gnata
else
CFLAGS_FOR_COMPILER_UNIT=-nostdinc -I$(RTSDIR)/adainclude -gnatg -gnatwP -Os
endif

.PHONY: all libc-gnat c-archive

all: $(RTSDIR)/adalib/libgnat.a $(ALL_ALI)

$(RTSDIR)/adalib/libgnat.a: \
		$(RTSDIR)/adainclude/ \
		$(RTSDIR)/adalib/ \
		$(BUILDDIR) \
		$(RTSDIR)/adainclude/system.ads \
		$(LIBRARY_SRCS) \
		$(LIB_C_GNAT) \
		$(ALL_OBJS)
	-rm $@
	$(AR) -q $@ $(ALL_OBJS)

$(RTSDIR)/adainclude:
	mkdir -p $@

$(RTSDIR)/adalib:
	mkdir $@

$(BUILDDIR):
	mkdir $@

$(RTSDIR)/adainclude/system.ads:
	ln -s $(GCCLIBDIR)/adainclude/system.ads $@

$(RTSDIR)/adainclude/%.ads: %.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: %.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: $(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: ada83/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: ada83/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: containers/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: containers/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: directories/$(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: directories/$(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: image/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: image/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: interfaces/$(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: interfaces/$(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: numerics/$(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: numerics/$(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: streams/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: streams/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: streams/$(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: streams/$(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: strings/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: strings/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: tasking/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: tasking/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: tasking/$(TARGET)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: tasking/$(TARGET)/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: textio/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: textio/%.adb
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.ads: zcx/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: zcx/%.adb
	ln -s $(abspath $<) $@

$(BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.adb $(RTSDIR)/adainclude/%.ads
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<
ifneq ($(WITHS),0)
	cd $(dir $@) && $(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -S $<
endif

$(BUILDDIR)/%.o: $(RTSDIR)/adainclude/%.ads
	$(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -c -o $@ $<
ifneq ($(WITHS),0)
	cd $(dir $@) && $(GCC) $(CFLAGS_FOR_COMPILER_UNIT) -S $<
endif

$(RTSDIR)/adalib/%.ali: $(BUILDDIR)/%.o
	install -m 444 $(basename $<).ali $@

$(IMPORTDIR)/c.ads: import.h
	headmaster -t ada -p -D $(IMPORTDIR) -gcc=$(GCC) $<

$(RTSDIR)/adalib/libc-gnat.a: $(IMPORTDIR)/c.ads
	make libc-gnat

c-archive: $(IMPORTDIR)/c.ads
ifneq ($(DESTDIR),)
	cd $(IMPORTDIR) && 7za -tzip a $(abspath $(DESTDIR)/import-$(TARGET).zip) c.ads c-*
else
	@echo please tell DESTDIR=[DIRECTORY]
endif

# nested

IMPLIB_SRC=$(wildcard $(IMPORTDIR)/c-*.ad?) $(IMPORTDIR)/c.ads
IMPLIB_ITEMS=$(filter-out c-unwind_pe,$(sort $(basename $(notdir $(IMPLIB_SRC)))))
IMPLIB_OBJS=$(addsuffix .o,$(addprefix $(BUILDDIR)/,$(IMPLIB_ITEMS)))
IMPLIB_ALI=$(addsuffix .ali,$(addprefix $(RTSDIR)/adalib/,$(IMPLIB_ITEMS)))

ifneq ($(IMPORTDIR),$(RTSDIR)/adainclude)

IMPLIB_LINKS=$(addprefix $(RTSDIR)/adainclude/,$(notdir $(IMPLIB_SRC)))

$(RTSDIR)/adainclude/%.ads: $(IMPORTDIR)/%.ads
	ln -s $(abspath $<) $@

$(RTSDIR)/adainclude/%.adb: $(IMPORTDIR)/%.adb
	ln -s $(abspath $<) $@

else
IMPLIB_LINKS=
endif

libc-gnat: $(IMPLIB_LINKS) $(IMPLIB_OBJS) $(IMPLIB_ALI)
	-rm $(RTSDIR)/adalib/libc-gnat.a
	$(AR) -q $(RTSDIR)/adalib/libc-gnat.a $(IMPLIB_OBJS)
