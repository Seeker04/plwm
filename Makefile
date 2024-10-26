# MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

#================================ Parameters ==================================

CC     ?= cc
CSTD    = c99
IFLAGS  = -I/usr/lib/swipl/include -I/usr/lib/swi-prolog/include -I/usr/include/freetype2
WFLAGS  = -Wall -Wextra -Wconversion -Wshadow -pedantic -pedantic-errors
OFLAGS  = -O2

CFLAGS  = -std=$(CSTD) $(IFLAGS) $(WFLAGS) $(OFLAGS) -fpic
LDFLAGS = -shared -lX11 -lXinerama -lXft

SWIFLAGS = -p foreign=$(INSTALLDIR_LIB) \
           --goal=main --toplevel=halt --stand_alone=true -O -o plwm -c src/plwm.pl

INSTALLDIR     = /usr/local/bin
INSTALLDIR_LIB = /usr/local/lib
INSTALLDIR_MAN = /usr/local/share/man/man1

ifdef XDG_CONFIG_HOME
CONFIG_PATH = $(XDG_CONFIG_HOME)/plwm
else
CONFIG_PATH = $(HOME)/.config/plwm
endif

#================================== Build =====================================

plwm: plx.so src/*.pl
	swipl $(SWIFLAGS)

plx.o: src/plx.c
	$(CC) -c $(CFLAGS) $< -o $@

plx.so: src/plx.o
	$(CC) $< $(LDFLAGS) -o $@

clean:
	rm -f src/plx.o plx.so plwm

rebuild: clean plwm

#============================== Static checks =================================

cppcheck:
	cppcheck -q --enable=all --language=c --std=$(CSTD) --suppress=missingIncludeSystem src/plx.c

clang-tidy:
	clang-tidy --checks='clang-analyzer-*' --header-filter=.* --extra-arg="-std=$(CSTD)" \
	--extra-arg="-I/usr/include/freetype2" \
	--extra-arg="-I/usr/lib/swipl/include" \
	--extra-arg="-I/usr/lib/swi-prolog/include" \
	src/plx.c --

#=============================== Unit tests ===================================

test:
	tests/run_all.sh

#============================ Install/uninstall ===============================

VERSION = ${shell sed -n 's/^version(\([0-9.]\+\))\.$$/\1/p' src/plwm.pl}

install: plwm
	install -D --mode=755 plwm $(INSTALLDIR)/plwm
	install -D --mode=755 plx.so $(INSTALLDIR_LIB)/plx.so
	mkdir -p $(INSTALLDIR_MAN)
	sed 's/VERSION/$(VERSION)/' < plwm.1 > $(INSTALLDIR_MAN)/plwm.1
	chmod 644 $(INSTALLDIR_MAN)/plwm.1

uninstall:
	rm -f $(INSTALLDIR)/plwm $(INSTALLDIR_LIB)/plx.so $(INSTALLDIR_MAN)/plwm.1

mkconfig:
	install -D src/config.pl $(CONFIG_PATH)/config.pl
	sed -i 's/module(config/module(runtime_config/' $(CONFIG_PATH)/config.pl

