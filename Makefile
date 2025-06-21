# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

#================================ Parameters ==================================

CC     ?= cc
CSTD    = c99
IFLAGS  = -I/usr/lib/swipl/include -I/usr/lib/swi-prolog/include -I/usr/include/freetype2
WFLAGS  = -Wall -Wextra -Wconversion -Wshadow -pedantic -pedantic-errors
OFLAGS  = -O2

CFLAGS  = -std=$(CSTD) $(IFLAGS) $(WFLAGS) $(OFLAGS) -fpic
LDFLAGS = -shared -lX11 -lXft -lXrandr

SWIFLAGS = -p foreign=$(INSTALLDIR_LIB) \
           --goal=main --toplevel=halt --stand_alone=true -O -o plwm -c src/plwm.pl

INSTALLDIR_BIN = /usr/local/bin
INSTALLDIR_LIB = /usr/local/lib
INSTALLDIR_CNF = /etc/plwm
INSTALLDIR_MAN = /usr/local/share/man/man1

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
	cppcheck -q --enable=all --language=c --std=$(CSTD) \
	--suppress=missingIncludeSystem --inline-suppr \
	--check-level=exhaustive --inconclusive \
	--error-exitcode=1 \
	src/plx.c

clang-tidy:
	clang-tidy --checks='clang-analyzer-*' --extra-arg="-std=$(CSTD)" \
	--extra-arg="-I/usr/include/freetype2" \
	--extra-arg="-I/usr/lib/swipl/include" \
	--extra-arg="-I/usr/lib/swi-prolog/include" \
	--warnings-as-errors='*' \
	src/plx.c --

#=============================== Unit tests ===================================

test:
	tests/run_unit_tests.sh

#============================ Install/uninstall ===============================

VERSION = ${shell sed -n 's/^version(\([0-9.]\+\))\.$$/\1/p' src/plwm.pl}

install: plwm
	install -D --mode=755 plwm $(INSTALLDIR_BIN)/plwm
	install -D --mode=755 plx.so $(INSTALLDIR_LIB)/plx.so
	install -D --mode=644 -C --backup=numbered config/config.pl $(INSTALLDIR_CNF)/config.pl
	mkdir -p $(INSTALLDIR_MAN)
	sed 's/VERSION/$(VERSION)/' < docs/plwm.1 > $(INSTALLDIR_MAN)/plwm.1
	chmod 644 $(INSTALLDIR_MAN)/plwm.1

uninstall:
	rm -f $(INSTALLDIR_BIN)/plwm \
	      $(INSTALLDIR_LIB)/plx.so \
	      $(INSTALLDIR_MAN)/plwm.1
	[ -d $(INSTALLDIR_CNF) ] && echo "Note: $(INSTALLDIR_CNF) is kept" || true

