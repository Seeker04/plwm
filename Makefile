# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

#================================ Parameters ==================================

CC     ?= cc
CSTD    = c99
IFLAGS  = -I/usr/lib/swipl/include -I/usr/lib/swi-prolog/include -I/usr/include/freetype2
WFLAGS  = -Wall -Wextra -Wconversion -Wshadow -pedantic -pedantic-errors
OFLAGS  = -O2

CFLAGS  = -std=$(CSTD) $(IFLAGS) $(WFLAGS) $(OFLAGS) -fpic
LDFLAGS = -shared -lX11 -lXft -lXrandr

LIB_PATH = /usr/local/lib

BIN_DIR = bin
PLWM = $(BIN_DIR)/plwm
PLX_O = $(BIN_DIR)/plx.o
PLX_SO = $(BIN_DIR)/plx.so

SWIFLAGS = -p foreign=$(LIB_PATH) \
           --goal=main --toplevel=halt --stand_alone=true -O -o $(PLWM) -c src/plwm.pl

#================================== Build =====================================

$(PLWM): src/*.pl $(PLX_SO)
	swipl $(SWIFLAGS)

$(PLX_SO): $(PLX_O)
	$(CC) $< $(LDFLAGS) -o $@

$(PLX_O): src/plx.c $(BIN_DIR)
	$(CC) -c $(CFLAGS) $< -o $@

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -f $(BIN_DIR)/*

rebuild: clean $(PLWM)

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

install:
	tools/install.sh

uninstall:
	tools/uninstall.sh

