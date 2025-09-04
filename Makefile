# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

#================================ Parameters ==================================

CC     ?= cc
CSTD    = c99
IFLAGS  = -I/usr/lib/swipl/include -I/usr/lib/swi-prolog/include -I/usr/include/freetype2
WFLAGS  = -Wall -Wextra -Wconversion -Wshadow -pedantic -pedantic-errors
OFLAGS  = -O2

CFLAGS  = -std=$(CSTD) $(IFLAGS) $(WFLAGS) $(OFLAGS) -fpic
LDFLAGS = -shared -lX11 -lXft -lXrandr

LIB_PATH = /usr/local/lib:$(BIN_DIR)

BIN_DIR = bin

PLWM_SWI = $(BIN_DIR)/plwm-swi
PLX_O = $(BIN_DIR)/plx.o
PLX_SO = $(BIN_DIR)/plx.so

SWIFLAGS = -p foreign=$(LIB_PATH) \
	--goal=main --toplevel=halt --stand_alone=true -O -o $(PLWM_SWI) none -c src/swi/init.pl src/plwm.pl 

PLWM_SCRYER = src/plwm-scryer
X11PLWM_O = $(BIN_DIR)/x11plwm.o
X11PLWM_SO = $(BIN_DIR)/x11plwm.so

#================================== Build =====================================

run-scryer: $(X11PLWM_SO)
	$(PLWM_SCRYER)

run-swi: $(PLWM_SWI)
	$(PLWM_SWI) -c config/config.pl

debs: deb-core deb-scryer deb-swi

deb-core: src/*
	cargo deb --variant=core --no-build

deb-scryer: src/scryer/* $(X11PLWM_SO)
	cargo deb --variant=scryer --no-build

deb-swi: src/scryer/* $(PLWM_SWI)
	cargo deb --variant=swi --no-build

$(X11PLWM_SO): $(X11PLWM_O)
	$(CC) $< $(LDFLAGS) -o $@

$(X11PLWM_O): src/scryer/x11plwm.c $(BIN_DIR)
	$(CC) -c $(CFLAGS) $< -o $@

$(PLWM_SWI): src/*.pl $(PLX_SO)
	swipl $(SWIFLAGS)

$(PLX_SO): $(PLX_O)
	$(CC) $< $(LDFLAGS) -o $@

$(PLX_O): src/swi/plx.c $(BIN_DIR)
	$(CC) -c $(CFLAGS) $< -o $@

$(BIN_DIR):
	mkdir $(BIN_DIR)

clean:
	rm -f $(BIN_DIR)/*

rebuild: clean $(PLWM_SWI)

#============================== Static checks =================================

cppcheck:
	cppcheck -q --enable=all --language=c --std=$(CSTD) \
	--suppress=missingIncludeSystem --inline-suppr \
	--check-level=exhaustive --inconclusive \
	--error-exitcode=1 \
	src/swi/plx.c \
	src/scryer/x11plwm.c

clang-tidy:
	clang-tidy --checks='clang-analyzer-*' --extra-arg="-std=$(CSTD)" \
	--extra-arg="-I/usr/include/freetype2" \	
	--extra-arg="-I/usr/lib/swipl/include" \
	--extra-arg="-I/usr/lib/swi-prolog/include" \
	--warnings-as-errors='*' \
	src/swi/plx.c \
	src/scryer/x11plwm.c --

#=============================== Unit tests ===================================

test:
	tests/run_unit_tests.sh

#============================ Install/uninstall ===============================

install:
	tools/install.sh

uninstall:
	tools/uninstall.sh
