# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

#================================ Parameters ==================================

CC     ?= cc
CSTD    = c99
IFLAGS  = -I/usr/include/freetype2
WFLAGS  = -Wall -Wextra -Wconversion -Wshadow -pedantic -pedantic-errors
OFLAGS  = -O2

CFLAGS  = -std=$(CSTD) $(IFLAGS) $(WFLAGS) $(OFLAGS) -fpic
LDFLAGS = -shared -lX11 -lXft -lXrandr

LIB_PATH = /usr/local/lib

BIN_DIR = bin
PLWM = $(BIN_DIR)/plwm
X11PLWM_O = $(BIN_DIR)/x11plwm.o
X11PLWM_SO = $(BIN_DIR)/x11plwm.so

#================================== Build =====================================

run: src/*.pl $(X11PLWM_SO)
	src/plwm

deb: src/* $(X11PLWM_SO)
	cargo deb --no-build

$(X11PLWM_SO): $(X11PLWM_O)
	$(CC) $< $(LDFLAGS) -o $@

$(X11PLWM_O): src/x11plwm.c $(BIN_DIR)
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
	src/x11plwm.c

clang-tidy:
	clang-tidy --checks='clang-analyzer-*' --extra-arg="-std=$(CSTD)" \
	--extra-arg="-I/usr/include/freetype2" \
	--warnings-as-errors='*' \
	src/x11plwm.c --

#=============================== Unit tests ===================================

test:
	tests/run_unit_tests.sh

#============================ Install/uninstall ===============================

install:
	tools/install.sh

uninstall:
	tools/uninstall.sh
