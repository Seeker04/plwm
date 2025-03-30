#!/usr/bin/env bash

# MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

set -e # stop immediately if any command fails

version=$(sed -n 's/^version(\([0-9.]\+\))\.$/\1/p' src/plwm.pl)
commit=$(git rev-parse --short HEAD)

trap "echo \"Checks FAILED for plwm v$version [$commit]!\"" ERR

echo "Checking integrity of plwm v$version [$commit]"

echo
echo "----------------------------------------------------------------------"
echo "Building plwm"
echo "----------------------------------------------------------------------"

make rebuild

echo
echo "----------------------------------------------------------------------"
echo "Installing plwm"
echo "----------------------------------------------------------------------"

sudo make install

echo
echo "----------------------------------------------------------------------"
echo "Verifying installed files"
echo "----------------------------------------------------------------------"

[ "$(stat -c "%a" /usr/local/bin/plwm)"              -eq 755 ] && echo "plwm mode OK"   || false
[ "$(stat -c "%a" /usr/local/lib/plx.so)"            -eq 755 ] && echo "plx.so mode OK" || false
[ "$(stat -c "%a" /usr/local/share/man/man1/plwm.1)" -eq 644 ] && echo "plwm.1 mode OK" || false

diff -q plwm                                /usr/local/bin/plwm              && echo "plwm diff OK"   || false
diff -q plx.so                              /usr/local/lib/plx.so            && echo "plx.so diff OK" || false
diff -q <(sed "s/VERSION/$version/" plwm.1) /usr/local/share/man/man1/plwm.1 && echo "plwm.1 diff OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Checking version info"
echo "----------------------------------------------------------------------"

[ "$(plwm -v)"        = "plwm version $version" ] && echo "plwm -v OK"        || false
[ "$(plwm --version)" = "plwm version $version" ] && echo "plwm --version OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Checking default configuration"
echo "----------------------------------------------------------------------"

make mkconfig

diff -q <(sed 's/module(config/module(runtime_config/' src/config.pl) $XDG_CONFIG_HOME/plwm/config.pl \
&& echo "config.pl install OK" || false

[[ "$(plwm -C)"      == $'xdg config loaded\nConfig: OK' ]] && echo "plwm -C OK"      || false
[[ "$(plwm --check)" == $'xdg config loaded\nConfig: OK' ]] && echo "plwm --check OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Static code analysis"
echo "----------------------------------------------------------------------"

make cppcheck
make clang-tidy

echo
echo "----------------------------------------------------------------------"
echo "Unit tests"
echo "----------------------------------------------------------------------"

make test

echo
tests/get_coverage.sh

echo
echo "----------------------------------------------------------------------"
echo "System tests"
echo "----------------------------------------------------------------------"

echo "TODO" # with xvfb

echo
echo "----------------------------------------------------------------------"
echo "Uninstalling plwm"
echo "----------------------------------------------------------------------"

sudo make uninstall

echo
echo "----------------------------------------------------------------------"
echo "Verifying uninstall"
echo "----------------------------------------------------------------------"

[ ! -e /usr/local/bin/plwm ]              && echo "plwm removed OK"   || false
[ ! -e /usr/local/lib/plx.so ]            && echo "plx.so removed OK" || false
[ ! -e /usr/local/share/man/man1/plwm.1 ] && echo "plwm.1 removed OK" || false

echo
echo "All checks PASSED for plwm v$version [$commit]"

