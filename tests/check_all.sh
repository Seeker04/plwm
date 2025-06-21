#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

set -e # stop immediately if any command fails

version=$(sed -n 's/^version(\([0-9.]\+\))\.$/\1/p' src/plwm.pl)
commit=$(git rev-parse --short HEAD)

trap "echo \"Checks FAILED for plwm v$version [$commit]!\"" ERR

echo "Checking integrity of plwm v$version [$commit]"

echo
echo "----------------------------------------------------------------------"
echo "Checking license"
echo "----------------------------------------------------------------------"

grep -q "Copyright (c)" LICENSE && echo "LICENSE OK" || false

grep -q "$(date +%Y)" LICENSE && echo "LICENSE year OK" || echo "WARNING: update LICENSE year!"

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

[ "$(stat -c "%a" /usr/local/bin/plwm)"              -eq 755 ] && echo "plwm mode OK"      || false
[ "$(stat -c "%a" /usr/local/lib/plx.so)"            -eq 755 ] && echo "plx.so mode OK"    || false
[ "$(stat -c "%a" /usr/local/share/man/man1/plwm.1)" -eq 644 ] && echo "plwm.1 mode OK"    || false
[ "$(stat -c "%a" /etc/plwm/config.pl)"              -eq 644 ] && echo "config.pl mode OK" || false

diff -q plwm                                     /usr/local/bin/plwm              && echo "plwm diff OK"      || false
diff -q plx.so                                   /usr/local/lib/plx.so            && echo "plx.so diff OK"    || false
diff -q <(sed "s/VERSION/$version/" docs/plwm.1) /usr/local/share/man/man1/plwm.1 && echo "plwm.1 diff OK"    || false
diff -q config/config.pl                         /etc/plwm/config.pl              && echo "config.pl diff OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Checking version info"
echo "----------------------------------------------------------------------"

[ "$(plwm -v        2>&1)" = "plwm version $version" ] && echo "plwm -v OK"        || false
[ "$(plwm --version 2>&1)" = "plwm version $version" ] && echo "plwm --version OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Checking default configuration"
echo "----------------------------------------------------------------------"

diff -q config/config.pl /etc/plwm/config.pl && echo "config.pl install OK" || false

# etc
[[ "$(plwm -C      2>&1)" == $'etc config loaded' ]] && echo "[etc] plwm -C OK"      || false
[[ "$(plwm --check 2>&1)" == $'etc config loaded' ]] && echo "[etc] plwm --check OK" || false

# xdg path
export XDG_CONFIG_HOME="$HOME/.config"
mkdir -p "$XDG_CONFIG_HOME/plwm"
cp /etc/plwm/config.pl "$XDG_CONFIG_HOME/plwm"
[[ "$(plwm -C      2>&1)" == $'xdg config loaded' ]] && echo "[xdg] plwm -C OK"      || false
[[ "$(plwm --check 2>&1)" == $'xdg config loaded' ]] && echo "[xdg] plwm --check OK" || false

# $HOME/.config
unset XDG_CONFIG_HOME
[[ "$(plwm -C      2>&1)" == $'home config loaded' ]] && echo "[home] plwm -C OK"      || false
[[ "$(plwm --check 2>&1)" == $'home config loaded' ]] && echo "[home] plwm --check OK" || false
rm -r "$HOME/.config/plwm"

# custom path
cp /etc/plwm/config.pl /tmp/
[[ "$(plwm -c       /tmp/config.pl -C 2>&1)" == $'-c user config loaded' ]] && echo "[-c] plwm -C OK"       || false
[[ "$(plwm --config=/tmp/config.pl -C 2>&1)" == $'-c user config loaded' ]] && echo "[-c] plwm --config OK" || false
rm /tmp/config.pl

echo
echo "----------------------------------------------------------------------"
echo "Checking -h and --help"
echo "----------------------------------------------------------------------"

[ "$(plwm -h     |& head -1 2>&1)" = "Usage: plwm [OPTION]..." ] && echo "plwm -h OK"     || false
[ "$(plwm --help |& head -1 2>&1)" = "Usage: plwm [OPTION]..." ] && echo "plwm --help OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Checking -l and --log"
echo "----------------------------------------------------------------------"

plwm -l /tmp/testlog1 -C
plwm -l /tmp/testlog2 -C

[[ "$(cat /tmp/testlog1 2>&1)" == $'etc config loaded' ]] && echo "plwm -l OK"    || false
[[ "$(cat /tmp/testlog2 2>&1)" == $'etc config loaded' ]] && echo "plwm --log OK" || false

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
[ -r /etc/plwm/config.pl ]                && echo "config.pl kept OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Verifying config backup at reinstall"
echo "----------------------------------------------------------------------"

sudo make install >/dev/null
[ $(ls /etc/plwm | wc -l) -eq 1 ] && echo "no backup if no change OK" || false

echo "extra line" | sudo tee -a /etc/plwm/config.pl >/dev/null
sudo make install >/dev/null
[[ -r /etc/plwm/config.pl && -r /etc/plwm/config.pl.~1~ ]] && echo "changed config.pl backed up OK" || false

[[ $(diff /etc/plwm/config.pl /etc/plwm/config.pl.~1~ |& grep "^[><] " 2>&1) == $'> extra line' ]] \
&& echo "backup diff OK" || false

echo
echo "All checks PASSED for plwm v$version [$commit]"

