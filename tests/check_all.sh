#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

set -e # stop immediately if any command fails

version=$(sed -n 's/^version(\([0-9.]\+\))\.$/\1/p' src/plwm.pl)
commit=$(git rev-parse --short HEAD)
arch="$(uname | tr A-Z a-z)-$(uname -m | tr A-Z a-z)"

trap "echo -e \"\nChecks FAILED for plwm v$version [$commit] on $arch!\"" ERR

echo
echo "Checking integrity of plwm v$version [$commit] on $arch"

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
echo "Verifying binaries"
echo "----------------------------------------------------------------------"

[ "$(stat -c "%a" bin/plwm)"   -eq 755 ] && echo "plwm mode OK"   || false
[ "$(stat -c "%a" bin/plx.so)" -eq 755 ] && echo "plx.so mode OK" || false

echo
echo "----------------------------------------------------------------------"
echo "Installing plwm"
echo "----------------------------------------------------------------------"

sudo make install

echo
echo "----------------------------------------------------------------------"
echo "Verifying install"
echo "----------------------------------------------------------------------"

verify_install()
{
	[ "$(stat -c "%a" /usr/local/bin/plwm)"              -eq 755 ] && echo "plwm mode OK"      || false
	[ "$(stat -c "%a" /usr/local/lib/plx.so)"            -eq 755 ] && echo "plx.so mode OK"    || false
	[ "$(stat -c "%a" /usr/local/share/man/man1/plwm.1)" -eq 644 ] && echo "plwm.1 mode OK"    || false
	[ "$(stat -c "%a" /etc/plwm/config.pl)"              -eq 644 ] && echo "config.pl mode OK" || false

	diff -q bin/plwm         /usr/local/bin/plwm              && echo "plwm diff OK"      || false
	diff -q bin/plx.so       /usr/local/lib/plx.so            && echo "plx.so diff OK"    || false
	diff -q docs/plwm.1      /usr/local/share/man/man1/plwm.1 && echo "plwm.1 diff OK"    || false
	diff -q config/config.pl /etc/plwm/config.pl              && echo "config.pl diff OK" || false
}

verify_install

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
unset XDG_CONFIG_HOME

# $HOME/.config
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

plwm -l    /tmp/testlog1 -C
plwm --log /tmp/testlog2 -C

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
echo "Checking documentation comments"
echo "----------------------------------------------------------------------"

preds=$(egrep -h "^[a-z_]+[^_]\(.*\)(:-|.)" src/*.pl | cut -d'(' -f1 | sort -u | grep -vx "version")

# note: we ignore version/0 and local helpers suffixed with an underscore e.g. valid_set_/2

undocumented=""
for pred in $preds; do
	grep -q "^%! $pred" src/*.pl || undocumented="$undocumented\n$pred"
done

[ -z "$undocumented" ] && echo "documentation comments OK" \
|| (echo -e "Predicates with no documentation comment:$undocumented"; false)

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

verify_uninstall()
{
	[ ! -e /usr/local/bin/plwm ]              && echo "plwm removed OK"   || false
	[ ! -e /usr/local/lib/plx.so ]            && echo "plx.so removed OK" || false
	[ ! -e /usr/local/share/man/man1/plwm.1 ] && echo "plwm.1 removed OK" || false
	[ -r /etc/plwm/config.pl ]                && echo "config.pl kept OK" || false
}

verify_uninstall

echo
echo "----------------------------------------------------------------------"
echo "Verifying config backup at reinstall"
echo "----------------------------------------------------------------------"

sudo make install >/dev/null 2>&1

[ $(ls /etc/plwm | wc -l) -eq 1 ] && echo "no backup if no change OK" || false

echo "extra line" | sudo tee -a /etc/plwm/config.pl >/dev/null

sudo make install >/dev/null 2>&1

[[ -r /etc/plwm/config.pl && -r /etc/plwm/config.pl.~1~ ]] && echo "changed config.pl backed up OK" || false

[[ $(diff /etc/plwm/config.pl /etc/plwm/config.pl.~1~ |& grep "^[><] " 2>&1) == $'> extra line' ]] \
&& echo "backup diff OK" || false

sudo make uninstall >/dev/null 2>&1

echo
echo "----------------------------------------------------------------------"
echo "Generating release"
echo "----------------------------------------------------------------------"

tools/generate_release.sh

echo
echo "----------------------------------------------------------------------"
echo "Verifying generated release"
echo "----------------------------------------------------------------------"

release_dir="plwm-${version}-${arch}"
release_archive="${release_dir}.tar.gz"

[ "$(stat -c "%a" $release_archive)" -eq 644 ] && echo "archive mode OK" || false

tar -xf $release_archive && echo "extraction OK" || false

[ "$(stat -c "%a" $release_dir)" -eq 755 ] && echo "release dir mode OK" || false

[ "$(stat -c "%a" ${release_dir}/bin/plwm)"          -eq 755 ] && echo "bin/plwm mode OK"          || false
[ "$(stat -c "%a" ${release_dir}/bin/plx.so)"        -eq 755 ] && echo "bin/plx.so mode OK"        || false
[ "$(stat -c "%a" ${release_dir}/config/config.pl)"  -eq 644 ] && echo "config/config.pl mode OK"  || false
[ "$(stat -c "%a" ${release_dir}/docs/plwm.1)"       -eq 644 ] && echo "docs/plwm.1 mode OK"       || false
[ "$(stat -c "%a" ${release_dir}/docs/README.md)"    -eq 644 ] && echo "docs/README.md mode OK"    || false
[ "$(stat -c "%a" ${release_dir}/docs/CHANGELOG.md)" -eq 644 ] && echo "docs/CHANGELOG.md mode OK" || false
[ "$(stat -c "%a" ${release_dir}/install.sh)"        -eq 755 ] && echo "install.sh mode OK"        || false
[ "$(stat -c "%a" ${release_dir}/uninstall.sh)"      -eq 755 ] && echo "uninstall.sh mode OK"      || false

diff -q bin/plwm           ${release_dir}/bin/plwm          && echo "plwm diff OK"              || false
diff -q bin/plx.so         ${release_dir}/bin/plx.so        && echo "plx.so diff OK"            || false
diff -q config/config.pl   ${release_dir}/config/config.pl  && echo "config/config.pl diff OK"  || false
diff -q docs/plwm.1        ${release_dir}/docs/plwm.1       && echo "docs/plwm.1 diff OK"       || false
diff -q docs/README.md     ${release_dir}/docs/README.md    && echo "docs/README.md diff OK"    || false
diff -q docs/CHANGELOG.md  ${release_dir}/docs/CHANGELOG.md && echo "docs/CHANGELOG.md diff OK" || false
diff -q tools/install.sh   ${release_dir}/install.sh        && echo "install.sh diff OK"        || false
diff -q tools/uninstall.sh ${release_dir}/uninstall.sh      && echo "uninstall.sh diff OK"      || false

echo
echo "----------------------------------------------------------------------"
echo "Installing release"
echo "----------------------------------------------------------------------"

cd $release_dir
sudo ./install.sh

echo
echo "----------------------------------------------------------------------"
echo "Verifying release install"
echo "----------------------------------------------------------------------"

verify_install

echo
echo "----------------------------------------------------------------------"
echo "Uninstalling release"
echo "----------------------------------------------------------------------"

sudo ./uninstall.sh

echo
echo "----------------------------------------------------------------------"
echo "Verifying release uninstall"
echo "----------------------------------------------------------------------"

verify_uninstall

echo
echo "All checks PASSED for plwm v$version [$commit] on $arch"

