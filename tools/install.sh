#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

trap 'echo "An error occured!"' ERR

set -e # stop immediately if any command fails

installdir_bin="${INSTALL_PREFIX:-}/usr/local/bin"
installdir_lib="${INSTALL_PREFIX:-}/usr/local/lib"
installdir_share="${INSTALL_PREFIX:-}/usr/local/share"
installdir_man="${INSTALL_PREFIX:-}/usr/local/share/man/man1"
installdir_cnf="${INSTALL_PREFIX:-}/etc/plwm"

set -x # trace commands

# core
install -D --mode=644 -C --backup=numbered config/config.pl ${installdir_cnf}/config.pl
install -D --mode=644 docs/plwm.1 ${installdir_man}/plwm.1
install -D --mode=644 img/plwm-logo.png ${installdir_share}/pixmaps/plwm-logo.png

# swi
install -D --mode=755 bin/plwm-swi ${installdir_bin}/plwm-swi
install -D --mode=644 bin/plx.so ${installdir_lib}/plx.so
install -D --mode=644 config/plwm-swi.desktop ${installdir_share}/xsessions/plwm-swi.desktop

# scryer
if [ "$1" = "--scryer" ] ; then
    install -D --mode=755 src/plwm-scryer ${installdir_share}/plwm/plwm-scryer
    install -D --mode=644 -t ${installdir_share}/plwm/ src/*.pl
    install -D --mode=644 -t ${installdir_share}/plwm/scryer/ src/scryer/*.pl
    install -D --mode=644 bin/x11plwm.so ${installdir_lib}/x11plwm.so

    ln -srf $(realpath --relative-to=${installdir_bin}/plwm-scryer) ${installdir_share}/plwm/plwm-scryer ${installdir_bin}/plwm-scryer
    install -D --mode=644 config/plwm-scryer.desktop ${installdir_share}/xsessions/plwm-scryer.desktop
fi

{ set +x; } 2>/dev/null # stop tracing

echo "plwm installed successfully"

