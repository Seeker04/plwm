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
rm -f ${installdir_man}/plwm.1 \
      ${installdir_share}/pixmaps/plwm-logo.png
# not uninstalling /etc/plwm/config.pl as that could contain changes by the user they want to keep

# swi
rm -f ${installdir_share}/xsessions/plwm-swi.desktop \
      ${installdir_bin}/plwm-swi \
      ${installdir_lib}/plx.so

# scryer
rm -f ${installdir_share}/xsessions/plwm-scryer.desktop \
      ${installdir_bin}/plwm-scryer \
      ${installdir_share}/plwm/plwm-scryer \
      ${installdir_share}/plwm/*.pl \
      ${installdir_share}/plwm/scryer/*.pl \
      ${installdir_lib}/x11plwm.so

{ set +x; } 2>/dev/null # stop tracing

[ -d $installdir_cnf ] && echo "Note: $installdir_cnf is kept"

echo "plwm uninstalled successfully"

