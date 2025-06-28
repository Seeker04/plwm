#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

trap 'echo "An error occured!"' ERR

set -e # stop immediately if any command fails

installdir_bin="/usr/local/bin"
installdir_lib="/usr/local/lib"
installdir_man="/usr/local/share/man/man1"
installdir_cnf="/etc/plwm"

set -x # trace commands

rm -f ${installdir_bin}/plwm \
      ${installdir_lib}/plx.so \
      ${installdir_man}/plwm.1

{ set +x; } 2>/dev/null # stop tracing

[ -d $installdir_cnf ] && echo "Note: $installdir_cnf is kept"

echo "plwm uninstalled successfully"

