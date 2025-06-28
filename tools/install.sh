#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

trap 'echo "An error occured!"' ERR

set -e # stop immediately if any command fails

installdir_bin="/usr/local/bin"
installdir_lib="/usr/local/lib"
installdir_man="/usr/local/share/man/man1"
installdir_cnf="/etc/plwm"

set -x # trace commands

install -D --mode=755 bin/plwm ${installdir_bin}/plwm
install -D --mode=755 bin/plx.so ${installdir_lib}/plx.so
install -D --mode=644 docs/plwm.1 ${installdir_man}/plwm.1
install -D --mode=644 -C --backup=numbered config/config.pl ${installdir_cnf}/config.pl

{ set +x; } 2>/dev/null # stop tracing

echo "plwm installed successfully"

