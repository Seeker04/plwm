#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

set -e # stop immediately if any command fails

trap 'echo "An error occured!"' ERR

version=$(sed -n 's/^version(\([0-9.]\+\))\.$/\1/p' src/plwm.pl)
commit=$(git rev-parse --short HEAD)
arch="$(uname | tr A-Z a-z)-$(uname -m | tr A-Z a-z)"

release_dir="plwm-${version}-${arch}"

release_archive="${release_dir}.tar.gz"

file_list="
bin/plwm
bin/plx.so
docs/plwm.1
docs/README.md
docs/CHANGELOG.md
config/config.pl"

# these will be added to the release root
file_list_root="
tools/install.sh
tools/uninstall.sh"

echo "Generating release..."
echo "Version: $version"
echo "Commit: $commit"
echo "Arch: $arch"
echo "Output: $release_archive"

mkdir $release_dir

trap "echo 'An error occured!'; rm -rf $release_dir" ERR

echo "Adding files:"
for file in $file_list; do
	echo "  $file"
	cp --parents $file $release_dir
done
for file in $file_list_root; do
	echo "  $file"
	cp $file $release_dir
done

tar -czf $release_archive $release_dir

rm -r $release_dir

echo "Release $release_archive generated successfully"

