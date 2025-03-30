#!/usr/bin/env bash

# MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

exit_code=0
for test in tests/unit_tests/*.plt; do
	echo $test
	swipl -g run_tests -t halt $test || exit_code=1
done

exit $exit_code

