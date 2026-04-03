#!/usr/bin/env bash

# MIT License, Copyright (c) 2023-2025 Barnabás Zahorán, see LICENSE

exit_code=0
for test in tests/unit_tests/*.plt; do
	echo $test
	swipl -g "consult('${test}'),run_tests" -t halt src/swi/init.pl -- || exit_code=1
done

exit $exit_code

