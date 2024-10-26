#!/usr/bin/env bash

exit_code=0
for test in tests/*.plt; do
	echo $test
	swipl -g run_tests -t halt $test || exit_code=1
done

exit $exit_code

