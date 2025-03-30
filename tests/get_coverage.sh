#!/usr/bin/env bash

# MIT License, Copyright (c) 2023 Barnabás Zahorán, see LICENSE

[[ "$1" == "-v" || "$1" == "--verbose" ]] && verbose=1

ignore_list="xf86names.pl config.pl"

echo "[Predicate coverage]"

for src in src/*.pl; do

	if grep -q "$(sed 's/src\///' <<< $src)" <<< $ignore_list; then
		[[ verbose -eq 1 ]] && echo "-- ignored $src --"
		continue
	fi

	preds=$(egrep "^\w.*\(.*\)(:-|.)" $src | cut -d'(' -f1 | grep -v "_$" | sort -u)

	pred_cnt=$(wc -l <<< $preds)
	covered_cnt=0
	testfile="$(sed 's/src/tests\/unit_tests/' <<< $src)t"
	if [[ -r $testfile ]]; then
		for pred in $preds; do
			if grep -q "^test(\"${pred}\>" $testfile; then
				((++covered_cnt))
			elif [[ verbose -eq 1 ]]; then
				echo "  $pred test missing"
			fi
		done
	fi
	echo "$src : $covered_cnt / $pred_cnt -" $(( covered_cnt * 100 / pred_cnt )) "%"

	((total_pred_cnt+=pred_cnt, total_covered_cnt+=covered_cnt))
done

echo "--------------------"
echo "Total coverage: "
echo "$total_covered_cnt / $total_pred_cnt -" $(( total_covered_cnt * 100 / total_pred_cnt )) "%"

