#!/usr/bin/env bash

set -o errexit
set -o nounset
set -o pipefail
# set -o xtrace

ROOT_DIR=$(dirname "$0")

LOX=${LOX:-${ROOT_DIR}/rlox-ast/target/release/rlox-ast}

# Print table header
printf "| %-20s | %-18s |\n" "File" "Took (s)"
printf "|:%-20s:|:%-18s:|\n" "--------------------" "------------------"

for SCRIPT_PATH in "${ROOT_DIR}"/resources/benchmark/*.lox; do
    SCRIPT=$(basename "${SCRIPT_PATH}" | tr -d "\n");
    TIMES=()
    for _ in {1..3}; do
        TIME=$("${LOX}" "${SCRIPT_PATH}" | grep "^elapsed:$" --after-context 1 | tail --lines 1 | tr -d "\n");
        TIMES+=("${TIME}")
    done
    MIN_TIME=$(printf "%s\n" "${TIMES[@]}" | LC_ALL=C sort --numeric-sort | head --lines 1 | tr -d '\n');
    # Format the MIN_TIME to 4 decimal places
    MIN_TIME=$(printf "%.5f" "${MIN_TIME}")
    printf "| %-20s | %-18s |\n" "${SCRIPT}" "${MIN_TIME}";
done
