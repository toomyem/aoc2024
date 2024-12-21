#!/bin/bash

set -euo pipefail

for d in day*
do
  echo "--> Solving $d"
  [[ "$d" = "day16" ]] && echo "Skip validation until performance is fixed" && continue
  SECONDS=0
  ( cd "$d" && ./run.sh )
  echo "Solved in $SECONDS secs"
done
