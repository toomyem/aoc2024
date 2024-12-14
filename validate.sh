#!/bin/bash

set -euo pipefail

for d in day*
do
  echo "Solving $d"
 ( cd "$d" && ./run.sh )
done
