#!/bin/bash

set -euo pipefail

if [[ "$#" -eq 0 ]]
then
  echo "Usage: $0 <day>"
  exit 1
fi

day="$1"
day_padded=$(printf "day%02d" "$day")

if [[ -d "$day_padded" ]]
then
  echo "$day_padded already exists"
  exit 0
fi

mkdir "$day_padded"
for f in template/*
do
  cp -v "$f" "$day_padded/"
done
for f in "$day_padded"/*
do
  sed -i "s/_day_/$day_padded/g" "$f"
done
