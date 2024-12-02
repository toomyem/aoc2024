#!/bin/bash

set -euo pipefail

if [[ "$#" -eq 0 ]]
then
  echo "Usage: $0 <day>"
  exit 1
fi

root=$(pwd)
while [[ ! -f "$root/dune-project" ]]
do
  [[ "$root" = "/" ]] && echo "dune-project not found" && exit 1
  root=$(dirname "$root")
done

day="$1"
day_padded=$(printf "day%02d" "$day")
input="$root/${day_padded}/main.input"

if [[ ! -d "$root/${day_padded}" ]]
then
  mkdir "$root/${day_padded}"
  echo -e "(executable\n  (name ${day_padded})\n  (libraries tools base pcre2))" > "$root/${day_padded}/dune"
  cat <<EOF > "$root/${day_padded}/${day_padded}.ml"
open Base

let () =
  let lines = Tools.read_lines () in
  let n = List.length lines in
Stdlib.Printf.printf "Solution 1: %d\n" n
EOF
fi

[[ -f "$input" ]] || wget -O "$input" --header "Cookie: session=${SESSION:?is not set}" "https://adventofcode.com/2024/day/$day/input"

if [[ "$#" -gt 1 ]]
then
  input="$2"
fi

( cd "$root" && dune build )
if [[ "$input" = "-" ]]
then
"$root/_build/default/${day_padded}/${day_padded}.exe"
else
"$root/_build/default/${day_padded}/${day_padded}.exe" < "$input"
fi
