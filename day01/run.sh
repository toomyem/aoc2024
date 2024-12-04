#!/bin/bash

set -euo pipefail
shopt -s extglob

day=$(basename "$(pwd)")
nr=${day/day*(0)}
exe="../_build/default/$day/main.exe"
input=puzzle.input

if [[ ! -s "$input" ]] 
then
  wget -O "$input" --header "Cookie: session=${SESSION:?is not set}" "https://adventofcode.com/2024/day/$nr/input"
fi

[[ "$#" -gt 0 ]] && input="$1"

dune build --no-print-directory

if [[ "$input" = "-" ]]
then
  "$exe"
else
  "$exe" < "$input"
fi
