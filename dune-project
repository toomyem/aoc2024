(lang dune 3.16)

(name aoc2024)

(generate_opam_files true)

(source
 (github toomyem/aoc2024))

(authors "toomyem")

(license MIT)

(documentation https://github.com/toomyem/aoc2024)

(package
 (name aoc2024)
 (synopsis "AOC 2024")
 (description "Solutions for Advent of Code 2024")
 (depends ocaml dune pcre2 yojson)
 (allow_empty)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
