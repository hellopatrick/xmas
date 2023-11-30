#!/bin/bash

num=$1
day="day$num"
year="${2:-2023}"
exc="$day.$year"
path="$year/$day"

# solution

mkdir -p "$path"

cat > "$path/dune" <<EOF
(executable
 (public_name $exc)
 (name main)
 (libraries xmas containers))

(cram
 (deps %{bin:$exc}))
EOF

cat > "$path/main.ml" <<EOF
open Containers

let input = IO.read_lines_l stdin

let _ = Printf.printf "part1=;part2="
EOF

# test files

mkdir -p "$path/test.t"

touch "$path/test.t/test"
touch "$path/test.t/input"

cat > "$path/test.t/run.t" <<EOF
https://adventofcode.com/$year/day/$num

  $ $exc < test
  part1=;part2=

  $ $exc < input
  part1=;part2=
EOF

dune build
