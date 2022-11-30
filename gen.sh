#!/bin/bash

num=$1
day="day$num"

# solution

mkdir -p "$day"

cat > "$day"/dune <<EOF
(executable
 (public_name $day)
 (name main)
 (libraries xmas core))
EOF

cat > "$day"/main.ml <<EOF
open Core

let input = In_channel.(input_lines stdin)

let _ = Printf.printf "part1=;part2="
EOF

# test files

mkdir -p test/"$day".t

touch test/"$day".t/test
touch test/"$day".t/input

cat > test/"$day".t/run.t <<EOF
https://adventofcode.com/2022/day/$num

  $ $day < test
  part1=;part2=

  $ $day < input
  part1=;part2=
EOF

cat >> test/dune <<EOF

(cram
 (applies_to $day)
 (deps %{bin:$day}))
EOF

dune build