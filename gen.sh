#!/bin/bash

num=$1
day="day$num"

# solution

mkdir -p "$day"

cat > "$day/dune" <<EOF
(executable
 (public_name $day)
 (name main)
 (libraries xmas containers))

(cram
 (deps %{bin:$day}))
EOF

cat > "$day/main.ml" <<EOF
open Containers

let input = IO.read_lines_l stdin

let _ = Printf.printf "part1=;part2="
EOF

# test files

mkdir -p "$day/test.t"

touch "$day/test.t/test"
touch "$day/test.t/input"

cat > "$day/test.t/run.t" <<EOF
https://adventofcode.com/2022/day/$num

  $ $day < test
  part1=;part2=

  $ $day < input
  part1=;part2=
EOF

dune build