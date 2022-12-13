#!/bin/bash

num=$1
day="day$num"
year="2022"

# solution

mkdir -p "$year/$day"

cat > "$year/$day/dune" <<EOF
(executable
 (public_name $day)
 (name main)
 (libraries xmas containers))

(cram
 (deps %{bin:$day}))
EOF

cat > "$year/$day/main.ml" <<EOF
open Containers

let input = IO.read_lines_l stdin

let _ = Printf.printf "part1=;part2="
EOF

# test files

mkdir -p "$year/$day/test.t"

touch "$year/$day/test.t/test"
touch "$year/$day/test.t/input"

cat > "$year/$day/test.t/run.t" <<EOF
https://adventofcode.com/2022/day/$num

  $ $day < test
  part1=;part2=

  $ $day < input
  part1=;part2=
EOF

dune build
