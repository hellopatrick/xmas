open Containers

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let part1 = 0
let part2 = 0
let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
