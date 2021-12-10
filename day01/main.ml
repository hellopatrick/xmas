(* https://adventofcode.com/2021/day/1 *)

open Core

let input =
  In_channel.read_lines "./input/day01.txt" |> List.map ~f:Int.of_string

let is_increase a b = (b, if Int.is_positive a && b > a then 1 else 0)
let sum_list = List.fold ~init:0 ~f:Int.( + )
let part1 = input |> List.folding_map ~init:0 ~f:is_increase |> sum_list

let part2 =
  input |> Xmas.Enum.window 3 |> List.map ~f:sum_list
  |> List.folding_map ~init:0 ~f:is_increase
  |> sum_list

let () = Printf.printf "part1=%d; part2=%d" part1 part2
