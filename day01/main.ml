open Core
open Xmas.Option

let input = In_channel.(input_lines stdin)

let sorted_elves_capacity l =
  List.map l ~f:(fun l -> int_of_string_opt l |? lazy 0)
  |> List.group ~break:(fun a _ -> a = 0)
  |> List.map ~f:Xmas.Enum.sum
  |> List.sort ~compare:Int.compare
  |> List.rev

let elves = sorted_elves_capacity input

let part1 = List.hd_exn elves

let part2 = List.take elves 3 |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
