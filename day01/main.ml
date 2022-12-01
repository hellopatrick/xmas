open Core
open Xmas.Option

let input = In_channel.(input_lines stdin)

let parse_elves l =
  List.map l ~f:(fun l -> int_of_string_opt l |? lazy 0)
  |> List.group ~break:(fun a _ -> a = 0)

let part1 l =
  parse_elves l |> List.map ~f:Xmas.Enum.sum
  |> List.max_elt ~compare:Int.compare
  |? lazy 0

let part2 l =
  let elves =
    parse_elves l |> List.map ~f:Xmas.Enum.sum
    |> List.sort ~compare:Int.compare
    |> List.rev
  in
  List.take elves 3 |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
