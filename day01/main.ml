open Core

let input = In_channel.(input_lines stdin)

let parse_elves l =
  let lines =
    List.map l ~f:(fun l -> int_of_string_opt l |> Option.value ~default:0)
  in
  List.group lines ~break:(fun _ b -> b = 0)

let part1 l =
  let elves = parse_elves l in
  let carrying = List.map elves ~f:Xmas.Enum.sum in
  List.max_elt carrying ~compare:Int.compare |> Option.value ~default:0

let part2 l =
  let elves = parse_elves l in
  let carrying = List.map elves ~f:Xmas.Enum.sum in
  let carrying_sorted =
    List.sort carrying ~compare:(fun a b -> Int.neg (Int.compare a b))
  in
  let top_three = List.take carrying_sorted 3 in
  Xmas.Enum.sum top_three

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
