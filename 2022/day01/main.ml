open Containers

let input = IO.(read_lines_l stdin)

let sorted_elves_capacity l =
  let f (elf, elves) r =
    if String.is_empty r then (0, elf :: elves)
    else (int_of_string r + elf, elves)
  in
  let last, elves = List.fold_left f (0, []) l in
  last :: elves |> List.sort compare |> List.rev

let _ =
  let elves = sorted_elves_capacity input in
  let part1 = List.hd elves in
  let part2 = List.take 3 elves |> Xmas.Enum.sum in
  Printf.printf "part1=%d;part2=%d" part1 part2
