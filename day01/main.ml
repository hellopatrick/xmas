open Core
open Xmas.Option

let input = In_channel.(input_lines stdin)

let sorted_elves_capacity l =
  let f (elf, elves) r =
    let open Core.Option in
    int_of_string_opt r >>| (fun i -> (i + elf, elves)) |? lazy (0, elf :: elves)
  in
  let last, elves = List.fold l ~init:(0, []) ~f in
  last :: elves |> List.sort ~compare |> List.rev

let _ =
  let elves = sorted_elves_capacity input in
  let part1 = List.hd_exn elves in
  let part2 = List.take elves 3 |> Xmas.Enum.sum in
  Printf.printf "part1=%d;part2=%d" part1 part2
