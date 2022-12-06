open Core

let input = In_channel.(input_all stdin) |> String.to_list

let part' s n =
  let rec aux s i =
    let header = List.take s n in
    let unique = Char.Set.of_list header in
    if Char.Set.length unique = n then n + i else aux (List.tl_exn s) (i + 1)
  in
  aux s 0

let _ = Printf.printf "part1=%d;part2=%d" (part' input 4) (part' input 14)
