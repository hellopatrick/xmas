open Containers

let input = IO.(read_all stdin) |> String.to_list

module CS = Set.Make (Char)

let part' s n =
  let rec aux s i =
    let header = List.take n s in
    let unique = CS.of_list header in
    if CS.cardinal unique = n then n + i else aux (List.tl s) (i + 1)
  in
  aux s 0

let _ = Printf.printf "part1=%d;part2=%d" (part' input 4) (part' input 14)
