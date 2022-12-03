open Core
open Xmas.Fn

let input = In_channel.(input_lines stdin)

let priority c =
  match c with
  | 'a' .. 'z' ->
      -96 + Char.to_int c
  | 'A' .. 'Z' ->
      -38 + Char.to_int c
  | _ ->
      failwith "unsupported char"

let char_set_of_string = Char.Set.of_list % String.to_list_rev

let part1 input =
  List.map
    ~f:(fun str ->
      let full_len = String.length str in
      let len = full_len / 2 in
      (String.sub str ~pos:0 ~len, String.sub str ~pos:len ~len) )
    input
  |> List.map ~f:(fun tuple -> Tuple2.map tuple ~f:char_set_of_string)
  |> List.map ~f:(fun (first, second) -> Char.Set.inter first second)
  |> List.fold ~init:0 ~f:(fun acc c -> acc + priority (Char.Set.choose_exn c))

let part2 input =
  let rec aux rucksacks common =
    match rucksacks with
    | a :: b :: c :: tl ->
        let cc =
          Char.Set.inter a b |> Char.Set.inter c |> Char.Set.choose_exn
        in
        aux tl (cc :: common)
    | _ ->
        common
  in
  let i = List.map ~f:char_set_of_string input in
  aux i [] |> List.fold ~init:0 ~f:(fun acc c -> acc + priority c)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
