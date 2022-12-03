open Core

let input = In_channel.(input_lines stdin)

module CharSet = struct
  include Char.Set

  let of_string = Fn.compose of_list String.to_list_rev
end

let priority c =
  match c with
  | 'a' .. 'z' ->
      -96 + Char.to_int c
  | 'A' .. 'Z' ->
      -38 + Char.to_int c
  | _ ->
      failwith "unsupported char"

let part1 input =
  List.map
    ~f:(fun str ->
      let len = String.length str in
      let len = len / 2 in
      ( CharSet.of_string (String.sub str ~pos:0 ~len)
      , CharSet.of_string (String.sub str ~pos:len ~len) ) )
    input
  |> List.map ~f:(fun (first, second) -> CharSet.inter first second)
  |> List.fold ~init:0 ~f:(fun acc c -> acc + priority (CharSet.choose_exn c))

let part2 input =
  let rec aux rucksacks common =
    match rucksacks with
    | a :: b :: c :: tl ->
        let cc = CharSet.inter a b |> CharSet.inter c |> CharSet.choose_exn in
        aux tl (cc :: common)
    | _ ->
        common
  in
  let i = List.map ~f:CharSet.of_string input in
  aux i [] |> List.fold ~init:0 ~f:(fun acc c -> acc + priority c)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
