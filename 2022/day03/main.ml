open Containers

let input = IO.(read_lines_l stdin)

module CharSet = struct
  include Set.Make (Char)

  let of_string s = s |> String.to_iter |> of_iter
end

let priority c =
  match c with
  | 'a' .. 'z' -> -96 + Char.to_int c
  | 'A' .. 'Z' -> -38 + Char.to_int c
  | _ -> failwith "unsupported char"

let part1 input =
  List.map
    (fun str ->
      let len = String.length str in
      let len = len / 2 in
      ( CharSet.of_string (String.sub str 0 len),
        CharSet.of_string (String.sub str len len) ))
    input
  |> List.map (fun (first, second) -> CharSet.inter first second)
  |> List.fold_left (fun acc c -> acc + priority (CharSet.choose c)) 0

let part2 input =
  let rec aux rucksacks common =
    match rucksacks with
    | a :: b :: c :: tl ->
        let cc = CharSet.inter a b |> CharSet.inter c |> CharSet.choose in
        aux tl (cc :: common)
    | _ -> common
  in
  let i = List.map CharSet.of_string input in
  aux i [] |> List.fold_left (fun acc c -> acc + priority c) 0

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
