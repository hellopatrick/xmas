open Containers

let input = IO.read_lines_l stdin
let is_numeric c = match c with '0' .. '9' -> true | _ -> false

let outside s =
  let len = String.length s in
  let first = String.get s 0 in
  let last = String.get s (len - 1) in
  Format.sprintf "%c%c" first last

let part1 input =
  List.map (String.filter is_numeric) input
  |> List.filter Xmas.Str.is_not_empty
  |> List.map outside
  |> List.filter_map Int.of_string
  |> List.fold_left ( + ) 0

(* well, since words can overlap, let's just sneak the digit into the word at the end,
   since fortunately, no digit spelled out overlaps more than this...*)
let digits =
  [
    ("one", "on1e");
    ("two", "tw2o");
    ("three", "thre3e");
    ("four", "fou4r");
    ("five", "fiv5e");
    ("six", "si6x");
    ("seven", "seve7n");
    ("eight", "eigh8t");
    ("nine", "nin9e");
  ]

let replace s =
  List.fold_left (fun s (sub, by) -> String.replace ~sub ~by s) s digits

let part2 input =
  let clean = List.map replace input in
  part1 clean

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
