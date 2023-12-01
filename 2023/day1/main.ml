open Containers

let input = IO.read_lines_l stdin

let outside s =
  let len = String.length s in
  if len = 0 then ""
  else
    let first = String.get s 0 in
    let last = String.get s (len - 1) in
    Format.sprintf "%c%c" first last

let calibration_value s =
  s
  |> String.filter Xmas.Parsing.is_digit
  |> outside |> Int.of_string |> Option.get_or ~default:0

let part1 input = List.map calibration_value input |> Xmas.Enum.sum

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

let replace_written_digits s =
  List.fold_left (fun s (sub, by) -> String.replace ~sub ~by s) s digits

let part2 input =
  input
  |> List.map replace_written_digits
  |> List.map calibration_value |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
