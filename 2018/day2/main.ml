open Containers
module CM = Map.Make (Char)

let input = IO.read_lines_l stdin |> List.sort String.compare

let has n s =
  s |> String.to_list |> List.group_by
  |> List.exists (fun l -> n = List.length l)

let part1 =
  let with_two = List.filter (has 2) input |> List.length in
  let with_three = List.filter (has 3) input |> List.length in
  with_two * with_three

let off_by_one s t =
  let s' = String.to_list s in
  let t' = String.to_list t in
  let cs = List.combine s' t' in
  let diffs = List.count (fun (a, b) -> not (Char.equal a b)) cs in
  diffs = 1

let common_chars s t =
  let s' = String.to_list s in
  let t' = String.to_list t in
  let cs = List.combine s' t' in
  let same =
    List.filter_map (fun (a, b) -> if Char.equal a b then Some a else None) cs
  in
  String.of_list same

let part2 =
  let groups = List.group_succ ~eq:off_by_one input in
  List.find_map
    (function [ a; b ] -> Some (common_chars a b) | _ -> None)
    groups
  |> Option.get_exn_or "impossible"

let _ = Printf.printf "part1=%d;part2=%s" part1 part2
