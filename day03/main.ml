open Core

let input = In_channel.(input_lines stdin)

let priority c =
  match c with
  | 'a' .. 'z' ->
      -96 + Char.to_int c
  | 'A' .. 'Z' ->
      -38 + Char.to_int c
  | _ ->
      0

let part1 input =
  List.map
    ~f:(fun str ->
      let len = String.length str in
      let half = len / 2 in
      (String.sub str ~pos:0 ~len:half, String.sub str ~pos:half ~len:half) )
    input
  |> List.map ~f:(fun tuple -> Tuple2.map tuple ~f:String.to_list_rev)
  |> List.map ~f:(fun tuple -> Tuple2.map tuple ~f:Char.Set.of_list)
  |> List.map ~f:(fun (first, second) -> Char.Set.inter first second)
  |> List.map ~f:Char.Set.choose_exn
  |> List.map ~f:priority |> Xmas.Enum.sum

let part2 input =
  let rec aux rucksacks common =
    match rucksacks with
    | a :: b :: c :: tl ->
        let c = Char.Set.inter (Char.Set.inter a b) c |> Char.Set.choose_exn in
        aux tl (c :: common)
    | _ ->
        common
  in
  let i = List.map ~f:(fun l -> Char.Set.of_list (String.to_list l)) input in
  aux i [] |> List.map ~f:priority |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
