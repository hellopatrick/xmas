open Core

let input =
  In_channel.input_lines In_channel.stdin |> List.map ~f:String.to_list

let parse chars =
  let is_pair a b =
    match (a, b) with
    | '<', '>' | '(', ')' | '{', '}' | '[', ']' -> true
    | _ -> false
  in
  let rec aux chars history =
    match chars with
    | [] -> Either.Second.return history
    | ('(' as hd) :: tl
    | ('[' as hd) :: tl
    | ('{' as hd) :: tl
    | ('<' as hd) :: tl ->
        aux tl (hd :: history)
    | hd :: tl -> (
        match history with
        | [] -> Either.First.return hd
        | hd' :: tl' ->
            if is_pair hd' hd then aux tl tl' else Either.First.return hd)
  in
  aux chars []

let invalid_chars, incomplete = List.partition_map input ~f:parse

let part1 =
  let score = function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> raise Xmas.Exc.Unreachable
  in
  invalid_chars |> List.map ~f:score |> Xmas.Enum.sum

let part2 =
  let autocomplete_score =
    let score = function
      | '(' -> 1
      | '[' -> 2
      | '{' -> 3
      | '<' -> 4
      | _ -> raise Xmas.Exc.Unreachable
    in
    List.fold ~init:0 ~f:(fun acc c -> (acc * 5) + score c)
  in
  let scores =
    incomplete
    |> List.map ~f:autocomplete_score
    |> List.sort ~compare:Int.compare
  in
  let len = List.length scores in
  List.nth_exn scores (len / 2)

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
