open Core

let input = In_channel.(input_lines stdin)

module Move = struct
  type t = Rock | Paper | Scissors

  let of_string = function
    | "A" | "X" ->
        Rock
    | "B" | "Y" ->
        Paper
    | "C" | "Z" ->
        Scissors
    | _ ->
        exit 0

  let score = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let next = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock

  let prev = function Rock -> Scissors | Scissors -> Paper | Paper -> Rock
end

let points = function
  | [Move.Rock; Move.Paper]
  | [Move.Paper; Move.Scissors]
  | [Move.Scissors; Move.Rock] ->
      6
  | [Move.Rock; Move.Scissors]
  | [Move.Scissors; Move.Paper]
  | [Move.Paper; Move.Rock] ->
      0
  | _ ->
      3

let parse input =
  List.map ~f:(fun l -> String.split l ~on:' ') input
  |> List.map ~f:(fun r -> List.map r ~f:Move.of_string)

let parse' input =
  List.map ~f:(fun l -> String.split l ~on:' ') input
  |> List.map ~f:(function
       | [them; res] -> (
           let them = Move.of_string them in
           match res with
           | "X" ->
               [them; Move.prev them]
           | "Y" ->
               [them; them]
           | "Z" ->
               [them; Move.next them]
           | _ ->
               exit 0 )
       | _ ->
           exit 0 )

let part1 input =
  let rounds = parse input in
  let results = List.map ~f:points rounds in
  let self = List.map ~f:(function [_; m] -> Move.score m | _ -> 0) rounds in
  Xmas.Enum.sum self + Xmas.Enum.sum results

let part2 input =
  let rounds = parse' input in
  let results = List.map ~f:points rounds in
  let self = List.map ~f:(function [_; m] -> Move.score m | _ -> 0) rounds in
  Xmas.Enum.sum self + Xmas.Enum.sum results

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
