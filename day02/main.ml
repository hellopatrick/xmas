open Core

let input = In_channel.(input_lines stdin)

module Move = struct
  type t = Rock | Paper | Scissors

  let of_char = function
    | 'A' | 'X' ->
        Rock
    | 'B' | 'Y' ->
        Paper
    | 'C' | 'Z' ->
        Scissors
    | _ ->
        failwith "Invalid move"

  let value = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let next = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock

  let prev = function Rock -> Scissors | Scissors -> Paper | Paper -> Rock

  let compare a b =
    match (a, b) with
    | Rock, Paper | Paper, Scissors | Scissors, Rock ->
        -1
    | Paper, Rock | Scissors, Paper | Rock, Scissors ->
        1
    | _ ->
        0
end

module Round = struct
  type t = {me: Move.t; them: Move.t}

  let points {me; them} =
    let res =
      match Move.compare me them with -1 -> 0 | 0 -> 3 | 1 -> 6 | _ -> 0
    in
    res + Move.value me
end

let parse input =
  List.map
    ~f:(fun l -> Round.{me= Move.of_char l.[2]; them= Move.of_char l.[0]})
    input

let parse' input =
  List.map
    ~f:(fun l ->
      let them = Move.of_char l.[0] in
      match l.[2] with
      | 'X' ->
          Round.{them; me= Move.prev them}
      | 'Y' ->
          Round.{them; me= them}
      | 'Z' ->
          Round.{them; me= Move.next them}
      | _ ->
          failwith "invalid round ending" )
    input

let part1 input =
  input |> parse |> List.fold ~init:0 ~f:(fun acc r -> acc + Round.points r)

let part2 input =
  input |> parse' |> List.fold ~init:0 ~f:(fun acc r -> acc + Round.points r)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
