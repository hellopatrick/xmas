open Core

type score = int

type board = int list list

type step = Stop of score | Continue of board

module IntPair = Tuple.Comparable (Int) (Int)

let bingo_numbers =
  In_channel.read_all "./input/day04a.txt"
  |> String.split ~on:',' |> List.map ~f:Int.of_string

let parse_board b =
  b |> List.map ~f:String.strip
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:String.strip)
  |> List.map ~f:(List.filter ~f:(fun s -> not (String.is_empty s)))
  |> List.map ~f:(List.map ~f:Int.of_string)

let boards =
  In_channel.read_lines "./input/day04b.txt"
  |> List.group ~break:(fun x y -> String.is_empty x || String.is_empty y)
  |> List.filter ~f:(fun l -> List.length l <> 1)
  |> List.map ~f:parse_board

let mark board n =
  List.map ~f:(List.map ~f:(fun s -> if s = n then -1 else s)) board

let is_winner board =
  let is_winner' = List.exists ~f:(List.for_all ~f:Int.is_negative) in
  is_winner' board || is_winner' (List.transpose_exn board)

let score board =
  let sum' r =
    r
    |> List.filter ~f:Int.is_non_negative
    |> List.reduce ~f:Int.( + ) |> Option.value ~default:0
  in
  List.fold ~init:0 ~f:(fun acc r -> acc + sum' r) board

let round board n =
  let marked_board = mark board n in
  if is_winner marked_board then Stop (n * score marked_board)
  else Continue marked_board

let play_board board numbers =
  let rec play' board numbers i =
    match numbers with
    | n :: tl -> (
      match round board n with
      | Stop score ->
          (i, score)
      | Continue board ->
          play' board tl (i + 1) )
    | _ ->
        raise (Failure "never scored")
  in
  play' board numbers 0

let play boards numbers =
  List.map boards ~f:(fun board -> play_board board numbers)

let played_results = play boards bingo_numbers

let part1 =
  List.min_elt played_results ~compare:IntPair.compare
  |> Option.map ~f:Tuple2.get2 |> Option.value ~default:0

let part2 =
  List.max_elt played_results ~compare:IntPair.compare
  |> Option.map ~f:Tuple2.get2 |> Option.value ~default:0

let () = Printf.printf "part1=%d; part2=%d" part1 part2
