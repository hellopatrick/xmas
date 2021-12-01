open Core

let input =
  let lines = In_channel.read_lines "./input/day01.txt" in
  List.map ~f:Int.of_string lines

let map a b = (b, if Int.is_positive a && b > a then 1 else 0)

let part1 =
  input
  |> List.folding_map ~init:0 ~f:map
  |> List.reduce ~f:Int.( + ) |> Option.value ~default:0

let slide3 list =
  let rec helper list state =
    match list with
    | a :: b :: c :: tail -> helper (b :: c :: tail) ([ a; b; c ] :: state)
    | _ -> state
  in
  helper list [] |> List.rev

let map3 a b =
  let sum = List.fold ~init:0 ~f:Int.( + ) b in
  (sum, if Int.is_positive a && sum > a then 1 else 0)

let part2 =
  input |> slide3
  |> List.folding_map ~init:0 ~f:map3
  |> List.reduce ~f:Int.( + ) |> Option.value ~default:0

let () =
  print_endline (Int.to_string part1);
  print_endline (Int.to_string part2)
