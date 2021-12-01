open Core

let input =
  let lines = In_channel.read_lines "./input/day01.txt" in
  List.map ~f:Int.of_string lines


let map a b = (b, if Int.is_positive a && b > a then 1 else 0)

let sum_list = List.fold ~init:0 ~f:Int.( + )

let part1 = input |> List.folding_map ~init:0 ~f:map |> sum_list

let slide3 = Xmas.Enum.window 3

let part2 =
  input
  |> slide3
  |> List.map ~f:sum_list
  |> List.folding_map ~init:0 ~f:map
  |> sum_list


let () =
  print_endline (Int.to_string part1) ;
  print_endline (Int.to_string part2)
