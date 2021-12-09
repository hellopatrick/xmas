open Core

let input =
  In_channel.read_all "./input/day07.txt"
  |> String.split ~on:',' |> List.map ~f:Int.of_string

let range =
  let min = input |> List.min_elt ~compare:Int.compare |> Option.value_exn in
  let max = input |> List.max_elt ~compare:Int.compare |> Option.value_exn in
  List.range min max

let part1 =
  let calculate_total_cost input dest =
    List.fold ~init:0 ~f:(fun acc n -> acc + Int.abs (n - dest)) input
  in
  let costs = List.map ~f:(calculate_total_cost input) range in
  List.min_elt ~compare:Int.compare costs |> Option.value_exn

let calculate_total_cost input dest =
  let calculate_individual_cost dest i =
    let d = Int.abs (i - dest) in
    d * (d + 1) / 2
  in
  List.fold ~init:0
    ~f:(fun acc n -> acc + calculate_individual_cost dest n)
    input

let part2 =
  let costs = List.map ~f:(calculate_total_cost input) range in
  List.min_elt ~compare:Int.compare costs |> Option.value_exn

let () = Printf.printf "part1=%d; part2=%d;" part1 part2