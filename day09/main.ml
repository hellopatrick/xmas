open Core
module Coords = Tuple.Comparable (Int) (Int)

let input =
  In_channel.read_lines "./input/day09.txt"
  |> List.foldi ~init:Coords.Map.empty ~f:(fun y m row ->
         String.foldi ~init:m
           ~f:(fun x m c -> Map.set ~key:(x, y) ~data:(Char.to_int c - 48) m)
           row)

let neighbors (x, y) = [ (x, y + 1); (x, y - 1); (x - 1, y); (x + 1, y) ]

let is_low_point map pt =
  let v = Map.find_exn map pt in
  let is_higher pt =
    Map.find map pt
    |> Option.map ~f:(fun i -> i > v)
    |> Option.value ~default:true
  in
  Xmas.Enum.all (neighbors pt) ~f:is_higher

let low_points = Map.filter_keys input ~f:(is_low_point input)

let part1 =
  low_points |> Map.map ~f:(fun h -> h + 1) |> Map.data |> Xmas.Enum.sum

let basin_size map start =
  let rec aux filled pt =
    match Coords.Map.find map pt with
    | Some 9 | None -> filled
    | Some _ ->
        if Coords.Set.mem filled pt then filled
        else List.fold ~init:(Coords.Set.add filled pt) ~f:aux (neighbors pt)
  in
  aux Coords.Set.empty start |> Coords.Set.length

let part2 =
  let low_points = low_points |> Map.keys in
  let basin_sizes = List.map low_points ~f:(basin_size input) in
  let sizes = List.sort basin_sizes ~compare:Int.compare |> List.rev in
  match sizes with
  | a :: b :: c :: _ -> a * b * c
  | _ -> raise Xmas.Exc.Unreachable

let () = Printf.printf "part1=%d; part2=%d" part1 part2