open Core
module Coords = Tuple.Comparable (Int) (Int)
module CM = Coords.Map

type state = Age of int | Flashing | Flashed

let input =
  In_channel.read_lines "./input/day11.txt"
  |> List.foldi ~init:CM.empty ~f:(fun i m row ->
         row |> String.to_list
         |> List.map ~f:(fun c -> Char.to_int c - 48)
         |> List.foldi ~init:m ~f:(fun j m v ->
                CM.add_exn m ~key:(j, i) ~data:(Age v)))

let age =
  let f = function Age 9 -> Flashing | Age v -> Age (v + 1) | _ -> Flashed in
  CM.map ~f

let find_neighbors (x, y) =
  [
    (x - 1, y - 1);
    (x - 1, y);
    (x - 1, y + 1);
    (x, y - 1);
    (x, y + 1);
    (x + 1, y - 1);
    (x + 1, y);
    (x + 1, y + 1);
  ]

let print m =
  let c = List.range 0 10 in
  List.iter c ~f:(fun y ->
      List.iter c ~f:(fun x ->
          match CM.find_exn m (x, y) with
          | Age v -> Printf.printf "%d" v
          | Flashing -> Printf.printf "*"
          | Flashed -> Printf.printf ".");
      Printf.printf "\n");
  Printf.printf "\n"

let handle m =
  let rec aux m =
    let m' =
      CM.mapi m ~f:(fun ~key ~data ->
          match data with
          | Age v ->
              let ns = find_neighbors key in
              let nearby_flashing =
                List.count ns ~f:(fun n ->
                    match CM.find m n with Some Flashing -> true | _ -> false)
              in
              Age (v + nearby_flashing)
          | otherwise -> otherwise)
      |> CM.map ~f:(function
           | Age v when v > 9 -> Flashing
           | Flashing -> Flashed
           | state -> state)
    in
    let has_flashing = CM.exists m' ~f:(phys_equal Flashing) in
    if has_flashing then aux m' else m'
  in
  aux (age m)

let reset =
  CM.map ~f:(function
    | Flashed -> Age 0
    | Flashing -> failwith "cannot reset flashing"
    | age -> age)

let _ =
  Xmas.Benchmark.time "part1=%d" (fun _ ->
      let rec aux m r total =
        if r > 0 then
          let m' = handle m in
          let dt = CM.count ~f:(phys_equal Flashed) m' in
          aux (reset m') (r - 1) (total + dt)
        else total
      in
      aux input 100 0)

let _ =
  Xmas.Benchmark.time "part2=%d" (fun _ ->
      let rec aux m r =
        let m' = handle m in
        let all_flashed = CM.for_all m' ~f:(phys_equal Flashed) in
        if all_flashed then r else aux (reset m') (r + 1)
      in
      aux input 1)
