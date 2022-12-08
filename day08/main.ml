open Containers

let input = IO.read_lines_l stdin

module Coordinate = struct
  type t = int * int

  let compare (t0, t1) (s0, s1) =
    match Int.compare t0 s0 with 0 -> Int.compare t1 s1 | v -> v

  let to_string (x, y) = Printf.sprintf "(%d, %d)" x y
end

module Map = struct
  include Map.Make (Coordinate)

  let is_visible coord m =
    let h = get_or coord m ~default:(-1) in
    let rec aux (x, y) (x', y') =
      match get (x + x', y + y') m with
      | None ->
          true
      | Some h' ->
          if h > h' then aux (x + x', y + y') (x', y') else false
    in
    aux coord (0, 1)
    || aux coord (0, -1)
    || aux coord (1, 0)
    || aux coord (-1, 0)

  let score coord m =
    let h = get_or coord m ~default:(-1) in
    let rec aux (x, y) (x', y') count =
      match get (x + x', y + y') m with
      | None ->
          count
      | Some h' ->
          if h > h' then aux (x + x', y + y') (x', y') (count + 1)
          else count + 1
    in
    aux coord (0, 1) 0
    * aux coord (0, -1) 0
    * aux coord (1, 0) 0
    * aux coord (-1, 0) 0
end

let parse lines =
  List.foldi
    (fun acc y line ->
      let chars = String.to_list line in
      List.foldi
        (fun acc x c ->
          let h = Char.code c - Char.code '0' in
          Map.add (x, y) h acc )
        acc chars )
    Map.empty lines

let part1 m = m |> Map.filter (fun c _ -> Map.is_visible c m) |> Map.cardinal

let part2 m =
  let scores = m |> Map.mapi (fun c _ -> Map.score c m) in
  Map.fold (fun _ v max -> if v > max then v else max) scores 0

let map = parse input

let _ = Printf.printf "part1=%d;part2=%d" (part1 map) (part2 map)
