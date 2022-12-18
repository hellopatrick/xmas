open Containers

let input = IO.read_lines_l stdin

module C = struct
  type t = int * int * int

  let compare (x0, y0, z0) (x1, y1, z1) =
    match Int.compare x0 x1 with
    | 0 -> (
      match Int.compare y0 y1 with
      | 0 -> (
        match Int.compare z0 z1 with 0 -> 0 | e -> e )
      | e ->
          e )
    | e ->
        e

  let neighbors (x, y, z) =
    [ (x + 1, y, z)
    ; (x - 1, y, z)
    ; (x, y + 1, z)
    ; (x, y - 1, z)
    ; (x, y, z - 1)
    ; (x, y, z + 1) ]
end

module R = struct
  type t = {min: int; max: int}

  let contains t v = t.min <= v && v <= t.max
end

module S = struct
  include Set.Make (C)

  let nmem elt t = not @@ mem elt t

  let exclude f t = filter (fun elt -> not @@ f elt) t

  let bounds t =
    let minx, maxx =
      fold
        (fun (x, _, _) (mn, mx) -> (Int.min x mn, Int.max x mx))
        t (Int.max_int, Int.min_int)
    in
    let miny, maxy =
      fold
        (fun (_, y, _) (mn, mx) -> (Int.min y mn, Int.max y mx))
        t (Int.max_int, Int.min_int)
    in
    let minz, maxz =
      fold
        (fun (_, _, z) (mn, mx) -> (Int.min z mn, Int.max z mx))
        t (Int.max_int, Int.min_int)
    in
    R.
      ( {min= minx - 1; max= maxx + 1}
      , {min= miny - 1; max= maxy + 1}
      , {min= minz - 1; max= maxz + 1} )
end

let parse input =
  let parse_line line = Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z)) in
  List.map parse_line input |> S.of_list

let part1 input =
  let cubes = parse input in
  S.fold
    (fun c acc ->
      C.neighbors c
      |> List.fold_left
           (fun acc c -> if S.mem c cubes then acc else acc + 1)
           acc )
    cubes 0

let part2 input =
  let cubes = parse input in
  let rx, ry, rz = S.bounds cubes in
  let start = (rx.min, ry.min, rz.min) in
  let rec loop q v =
    match q with
    | [] ->
        v
    | cube :: q ->
        let visited = S.add cube v in
        let next =
          C.neighbors cube
          |> List.filter (fun (x, y, z) ->
                 R.contains rx x && R.contains ry y && R.contains rz z
                 && (not @@ S.mem (x, y, z) cubes)
                 && (not @@ S.mem (x, y, z) visited) )
        in
        let visited = S.add_list visited next in
        loop (next @ q) visited
  in
  let visited = loop [start] (S.singleton start) in
  S.fold
    (fun c acc ->
      C.neighbors c
      |> List.fold_left
           (fun acc c -> if S.mem c visited then acc + 1 else acc)
           acc )
    cubes 0

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
