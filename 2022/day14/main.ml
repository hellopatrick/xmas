open Containers
module C = Xmas.Coordinate
module M = Map.Make (C)

module P = struct
  open Angstrom
  open Xmas.Parsing

  let comma = char ','

  let coord = lift3 (fun x _ y -> (x, y)) number comma number

  let arrow = string " -> "

  let coord_list = sep_by arrow coord

  let parse line =
    parse_string ~consume:All coord_list line |> Result.get_or_failwith
end

module B = struct
  type t = Wall | Sand

  let equal t s =
    match (t, s) with Wall, Wall | Sand, Sand -> true | _ -> false
end

let draw_line (sx, sy) (fx, fy) m =
  let rec aux map now dir =
    let map' = M.add now B.Wall map in
    if C.equal now (fx, fy) then map' else aux map' (C.add dir now) dir
  in
  let dir =
    if sx = fx then (0, Int.sign (fy - sy)) else (Int.sign (fx - sx), 0)
  in
  aux m (sx, sy) dir

let parse input =
  let rec draw map path =
    match path with
    | (sx, sy) :: (fx, fy) :: tl ->
        let map' = draw_line (sx, sy) (fx, fy) map in
        draw map' ((fx, fy) :: tl)
    | _ ->
        map
  in
  let lines = List.map P.parse input in
  List.fold_left draw M.empty lines

let max_y m = M.fold (fun (_, y) _ acc -> Int.max y acc) m 0

let next_pos ?(floor = Int.max_int) (nx, ny) m =
  if ny + 1 >= floor then None
  else if M.find_opt (nx, ny + 1) m |> Option.is_none then Some (nx, ny + 1)
  else if M.find_opt (nx - 1, ny + 1) m |> Option.is_none then
    Some (nx - 1, ny + 1)
  else if M.find_opt (nx + 1, ny + 1) m |> Option.is_none then
    Some (nx + 1, ny + 1)
  else None

let run m =
  let my = max_y m in
  let rec track (nx, ny) m =
    if ny > my then (m, None)
    else
      match next_pos (nx, ny) m with
      | Some c ->
          track c m
      | None ->
          (M.add (nx, ny) B.Sand m, Some (nx, ny))
  in
  let rec aux s m =
    match track s m with
    | m', Some c when C.equal c s ->
        m'
    | m', Some _ ->
        aux s m'
    | m', None ->
        m'
  in
  aux (500, 0) m

let part1 m = m |> run |> M.filter (fun _ b -> B.equal b Sand) |> M.cardinal

let part2 m =
  let floor = 2 + max_y m in
  m
  |> draw_line (-1000, floor) (1000, floor)
  |> run
  |> M.filter (fun _ b -> B.equal b Sand)
  |> M.cardinal

let _ =
  let input = IO.read_lines_l stdin in
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
