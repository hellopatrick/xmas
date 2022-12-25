open Containers
module C = Xmas.Coordinate

module M = struct
  include Map.Make (C)

  (* not mem *)
  let nmem a m = not (mem a m)
end

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

let draw_wall (sx, sy) (fx, fy) m =
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
        let map' = draw_wall (sx, sy) (fx, fy) map in
        draw map' ((fx, fy) :: tl)
    | _ -> map
  in
  let lines = List.map P.parse input in
  List.fold_left draw M.empty lines

let max_y m = M.fold (fun (_, y) _ acc -> Int.max y acc) m 0

let next_pos ?(floor = Int.max_int) (x, y) m =
  let y' = y + 1 in
  if y' >= floor then None
  else if M.nmem (x, y') m then Some (x, y')
  else if M.nmem (x - 1, y') m then Some (x - 1, y')
  else if M.nmem (x + 1, y') m then Some (x + 1, y')
  else None

let run ?(floor = Int.max_int) ?(void = Int.max_int) m =
  let rec track (x, y) m =
    if y > void then (m, None)
    else
      match next_pos ~floor (x, y) m with
      | Some c -> track c m
      | None -> (M.add (x, y) B.Sand m, Some (x, y))
  in
  let rec aux s m =
    match track s m with
    | m', Some c when C.equal c s -> m'
    | m', Some _ -> aux s m'
    | m', None -> m'
  in
  aux (500, 0) m

let count_sand _ b acc = if B.(equal b Sand) then acc + 1 else acc

let part1 m =
  let m' = m |> run ~void:(max_y m) in
  M.fold count_sand m' 0

let part2 m =
  let m' = m |> run ~floor:(2 + max_y m) in
  M.fold count_sand m' 0

let _ =
  let input = IO.read_lines_l stdin in
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
