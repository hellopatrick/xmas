open Containers

let input = IO.read_lines_l stdin

module C = Xmas.Coordinate
module M = Map.Make (C)
module S = Set.Make (C)

module Range = struct
  type t = int * int

  let overlap (t0, t1) (s0, s1) = t0 <= s1 && t1 >= s0

  let merge (t0, t1) (s0, s1) =
    if overlap (t0, t1) (s0, s1) then Some (Int.min t0 s0, Int.max t1 s1)
    else None

  let length (t0, t1) = t1 - t0 + 1

  let clamp (c0, c1) (t0, t1) = (Int.max c0 t0, Int.min c1 t1)
end

let parse lines =
  let parse_line line =
    Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
      (fun sx sy bx by -> ((sx, sy), (bx, by)))
  in
  List.map parse_line lines

let find_taken_ranges info ty =
  let free_regions =
    List.map (fun (s, b) -> (s, Xmas.Coordinate.manhattan_distance s b)) info
    |> M.of_list
  in
  let have_knowledge =
    List.map (fun ((sx, sy), _) -> ((sx, sy), Int.abs (sy - ty))) info
    |> List.filter (fun (s, td) ->
           match M.get s free_regions with None -> false | Some v -> td < v )
  in
  let sweep (sx, sy) dist =
    let c = (sx, ty) in
    let md = C.manhattan_distance (sx, sy) c in
    let dd = dist - md in
    (sx - dd, sx + dd)
  in
  List.map
    (fun ((sx, sy), _) ->
      let max_d = M.find (sx, sy) free_regions in
      sweep (sx, sy) max_d )
    have_knowledge
  |> List.sort (fun (s0, _) (t0, _) -> Int.compare s0 t0)
  |> List.fold_left
       (fun acc r ->
         match acc with
         | [] ->
             [r]
         | hd :: tl -> (
           match Range.merge r hd with
           | Some nr ->
               nr :: tl
           | _ ->
               r :: hd :: tl ) )
       []
  |> List.rev

let part1 info ty =
  let beacons_on_ty =
    List.filter_map
      (fun (_, (bx, by)) -> if by = ty then Some (bx, by) else None)
      info
    |> S.of_list |> S.cardinal
  in
  let sum =
    find_taken_ranges info ty
    |> List.fold_left (fun acc r -> acc + Range.length r) 0
  in
  sum - beacons_on_ty

let part2 info max =
  let clamper = Range.clamp (0, max) in
  let rec aux ty =
    if ty > max then -1
    else
      let ranges = find_taken_ranges info ty |> List.map clamper in
      let sum =
        ranges |> List.fold_left (fun acc r -> acc + Range.length r) 0
      in
      if 1 = max + 1 - sum then
        let x =
          match ranges with
          | (0, a) :: _ ->
              a + 1
          | (a, b) :: _ when b = max ->
              a - 1
          | (_, a) :: _ ->
              a + 1
          | _ ->
              failwith ""
        in
        (4000000 * x) + ty
      else aux (ty + 1)
  in
  aux 0

let _ =
  let ty, tl = List.hd_tl input in
  let info = parse tl in
  let ty = Int.of_string_exn ty in
  Printf.printf "part1=%d;part2=%d" (part1 info ty) (part2 info (ty * 2))
