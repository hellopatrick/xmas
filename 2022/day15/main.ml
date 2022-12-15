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

  let compare (c0, c1) (t0, t1) =
    let first = Int.compare c0 t0 in
    if first = 0 then Int.compare c1 t1 else first
end

let parse lines =
  let parse_line line =
    Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
      (fun sx sy bx by -> ((sx, sy), (bx, by)))
  in
  List.map parse_line lines

let find_taken_ranges sensor_ranges ty =
  let sensors =
    M.filter (fun (_, sy) range -> range > Int.abs (sy - ty)) sensor_ranges
  in
  let sweep (sx, sy) dist =
    let dd = dist - Int.abs (ty - sy) in
    (sx - dd, sx + dd)
  in
  M.fold
    (fun c d acc ->
      let r = sweep c d in
      match
        List.sorted_insert ~cmp:(fun a b -> Int.neg @@ Range.compare a b) r acc
      with
      | a :: b :: tl -> (
        match Range.merge a b with Some r -> r :: tl | _ -> a :: b :: tl )
      | otherwise ->
          otherwise )
    sensors []

let part1 info ty =
  let beacons_on_ty =
    List.filter_map
      (fun (_, (bx, by)) -> if by = ty then Some (bx, by) else None)
      info
    |> S.of_list |> S.cardinal
  in
  let sensor_ranges =
    List.map (fun (s, b) -> (s, C.manhattan_distance s b)) info |> M.of_list
  in
  let sum =
    find_taken_ranges sensor_ranges ty
    |> List.fold_left (fun acc r -> acc + Range.length r) 0
  in
  sum - beacons_on_ty

let part2 info max =
  let clamp = Range.clamp (0, max) in
  let sensor_ranges =
    List.map (fun (s, b) -> (s, C.manhattan_distance s b)) info |> M.of_list
  in
  let sweep = find_taken_ranges sensor_ranges in
  let rec aux ty =
    if ty > max then failwith "never found."
    else
      let ranges = sweep ty |> List.map clamp in
      let sum =
        ranges |> List.fold_left (fun acc r -> acc + Range.length r) 0
      in
      if 1 = max + 1 - sum then
        let front = List.hd ranges in
        let x =
          match front with
          | 0, a ->
              a + 1
          | a, b when b = max ->
              a - 1
          | _, b ->
              b + 1
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
