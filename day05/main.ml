open Core
module IntPair = Tuple.Comparable (Int) (Int)

let parse_line str =
  Scanf.sscanf str "%d,%d -> %d,%d" (fun a b c d -> ((a, b), (c, d)))

let input = In_channel.read_lines "./input/day05.txt" |> List.map ~f:parse_line

let is_horizontal ((_, y0), (_, y1)) = y0 = y1

let is_vertical ((x0, _), (x1, _)) = x0 = x1

let is_horizontal_or_vertical line = is_horizontal line || is_vertical line

let sign i = if i > 0 then 1 else if i < 0 then -1 else 0

let compare sx a b = if sx > 0 then a <= b else if sx = 0 then a = b else a >= b

let draw_line map ((x0, y0), (x1, y1)) =
  let sx = sign (x1 - x0) in
  let sy = sign (y1 - y0) in
  let can_draw_x = compare sx in
  let can_draw_y = compare sy in
  let rec draw map x y =
    if can_draw_x x x1 && can_draw_y y y1 then
      draw
        (Map.update map (x, y) ~f:(function Some v -> v + 1 | None -> 1))
        (x + sx) (y + sy)
    else map
  in
  draw map x0 y0

let part1 =
  let input = List.filter input ~f:is_horizontal_or_vertical in
  let init = IntPair.Map.empty in
  List.fold input ~init ~f:draw_line
  |> Map.filter ~f:(fun c -> c > 1)
  |> Map.length

let part2 =
  let init = IntPair.Map.empty in
  List.fold input ~init ~f:draw_line
  |> Map.filter ~f:(fun c -> c > 1)
  |> Map.length

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
