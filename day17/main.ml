open Core

let input = In_channel.input_all Stdio.stdin

type coord = {x: int; y: int}

type bounds = {upper_left: coord; bottom_right: coord}

let area =
  Scanf.sscanf input "target area: x=%d..%d, y=%d..%d" (fun x0 x1 y0 y1 ->
      {upper_left= {x= x0; y= y1}; bottom_right= {x= x1; y= y0}} )

let in_region area coord =
  coord.x >= area.upper_left.x
  && coord.y <= area.upper_left.y
  && coord.x <= area.bottom_right.x
  && coord.y >= area.bottom_right.y

let max_y area {x= vx; y= vy} =
  let rec aux {x= px; y= py} {x= vx; y= vy} max has_hit =
    let nx = px + vx in
    let ny = py + vy in
    let max = Int.max max ny in
    if nx > area.bottom_right.x || ny < area.bottom_right.y then
      if has_hit then Some max else None
    else
      let vx = Int.max 0 (vx - 1) in
      let vy = vy - 1 in
      aux {x= nx; y= ny} {x= vx; y= vy} max (in_region area {x= nx; y= ny})
  in
  aux {x= 0; y= 0} {x= vx; y= vy} 0 false

let possible_initial_velocities area =
  let min_vx = 1 in
  let max_vx = area.bottom_right.x in
  let min_vy = area.bottom_right.y in
  let max_vy = Int.neg area.bottom_right.y in
  let x_range =
    Sequence.range ~start:`inclusive ~stop:`inclusive min_vx max_vx
  in
  let y_range =
    Sequence.range ~start:`inclusive ~stop:`inclusive min_vy max_vy
  in
  Sequence.cartesian_product x_range y_range

let max_ys area =
  let possibilities = possible_initial_velocities area in
  Sequence.filter_map possibilities ~f:(fun (x, y) -> max_y area {x; y})

let part1 area =
  let max = max_ys area in
  Sequence.max_elt max ~compare:Int.compare |> Option.value ~default:0

let part2 area =
  let max = max_ys area in
  Sequence.length max

let _ = Printf.printf "part1=%d;part2=%d" (part1 area) (part2 area)
