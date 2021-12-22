open Core

module Cuboid = struct
  type t = {xmin: int; xmax: int; ymin: int; ymax: int; zmin: int; zmax: int}

  let volume t =
    (t.xmax - t.xmin + 1) * (t.ymax - t.ymin + 1) * (t.zmax - t.zmin + 1)

  let intersection t s =
    if
      t.xmin <= s.xmax && s.xmin <= t.xmax && t.ymin <= s.ymax
      && s.ymin <= t.ymax && t.zmin <= s.zmax && s.zmin <= t.zmax
    then
      let cuboid =
        { xmin= Int.max t.xmin s.xmin
        ; xmax= Int.min t.xmax s.xmax
        ; ymin= Int.max t.ymin s.ymin
        ; ymax= Int.min t.ymax s.ymax
        ; zmin= Int.max t.zmin s.zmin
        ; zmax= Int.min t.zmax s.zmax }
      in
      Some cuboid
    else None
end

module Instruction = struct
  type t = On of Cuboid.t | Off of Cuboid.t

  let cuboid t = match t with On c -> c | Off c -> c
end

let parse line =
  if String.is_prefix line ~prefix:"on" then
    Scanf.sscanf line "on x=%d..%d,y=%d..%d,z=%d..%d"
      (fun xmin xmax ymin ymax zmin zmax ->
        Instruction.On {xmin; xmax; ymin; ymax; zmin; zmax} )
  else
    Scanf.sscanf line "off x=%d..%d,y=%d..%d,z=%d..%d"
      (fun xmin xmax ymin ymax zmin zmax ->
        Instruction.Off {xmin; xmax; ymin; ymax; zmin; zmax} )

let input = In_channel.(input_lines stdin) |> List.map ~f:parse

let rec count_untouched cube rest =
  let volume = Cuboid.volume cube in
  let rec aux rest cnt =
    match rest with
    | hd :: tl -> (
      match Cuboid.intersection hd cube with
      | None ->
          aux tl cnt
      | Some i ->
          aux tl (cnt + count_untouched i tl) )
    | _ ->
        cnt
  in
  let touched = aux rest 0 in
  volume - touched

let solve cubes =
  let rec aux cubes cnt =
    match cubes with
    | [] ->
        cnt
    | Instruction.On cube :: tl ->
        aux tl (cnt + count_untouched cube (List.map tl ~f:Instruction.cuboid))
    | _ :: tl ->
        aux tl cnt
  in
  aux cubes 0

(* original part1 was brute forced (see past commits.) *)
let part1 =
  let bounds =
    Cuboid.{xmin= -50; xmax= 50; ymin= -50; ymax= 50; zmin= -50; zmax= 50}
  in
  let constrained =
    input
    (* so ugly... *)
    |> List.filter_map ~f:(function
         | Instruction.On c ->
             Cuboid.intersection c bounds
             |> Option.map ~f:(fun i -> Instruction.On i)
         | Instruction.Off c ->
             Cuboid.intersection c bounds
             |> Option.map ~f:(fun i -> Instruction.Off i) )
  in
  solve constrained

let part2 = solve input

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
