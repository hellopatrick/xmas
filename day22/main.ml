open Core

module M = struct
  type t = int * int * int [@@deriving show, ord, eq, sexp, hash]
end

module Cuboid = struct
  type t = {xmin: int; xmax: int; ymin: int; ymax: int; zmin: int; zmax: int}
  [@@deriving show, ord, eq, sexp, hash]

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
  [@@deriving show, ord, eq, sexp, hash]

  let cuboid t = match t with On c -> c | Off c -> c
end

module CoordTable = Hashtbl.Make (M)

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

let part1 =
  let start = `inclusive and stop = `inclusive in
  let m = CoordTable.create () in
  let () =
    List.iter input ~f:(fun i ->
        let on, Cuboid.{xmin; xmax; ymin; ymax; zmin; zmax} =
          match i with
          | On {xmin; xmax; ymin; ymax; zmin; zmax} ->
              (true, {xmin; xmax; ymin; ymax; zmin; zmax})
          | Off {xmin; xmax; ymin; ymax; zmin; zmax} ->
              (false, {xmin; xmax; ymin; ymax; zmin; zmax})
        in
        let xs =
          Sequence.range ~start ~stop (Int.max (-50) xmin) (Int.min 50 xmax)
        and ys =
          Sequence.range ~start ~stop (Int.max (-50) ymin) (Int.min 50 ymax)
        and zs =
          Sequence.range ~start ~stop (Int.max (-50) zmin) (Int.min 50 zmax)
        in
        Sequence.iter xs ~f:(fun x ->
            Sequence.iter ys ~f:(fun y ->
                Sequence.iter zs ~f:(fun z ->
                    CoordTable.update m (x, y, z) ~f:(fun _ -> on) ) ) ) )
  in
  CoordTable.count m ~f:ident

let part2 =
  let rec aux cubes cnt =
    match cubes with
    | [] ->
        cnt
    | Instruction.On cube :: tl ->
        aux tl (cnt + count_untouched cube (List.map tl ~f:Instruction.cuboid))
    | _ :: tl ->
        aux tl cnt
  in
  aux input 0

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
