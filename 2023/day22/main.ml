open Containers

type coord = { x : int; y : int; z : int }
type brick = coord * coord

module Input = struct
  let parse_line line =
    Scanf.sscanf line "%d,%d,%d~%d,%d,%d" (fun x0 y0 z0 x1 y1 z1 ->
        ({ x = x0; y = y0; z = z0 }, { x = x1; y = y1; z = z1 }))

  let parse lines = List.map parse_line lines
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty

let bricks =
  Input.parse input |> List.sort (fun (_, b) (_, c) -> Int.compare b.z c.z)

let pp (b0, b1) =
  Printf.printf "%d,%d,%d~%d,%d,%d\n" b0.x b0.y b0.z b1.x b1.y b1.z

let fall bricks =
  let settle brick fallen =
    let rec aux (b0, b1) =
      let z = b0.z in
      if z <= 1 then (b0, b1)
      else
        let cannot_fall =
          Seq.range b0.x b1.x
          |> Seq.concat_map (fun x ->
                 Seq.range b0.y b1.y |> Seq.map (fun y -> (x, y)))
          |> Seq.exists (fun (x, y) ->
                 List.exists
                   (fun (c0, c1) ->
                     let z' = z - 1 in
                     (c0.x <= x && x <= c1.x)
                     && (c0.y <= y && y <= c1.y)
                     && c0.z <= z' && z' <= c1.z)
                   fallen)
        in
        if cannot_fall then (b0, b1)
        else aux ({ b0 with z = b0.z - 1 }, { b1 with z = b1.z - 1 })
    in
    aux brick
  in

  let rec aux falling fallen =
    match falling with
    | [] -> fallen
    | brick :: tl ->
        let brick' = settle brick fallen in
        aux tl (brick' :: fallen)
  in
  aux bricks [] |> List.rev

let settled_bricks = fall bricks
let equal_coord c d = c.x = d.x && c.y = d.y && c.z = d.z
let equal_brick (b0, b1) (c0, c1) = equal_coord b0 c0 && equal_coord b1 c1

let part2, part1 =
  List.foldi
    (fun (collapsing, useless) i _ ->
      let bricks' = List.remove_at_idx i settled_bricks in
      let settled_bricks' = fall bricks' in
      let changed =
        List.combine bricks' settled_bricks'
        |> List.count (fun (a, b) -> not @@ equal_brick a b)
      in
      (collapsing + changed, if changed > 0 then useless else useless + 1))
    (0, 0) settled_bricks

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
