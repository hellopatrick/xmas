open Containers
module C = Xmas.Coordinate.C
module CM = Xmas.Coordinate.Map
module CS = Xmas.Coordinate.Set

type square = Start | Rock | Garden

module InfiniteGrid = struct
  type t = { grid : square array array; height : int; width : int }

  let make grid =
    let height, width = (Array.length grid, Array.length grid.(0)) in
    { grid; height; width }

  let get t (x, y) =
    let x' = x mod t.width in
    let x' = if x' < 0 then t.width + x' else x' in
    let y' = y mod t.height in
    let y' = if y' < 0 then t.height + y' else y' in
    match Xmas.Grid.get t.grid (x', y') with
    | Some sq -> sq
    | None -> failwith "impossible"
end

module Input = struct
  let parse input =
    Array.of_list input
    |> Array.map (fun r ->
           String.to_array r
           |> Array.map (function
                | '#' -> Rock
                | '.' -> Garden
                | 'S' -> Start
                | _ -> failwith "invalid"))
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let maze = Input.parse input

let sx, sy =
  Array.find_map_i
    (fun y row ->
      Array.find_map_i
        (fun x sq -> match sq with Start -> Some (x, y) | _ -> None)
        row)
    maze
  |> Option.get_exn_or "start must exist."

let part1 =
  let neighbors maze (x, y) =
    [ (x, y + 1); (x, y - 1); (x + 1, y); (x - 1, y) ]
    |> List.filter (fun (x, y) ->
           match Xmas.Grid.get maze (x, y) with
           | None | Some Rock -> false
           | _ -> true)
  in
  let available_steps maze from =
    CS.fold (fun pt acc -> CS.add_list acc (neighbors maze pt)) from CS.empty
  in
  let rec aux maze n pts =
    if n = 0 then pts
    else
      let pts' = available_steps maze pts in
      aux maze (n - 1) pts'
  in
  aux maze 64 (CS.singleton (sx, sy)) |> CS.cardinal

let part2 =
  let maze = InfiniteGrid.make maze in

  let grids = 26501365 / maze.width in
  let rem = 26501365 mod maze.width in

  let neighbors maze (x, y) =
    [ (x, y + 1); (x, y - 1); (x + 1, y); (x - 1, y) ]
    |> List.filter (fun (x, y) ->
           match InfiniteGrid.get maze (x, y) with Rock -> false | _ -> true)
  in
  let available_steps maze from =
    CS.fold (fun pt acc -> CS.add_list acc (neighbors maze pt)) from CS.empty
  in
  let rec aux maze n pts =
    if n = 0 then pts
    else
      let pts' = available_steps maze pts in
      aux maze (n - 1) pts'
  in

  let a = aux maze rem (CS.singleton (sx, sy)) in
  let b = aux maze maze.width a in
  let c = aux maze maze.width b in

  let ab = CS.cardinal b - CS.cardinal a in
  let two_a = CS.cardinal c - CS.cardinal a - (2 * ab) in

  (two_a / 2 * (grids * grids)) + ((ab - (two_a / 2)) * grids) + CS.cardinal a

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
