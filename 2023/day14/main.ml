open Containers
module CM = Xmas.Coordinate.Map

type t = Round | Square | Ground

module Input = struct
  let parse lines =
    List.foldi
      (fun acc y row ->
        String.foldi
          (fun acc x c ->
            match c with
            | 'O' -> CM.add (x, y) Round acc
            | '#' -> CM.add (x, y) Square acc
            | '.' -> CM.add (x, y) Ground acc
            | _ -> acc)
          acc row)
      CM.empty lines
end

let input =
  IO.read_lines_l stdin |> List.filter (fun line -> not @@ String.is_empty line)

type dir = North | South | West | East

let move dir coord =
  match dir with
  | North -> Xmas.Coordinate.add coord (0, -1)
  | South -> Xmas.Coordinate.add coord (0, 1)
  | East -> Xmas.Coordinate.add coord (1, 0)
  | West -> Xmas.Coordinate.add coord (-1, 0)

let can_move dir coord grid =
  match CM.get coord grid with
  | Some Round -> (
      match CM.get (move dir coord) grid with Some Ground -> true | _ -> false)
  | _ -> false

let rec tilt dir grid =
  let move = move dir in

  let rocks = CM.filter (fun c _ -> can_move dir c grid) grid in
  if CM.cardinal rocks > 0 then
    let grid' =
      CM.fold
        (fun c _ acc -> CM.add_list acc [ (c, Ground); (move c, Round) ])
        rocks grid
    in
    tilt dir grid'
  else grid

let cycle grid = grid |> tilt North |> tilt West |> tilt South |> tilt East
let start = Input.parse input

let load grid =
  let my = 1 + CM.fold (fun (_, y) _ acc -> max y acc) grid 0 in
  CM.fold
    (fun (_, y) c acc -> match c with Round -> acc + (my - y) | _ -> acc)
    grid 0

let part1 = start |> tilt North |> load

let part2 =
  let grids = Hashtbl.create 10000 in
  let loads = Hashtbl.create 10000 in
  let rec aux grid i =
    match Hashtbl.get grids grid with
    | Some j -> (j, i)
    | None ->
        Hashtbl.add grids grid i;
        Hashtbl.add loads i (load grid);
        aux (cycle grid) (i + 1)
  in
  let first, next = aux start 0 in
  let n = (1_000_000_000 - first) mod (next - first) in
  let j = first + n in
  Hashtbl.get loads j |> Option.get_exn_or "load should exist."

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
