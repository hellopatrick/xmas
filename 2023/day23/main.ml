open Containers
module CS = Xmas.Coordinate.Set
module CM = Xmas.Coordinate.Map

module Square = struct
  type t = Path | Forest | UpSlope | DownSlope | LeftSlope | RightSlope

  let of_char = function
    | '.' -> Path
    | '#' -> Forest
    | '^' -> UpSlope
    | 'v' -> DownSlope
    | '>' -> RightSlope
    | '<' -> LeftSlope
    | _ -> failwith "unknown char"
end

module Dir = struct
  type t = Up | Down | Left | Right

  let equal t s =
    match (t, s) with
    | Up, Up -> true
    | Down, Down -> true
    | Left, Left -> true
    | Right, Right -> true
    | _, _ -> false
end

module Input = struct
  let parse lines =
    List.map
      (fun line -> String.to_array line |> Array.map Square.of_char)
      lines
    |> Array.of_list
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let forest = Input.parse input
let start = (1, 0)

let goal =
  let n = Array.length forest in
  (n - 2, n - 1)

let dfs forest succ start goal =
  let rec aux visited v steps =
    if Xmas.Coordinate.equal v goal then Some 0
    else if CS.mem v visited then None
    else
      let visited' = CS.add v visited in
      let neighbors = succ forest v in
      let distances =
        List.filter_map
          (fun ((x, y), dist) ->
            match aux visited' (x, y) (steps + 1) with
            | None -> None
            | Some d -> Some (d + dist))
          neighbors
      in
      match distances with
      | [] -> None
      | _ -> Some (List.fold_left max 0 distances)
  in
  aux CS.empty start 0

let part1 =
  let succ grid (x, y) =
    [
      (x - 1, y, Dir.Left);
      (x + 1, y, Dir.Right);
      (x, y - 1, Dir.Up);
      (x, y + 1, Dir.Down);
    ]
    |> List.filter (fun (x, y, dir) ->
           match Xmas.Grid.get grid (x, y) with
           | None -> false
           | Some Square.Forest -> false
           | Some Square.Path -> true
           | Some Square.UpSlope -> Dir.equal Up dir
           | Some Square.DownSlope -> Dir.equal Down dir
           | Some Square.LeftSlope -> Dir.equal Left dir
           | Some Square.RightSlope -> Dir.equal Right dir)
    |> List.map (fun (x, y, _) -> ((x, y), 1))
  in
  dfs forest succ start goal |> Option.get_exn_or "?"

let follow_path graph start curr =
  let get n = CM.get_or n graph ~default:[] in
  let rec aux prev curr cnt =
    match get curr with
    | [ (a, _); (b, _) ] ->
        if Xmas.Coordinate.equal a prev then aux curr b (cnt + 1)
        else aux curr a (cnt + 1)
    | _ -> (curr, cnt)
  in
  aux start curr 1

let consolidate graph =
  CM.fold
    (fun (x, y) neighbors acc ->
      if List.length neighbors <> 2 then
        CM.add (x, y)
          (List.map
             (fun ((nx, ny), _) -> follow_path graph (x, y) (nx, ny))
             neighbors)
          acc
      else acc)
    graph CM.empty

let prune forest =
  let adj =
    Array.foldi
      (fun acc y row ->
        Array.foldi
          (fun acc x sq ->
            match sq with
            | Square.Forest -> acc
            | _ ->
                let neighbors =
                  [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
                  |> List.filter_map (fun (x', y') ->
                         match Xmas.Grid.get forest (x', y') with
                         | None -> None
                         | Some Square.Forest -> None
                         | Some _ -> Some ((x', y'), 1))
                in
                CM.add (x, y) neighbors acc)
          acc row)
      CM.empty forest
  in
  consolidate adj

let part2 =
  let succ forest (x, y) = CM.get_or (x, y) forest ~default:[] in
  dfs (prune forest) succ start goal |> Option.get_exn_or "?"

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
