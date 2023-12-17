open Containers

module Turn = struct
  type t = Left | Right [@@deriving eq]
end

module Dir = struct
  type t = North | South | East | West [@@deriving eq]

  let turn (turn : Turn.t) t =
    match (t, turn) with
    | East, Left -> North
    | East, Right -> South
    | North, Left -> West
    | North, Right -> East
    | South, Left -> East
    | South, Right -> West
    | West, Left -> South
    | West, Right -> North

  let step pt t =
    let open Xmas.Coordinate in
    match t with
    | North -> add pt (0, -1)
    | South -> add pt (0, 1)
    | East -> add pt (1, 0)
    | West -> add pt (-1, 0)
end

module Crucible = struct
  type t = { pos : Xmas.Coordinate.t; dir : Dir.t; momentum : int }

  let equal t s = Xmas.Coordinate.equal t.pos s.pos
  let is_at pt { pos; _ } = Xmas.Coordinate.equal pt pos
  let start = { pos = (0, 0); dir = East; momentum = 0 }
end

module Input = struct
  let parse lines =
    let to_int c = int_of_char c - int_of_char '0' in
    List.map (fun row -> String.to_array row |> Array.map to_int) lines
    |> Array.of_list
end

let graph city f =
  let f = f city in
  CCGraph.make (fun c ->
      let edges =
        f c
        |> List.filter_map (fun (c : Crucible.t) ->
               Xmas.Grid.get city c.pos |> Option.map (fun v -> (v, c)))
      in
      List.to_iter edges)

let walk graph (x, y) =
  let tbl = CCGraph.mk_table ~eq:Crucible.equal (x * y) in
  let dist = Fun.id in
  let start = CCGraph.Iter.return Crucible.start in
  let goal = Crucible.is_at (x, y) in
  let iter = CCGraph.Traverse.dijkstra ~tbl ~dist ~graph start in
  let _, dist, _ = Iter.find_pred_exn (fun (c, _, _) -> goal c) iter in
  dist

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let city = Input.parse input
let goal = List.length input - 1

let part1 =
  let neighbors city ({ pos; dir; momentum } : Crucible.t) =
    let left = Dir.turn Left dir in
    let right = Dir.turn Right dir in

    let nl = Dir.step pos left in
    let nr = Dir.step pos right in
    let nf = Dir.step pos dir in

    let possiblities : Crucible.t list =
      if momentum < 2 then
        [
          { pos = nl; dir = left; momentum = 0 };
          { pos = nr; dir = right; momentum = 0 };
          { pos = nf; dir; momentum = momentum + 1 };
        ]
      else
        [
          { pos = nl; dir = left; momentum = 0 };
          { pos = nr; dir = right; momentum = 0 };
        ]
    in
    List.filter (fun Crucible.{ pos; _ } -> Xmas.Grid.mem city pos) possiblities
  in
  walk (graph city neighbors) (goal, goal)

let part2 =
  let neighbors city ({ pos; dir; momentum } : Crucible.t) =
    let left = Dir.turn Left dir in
    let right = Dir.turn Right dir in

    let nl = Dir.step pos left in
    let nr = Dir.step pos right in
    let nf = Dir.step pos dir in

    let possiblities : Crucible.t list =
      if momentum < 3 then [ { pos = nf; dir; momentum = momentum + 1 } ]
      else if momentum < 9 then
        [
          { pos = nl; dir = left; momentum = 0 };
          { pos = nr; dir = right; momentum = 0 };
          { pos = nf; dir; momentum = momentum + 1 };
        ]
      else
        [
          { pos = nl; dir = left; momentum = 0 };
          { pos = nr; dir = right; momentum = 0 };
        ]
    in
    List.filter (fun Crucible.{ pos; _ } -> Xmas.Grid.mem city pos) possiblities
  in
  walk (graph city neighbors) (goal, goal)

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
