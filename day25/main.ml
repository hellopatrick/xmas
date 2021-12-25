open Core

module Tile = struct
  type t = Empty | East | South

  let of_char = function '>' -> East | 'v' -> South | _ -> Empty

  let to_char = function Empty -> '.' | East -> '>' | South -> 'v'

  let equal a b =
    match (a, b) with
    | East, East | South, South | Empty, Empty ->
        true
    | _, _ ->
        false
end

module XY = Tuple.Comparable (Int) (Int)
module M = XY.Map

let parse lines =
  let m =
    List.foldi lines ~init:M.empty ~f:(fun y acc line ->
        String.foldi line ~init:acc ~f:(fun x acc c ->
            M.set acc ~key:(x, y) ~data:(Tile.of_char c) ) )
  in
  let (maxx, maxy), _ = M.max_elt_exn m in
  (maxx, maxy, m)

let maxx, maxy, input = In_channel.(input_lines stdin) |> parse

let xs = List.range ~start:`inclusive ~stop:`inclusive 0 maxx

let ys = List.range ~start:`inclusive ~stop:`inclusive 0 maxy

let shift_right m =
  List.fold ys ~init:(M.empty, 0) ~f:(fun acc y ->
      List.fold xs ~init:acc ~f:(fun (m', cnt) x ->
          let t = M.find_exn m (x, y) in
          match t with
          | Tile.East -> (
              let x' = if x < maxx then x + 1 else 0 in
              let neighbor = M.find_exn m (x', y) in
              match neighbor with
              | Tile.Empty ->
                  let m' = M.set m' ~key:(x, y) ~data:Tile.Empty in
                  let m' = M.set m' ~key:(x', y) ~data:Tile.East in
                  (m', cnt + 1)
              | _ ->
                  let m' = M.set m' ~key:(x, y) ~data:Tile.East in
                  (m', cnt) )
          | Tile.South ->
              let m' = M.set m' ~key:(x, y) ~data:Tile.South in
              (m', cnt)
          | Tile.Empty ->
              let m' =
                M.update m' (x, y) ~f:(function
                  | None ->
                      Tile.Empty
                  | Some t ->
                      t )
              in
              (m', cnt) ) )

let shift_down m =
  List.fold xs ~init:(M.empty, 0) ~f:(fun acc x ->
      List.fold ys ~init:acc ~f:(fun (m', cnt) y ->
          let t = M.find_exn m (x, y) in
          match t with
          | Tile.South -> (
              let y' = if y < maxy then y + 1 else 0 in
              let neighbor = M.find_exn m (x, y') in
              match neighbor with
              | Tile.Empty ->
                  let m' = M.set m' ~key:(x, y) ~data:Tile.Empty in
                  let m' = M.set m' ~key:(x, y') ~data:Tile.South in
                  (m', cnt + 1)
              | _ ->
                  let m' = M.set m' ~key:(x, y) ~data:Tile.South in
                  (m', cnt) )
          | Tile.East ->
              let m' = M.set m' ~key:(x, y) ~data:Tile.East in
              (m', cnt)
          | Tile.Empty ->
              let m' =
                M.update m' (x, y) ~f:(function
                  | None ->
                      Tile.Empty
                  | Some t ->
                      t )
              in
              (m', cnt) ) )

let pp m =
  List.iter ys ~f:(fun y ->
      List.iter xs ~f:(fun x ->
          let t = M.find_exn m (x, y) in
          let c = Tile.to_char t in
          Printf.printf "%c" c ) ;
      Printf.printf "\n" ) ;
  Printf.printf "----------\n"

let run m =
  let rec aux m rnd =
    let m, r = shift_right m in
    let m, d = shift_down m in
    if r + d > 0 then aux m (rnd + 1) else rnd
  in
  aux m 1

let part1 = run input

let _ = Printf.printf "part1=%d" part1
