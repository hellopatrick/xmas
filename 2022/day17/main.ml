open Containers

let input = IO.read_all stdin |> String.trim

module J = struct
  type t = Left | Right

  let of_char = function
    | '<' ->
        Left
    | '>' ->
        Right
    | _ ->
        failwith "invalid char"

  let show = function Left -> "<" | _ -> ">"

  let dir = function Left -> (-1, 0) | _ -> (1, 0)
end

module S = struct
  type t = Flat | Plus | J | I | Square

  let seq = [|Flat; Plus; J; I; Square|]

  let coords = function
    | Flat ->
        [(0, 0); (1, 0); (2, 0); (3, 0)]
    | Plus ->
        [(0, 1); (1, 1); (2, 1); (1, 0); (1, 2)]
    | J ->
        [(0, 0); (1, 0); (2, 0); (2, 1); (2, 2)]
    | I ->
        [(0, 0); (0, 1); (0, 2); (0, 3)]
    | Square ->
        [(0, 0); (0, 1); (1, 0); (1, 1)]
end

module C = Xmas.Coordinate

module TS = struct
  include Set.Make (C)

  let max_y t = fold (fun (_, y) acc -> max y acc) t (-1)

  let max_height_for_column x t =
    fold (fun (x', y) acc -> if x' = x then max y acc else acc) t (-1)

  let forget t =
    let h = max_y t in
    filter (fun (_, y) -> h - 100 <= y) t

  let hit t (x, y) shape =
    List.exists
      (fun (dx, dy) ->
        let x', y' = (x + dx, y + dy) in
        mem (x', y') t || x' < 0 || y' < 0 || x' > 6 )
      shape

  let settle t (x, y) shape =
    List.fold_left
      (fun acc (dx, dy) ->
        let pos = (x + dx, y + dy) in
        add pos acc )
      t shape

  let pp t =
    let y = max_y t in
    List.range y 0
    |> List.iter (fun y ->
           List.iter
             (fun x -> if mem (x, y) t then print_char '#' else print_char ' ')
             (List.range 0 6) ;
           print_newline () )

  let key t =
    let max = max_y t in
    ( max - max_height_for_column 0 t
    , max - max_height_for_column 1 t
    , max - max_height_for_column 2 t
    , max - max_height_for_column 3 t
    , max - max_height_for_column 4 t
    , max - max_height_for_column 5 t
    , max - max_height_for_column 6 t )
end

module Tetris = struct
  type t = {board: TS.t; shape_idx: int; jets: J.t array; jet_idx: int}

  let current_shape i = S.seq.(i mod 5)

  let current_jet i jets =
    let n = Array.length jets in
    jets.(i mod n)

  let height {board; _} = 1 + TS.max_y board

  let key {board; shape_idx; jet_idx; jets} =
    (shape_idx mod 5, jet_idx mod Array.length jets, TS.key board)

  let simulate {board; shape_idx; jets; jet_idx} =
    let coord = (2, 4 + TS.max_y board) in
    let shape = current_shape shape_idx in
    let piece = S.coords shape in
    let rec aux jet_idx coord =
      let j = current_jet jet_idx jets in
      let dir = J.dir j in
      let coord' = C.add dir coord in
      let coord' = if TS.hit board coord' piece then coord else coord' in
      let coord'' = C.add (0, -1) coord' in
      let coord'' = if TS.hit board coord'' piece then coord' else coord'' in
      let jet_idx = jet_idx + 1 in
      if C.equal coord' coord'' then
        let board = TS.settle board coord'' piece in
        let board = TS.forget board in
        {board; jets; jet_idx; shape_idx= shape_idx + 1}
      else aux jet_idx coord''
    in
    aux jet_idx coord
end

let parse input = String.to_array input |> Array.map J.of_char

let show l = Printf.sprintf "[%s]" (List.map C.pp l |> String.concat "; ")

let show_list l =
  Printf.printf "[%s]\n" (List.map string_of_int l |> String.concat ", ")

let part1 input =
  let open Tetris in
  let jets = parse input in
  let tetris = {board= TS.empty; shape_idx= 0; jets; jet_idx= 0} in
  let rec aux t = if t.shape_idx >= 2022 then t else aux @@ Tetris.simulate t in
  aux tetris |> Tetris.height

let part2 input =
  let open Tetris in
  let end_state = 10_000_00_000_000 in
  let jets = parse input in
  let t = {board= TS.empty; shape_idx= 0; jets; jet_idx= 0} in
  let cache = Hashtbl.create 100 in
  let rec aux target t =
    if t.shape_idx >= target then (t, Tetris.height t, (0, 0))
    else
      let t' = Tetris.simulate t in
      let key = Tetris.key t in
      match Hashtbl.get cache key with
      | Some (h, si) ->
          (t', Tetris.height t', (h, si))
      | None ->
          Hashtbl.add cache key (Tetris.height t', t'.shape_idx) ;
          aux target t'
  in
  let t, h, (ph, psi) = aux end_state t in
  let di = t.shape_idx - psi in
  let dh = h - ph in
  let remaining = end_state - t.shape_idx in
  let repeats = remaining / di in
  let height = h + (dh * repeats) in
  Hashtbl.clear cache ;
  let _, h', _ = aux (t.shape_idx + (remaining mod di)) t in
  height + (h' - h)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
