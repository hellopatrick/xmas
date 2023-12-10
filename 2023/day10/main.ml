open Containers
module C = Xmas.Coordinate
module CM = Map.Make (C)
module CS = Set.Make (C)

module Input = struct
  let is_connectable map (x, y) allowed =
    match CM.get (x, y) map with
    | None -> None
    | Some c -> if List.exists (Char.equal c) allowed then Some (x, y) else None

  let parse input =
    let lines = input |> String.lines |> List.map String.to_list in
    List.foldi
      (fun acc y row -> List.foldi (fun acc x c -> CM.add (x, y) c acc) acc row)
      CM.empty lines

  let create_graph map =
    let is_connectable = is_connectable map in
    CM.mapi
      (fun (x, y) c ->
        match c with
        | '|' ->
            [
              is_connectable (x, y - 1) [ '|'; 'F'; '7'; 'S' ];
              is_connectable (x, y + 1) [ '|'; 'J'; 'L'; 'S' ];
            ]
        | '-' ->
            [
              is_connectable (x - 1, y) [ '-'; 'F'; 'L'; 'S' ];
              is_connectable (x + 1, y) [ '-'; 'J'; '7'; 'S' ];
            ]
        | 'L' ->
            [
              is_connectable (x, y - 1) [ '|'; '7'; 'F'; 'S' ];
              is_connectable (x + 1, y) [ '-'; '7'; 'J'; 'S' ];
            ]
        | 'J' ->
            [
              is_connectable (x, y - 1) [ '|'; 'F'; '7'; 'S' ];
              is_connectable (x - 1, y) [ '-'; 'F'; 'L'; 'S' ];
            ]
        | 'F' ->
            [
              is_connectable (x, y + 1) [ '|'; 'J'; 'L'; 'S' ];
              is_connectable (x + 1, y) [ '-'; 'J'; '7'; 'S' ];
            ]
        | '7' ->
            [
              is_connectable (x, y + 1) [ '|'; 'J'; 'L'; 'S' ];
              is_connectable (x - 1, y) [ '-'; 'F'; 'L'; 'S' ];
            ]
        | 'S' ->
            [
              is_connectable (x, y - 1) [ '|'; '7'; 'F' ];
              is_connectable (x, y + 1) [ '|'; 'L'; 'J' ];
              is_connectable (x - 1, y) [ '-'; 'F'; 'L' ];
              is_connectable (x + 1, y) [ '-'; 'J'; '7' ];
            ]
        | _ -> [])
      map
    |> CM.map List.keep_some
end

let input = IO.read_all stdin
let map = Input.parse input
let graph = Input.create_graph map

let ((sx, sy) as start), _ =
  CM.filter (fun _ c -> Char.equal c 'S') map |> CM.choose

let loop start graph =
  let rec aux (x, y) visited =
    let nexts = CM.get_or (x, y) graph ~default:[] in
    match List.find_opt (fun c -> not @@ CS.mem c visited) nexts with
    | Some n -> aux n (CS.add n visited)
    | None -> visited
  in
  aux start (CS.singleton start)

let fence = loop start graph
let part1 = CS.cardinal fence / 2
let min_x, min_y = (0, 0)

let max_x, max_y =
  CM.fold (fun (x, y) _ (mx, my) -> (max x mx, max y my)) graph (0, 0)

let xs = List.range min_x max_x
let ys = List.range min_y max_y

let cs =
  List.cartesian_product [ xs; ys ]
  |> List.filter_map (function [ x; y ] -> Some (x, y) | _ -> None)

let debug = function
  | '-' -> "─"
  | '|' -> "│"
  | 'L' -> "╰"
  | 'J' -> "╯"
  | '7' -> "╮"
  | 'F' -> "╭"
  | 'S' -> "S"
  | _ -> "░"

let is_inside (x, y) =
  if CS.mem (x, y) fence then false
  else
    let rec aux (x, y) cnt carry =
      if x < 0 then cnt mod 2 = 1
      else if CS.mem (x, y) fence then
        match CM.get (x, y) map with
        | Some '-' -> aux (x - 1, y) cnt carry
        | Some '7' as c -> aux (x - 1, y) cnt c
        | Some 'J' as c -> aux (x - 1, y) cnt c
        | Some 'L' ->
            aux
              (x - 1, y)
              (match carry with Some 'J' -> cnt | _ -> cnt + 1)
              None
        | Some 'F' ->
            aux
              (x - 1, y)
              (match carry with Some '7' -> cnt | _ -> cnt + 1)
              None
        | Some '|' -> aux (x - 1, y) (cnt + 1) carry
        | _ -> false
      else aux (x - 1, y) cnt carry
    in
    aux (x, y) 0 None

(* let _ =
   List.iter
     (fun y ->
       List.iter
         (fun x ->
           if CS.mem (x, y) fence then
             let c = CM.get_or ~default:'.' (x, y) map in
             Printf.printf "%s" (debug c)
           else if is_inside (x, y) then Printf.printf "▓"
           else Printf.printf "░")
         xs;
       Printf.printf "\n")
     ys *)

let part2 = List.map (fun (x, y) -> is_inside (x, y)) cs |> List.count Fun.id
let _ = Printf.printf "part1=%d;part2=%d" part1 part2
