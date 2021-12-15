open Core

module Coord = struct
  type t = int * int

  include Tuple.Hashable (Int) (Int)
  include Tuple.Comparable (Int) (Int)
end

type map = int Coord.Table.t

module CT = Coord.Table

let raw_input = In_channel.input_lines In_channel.stdin

let graph =
  let aux input =
    let len = List.length input in
    let tbl = Coord.Table.create () in
    let range = List.range 0 5 in
    List.iteri
      ~f:(fun y row ->
        row |> String.to_list
        |> List.iteri ~f:(fun x c ->
               List.iter range ~f:(fun dx ->
                   List.iter range ~f:(fun dy ->
                       Coord.Table.set
                         ~key:(x + (dx * len), y + (dy * len))
                         ~data:
                           (let data = Char.to_int c - 48 + dx + dy in
                            if data > 9 then (data % 10) + 1 else data)
                         tbl))))
      input;
    tbl
  in
  aux raw_input

let print graph n =
  let range = List.range 0 n in
  List.iter range ~f:(fun y ->
      List.iter range ~f:(fun x ->
          Printf.printf "%d" (CT.find_exn graph (x, y)));
      Printf.printf "\n")

module PQ = Xmas.Priority_queue

let neighbors graph (x, y) =
  let dirs = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
  List.map dirs ~f:(fun (dx, dy) -> (dx + x, dy + y))
  |> List.filter ~f:(fun k -> CT.mem graph k)

let dijkstra graph source goal =
  let d = CT.create () in
  let p = CT.create () in
  CT.set d ~key:source ~data:0;

  let q = PQ.insert PQ.empty 0 source in

  let rec aux q =
    match PQ.extract q with
    | None -> (d, p)
    | Some (_, u, q) ->
        if Coord.equal goal u then (d, p)
        else
          let neighbors = neighbors graph u in
          let q =
            List.fold neighbors ~init:q ~f:(fun acc v ->
                let dist = CT.find_exn d u in
                let len = CT.find_exn graph v in
                let curr =
                  CT.find_or_add d v ~default:(fun () -> Int.max_value)
                in
                let alt = dist + len in
                if alt < curr then (
                  CT.set d ~key:v ~data:alt;
                  CT.set p ~key:v ~data:u;
                  PQ.insert acc alt v)
                else acc)
          in
          aux q
  in

  aux q

let solve target =
  let tbl, _ = dijkstra graph (0, 0) (target, target) in
  CT.find_exn tbl (target, target)

let part1 = solve (List.length raw_input - 1)
let part2 = solve ((5 * List.length raw_input) - 1)
let () = Printf.printf "part1=%d;part2=%d" part1 part2
