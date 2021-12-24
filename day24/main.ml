open Core

type register = string [@@deriving show]

type value = Register of register | Number of int [@@deriving show]

type instruction =
  | Input of register
  | Add of register * value
  | Mul of register * value
  | Div of register * value
  | Mod of register * value
  | Eql of register * value
[@@deriving show]

module S = Map.Make (String)

let parse_value v =
  match v with
  | "w" | "x" | "y" | "z" ->
      Register v
  | _ ->
      Number (Int.of_string v)

let parse l =
  match String.split l ~on:' ' with
  | ["inp"; r] ->
      Input r
  | ["add"; r; v] ->
      Add (r, parse_value v)
  | ["mul"; r; v] ->
      Mul (r, parse_value v)
  | ["div"; r; v] ->
      Div (r, parse_value v)
  | ["mod"; r; v] ->
      Mod (r, parse_value v)
  | ["eql"; r; v] ->
      Eql (r, parse_value v)
  | _ ->
      failwith "unknown instruction"

let input = In_channel.(input_lines stdin) |> List.map ~f:parse

let params =
  List.filteri input ~f:(fun i _ ->
      let m = i % 18 in
      m = 4 || m = 5 || m = 15 )
  |> List.chunks_of ~length:3
  |> List.map ~f:(function
       | [Div (_, Number z); Add (_, Number x); Add (_, Number w)] ->
           (z, x, w)
       | _ ->
           failwith "not possible" )

(*
  take the input and just whittle it down its basis
  z_divisor is either 1 or 26 (split evenly). 
  when it is 26 we want to be decreasing otherwise we'll never hit 0.
  we use this to whittle down our paths.
*)
let calculate (z_divisor, x_add, w_add) w z =
  if w = (z % 26) + x_add then z / z_divisor
  else w + w_add + (z / z_divisor * 26)

module IM = Map.Make (Int)

let constraints =
  let digits = List.range 1 10 in
  let init = IM.of_alist_exn [(0, (0, 0))] in
  List.fold params ~init ~f:(fun acc (z_divisor, x_add, w_add) ->
      let calculate' = calculate (z_divisor, x_add, w_add) in
      let init = IM.empty in
      (* for the previous zs, let's see the new zs that bring us here...*)
      IM.fold acc ~init ~f:(fun ~key:z ~data:(min, max) acc ->
          (* now we try each digit... *)
          List.fold digits ~init:acc ~f:(fun acc w ->
              let z' = calculate' w z in
              (* if not decreasing when z_divisor=26, then we cannot be approaching 0. *)
              if z_divisor = 1 || z' < z then
                let mn = (min * 10) + w in
                let mx = (max * 10) + w in
                (* if we've reached this z' via a new extreme path, keep it. *)
                IM.update acc z' ~f:(function
                  | None ->
                      (mn, mx)
                  | Some (mn', mx') ->
                      (Int.min mn mn', Int.max mx' mx) )
              else acc ) ) )

let _ =
  let min, max = IM.find_exn constraints 0 in
  Printf.printf "min=%d max=%d" min max
