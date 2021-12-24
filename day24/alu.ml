(* not used... but built anyways, for... fun? *)
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

let run_input state register stdin =
  match stdin with
  | hd :: tl ->
      let state = S.set state ~key:register ~data:hd in
      (state, tl)
  | _ ->
      failwith "no input"

let get_value state v =
  match v with Number n -> n | Register r -> S.find_exn state r

let debug map instruction =
  match instruction with
  | Add ("z", Register "y") :: _ ->
      let z = S.find_exn map "z" in
      let y = S.find_exn map "y" in
      printf "z=%d\n" (z + y)
  | _ ->
      ()

let run instructions stdin =
  let state = S.of_alist_exn [("w", 0); ("x", 0); ("y", 0); ("z", 0)] in
  let rec aux instructions state stdin =
    debug state instructions ;
    match instructions with
    | Input r :: rest ->
        let state, stdin = run_input state r stdin in
        aux rest state stdin
    | Add (r, v) :: rest ->
        let v = get_value state v in
        let state =
          S.update state r ~f:(function
            | None ->
                failwith "unknown register"
            | Some rv ->
                rv + v )
        in
        aux rest state stdin
    | Mul (r, v) :: rest ->
        let v = get_value state v in
        let state =
          S.update state r ~f:(function
            | None ->
                failwith "unknown register"
            | Some rv ->
                rv * v )
        in
        aux rest state stdin
    | Div (r, v) :: rest ->
        let v = get_value state v in
        let state =
          S.update state r ~f:(function
            | None ->
                failwith "unknown register"
            | Some rv ->
                rv / v )
        in
        aux rest state stdin
    | Mod (r, v) :: rest ->
        let v = get_value state v in
        let state =
          S.update state r ~f:(function
            | None ->
                failwith "unknown register"
            | Some rv ->
                rv % v )
        in
        aux rest state stdin
    | Eql (r, v) :: rest ->
        let v = get_value state v in
        let state =
          S.update state r ~f:(function
            | None ->
                failwith "unknown register"
            | Some rv ->
                if rv = v then 1 else 0 )
        in
        aux rest state stdin
    | _ ->
        state
  in
  aux instructions state stdin