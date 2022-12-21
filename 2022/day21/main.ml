open Containers
module M = Map.Make (String)

module Op = struct
  type t = Add | Sub | Mul | Div | Cmp

  let eval a b = function
    | Add ->
        a + b
    | Sub ->
        a - b
    | Mul ->
        a * b
    | Div ->
        a / b
    | Cmp ->
        Int.compare a b
end

module Monkey = struct
  type t = Op of Op.t * string * string | Val of int
end

module Parser = struct
  open Angstrom
  open Xmas.Parsing

  let name = take 4

  let num = number >>| fun i -> Monkey.Val i

  let op =
    whitespace *> (string "+" <|> string "*" <|> string "-" <|> string "/")
    <* whitespace

  let eval =
    lift3
      (fun a b c ->
        match b with
        | "+" ->
            Monkey.Op (Add, a, c)
        | "-" ->
            Monkey.Op (Sub, a, c)
        | "*" ->
            Monkey.Op (Mul, a, c)
        | "/" ->
            Monkey.Op (Div, a, c)
        | _ ->
            failwith "impossible" )
      name op name
    <|> num

  let prefix = name <* string ": "

  let line = lift2 (fun a b -> (a, b)) prefix eval

  let parse str = parse_string ~consume:All line str |> Result.get_or_failwith
end

let input = IO.read_lines_l stdin

let parse input =
  let monkeys = List.map Parser.parse input in
  List.fold_left
    (fun acc (name, monkey) -> M.add name monkey acc)
    M.empty monkeys

let eval m =
  let cache = Hashtbl.create 2000 in
  let rec aux n =
    match Hashtbl.get cache n with
    | Some v ->
        v
    | None ->
        let open Monkey in
        let v =
          match M.get n m with
          | Some (Val v) ->
              v
          | Some (Op (op, a, b)) ->
              Op.eval (aux a) (aux b) op
          | _ ->
              failwith "impossible"
        in
        Hashtbl.add cache n v ; v
  in
  aux "root"

let part1 input =
  let monkeys = parse input in
  eval monkeys

let part2 input =
  let m =
    M.update "root"
      (function
        | Some (Monkey.Op (_, a, b)) -> Some (Monkey.Op (Cmp, a, b)) | o -> o )
      (parse input)
  in
  let rec aux l r dir =
    let i = (l + r) lsr 1 in
    if i = l || i = r then -1
    else
      let m' = M.add "humn" (Monkey.Val i) m in
      let res = eval m' in
      if res = 0 then i
      else if dir then if res < 0 then aux l i dir else aux i r dir
      else if res < 0 then aux i r dir
      else aux l i dir
  in
  match aux 1 (Int.pow 10 13) true with
  | -1 ->
      aux 1 (Int.pow 10 13) false
  | v ->
      v

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
