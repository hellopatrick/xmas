open Core

let input = In_channel.input_all Stdio.stdin |> String.strip

let hex_string_to_bytes str =
  str |> String.to_list
  |> List.concat_map ~f:(function
       | '0' -> [0; 0; 0; 0]
       | '1' -> [0; 0; 0; 1]
       | '2' -> [0; 0; 1; 0]
       | '3' -> [0; 0; 1; 1]
       | '4' -> [0; 1; 0; 0]
       | '5' -> [0; 1; 0; 1]
       | '6' -> [0; 1; 1; 0]
       | '7' -> [0; 1; 1; 1]
       | '8' -> [1; 0; 0; 0]
       | '9' -> [1; 0; 0; 1]
       | 'A' -> [1; 0; 1; 0]
       | 'B' -> [1; 0; 1; 1]
       | 'C' -> [1; 1; 0; 0]
       | 'D' -> [1; 1; 0; 1]
       | 'E' -> [1; 1; 1; 0]
       | _ -> [1; 1; 1; 1] )

let bits_to_int bits =
  bits |> List.fold ~init:0 ~f:(fun acc b -> Int.shift_left acc 1 + b)

let parse_version v0 v1 v2 = bits_to_int [v0; v1; v2]

type op = Sum | Product | Min | Max | GreaterThan | LessThan | Equal
[@@deriving show]

type packet = Literal of int * int list | Operator of int * op * packet list
[@@deriving show]

let parse_literal v bits =
  let rec aux extracted bits =
    match bits with
    | 1 :: a :: b :: c :: d :: tl -> aux (d :: c :: b :: a :: extracted) tl
    | 0 :: a :: b :: c :: d :: tl ->
        (List.rev (d :: c :: b :: a :: extracted), tl)
    | _ -> failwith "unexpected end in literal parsing" in
  let res, bits = aux [] bits in
  (Literal (v, res), bits)

let to_op o1 o2 o3 =
  match (o1, o2, o3) with
  | 0, 0, 0 -> Sum
  | 0, 0, 1 -> Product
  | 0, 1, 0 -> Min
  | 0, 1, 1 -> Max
  | 1, 0, 1 -> GreaterThan
  | 1, 1, 0 -> LessThan
  | 1, 1, 1 -> Equal
  | _ -> failwith "invalid op"

let rec parse bits =
  let v, bits =
    match bits with
    | v0 :: v1 :: v2 :: tl -> (parse_version v0 v1 v2, tl)
    | _ -> failwith "not enough bits for version" in
  let packet, rest =
    match bits with
    | 1 :: 0 :: 0 :: rest -> parse_literal v rest
    | o1 :: o2 :: o3 :: 0 :: rest ->
        parse_operator_length v (to_op o1 o2 o3) rest
    | o1 :: o2 :: o3 :: 1 :: rest ->
        parse_operator_count v (to_op o1 o2 o3) rest
    | _ -> failwith "?" in
  (packet, rest)

and parse_operator_length v op bits =
  let len, rest = List.split_n bits 15 in
  let len = bits_to_int len in
  let rest, remainder = List.split_n rest len in
  let rec aux bits children =
    let child, rest = parse bits in
    match rest with [] -> child :: children | _ -> aux rest (child :: children)
  in
  (Operator (v, op, List.rev (aux rest [])), remainder)

and parse_operator_count v op bits =
  let len, rest = List.split_n bits 11 in
  let count = bits_to_int len in
  let rec aux bits children count =
    if count = 0 then (children, bits)
    else
      let child, rest = parse bits in
      aux rest (child :: children) (count - 1) in
  let children, remainder = aux rest [] count in
  (Operator (v, op, List.rev children), remainder)

let input = hex_string_to_bytes input

let part1 =
  let t, _ = parse input in
  let rec aux packet sum =
    match packet with
    | Literal (version, _) -> sum + version
    | Operator (version, _, packets) ->
        sum + version + Xmas.Enum.sum (List.map packets ~f:(fun p -> aux p 0))
  in
  aux t 0

let rec eval = function
  | Literal (_, bits) -> bits_to_int bits
  | Operator (_, Sum, packets) ->
      List.fold packets ~init:0 ~f:(fun acc p -> acc + eval p)
  | Operator (_, Product, packets) ->
      List.fold packets ~init:1 ~f:(fun acc p -> acc * eval p)
  | Operator (_, Min, packets) ->
      List.map packets ~f:eval
      |> List.min_elt ~compare:Int.compare
      |> Option.value_exn
  | Operator (_, Max, packets) ->
      List.map packets ~f:eval
      |> List.max_elt ~compare:Int.compare
      |> Option.value_exn
  | Operator (_, GreaterThan, a :: b :: _) -> if eval a > eval b then 1 else 0
  | Operator (_, LessThan, a :: b :: _) -> if eval a < eval b then 1 else 0
  | Operator (_, Equal, a :: b :: _) -> if eval a = eval b then 1 else 0
  | _ -> failwith "invalid operation"

let part2 =
  let t, _ = parse input in
  eval t

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
