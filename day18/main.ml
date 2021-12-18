open Core

type number =
  | Regular of int [@printer fun fmt i -> fprintf fmt "%d" i]
  | Pair of (number * number)
      [@printer
        fun fmt (a, b) -> fprintf fmt "[%s,%s]" (show_number a) (show_number b)]
[@@deriving show]

let parse line =
  let json = Yojson.Basic.from_string line in
  let rec aux inc =
    match inc with
    | `List [left; right] ->
        Pair (aux left, aux right)
    | `Int regular ->
        Regular regular
    | _ ->
        failwith "invalid number"
  in
  aux json

let explode a =
  let rec add dir a v =
    match a with
    | Regular n ->
        Regular (n + v)
    | Pair (a, b) -> (
      match dir with
      | `left ->
          Pair (add dir a v, b)
      | `right ->
          Pair (a, add dir b v) )
  in
  let rec helper a lvl =
    match a with
    | Regular _ ->
        None
    | Pair (Regular a, Regular b) ->
        if lvl >= 4 then Some (Regular 0, a, b) else None
    | Pair (left, right) -> (
      match helper left (lvl + 1) with
      | Some (n, a, b) ->
          Some (Pair (n, add `left right b), a, 0)
      | None -> (
        match helper right (lvl + 1) with
        | Some (n, a, b) ->
            Some (Pair (add `right left a, n), 0, b)
        | None ->
            None ) )
  in
  helper a 0 |> Option.map ~f:Tuple3.get1

let rec split a =
  match a with
  | Regular n ->
      if n < 10 then None
      else Some (Pair (Regular (n / 2), Regular ((n + 1) / 2)))
  | Pair (a, b) -> (
    match split a with
    | Some spl ->
        Some (Pair (spl, b))
    | None -> (
      match split b with Some spl -> Some (Pair (a, spl)) | None -> None ) )

let rec simplify a =
  let exp = explode a in
  match exp with
  | Some exp ->
      simplify exp
  | None -> (
    match split a with Some spl -> simplify spl | None -> a )

let add a b = simplify (Pair (a, b))

let rec magnitude = function
  | Regular v ->
      v
  | Pair (a, b) ->
      (3 * magnitude a) + (2 * magnitude b)

let input = In_channel.input_lines Stdio.stdin |> List.map ~f:parse

let part1 =
  List.reduce input ~f:add |> Option.map ~f:magnitude |> Option.value_exn

let part2 =
  Sequence.cartesian_product (Sequence.of_list input) (Sequence.of_list input)
  |> Sequence.map ~f:(fun (a, b) -> add a b)
  |> Sequence.map ~f:magnitude
  |> Sequence.max_elt ~compare:Int.compare
  |> Option.value_exn

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
