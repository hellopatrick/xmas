open Core

type fold = X of int | Y of int

let fold_to_string = function
  | X x -> Printf.sprintf "x=%d" x
  | Y y -> Printf.sprintf "y=%d" y

type coord = int * int
type prompt = { coords : coord list; folds : fold list }

let parse lines =
  let parse_line line coords folds =
    if String.is_empty line then (coords, folds)
    else if String.is_prefix line ~prefix:"fold" then
      let fold =
        Scanf.sscanf line "fold along %c=%d" (fun axis coord ->
            if Char.equal axis 'x' then X coord else Y coord)
      in
      (coords, fold :: folds)
    else
      let coord = Scanf.sscanf line "%d,%d" (fun x y -> (x, y)) in
      (coord :: coords, folds)
  in
  let rec aux lines coords folds =
    match lines with
    | line :: tl ->
        let coords, folds = parse_line line coords folds in
        aux tl coords folds
    | _ -> (coords, folds)
  in
  let coords, folds = aux lines [] [] in
  { coords; folds = List.rev folds }

let input = In_channel.input_lines In_channel.stdin |> parse

module Points = Tuple.Comparable (Int) (Int)
module PM = Points.Map

let paper { coords; _ } =
  let init = PM.empty in
  List.fold coords ~init ~f:(fun acc key -> PM.set acc ~key ~data:1)

let fold paper instruction =
  let init, paper' =
    PM.partitioni_tf paper ~f:(fun ~key:(x, y) ~data:_ ->
        match instruction with X x' -> x < x' | Y y' -> y < y')
  in
  PM.fold paper' ~init ~f:(fun ~key:(x, y) ~data acc ->
      let x', y' =
        match instruction with
        | X x' -> ((2 * x') - x, y)
        | Y y' -> (x, (2 * y') - y)
      in
      PM.update acc (x', y') ~f:(function Some v -> v + data | None -> data))

let paper_to_string p =
  let keys = PM.keys p in
  let mx, _ =
    List.max_elt keys ~compare:(fun (x0, _) (x1, _) -> Int.compare x0 x1)
    |> Option.value_exn
  in
  let _, my =
    List.max_elt keys ~compare:(fun (_, y0) (_, y1) -> Int.compare y0 y1)
    |> Option.value_exn
  in
  let xs = List.range 0 (mx + 1) in
  let ys = List.range 0 (my + 1) in
  List.map ys ~f:(fun y ->
      List.map xs ~f:(fun x -> if PM.mem p (x, y) then "#" else ".")
      |> String.concat)
  |> String.concat ~sep:"\n"

let init = paper input

let part1 =
  let p =
    match input.folds with f :: _ -> fold init f | _ -> failwith "unreachable"
  in
  PM.length p

let part2 = List.fold input.folds ~init ~f:fold
let _ = Printf.printf "part1=%d\n\n" part1
let _ = Printf.printf "%s" (paper_to_string part2)