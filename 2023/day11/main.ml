open Containers
module C = Xmas.Coordinate
module CM = C.Map

module Space = struct
  type t = Galaxy | Empty | BlankX | BlankY

  let is_empty = function Empty | BlankX | BlankY -> true | _ -> false

  let of_char = function
    | '#' -> Galaxy
    | '.' -> Empty
    | _ -> failwith "invalid"
end

type t = Galaxy | Empty

module Input = struct
  let parse_line line = line |> String.to_list |> List.map Space.of_char

  let parse ?(expansion = 1) input =
    let lines = List.map parse_line input in

    let empty blank lines =
      List.map
        (fun a ->
          if List.for_all Space.is_empty a then
            List.init (List.length a) (fun _ -> blank)
          else a)
        lines
    in

    let map =
      lines |> empty Space.BlankY |> Xmas.Enum.transpose |> empty Space.BlankX
      |> Xmas.Enum.transpose
    in

    let m, _, _ =
      List.foldi
        (fun (acc, _, dy) y line ->
          let dy' =
            if List.exists (function Space.BlankY -> true | _ -> false) line
            then expansion
            else 0
          in

          List.foldi
            (fun (acc, dx, dy) x c ->
              match c with
              | Space.Empty | Space.BlankY -> (acc, dx, dy)
              | Space.BlankX -> (acc, dx + expansion, dy)
              | Space.Galaxy ->
                  (CM.add (x + dx, y + dy) Space.Galaxy acc, dx, dy))
            (acc, 0, dy + dy')
            line)
        (CM.empty, 0, 0) map
    in
    m
end

let input = IO.read_lines_l stdin

let solve universe =
  let locs = CM.keys universe |> List.of_iter in
  let pairs =
    Xmas.Enum.choose 2 locs
    |> List.filter_map (function [ x; y ] -> Some (x, y) | _ -> None)
  in
  List.map (fun (a, b) -> Xmas.Coordinate.manhattan_distance a b) pairs
  |> List.fold_left ( + ) 0

let part1 = Input.parse input |> solve
let part2 = Input.parse ~expansion:(1_000_000 - 1) input |> solve
let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
