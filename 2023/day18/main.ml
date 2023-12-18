open Containers
module CM = Xmas.Coordinate.Map

module Dir = struct
  type t = Up | Down | Left | Right

  let of_char = function
    | 'U' | '3' -> Up
    | 'D' | '1' -> Down
    | 'L' | '2' -> Left
    | 'R' | '0' -> Right
    | _ -> failwith "invalid"

  let to_vector = function
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Right -> (1, 0)
    | Left -> (-1, 0)
end

type action = { dir : Dir.t; count : int; hex : string }

module Input = struct
  let parse_line line =
    Scanf.sscanf line "%c %d (#%[0-9a-z])" (fun c count hex ->
        { dir = Dir.of_char c; count; hex })

  let parse = List.map parse_line
end

let dig actions =
  let p, r =
    List.fold_left
      (fun (p, acc) action ->
        match acc with
        | pt :: tl ->
            let v = Dir.to_vector action.dir in
            let l = action.count in
            let dp = Xmas.Coordinate.scalar_mul l v in
            (p + l, Xmas.Coordinate.add pt dp :: pt :: tl)
        | _ -> failwith "not allowed")
      (0, [ (0, 0) ])
      actions
  in
  (p, List.rev r)

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let pairwise vertices = List.combine_shortest vertices (List.tl vertices)

let shoelace vertices =
  let pairs = pairwise vertices in
  let area =
    List.fold_left
      (fun acc ((x, y), (x', y')) -> acc + ((x + x') * (y' - y)))
      0 pairs
    |> Int.abs
  in
  area / 2

let part1 =
  let actions = Input.parse input in
  let perimeter, vertices = dig actions in
  let area = shoelace vertices in
  area - (perimeter / 2) + 1 + perimeter

let fix action =
  let hex = action.hex in
  let dir = String.get hex 5 |> Dir.of_char in
  let count = int_of_string (Printf.sprintf "0x%s" (String.sub hex 0 5)) in
  { dir; count; hex }

let part2 =
  let actions = Input.parse input |> List.map fix in
  let perimeter, vertices = dig actions in
  let area = shoelace vertices in
  area - (perimeter / 2) + 1 + perimeter

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
