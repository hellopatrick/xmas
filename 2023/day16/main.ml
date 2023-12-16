open Containers

type t =
  | Empty
  | LeftLeaningMirror
  | RightLeaningMirror
  | HorizontalSplitter
  | VerticalSplitter

type dir = Up | Down | Left | Right

let dir_to_int = function Up -> 0 | Down -> 1 | Left -> 2 | Right -> 3

let t_of_string = function
  | '.' -> Empty
  | '\\' -> LeftLeaningMirror
  | '/' -> RightLeaningMirror
  | '-' -> HorizontalSplitter
  | '|' -> VerticalSplitter
  | _ -> failwith "unsupported"

module CM = Xmas.Coordinate.Map
module CS = Xmas.Coordinate.Set

module BS = Set.Make (struct
  type t = (int * int) * dir

  let compare (p, d) (q, d') =
    let pq = Xmas.Coordinate.compare p q in
    if pq <> 0 then pq else Int.compare (dir_to_int d) (dir_to_int d')
end)

module Input = struct
  let parse input =
    List.foldi
      (fun map y line ->
        String.foldi (fun map x c -> CM.add (x, y) (t_of_string c) map) map line)
      CM.empty input
end

let input =
  IO.read_lines_l stdin |> List.filter (fun line -> not @@ String.is_empty line)

let step p = function
  | Up -> Xmas.Coordinate.add (0, -1) p
  | Down -> Xmas.Coordinate.add (0, 1) p
  | Left -> Xmas.Coordinate.add (-1, 0) p
  | Right -> Xmas.Coordinate.add (1, 0) p

let next map p dir =
  match CM.get p map with
  | None -> []
  | Some Empty -> [ dir ]
  | Some LeftLeaningMirror ->
      [
        (match dir with
        | Up -> Left
        | Down -> Right
        | Left -> Up
        | Right -> Down);
      ]
  | Some RightLeaningMirror ->
      [
        (match dir with
        | Up -> Right
        | Down -> Left
        | Left -> Down
        | Right -> Up);
      ]
  | Some HorizontalSplitter -> (
      match dir with Left | Right -> [ dir ] | Up | Down -> [ Left; Right ])
  | Some VerticalSplitter -> (
      match dir with Left | Right -> [ Up; Down ] | Up | Down -> [ dir ])

let map = Input.parse input

let part1 =
  let rec aux energized seen beams =
    match beams with
    | [] -> energized
    | (p, dir) :: rest ->
        if BS.mem (p, dir) seen then aux energized seen rest
        else if not @@ CM.mem p map then aux energized seen rest
        else
          let energized = CS.add p energized in
          let seen = BS.add (p, dir) seen in
          let qs = next map p dir |> List.map (fun dir -> (step p dir, dir)) in
          aux energized seen (List.append rest qs)
  in
  aux CS.empty BS.empty [ ((0, 0), Right) ] |> CS.cardinal

let part2 = 0
let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
