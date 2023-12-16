open Containers

module Component = struct
  type t =
    | Empty
    | LeftLeaningMirror
    | RightLeaningMirror
    | HorizontalSplitter
    | VerticalSplitter
  [@@deriving ord, eq]

  let of_char = function
    | '.' -> Empty
    | '\\' -> LeftLeaningMirror
    | '/' -> RightLeaningMirror
    | '-' -> HorizontalSplitter
    | '|' -> VerticalSplitter
    | _ -> failwith "unsupported"
end

module Dir = struct
  type t = Up | Down | Left | Right [@@deriving ord, eq]
end

module CM = Xmas.Coordinate.Map
module CS = Xmas.Coordinate.Set

module BS = Set.Make (struct
  type t = Xmas.Coordinate.t * Dir.t [@@deriving ord]
end)

module Input = struct
  let parse input =
    List.foldi
      (fun map y line ->
        String.foldi
          (fun map x c -> CM.add (x, y) (Component.of_char c) map)
          map line)
      CM.empty input
end

let input =
  IO.read_lines_l stdin |> List.filter (fun line -> not @@ String.is_empty line)

let step p dir =
  match dir with
  | Dir.Up -> Xmas.Coordinate.add (0, -1) p
  | Dir.Down -> Xmas.Coordinate.add (0, 1) p
  | Dir.Left -> Xmas.Coordinate.add (-1, 0) p
  | Dir.Right -> Xmas.Coordinate.add (1, 0) p

let next map p dir =
  match CM.get p map with
  | None -> []
  | Some Component.Empty -> [ dir ]
  | Some Component.LeftLeaningMirror ->
      [
        (match dir with
        | Dir.Up -> Left
        | Dir.Down -> Right
        | Dir.Left -> Up
        | Dir.Right -> Down);
      ]
  | Some RightLeaningMirror ->
      [
        (match dir with
        | Dir.Up -> Right
        | Dir.Down -> Left
        | Dir.Left -> Down
        | Dir.Right -> Up);
      ]
  | Some HorizontalSplitter -> (
      match dir with
      | Dir.Left | Dir.Right -> [ dir ]
      | Dir.Up | Dir.Down -> [ Dir.Left; Dir.Right ])
  | Some VerticalSplitter -> (
      match dir with
      | Dir.Left | Dir.Right -> [ Dir.Up; Dir.Down ]
      | Dir.Up | Dir.Down -> [ dir ])

let map = Input.parse input
let mx, my = (List.length input - 1, List.length input - 1)

let walk map p dir =
  let seen = Hashtbl.create 1_000 in
  let rec aux beams =
    match beams with
    | [] -> seen
    | (((px, py) as p), dir) :: rest ->
        if px < 0 || py < 0 || px > mx || py > my then aux rest
        else if Hashtbl.mem seen (p, dir) then aux rest
        else
          let _ = Hashtbl.add seen (p, dir) true in
          let qs =
            next map p dir
            |> List.fold_left (fun acc dir -> (step p dir, dir) :: acc) rest
          in
          aux qs
  in
  let seen = aux [ (p, dir) ] in
  Hashtbl.fold (fun pt _ acc -> CS.add (fst pt) acc) seen CS.empty
  |> CS.cardinal

let part1 = walk map (0, 0) Right

let starts =
  let downs = Seq.init (mx + 1) (fun x -> ((x, 0), Dir.Down))
  and ups = Seq.init (mx + 1) (fun x -> ((x, my), Dir.Up))
  and lefts = Seq.init (my + 1) (fun y -> ((mx, y), Dir.Left))
  and rights = Seq.init (my + 1) (fun y -> ((0, y), Dir.Right)) in
  Seq.append downs ups |> Seq.append lefts |> Seq.append rights

let part2 =
  let walk = walk map in
  Seq.fold (fun acc (p, dir) -> max acc (walk p dir)) 0 starts

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
