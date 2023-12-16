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
  let downs = Seq.init (mx + 1) (fun x -> ((x, 0), Down))
  and ups = Seq.init (mx + 1) (fun x -> ((x, my), Up))
  and lefts = Seq.init (my + 1) (fun y -> ((mx, y), Left))
  and rights = Seq.init (my + 1) (fun y -> ((0, y), Right)) in
  Seq.append downs ups |> Seq.append lefts |> Seq.append rights

let part2 =
  let walk = walk map in
  Seq.fold (fun acc (p, dir) -> max acc (walk p dir)) 0 starts

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
