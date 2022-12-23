open Containers

module Step = struct
  type t = Forward of int | Left | Right

  let pp = function
    | Left ->
        "Left"
    | Right ->
        "Right"
    | Forward i ->
        Printf.sprintf "Go %d" i
end

module Facing = struct
  type t = Up | Down | Left | Right

  let score = function Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3

  let pp = function
    | Up ->
        "up"
    | Down ->
        "down"
    | Left ->
        "left"
    | Right ->
        "right"

  let rotate t by =
    match by with
    | Step.Left -> (
      match t with Up -> Left | Left -> Down | Down -> Right | Right -> Up )
    | Step.Right -> (
      match t with Up -> Right | Right -> Down | Down -> Left | Left -> Up )
    | Step.Forward _ ->
        t

  let dir = function
    | Up ->
        (0, -1)
    | Down ->
        (0, 1)
    | Left ->
        (-1, 0)
    | Right ->
        (1, 0)
end

module Parser = struct
  open Angstrom
  open Xmas.Parsing

  let forward = number >>| fun i -> Step.Forward i

  let left = char 'L' >>| fun _ -> Step.Left

  let right = char 'R' >>| fun _ -> Step.Right

  let step = forward <|> left <|> right

  let seq = many1 step <* end_of_line

  let parse str = parse_string ~consume:All seq str |> Result.get_or_failwith
end

module C = Xmas.Coordinate

module Side = struct
  type side = A | B | C | D | E | F

  let pp = function
    | A ->
        "a"
    | B ->
        "b"
    | C ->
        "c"
    | D ->
        "d"
    | E ->
        "e"
    | F ->
        "f"
end

module M = struct
  include Map.Make (C)

  let get_side (x, y) =
    if x < 100 && y < 50 then Side.A
    else if y < 50 then B
    else if y < 100 then C
    else if x < 50 && y < 150 then D
    else if y < 150 then E
    else F

  let origin = function
    | Side.A ->
        ((50, 0), (99, 49))
    | B ->
        ((100, 0), (149, 49))
    | C ->
        ((50, 50), (99, 99))
    | D ->
        ((0, 100), (49, 149))
    | E ->
        ((50, 100), (99, 149))
    | F ->
        ((0, 150), (49, 199))

  let bounds t =
    keys t |> List.of_iter
    |> List.fold_left
         (fun (mx, my) (x, y) -> (Int.max x mx, Int.max y my))
         (0, 0)

  let clamp (x, y) facing t =
    match get (x, y) t with
    | Some _ ->
        (x, y)
    | None ->
        let mx, my = bounds t in
        let dir = Facing.dir facing in
        let test =
          match facing with
          | Up ->
              (x, my)
          | Down ->
              (x, 0)
          | Left ->
              (mx, y)
          | Right ->
              (0, y)
        in
        let rec aux pos =
          match get pos t with Some _ -> pos | None -> aux (C.add pos dir)
        in
        aux test

  let cube_clamp (x, y) side facing t =
    match get (x, y) t with
    | Some _ ->
        ((x, y), facing)
    | None ->
        let (x0, y0), _ = origin side in
        let dx, dy = (x - x0, y - y0) in
        let pos', facing' =
          match (side, facing) with
          | A, Facing.Up ->
              let (x1, y1), (_, _) = origin F in
              ((x1, y1 + dx), Facing.Right)
          | A, Left ->
              let (x1, _), (_, y2) = origin D in
              ((x1, y2 - dy), Right)
          | B, Up ->
              let (x1, _), (_, y2) = origin F in
              ((x1 + dx, y2), Up)
          | B, Down ->
              let (_, y1), (x2, _) = origin C in
              ((x2, y1 + dx), Left)
          | B, Right ->
              let (_, _), (x2, y2) = origin E in
              ((x2, y2 - dy), Left)
          | C, Left ->
              let (x1, y1), (_, _) = origin D in
              ((x1 + dy, y1), Down)
          | C, Right ->
              let (x1, _), (_, y2) = origin B in
              ((x1 + dy, y2), Up)
          | D, Up ->
              let (x1, y1), (_, _) = origin C in
              ((x1, y1 + dx), Right)
          | D, Left ->
              let (x1, _), (_, y2) = origin A in
              ((x1, y2 - dy), Right)
          | E, Down ->
              let (_, y1), (x2, _) = origin F in
              ((x2, y1 + dx), Left)
          | E, Right ->
              let (_, _), (x2, y2) = origin B in
              ((x2, y2 - dy), Left)
          | F, Down ->
              let (x1, y1), (_, _) = origin B in
              ((x1 + dx, y1), Down)
          | F, Left ->
              let (x1, y1), (_, _) = origin A in
              ((x1 + dy, y1), Down)
          | F, Right ->
              let (x1, _), (_, y2) = origin E in
              ((x1 + dy, y2), Up)
          | _, _ ->
              failwith "impossible."
        in
        (pos', facing')

  let start t =
    let mx, my = bounds t in
    let rec aux x y =
      if x > mx then aux 0 (y + 1)
      else if y > my then failwith "impossible"
      else match get (x, y) t with Some _ -> (x, y) | None -> aux (x + 1) y
    in
    aux 0 0
end

module Space = struct
  type t = Open | Wall
end

let parse_map lines =
  List.foldi
    (fun acc y l ->
      String.foldi
        (fun acc x c ->
          match c with
          | '#' ->
              M.add (x, y) Space.Wall acc
          | '.' ->
              M.add (x, y) Space.Open acc
          | _ ->
              acc )
        acc l )
    M.empty lines

let parse input =
  match String.split ~by:"\n\n" input with
  | [map; steps] ->
      let map = String.split_on_char '\n' map |> parse_map in
      let steps = Parser.parse steps in
      (map, steps)
  | _ ->
      failwith "invalid input"

let walk ?(cube = false) m steps start facing =
  let rec move pos i facing =
    if i = 0 then (pos, facing)
    else
      let dir = Facing.dir facing in
      let pos' = C.add pos dir in
      let pos', facing' =
        if cube then
          let side = M.get_side pos in
          M.cube_clamp pos' side facing m
        else (M.clamp pos' facing m, facing)
      in
      match M.get pos' m with
      | Some Space.Open ->
          move pos' (i - 1) facing'
      | Some Space.Wall ->
          (pos, facing)
      | None ->
          failwith "impossible"
  in
  let act step pos facing =
    match step with
    | Step.Forward i ->
        move pos i facing
    | turn ->
        (pos, Facing.rotate facing turn)
  in
  let rec aux steps pos facing =
    match steps with
    | [] ->
        (pos, facing)
    | step :: tl ->
        let pos', facing' = act step pos facing in
        aux tl pos' facing'
  in
  aux steps start facing

let part1 input =
  let map, steps = parse input in
  let start = M.start map in
  let (x, y), face = walk map steps start Facing.Right in
  (1000 * (y + 1)) + (4 * (x + 1)) + Facing.score face

let part2 input =
  let map, steps = parse input in
  let start = M.start map in
  let (x, y), face = walk ~cube:true map steps start Facing.Right in
  (1000 * (y + 1)) + (4 * (x + 1)) + Facing.score face

let input = IO.read_all stdin

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
