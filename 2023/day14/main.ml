open Containers
module CM = Xmas.Coordinate.Map

type t = Round | Square

module Input = struct
  let parse lines =
    List.foldi
      (fun acc y row ->
        String.foldi
          (fun acc x c ->
            match c with
            | 'O' -> CM.add (x, y) Round acc
            | '#' -> CM.add (x, y) Square acc
            | _ -> acc)
          acc row)
      CM.empty lines
end

let input =
  IO.read_lines_l stdin |> List.filter (fun line -> not @@ String.is_empty line)

type dir = North | South | West | East

let bounds grid =
  CM.bindings grid |> List.map fst
  |> List.fold_left (fun (mx, my) (x, y) -> (max mx x, max my y)) (0, 0)

let tilt dir grid =
  let mx, my = bounds grid in

  let order =
    match dir with
    | North ->
        Seq.range 0 my
        |> Seq.flat_map (fun y -> Seq.range 0 mx |> Seq.map (fun x -> (x, y)))
    | South ->
        Seq.range my 0
        |> Seq.flat_map (fun y -> Seq.range 0 mx |> Seq.map (fun x -> (x, y)))
    | West ->
        Seq.range 0 mx
        |> Seq.flat_map (fun x -> Seq.range 0 my |> Seq.map (fun y -> (x, y)))
    | East ->
        Seq.range mx 0
        |> Seq.flat_map (fun x -> Seq.range 0 my |> Seq.map (fun y -> (x, y)))
  in

  let rocks =
    CM.filter (fun _ c -> match c with Square -> true | _ -> false) grid
  in

  Seq.fold
    (fun acc (x, y) ->
      match CM.get (x, y) grid with
      | Some Square | None -> acc
      | Some Round ->
          let x', y' =
            match dir with
            | North -> (
                let ys = Seq.range (y - 1) 0 in
                match Seq.find (fun y -> CM.mem (x, y) acc) ys with
                | Some y' -> (x, y' + 1)
                | None -> (x, 0))
            | South -> (
                let ys = Seq.range (y + 1) my in
                match Seq.find (fun y -> CM.mem (x, y) acc) ys with
                | Some y' -> (x, y' - 1)
                | None -> (x, my))
            | East -> (
                let xs = Seq.range (x + 1) mx in
                match Seq.find (fun x -> CM.mem (x, y) acc) xs with
                | Some x' -> (x' - 1, y)
                | None -> (mx, y))
            | West -> (
                let xs = Seq.range (x - 1) 0 in
                match Seq.find (fun x -> CM.mem (x, y) acc) xs with
                | Some x' -> (x' + 1, y)
                | None -> (0, y))
          in
          CM.add (x', y') Round acc)
    rocks order

let cycle grid = grid |> tilt North |> tilt West |> tilt South |> tilt East

let debug _ =
  let n = List.length input in
  let grid = Input.parse input in
  let grid' = cycle grid in
  let print g n =
    Seq.range 0 (n - 1)
    |> Seq.iter (fun y ->
           Seq.range 0 (n - 1)
           |> Seq.iter (fun x ->
                  match CM.get (x, y) g with
                  | Some Round -> print_char 'O'
                  | Some Square -> print_char '#'
                  | None -> print_char '.');
           print_newline ())
  in
  prerr_endline "---";
  print grid n;
  print_endline "---";
  print grid' n

let start = Input.parse input

let load grid =
  let _, my = bounds grid in
  CM.fold
    (fun (x, y) c acc -> match c with Round -> acc + (my + 1 - y) | _ -> acc)
    grid 0

let part1 = start |> tilt North |> load

let part2 =
  let grids = Hashtbl.create 10000 in
  let loads = Hashtbl.create 10000 in
  let rec aux grid i =
    match Hashtbl.get grids grid with
    | Some j -> (j, i)
    | None ->
        Hashtbl.add grids grid i;
        Hashtbl.add loads i (load grid);
        aux (cycle grid) (i + 1)
  in
  let first, next = aux start 0 in
  let n = (1_000_000_000 - first) mod (next - first) in
  let j = first + n in
  Hashtbl.get loads j |> Option.get_exn_or "load should exist."

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
