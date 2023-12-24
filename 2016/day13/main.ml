open Containers

let count_bits n =
  let rec aux n count =
    if n = 0 then count else aux (n land (n - 1)) (count + 1)
  in
  aux n 0

module I = struct
  let parse input =
    match input with
    | [ fave; goal ] ->
        (Int.of_string_exn fave, Scanf.sscanf goal "%d,%d" (fun x y -> (x, y)))
    | _ -> failwith "invalid"
end

module G = struct
  type t = Wall | Open

  let is_open fave (x, y) =
    if x < 0 || y < 0 then false
    else
      let v = fave + ((x * x) + (3 * x) + (2 * x * y) + y + (y * y)) in
      let bits = count_bits v in
      bits mod 2 = 0

  let succ fave (x, y) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
    |> List.filter (is_open fave)
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let fave, goal = I.parse input

let graph =
  CCGraph.make (fun (x, y) ->
      G.succ fave (x, y) |> List.map (fun (x, y) -> (1, (x, y))) |> List.to_iter)

let walk graph (x, y) =
  let tbl = CCGraph.mk_table ~eq:Xmas.Coordinate.equal (x * y) in
  let dist = Fun.id in
  let start = CCGraph.Iter.return (1, 1) in
  let goal = Xmas.Coordinate.equal (x, y) in
  let iter = CCGraph.Traverse.dijkstra ~tbl ~dist ~graph start in
  let _, dist, _ = Iter.find_pred_exn (fun (c, _, _) -> goal c) iter in
  dist

let part1 = walk graph goal

let part2 =
  let tbl = CCGraph.mk_table ~eq:Xmas.Coordinate.equal (fst goal * snd goal) in
  let dist = Fun.id in
  let start = CCGraph.Iter.return (1, 1) in
  let iter = CCGraph.Traverse.dijkstra ~tbl ~dist ~graph start in
  Iter.filter_count (fun (_, d, _) -> d <= 50) iter

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
