open Containers

let input = IO.read_lines_l stdin

module M = Map.Make (Xmas.Coordinate)
module S = Set.Make (Xmas.Coordinate)

let parse i =
  List.foldi
    (fun acc y line ->
      line |> String.to_list
      |> List.foldi (fun acc x c -> M.add (x, y) c acc) acc )
    M.empty i

let starts m c =
  M.filter (fun _ h -> Char.equal h c) m |> M.keys |> List.of_iter

let goal m = m |> M.filter (fun _ h -> Char.equal h 'E') |> M.choose |> fst

let neighbors (x, y) = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

let height = function
  | 'S' ->
      Char.code 'a'
  | 'E' ->
      Char.code 'z'
  | c ->
      Char.code c

let solve m s e =
  let q = Queue.create () in
  let rec aux d =
    if Queue.is_empty q then Int.max_int
    else
      let loc = Queue.pop q in
      let h = M.get loc m |> Option.get_exn_or "unreach." in
      let dist = M.get loc d |> Option.get_exn_or "unreach." in
      if Xmas.Coordinate.equal loc e then dist
      else
        let n =
          List.filter
            (fun l ->
              match (M.get l m, M.get l d) with
              | None, _ | _, Some _ ->
                  false
              | Some h', _ ->
                  let dh = height h' - height h in
                  dh <= 1 )
            (neighbors loc)
        in
        Queue.add_seq q (List.to_seq n) ;
        let d =
          List.fold_left
            (fun acc c -> M.update c (Fun.const (Some (dist + 1))) acc)
            d n
        in
        aux d
  in
  let _ = Queue.add s q in
  aux (M.of_list [(s, 0)])

let part1 m =
  let start = starts m 'S' |> List.hd in
  let goal = goal m in
  solve m start goal

let part2 m =
  let ss = starts m 'a' in
  let goal = goal m in
  List.fold_left
    (fun acc start ->
      let steps = solve m start goal in
      Int.min acc steps )
    Int.max_int ss

let _ =
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
