open Containers

let input = IO.read_lines_l stdin

module C = Xmas.Coordinate
module M = Map.Make (C)
module S = Set.Make (C)

let parse i =
  List.foldi
    (fun acc y line -> String.foldi (fun acc x c -> M.add (x, y) c acc) acc line)
    M.empty i

let starts c m =
  m |> M.filter (fun _ h -> Char.equal h c) |> M.keys |> List.of_iter

let goal m = m |> M.filter (fun _ h -> Char.equal h 'E') |> M.choose |> fst

let neighbors (x, y) = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

let height = function
  | 'S' ->
      Char.code 'a'
  | 'E' ->
      Char.code 'z'
  | c ->
      Char.code c

let get a = Option.get_exn_or "not found" a

let solve m s e =
  let q = Queue.create () in
  let rec aux ds =
    if Queue.is_empty q then Int.max_int
    else
      let loc = Queue.pop q in
      let h = M.get loc m |> get in
      let dist = M.get loc ds |> get in
      if C.equal loc e then dist
      else
        let n =
          List.filter
            (fun l ->
              match (M.get l m, M.get l ds) with
              | None, _ | _, Some _ ->
                  false
              | Some h', _ ->
                  let dh = height h' - height h in
                  dh <= 1 )
            (neighbors loc)
        in
        Queue.add_seq q (List.to_seq n) ;
        let ds' =
          List.fold_left
            (fun acc c -> M.update c (Fun.const (Some (dist + 1))) acc)
            ds n
        in
        aux ds'
  in
  let _ = Queue.add s q in
  aux (M.of_list [(s, 0)])

let part1 m =
  let start = starts 'S' m |> List.hd in
  let goal = goal m in
  solve m start goal

let part2 m =
  let ss = starts 'a' m in
  let goal = goal m in
  List.fold_left
    (fun acc start ->
      let steps = solve m start goal in
      Int.min acc steps )
    Int.max_int ss

let _ =
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
