open Containers

let input = IO.read_lines_l stdin

module C = Xmas.Coordinate
module M = Map.Make (C)
module S = Set.Make (C)

let starts c m =
  m |> M.filter (fun _ (h, _) -> Char.equal h c) |> M.keys |> List.of_iter

let goal m = m |> M.filter (fun _ (h, _) -> Char.equal h 'E') |> M.choose |> fst
let neighbors (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]

let height = function
  | 'S' -> Char.code 'a'
  | 'E' -> Char.code 'z'
  | c -> Char.code c

let parse i =
  let me =
    List.foldi
      (fun acc y line ->
        String.foldi (fun acc x c -> M.add (x, y) c acc) acc line)
      M.empty i
  in
  M.mapi
    (fun c v ->
      let h = height v in
      ( v,
        List.filter
          (fun c' ->
            match M.get c' me with
            | None -> false
            | Some h' ->
                let dh = height h' - h in
                dh <= 1)
          (neighbors c) ))
    me

let get ?(def = "not-found") a = Option.get_exn_or def a

let solve m s e =
  let q = Queue.create () in
  let rec aux ds =
    if Queue.is_empty q then None
    else
      let loc = Queue.pop q in
      let dist = M.get loc ds in
      if C.equal loc e then dist
      else
        match dist with
        | Some d ->
            let adj =
              M.get loc m |> get ~def:"not-found-adj" |> snd
              |> List.filter (fun c -> M.find_opt c ds |> Option.is_none)
            in
            let ds' =
              List.fold_left (fun acc c -> M.add c (d + 1) acc) ds adj
            in
            let _ = Queue.add_seq q (List.to_seq adj) in
            aux ds'
        | None -> failwith "impossible"
  in
  let _ = Queue.add s q in
  aux (M.of_list [ (s, 0) ])

let part1 m =
  let start = starts 'S' m |> List.hd in
  let goal = goal m in
  solve m start goal |> get ~def:"part-1"

let part2 m =
  let ss = starts 'a' m in
  let goal = goal m in
  List.fold_left
    (fun acc start ->
      let steps = solve m start goal in
      match steps with None -> acc | Some s -> Int.min acc s)
    Int.max_int ss

let _ =
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
