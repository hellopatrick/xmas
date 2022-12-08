open Containers

let input = IO.read_lines_l stdin

module C = struct
  type t = int * int

  let compare (t0, t1) (s0, s1) =
    match Int.compare t0 s0 with 0 -> Int.compare t1 s1 | v -> v

  let add (t0, t1) (s0, s1) = (t0 + s0, t1 + s1)

  let to_string (x, y) = Printf.sprintf "(%d, %d)" x y
end

module Map = struct
  include Map.Make (C)

  let is_visible coord m =
    let h = get_or coord m ~default:(-1) in
    let rec aux c d =
      let neighbor = C.add c d in
      match get neighbor m with
      | None ->
          true
      | Some h' ->
          if h > h' then aux neighbor d else false
    in
    aux coord (0, 1)
    || aux coord (0, -1)
    || aux coord (1, 0)
    || aux coord (-1, 0)

  let score coord m =
    let h = get_or coord m ~default:(-1) in
    let rec aux c d count =
      let neighbor = C.add c d in
      match get neighbor m with
      | None ->
          count
      | Some h' ->
          let count = count + 1 in
          if h > h' then aux neighbor d count else count
    in
    aux coord (0, 1) 0
    * aux coord (0, -1) 0
    * aux coord (1, 0) 0
    * aux coord (-1, 0) 0
end

let parse lines =
  List.foldi
    (fun acc y line ->
      let chars = String.to_list line in
      List.foldi
        (fun acc x c ->
          let h = Char.code c - Char.code '0' in
          Map.add (x, y) h acc )
        acc chars )
    Map.empty lines

let part1 m =
  Map.fold (fun c _ acc -> if Map.is_visible c m then acc + 1 else acc) m 0

let part2 m =
  Map.fold
    (fun c _ max ->
      let score = Map.score c m in
      if score > max then score else max )
    m 0

let map = parse input

let _ = Printf.printf "part1=%d;part2=%d" (part1 map) (part2 map)
