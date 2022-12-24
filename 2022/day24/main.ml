open Containers
module C = Xmas.Coordinate

module Space = struct
  type t = Wall | North | South | East | West

  let dir = function
    | Wall ->
        (0, 0)
    | North ->
        (0, -1)
    | South ->
        (0, 1)
    | East ->
        (1, 0)
    | West ->
        (-1, 0)
end

module M = struct
  include Map.Make (C)

  let bounds t =
    fold
      (fun (cx, cy) _ ((x, y), (x', y')) ->
        ((min cx x, min cy y), (max cx x', max cy y')) )
      t
      ((max_int, max_int), (min_int, min_int))

  let start t =
    let (x, y), (x', _) = bounds t in
    Seq.range x x'
    |> Seq.filter_map (fun x -> if mem (x, y) t then None else Some (x, y))
    |> Seq.head_exn

  let goal t =
    let (x, _), (x', y) = bounds t in
    Seq.range x x'
    |> Seq.filter_map (fun x -> if mem (x, y) t then None else Some (x, y))
    |> Seq.head_exn

  let safe ((wx, wy), (wx', wy')) (sx, sy) (gx, gy) pt t =
    let dps = [(0, 0); (-1, 0); (1, 0); (0, 1); (0, -1)] in
    List.filter_map
      (fun dp ->
        let x, y = C.add pt dp in
        if x = gx && y = gy then Some (x, y)
        else if x = sx && y = sy then Some (x, y)
        else if x <= wx || x >= wx' || y <= wy || y >= wy' then None
        else if mem (x, y) t then None
        else Some (x, y) )
      dps
end

let input = IO.read_lines_l stdin

let parse input =
  List.foldi
    (fun acc y line ->
      String.foldi
        (fun acc x c ->
          match c with
          | '#' ->
              M.add (x, y) [Space.Wall] acc
          | '<' ->
              M.add (x, y) [Space.West] acc
          | '>' ->
              M.add (x, y) [Space.East] acc
          | '^' ->
              M.add (x, y) [Space.North] acc
          | 'v' ->
              M.add (x, y) [Space.South] acc
          | _ ->
              acc )
        acc line )
    M.empty input

let run ((wx, wy), (wx', wy')) m =
  M.fold
    (fun (x, y) pieces acc ->
      List.fold_left
        (fun acc p ->
          match p with
          | Space.Wall ->
              M.add (x, y) [Space.Wall] acc
          | p ->
              let dx, dy = Space.dir p in
              let x', y' = (x + dx, y + dy) in
              let x', y' =
                match M.get (x', y') m with
                | Some [Space.Wall] -> (
                  match p with
                  | Space.North ->
                      (x', wy' - 1)
                  | Space.South ->
                      (x', wy + 1)
                  | Space.East ->
                      (wx + 1, y')
                  | Space.West ->
                      (wx' - 1, y')
                  | _ ->
                      (x', y') )
                | _ ->
                    (x', y')
              in
              M.update (x', y')
                (function Some l -> Some (p :: l) | None -> Some [p])
                acc )
        acc pieces )
    m M.empty

let bfs m bounds start goal =
  let seen = Hashtbl.create 1_000 in
  let m_cache = Hashtbl.create 1_000 in
  let q = Queue.create () in
  let run = run bounds in
  let safe = M.safe bounds start goal in
  Queue.push (0, m, start) q ;
  let rec aux () =
    if Queue.is_empty q then failwith "never-reached-goal"
    else
      let t, m, pt = Queue.pop q in
      if C.equal pt goal then (t, m)
      else if Hashtbl.mem seen (t, pt) then aux ()
      else
        let m' = Hashtbl.get_or_add m_cache ~f:(fun _ -> run m) ~k:t in
        safe pt m' |> Seq.of_list
        |> Seq.map (fun pt -> (t + 1, m', pt))
        |> Queue.add_seq q ;
        Hashtbl.add seen (t, pt) true ;
        aux ()
  in
  aux ()

let part1 input =
  let m = parse input in
  let bounds = M.bounds m in
  let start = M.start m in
  let goal = M.goal m in
  let t, _ = bfs m bounds start goal in
  t

let part2 input =
  let m = parse input in
  let bounds = M.bounds m in
  let start = M.start m in
  let goal = M.goal m in
  let t0, m' = bfs m bounds start goal in
  let t1, m' = bfs m' bounds goal start in
  let t2, _ = bfs m' bounds start goal in
  t0 + t1 + t2

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
