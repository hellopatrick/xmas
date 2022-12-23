open Containers
module C = Xmas.Coordinate
module M = Map.Make (C)
module S = Set.Make (C)

let parse input =
  List.foldi
    (fun acc y line ->
      String.foldi
        (fun acc x c -> if Char.equal c '#' then M.add (x, y) true acc else acc)
        acc line )
    M.empty input

let input = IO.read_lines_l stdin

let north x y m =
  let cs = [(x - 1, y - 1); (x, y - 1); (x + 1, y - 1)] in
  if List.count (fun c -> M.mem c m) cs = 0 then Some (x, y - 1) else None

let south x y m =
  let cs = [(x - 1, y + 1); (x, y + 1); (x + 1, y + 1)] in
  if List.count (fun c -> M.mem c m) cs = 0 then Some (x, y + 1) else None

let west x y m =
  let cs = [(x - 1, y - 1); (x - 1, y); (x - 1, y + 1)] in
  if List.count (fun c -> M.mem c m) cs = 0 then Some (x - 1, y) else None

let east x y m =
  let cs = [(x + 1, y - 1); (x + 1, y); (x + 1, y + 1)] in
  if List.count (fun c -> M.mem c m) cs = 0 then Some (x + 1, y) else None

let choices n x y =
  match n mod 4 with
  | 0 ->
      [north x y; south x y; west x y; east x y]
  | 1 ->
      [south x y; west x y; east x y; north x y]
  | 2 ->
      [west x y; east x y; north x y; south x y]
  | _ ->
      [east x y; north x y; south x y; west x y]

let propose m (x, y) i =
  let any =
    [ (x - 1, y - 1)
    ; (x, y - 1)
    ; (x + 1, y - 1)
    ; (x - 1, y)
    ; (x + 1, y)
    ; (x - 1, y + 1)
    ; (x, y + 1)
    ; (x + 1, y + 1) ]
  in
  if List.count (fun c -> M.mem c m) any = 0 then None
  else
    let cs = choices i x y in
    List.fold_left
      (fun acc f -> match acc with None -> f m | Some c -> Some c)
      None cs

let bounds m =
  M.fold
    (fun (cx, cy) _ ((x, y), (x', y')) ->
      ((min cx x, min cy y), (max cx x', max cy y')) )
    m
    ((max_int, max_int), (min_int, min_int))

let pp m =
  let (x0, y0), (x1, y1) = bounds m in
  for y = y0 to y1 do
    for x = x0 to x1 do
      if M.mem (x, y) m then print_char '#' else print_char '.'
    done ;
    print_newline ()
  done ;
  print_endline (String.init (x1 - x0 + 2) (fun _ -> '-'))

let step m i =
  let counts, proposed =
    M.fold
      (fun c _ (counts, proposed) ->
        match propose m c i with
        | Some c' ->
            ( M.update c'
                (function Some v -> Some (v + 1) | None -> Some 1)
                counts
            , M.add c c' proposed )
        | None ->
            (counts, proposed) )
      m (M.empty, M.empty)
  in
  let m' =
    M.fold
      (fun c _ acc ->
        match M.get c proposed with
        | Some c' ->
            if M.find c' counts = 1 then acc |> M.remove c |> M.add c' true
            else acc
        | None ->
            acc )
      m m
  in
  (m', counts)

let run m n =
  let rec aux m i =
    if i = n then m
    else
      let m', _ = step m i in
      aux m' (i + 1)
  in
  aux m 0

let part1 m =
  let m' = run m 10 in
  let (x, y), (x', y') = bounds m' in
  ((x' - x + 1) * (y' - y + 1)) - M.cardinal m'

let part2 m =
  let rec aux m i =
    let m', proposed = step m i in
    if M.exists (fun _ i -> i = 1) proposed then aux m' (i + 1) else i
  in
  1 + aux m 0

let _ =
  let m = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 m) (part2 m)
