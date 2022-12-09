open Containers

let input = IO.read_lines_l stdin

let parse input =
  List.map
    (fun line ->
      Scanf.sscanf line "%c %d" (fun dir dist ->
          match dir with
          | 'U' ->
              List.init dist (fun _ -> (0, 1))
          | 'D' ->
              List.init dist (fun _ -> (0, -1))
          | 'L' ->
              List.init dist (fun _ -> (-1, 0))
          | 'R' ->
              List.init dist (fun _ -> (1, 0))
          | _ ->
              failwith "invalid" ) )
    input
  |> List.flatten

module C = struct
  type t = int * int

  let compare (t0, t1) (s0, s1) =
    match Int.compare t0 s0 with 0 -> Int.compare t1 s1 | v -> v

  let add (t0, t1) (s0, s1) = (t0 + s0, t1 + s1)

  let to_string (x, y) = Printf.sprintf "(%d, %d)" x y

  let touching (hx, hy) (tx, ty) =
    Int.abs (hx - tx) <= 1 && Int.abs (hy - ty) <= 1

  let approach (hx, hy) (tx, ty) =
    let dx = hx - tx in
    let dy = hy - ty in
    let dx_sign = Int.sign dx in
    let dy_sign = Int.sign dy in
    (tx + dx_sign, ty + dy_sign)

  let follow h t = if touching h t then t else approach h t
end

module V = Set.Make (C)

let solve input len =
  let steps = parse input in
  let rope = List.init (len - 1) (fun _ -> (0, 0)) in
  let _, _, v =
    List.fold_left
      (fun (h, r, v) mv ->
        let h' = C.add h mv in
        let t', r' =
          List.fold_left
            (fun (h, acc) t ->
              let t' = C.follow h t in
              (t', t' :: acc) )
            (h', []) r
        in
        (h', List.rev r', V.add t' v) )
      ((0, 0), rope, V.of_list [(0, 0)])
      steps
  in
  V.cardinal v

let _ = Printf.printf "part1=%d;part2=%d" (solve input 2) (solve input 10)
