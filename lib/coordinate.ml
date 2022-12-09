type t = int * int

let zero _ = (0, 0)

let compare (t0, t1) (s0, s1) =
  match Int.compare t0 s0 with 0 -> Int.compare t1 s1 | v -> v

let add (t0, t1) (s0, s1) = (t0 + s0, t1 + s1)

let to_string (x, y) = Printf.sprintf "(%d, %d)" x y
