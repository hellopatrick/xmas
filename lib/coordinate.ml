module C = struct
  type t = int * int

  let zero _ = (0, 0)
  let equal (t0, t1) (s0, s1) = t0 = s0 && t1 = s1

  let compare (t0, t1) (s0, s1) =
    match Int.compare t0 s0 with 0 -> Int.compare t1 s1 | v -> v

  let add (t0, t1) (s0, s1) = (t0 + s0, t1 + s1)
  let pp (x, y) = Printf.sprintf "(%d, %d)" x y

  let manhattan_distance (t0, t1) (s0, s1) =
    Int.abs (t0 - s0) + Int.abs (t1 - s1)
end

include C
module Set = CCSet.Make (C)
module Map = CCMap.Make (C)
