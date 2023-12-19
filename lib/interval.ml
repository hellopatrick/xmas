type t = int * int
(* Interval.t represent an interval [a,b) or all { x | a <= x && x < b} *)

let is_empty (a, b) = a >= b
let intersects (a, b) (c, d) = if a < c then b > c else a < d
let length (a, b) = if is_empty (a, b) then 0 else b - a
