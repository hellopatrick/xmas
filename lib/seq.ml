open Containers

let rec fold_until f acc res =
  match res () with
  | Seq.Nil -> acc
  | Seq.Cons (s, cont) -> (
      match f acc s with
      | `Continue acc -> fold_until f acc cont
      | `Stop acc -> acc)
