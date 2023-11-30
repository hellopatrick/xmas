open Containers

let rec fold_while f acc res =
  match res () with
  | Seq.Nil -> acc
  | Seq.Cons (s, cont) -> (
      match f acc s with
      | acc, `Continue -> fold_while f acc cont
      | acc, `Stop -> acc)
