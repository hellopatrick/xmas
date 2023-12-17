open Containers

type 'a t = 'a array array

let get t (x, y) =
  Array.get_safe t y |> Option.flat_map (fun row -> Array.get_safe row x)

let mem t (x, y) = get t (x, y) |> Option.is_some
