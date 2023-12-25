open Containers

type conn = { start : string; ends : string list }

module SM = Map.Make (String)
module SS = Set.Make (String)

module Input = struct
  let parse_line acc l =
    match String.split ~by:": " l with
    | [ key; vals ] -> SM.add key (String.split_on_char ' ' vals) acc
    | _ -> failwith "invalid line"

  let parse lines = List.fold_left parse_line SM.empty lines
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty
let conns = Input.parse input
let part1 = SM.fold
let part2 = 0
let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
