open Containers

let input = IO.read_lines_l stdin

let parse input = List.map int_of_string input

let mix nsi =
  let len = List.length nsi in
  let rec aux nsi n =
    if n >= len then nsi
    else
      match List.find_idx (fun (i, _) -> i = n) nsi with
      | Some (idx, (i, v)) ->
          let nsi' =
            nsi |> List.remove_at_idx idx
            |> List.insert_at_idx ((idx + v) mod (len - 1)) (i, v)
          in
          aux nsi' (n + 1)
      | None ->
          failwith "impossible."
  in
  aux nsi 0

let solve ?(key = 1) ?(mixes = 1) input =
  let ns = parse input in
  let nsi = List.mapi (fun i n -> (i, n * key)) ns in
  let len = List.length nsi in
  let rec aux nsi n = if n >= mixes then nsi else aux (mix nsi) (n + 1) in
  let nsi = aux nsi 0 in
  let i0, _ =
    List.find_idx (fun (_, v) -> v = 0) nsi |> Option.get_exn_or "must exist"
  in
  let _, a = List.nth nsi ((i0 + 1000) mod len) in
  let _, b = List.nth nsi ((i0 + 2000) mod len) in
  let _, c = List.nth nsi ((i0 + 3000) mod len) in
  a + b + c

let part1 input = solve input

let part2 input = solve ~key:811589153 ~mixes:10 input

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
