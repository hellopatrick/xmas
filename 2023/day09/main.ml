open Containers

module Input = struct
  let parse line = String.split_on_char ' ' line |> List.map int_of_string
end

let input = IO.read_lines_l stdin
let series = List.map Input.parse input

let running_diff series =
  let rec aux acc s =
    match s with
    | a :: b :: tl -> aux ((b - a) :: acc) (b :: tl)
    | _ -> List.rev acc
  in
  aux [] series

let derivatives series =
  let rec aux acc =
    match acc with
    | last :: _ ->
        if List.for_all (Int.equal 0) last then acc
        else
          let diffs = running_diff last in
          aux (diffs :: acc)
    | [] -> failwith "impossible"
  in
  aux [ series ]

let predict series =
  let ds = derivatives series in
  let ds = List.map List.rev ds in
  List.fold_left
    (fun acc d ->
      match d with last :: _ -> last + acc | _ -> failwith "impossible")
    0 ds

let backtrack series =
  let ds = derivatives series in
  List.fold_left
    (fun acc d ->
      match d with last :: _ -> last - acc | _ -> failwith "impossible")
    0 ds

let part1 = List.map predict series |> List.fold_left ( + ) 0
let part2 = List.map backtrack series |> List.fold_left ( + ) 0
let _ = Printf.printf "part1=%d;part2=%d" part1 part2
