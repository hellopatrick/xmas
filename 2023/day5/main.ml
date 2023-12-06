open Containers

let input = IO.read_all stdin

module Interval = struct
  (* Interval.t = [a, b) *)
  type t = int * int

  let is_empty (a, b) = a >= b
  let intersects (a, b) (c, d) = if a < c then b > c else a < d
end

module Input = struct
  type t = int * int

  open Angstrom
  open Xmas.Parsing

  let parse =
    let list_of_nums = sep_by1 whitespace number in
    let* seeds =
      string "seeds: " *> list_of_nums <* end_of_line <* end_of_line
    in
    let nums =
      sep_by1 end_of_line
        (list_of_nums >>| function
         | [ a; b; c ] -> ((b, b + c), (a, a + c))
         | _ -> failwith "impossible")
    in
    let* blocks = sep_by1 (many1 end_of_line) (chomp_eol *> nums) in
    return (seeds, blocks)
end

let seeds, maps =
  let open Angstrom in
  parse_string ~consume:Prefix Input.parse input |> Result.get_or_failwith

let walk maps seed =
  List.fold_left
    (fun i map ->
      List.fold_while
        (fun i ((src, src'), (dst, _)) ->
          if i >= src && i < src' then (dst + i - src, `Stop) else (i, `Continue))
        i map)
    seed maps

let part1 = List.map (walk maps) seeds |> List.sort Int.compare |> List.hd

let map_interval maps i =
  let maps = List.sort (fun ((a, _), _) ((b, _), _) -> Int.compare a b) maps in
  let rec aux (a, b) acc =
    if Interval.is_empty (a, b) then acc
    else
      match
        List.find_opt
          (fun ((a', b'), _) -> Interval.intersects (a, b) (a', b'))
          maps
      with
      | None -> (a, b) :: acc
      | Some ((ss, se), (ds, de)) ->
          if a < ss then aux (ss, b) ((a, ss) :: acc)
          else if b > se then
            let ds' = a - ss + ds in
            aux (se, b) ((ds', de) :: acc)
          else
            let ds' = a - ss + ds in
            let de' = ds' + (b - a) in
            (ds', de') :: acc
  in
  aux i []

let seed_ranges =
  let rec aux acc = function
    | [] -> acc
    | a :: b :: rest -> aux ((a, a + b) :: acc) rest
    | _ -> failwith "impossible"
  in
  aux [] seeds

let part2 =
  List.map
    (fun r ->
      List.fold_left
        (fun rs maps -> List.flat_map (map_interval maps) rs)
        [ r ] maps)
    seed_ranges
  |> List.flatten
  |> List.fold_left (fun acc (a, _) -> min acc a) Int.max_int

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
