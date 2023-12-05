open Containers

let input = IO.read_all stdin

module Input = struct
  type t = int

  open Angstrom
  open Xmas.Parsing

  let parse =
    let list_of_nums = sep_by1 whitespace number in
    let* prefix =
      string "seeds: " *> list_of_nums <* end_of_line <* end_of_line
    in
    let header = take_till is_whitespace <* chomp_eol in
    let nums =
      sep_by1 end_of_line
        (list_of_nums >>| function
         | [ a; b; c ] -> (a, b, c)
         | _ -> failwith "impossible")
    in
    let block = lift2 (fun s nums -> (s, nums)) header nums in
    let* blocks = sep_by1 (many1 end_of_line) block in
    return (prefix, blocks)
end

let seeds, maps =
  let open Angstrom in
  parse_string ~consume:Prefix Input.parse input |> Result.get_or_failwith

let walk maps seed =
  List.fold_left
    (fun i (_, map) ->
      List.fold_while
        (fun i (dst, src, range) ->
          if i >= src && i < src + range then
            let dt = i - src in
            (dst + dt, `Stop)
          else (i, `Continue))
        i map)
    seed maps

let part1 = List.map (walk maps) seeds |> List.sort Int.compare |> List.hd

let part2 =
  let chunks = List.chunks 2 seeds in
  let ranges =
    List.map
      (function [ a; b ] -> Seq.range a (a + b) | _ -> failwith "impossible")
      chunks
  in
  let seeds = Seq.concat (Seq.of_list ranges) in
  Seq.map (walk maps) seeds |> Seq.fold min Int.max_int

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
