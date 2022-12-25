open Containers

module Packet = struct
  type t = Num of int | Lst of t list

  let rec compare t s =
    match (t, s) with
    | Num a, Num b -> Int.compare a b
    | Lst a, Lst b -> compare_lst a b
    | Num _, Lst _ -> compare (Lst [ t ]) s
    | Lst _, Num _ -> compare t (Lst [ s ])

  and compare_lst a b =
    match (a, b) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | hda :: tla, hdb :: tlb -> (
        match compare hda hdb with 0 -> compare_lst tla tlb | res -> res)

  let equals t s = match compare t s with 0 -> true | _ -> false

  let rec pp t =
    match t with
    | Num i -> Printf.sprintf "%d" i
    | Lst t -> Printf.sprintf "[%s]" (List.map pp t |> String.concat ",")
end

module PP = struct
  open Angstrom
  open Xmas.Parsing
  open Packet

  let pkt =
    let comma = char ',' in
    let lsb = char '[' in
    let rsb = char ']' in
    let num = number >>| fun n -> Num n in
    let to_lst l = Lst l in
    fix (fun packet ->
        let lst = packet >>| to_lst in
        lsb *> sep_by comma (num <|> lst) <* rsb)
    >>| to_lst

  let parse s = s |> parse_string ~consume:All pkt |> Result.get_or_failwith
end

let part1 packets =
  List.chunks 2 packets
  |> List.map (function [ a; b ] -> (a, b) | _ -> failwith "only pairs.")
  |> List.foldi
       (fun acc i (a, b) -> if Packet.compare a b < 0 then acc + i + 1 else acc)
       0

let part2 packets =
  let d2, d6 = (PP.parse "[[2]]", PP.parse "[[6]]") in
  let packets = d2 :: d6 :: packets |> List.sort Packet.compare in
  let i2, _ =
    List.find_idx (Packet.equals d2) packets
    |> Option.get_exn_or "[[2]] must exist"
  in
  let i6, _ =
    List.find_idx (Packet.equals d6) packets
    |> Option.get_exn_or "[[6]] must exist"
  in
  (i2 + 1) * (i6 + 1)

let _ =
  let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty in
  let packets = List.map PP.parse input in
  Printf.printf "part1=%d;part2=%d" (part1 packets) (part2 packets)
