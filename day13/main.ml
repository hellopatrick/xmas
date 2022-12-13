open Containers

module Packet = struct
  type t = Num of int | Lst of t list

  let rec compare t s =
    match (t, s) with
    | Num a, Num b ->
        Int.compare a b
    | Lst a, Lst b ->
        compare_lst a b
    | Num _, Lst _ ->
        compare (Lst [t]) s
    | Lst _, Num _ ->
        compare t (Lst [s])

  and compare_lst a b =
    match (a, b) with
    | [], [] ->
        0
    | [], _ ->
        -1
    | _, [] ->
        1
    | hda :: tla, hdb :: tlb -> (
      match compare hda hdb with 0 -> compare_lst tla tlb | res -> res )

  let equals t s = match compare t s with 0 -> true | _ -> false

  let rec pp t =
    match t with
    | Num i ->
        Printf.sprintf "%d" i
    | Lst t ->
        Printf.sprintf "[%s]" (List.map pp t |> String.concat ",")
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
        lsb *> sep_by comma (num <|> lst) <* rsb )
    >>| to_lst

  let parse s = parse_string ~consume:All pkt s |> Result.get_or_failwith
end

let parse ls =
  let rec aux ls out =
    match ls with
    | [] ->
        List.rev out
    | "" :: tl ->
        aux tl out
    | a :: b :: tl ->
        aux tl ((PP.parse a, PP.parse b) :: out)
    | _ ->
        failwith "bad input."
  in
  aux ls []

let part1 pairs =
  List.foldi
    (fun acc i (a, b) -> if Packet.compare a b < 0 then acc + i + 1 else acc)
    0 pairs

let part2 pairs =
  let d2 = PP.parse "[[2]]" in
  let d6 = PP.parse "[[6]]" in
  let packets = List.flat_map (fun (a, b) -> [a; b]) pairs in
  let packets = d2 :: d6 :: packets |> List.sort Packet.compare in
  let i2 =
    ( List.find_idx (Packet.equals d2) packets
    |> Option.get_exn_or "[[2]] must exist"
    |> fst )
    + 1
  in
  let i6 =
    ( List.find_idx (Packet.equals d6) packets
    |> Option.get_exn_or "[[6]] must exist"
    |> fst )
    + 1
  in
  i2 * i6

let _ =
  let input = IO.read_lines_l stdin in
  let pairs = parse input in
  Printf.printf "part1=%d;part2=%d" (part1 pairs) (part2 pairs)
