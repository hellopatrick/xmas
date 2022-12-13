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
    | hda :: tla, hdb :: tlb ->
        let res = compare hda hdb in
        if 0 = res then compare_lst tla tlb else res

  let rec pp t =
    match t with
    | Num i ->
        Printf.sprintf "%d" i
    | Lst t ->
        Printf.sprintf "[%s]" (List.map pp t |> String.concat ",")
end

let input = IO.read_lines_l stdin

module PP = struct
  open Angstrom
  open Xmas.Parsing
  open Packet

  let comma = char ','

  let f =
    fix (fun packet ->
        let pkt =
          advance 1 *> sep_by comma packet <* char ']' >>| fun l -> Lst l
        in
        let empty = string "[]" >>| fun _ -> Lst [] in
        let num = number >>| fun i -> Num i in
        peek_char_fail
        >>= function
        | '[' ->
            choice [empty; pkt]
        | '0' .. '9' ->
            num
        | c ->
            failwith @@ Printf.sprintf "unknown char: %c" c )

  let parse s = parse_string ~consume:All f s |> Result.get_or_failwith
end

let parse ls =
  let rec aux ls out =
    match ls with
    | [] ->
        List.rev out
    | a :: b :: tl ->
        aux tl ((PP.parse a, PP.parse b) :: out)
    | hd :: _ ->
        failwith hd
  in
  aux (List.filter (fun a -> String.length a > 0) ls) []

let part1 input =
  let pairs = parse input in
  List.foldi
    (fun acc i (a, b) -> if Packet.compare a b < 0 then acc + i + 1 else acc)
    0 pairs

let part2 input =
  let d2 = PP.parse "[[2]]" in
  let d6 = PP.parse "[[6]]" in
  let packets = parse input |> List.flat_map (fun (a, b) -> [a; b]) in
  let packets = d2 :: d6 :: packets in
  let packets = List.sort Packet.compare packets in
  let i2 =
    List.find_idx (fun a -> 0 = Packet.compare a d2) packets
    |> Option.get_exn_or "impossible"
    |> fst
  in
  let i6 =
    List.find_idx (fun a -> 0 = Packet.compare a d6) packets
    |> Option.get_exn_or "impossible"
    |> fst
  in
  (i2 + 1) * (i6 + 1)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
