open Containers

let input = IO.read_lines_l stdin

type t = Num of int | Lst of t list

let rec comp t s =
  match (t, s) with
  | Num a, Num b ->
      Int.compare a b
  | Lst a, Lst b ->
      comp_lst a b
  | Num _, Lst _ ->
      comp (Lst [t]) s
  | Lst _, Num _ ->
      comp t (Lst [s])

and comp_lst a b =
  match (a, b) with
  | [], [] ->
      0
  | [], _ ->
      -1
  | _, [] ->
      1
  | hda :: tla, hdb :: tlb ->
      let res = comp hda hdb in
      if 0 = res then comp_lst tla tlb else res

let rec pp t =
  match t with
  | Num i ->
      Printf.sprintf "%d" i
  | Lst t ->
      Printf.sprintf "[%s]" (List.map pp t |> String.concat ",")

module P = struct
  open Angstrom
  open Xmas.Parsing

  let o = string "["

  let comma = string ","

  let c = string "]"

  let list = o *> sep_by comma number <* c

  let wrapped p = string "[" *> p <* string "]"

  let f =
    fix (fun packet ->
        let pkt =
          char '[' *> sep_by comma packet <* char ']' >>| fun l -> Lst l
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
        aux tl ((P.parse a, P.parse b) :: out)
    | hd :: _ ->
        failwith hd
  in
  aux (List.filter (fun a -> String.length a > 0) ls) []

let part1 input =
  let pairs = parse input in
  List.mapi (fun i (a, b) -> if -1 = comp a b then i + 1 else 0) pairs
  |> Xmas.Enum.sum

let part2 input =
  let d2 = P.parse "[[2]]" in
  let d6 = P.parse "[[6]]" in
  let packets = parse input |> List.flat_map (fun (a, b) -> [a; b]) in
  let packets = d2 :: d6 :: packets in
  let packets = List.sort comp packets in
  let i2 =
    List.find_idx (fun a -> 0 = comp a d2) packets
    |> Option.get_exn_or "impossible"
    |> fst
  in
  let i6 =
    List.find_idx (fun a -> 0 = comp a d6) packets
    |> Option.get_exn_or "impossible"
    |> fst
  in
  (i2 + 1) * (i6 + 1)

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
