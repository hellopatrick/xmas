open Core
module CM = Map.Make (Char)
module CharPair = Tuple.Comparable (Char) (Char)
module CharPairMap = CharPair.Map

type prompt = {
  start : string;
  current : int CharPairMap.t;
  rules : char CharPairMap.t;
}

let parse =
  let parse_rewrite line =
    Scanf.sscanf line "%c%c -> %c" (fun il ir o -> ((il, ir), o))
  in
  function
  | start :: "" :: rules ->
      let rules = List.map rules ~f:parse_rewrite |> CharPairMap.of_alist_exn in
      let rec aux letters counts =
        match letters with
        | c0 :: c1 :: tl ->
            aux (c1 :: tl)
              (CharPairMap.update counts (c0, c1) ~f:(function
                | Some v -> v + 1
                | None -> 1))
        | _ -> counts
      in
      let current = aux (String.to_list start) CharPairMap.empty in
      { start; current; rules }
  | _ -> failwith "invalid input"

let input = In_channel.input_lines In_channel.stdin |> parse

let rewrite { current; rules; start } =
  let next =
    CharPairMap.fold current ~init:CharPairMap.empty
      ~f:(fun ~key:(c0, c1) ~data acc ->
        match CharPairMap.find rules (c0, c1) with
        | Some c ->
            let acc =
              CharPairMap.update acc (c0, c) ~f:(function
                | Some c -> c + data
                | None -> data)
            in
            let acc =
              CharPairMap.update acc (c, c1) ~f:(function
                | Some c -> c + data
                | None -> data)
            in
            acc
        | None -> acc)
  in
  { current = next; rules; start }

let part1 =
  let rec aux state i =
    if i <= 0 then state
    else
      let next = rewrite state in
      aux next (i - 1)
  in
  let res = aux input 10 in
  let counts =
    CharPairMap.fold res.current
      ~init:
        (CM.of_alist_exn
           [
             (String.get res.start 0, 1);
             (String.get res.start (String.length res.start - 1), 1);
           ])
      ~f:(fun ~key:(c0, c1) ~data acc ->
        let acc =
          CM.update acc c0 ~f:(function Some c -> c + data | None -> data)
        in
        let acc =
          CM.update acc c1 ~f:(function Some c -> c + data | None -> data)
        in
        acc)
  in
  let counts = CM.data counts in
  let max = List.max_elt counts ~compare:Int.compare |> Option.value_exn in
  let min = List.min_elt counts ~compare:Int.compare |> Option.value_exn in
  (max - min) / 2

let part2 =
  let rec aux state i =
    if i <= 0 then state
    else
      let next = rewrite state in
      aux next (i - 1)
  in
  let res = aux input 40 in
  let counts =
    CharPairMap.fold res.current
      ~init:
        (CM.of_alist_exn
           [
             (String.get res.start 0, 1);
             (String.get res.start (String.length res.start - 1), 1);
           ])
      ~f:(fun ~key:(c0, c1) ~data acc ->
        let acc =
          CM.update acc c0 ~f:(function Some c -> c + data | None -> data)
        in
        let acc =
          CM.update acc c1 ~f:(function Some c -> c + data | None -> data)
        in
        acc)
  in
  let counts = CM.data counts in
  let max = List.max_elt counts ~compare:Int.compare |> Option.value_exn in
  let min = List.min_elt counts ~compare:Int.compare |> Option.value_exn in
  (max - min) / 2

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
