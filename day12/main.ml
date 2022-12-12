open Containers

let input = IO.read_lines_l stdin

module M = Map.Make (Xmas.Coordinate)
module S = Set.Make (Xmas.Coordinate)

let parse i =
  let m =
    List.foldi
      (fun acc y line ->
        line |> String.to_list
        |> List.foldi
             (fun acc x c ->
               let height =
                 if Char.equal c 'S' then -1
                 else if Char.equal c 'E' then 26
                 else Char.code c - Char.code 'a'
               in
               M.add (x, y) height acc )
             acc )
      M.empty i
  in
  let s =
    M.bindings m
    |> List.find_map (fun (c, v) -> if v = -1 then Some c else None)
    |> Option.get_exn_or "invalid"
  in
  let e =
    M.bindings m
    |> List.find_map (fun (c, v) -> if v = 26 then Some c else None)
    |> Option.get_exn_or "invalid"
  in
  (m, s, e)

let neighbors (x, y) = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)]

let part1 i =
  let m, s, e = parse i in
  let q = Queue.create () in
  let rec aux d =
    let loc = Queue.pop q in
    let v = M.get loc m |> Option.get_exn_or "unreach." in
    let dist = M.get_or loc d ~default:0 in
    if Xmas.Coordinate.equal loc e then dist
    else
      let n = neighbors loc in
      let n =
        List.filter
          (fun l ->
            match M.get l m with
            | None ->
                false
            | Some c ->
                let dh = c - v in
                if dh > 1 then false else M.get l d |> Option.is_none )
          n
      in
      Queue.add_seq q (List.to_seq n) ;
      let d =
        List.fold_left
          (fun acc c -> M.update c (fun _ -> Some (dist + 1)) acc)
          d n
      in
      aux d
  in
  let _ = Queue.add s q in
  aux (M.of_list [(s, 0)])

let _ = Printf.printf "part1=%d;part2=" (part1 input)
