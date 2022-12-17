open Containers

module V = struct
  type t = {flow: int; conns: int list}
end

module SM = Map.Make (String)
module IM = Map.Make (Int)

module P = struct
  open Angstrom
  open Xmas.Parsing

  let name = string "Valve " *> take 2

  let rate = string " has flow rate=" *> number

  let comma = string ", "

  let tunnel = string "; tunnel leads to valve "

  let tunnels = string "; tunnels lead to valves "

  let list_of_tunnels = (tunnel <|> tunnels) *> sep_by comma (take 2)

  let valve =
    lift3
      (fun name strength tunnels -> (name, strength, tunnels))
      name rate list_of_tunnels

  let parse line =
    parse_string ~consume:All valve line |> Result.get_or_failwith
end

let input = IO.read_lines_l stdin

let parse input =
  let data =
    List.map P.parse input
    |> List.sort (fun (a, _, _) (b, _, _) -> String.compare a b)
  in
  let idxs =
    List.foldi (fun acc i (name, _, _) -> SM.add name i acc) SM.empty data
  in
  List.foldi
    (fun acc i (_, flow, conns) ->
      let open V in
      let conns = List.map (fun conn -> SM.find conn idxs) conns in
      let v = {flow; conns} in
      IM.add i v acc )
    IM.empty data

let floyd_warshall nodes =
  let n = IM.cardinal nodes in
  let arr = Array.make_matrix n n n in
  IM.iter
    (fun x (v : V.t) -> v.conns |> List.iter (fun y -> arr.(y).(x) <- 1))
    nodes ;
  for z = 0 to n - 1 do
    for y = 0 to n - 1 do
      for x = 0 to n - 1 do
        arr.(y).(x) <- Int.min arr.(y).(x) (arr.(y).(z) + arr.(z).(x))
      done
    done
  done ;
  arr

let pp l =
  let res = l |> List.map Int.to_string |> String.concat ", " in
  Printf.sprintf "[%s]" res

let single l =
  Seq.init (List.length l) (fun i ->
      let first, second = List.take_drop i l in
      let chosen, tl = List.hd_tl second in
      (chosen, first @ tl) )

let cache = Hashtbl.create 10

let dfs nodes fw start rem t =
  let get k = Hashtbl.get cache k in
  let set k v = Hashtbl.add cache k v in
  let rec aux curr rem t =
    let k = (curr, pp rem, t) in
    match get k with
    | Some c ->
        c
    | _ ->
        let paths = single rem in
        let res =
          paths
          |> Seq.filter (fun (pt, _) -> fw.(curr).(pt) <= t)
          |> Seq.fold_left
               (fun acc (pt, rem') ->
                 let d = fw.(curr).(pt) in
                 let V.{flow; _} = IM.find pt nodes in
                 let t' = t - d - 1 in
                 let flow' = (flow * t') + aux pt rem' t' in
                 Int.max acc flow' )
               0
        in
        set k res ; res
  in
  aux start rem t

let part1 vm sp start =
  let nz =
    IM.fold (fun i (v : V.t) acc -> if v.flow > 0 then i :: acc else acc) vm []
  in
  dfs vm sp start nz 30

let cache' = Hashtbl.create 10

let dfs' nodes fw start nz t =
  let get k = Hashtbl.get cache' k in
  let set k v = Hashtbl.add cache' k v in
  let rec aux curr rem t =
    let k = (curr, rem, t) in
    match get k with
    | Some c ->
        c
    | _ ->
        let res =
          single rem
          |> Seq.filter (fun (pt, _) -> fw.(curr).(pt) <= t)
          |> Seq.fold_left
               (fun acc (pt, rem') ->
                 let d = fw.(curr).(pt) in
                 let V.{flow; _} = IM.find pt nodes in
                 let t' = t - d - 1 in
                 let flow' = (flow * t') + aux pt rem' t' in
                 Int.max acc flow' )
               (dfs nodes fw start rem 26)
        in
        set k res ; res
  in
  aux start nz t

let part2 vm sp start =
  let nz =
    IM.fold (fun i (v : V.t) acc -> if v.flow > 0 then i :: acc else acc) vm []
  in
  dfs' vm sp start nz 26

let _ =
  let vm = parse input in
  let sp = floyd_warshall vm in
  Printf.printf "part1=%d;part2=%d" (part1 vm sp 0) (part2 vm sp 0)
