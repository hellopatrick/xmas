open Containers

let timelimit = 30

module V = struct
  type t = {name: string; strength: int; tunnels: string list}

  let compare t s = String.compare t.name s.name
end

module VM = struct
  include Map.Make (String)
end

module IM = struct
  include Map.Make (Int)
end

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
      (fun name strength tunnels -> V.{name; strength; tunnels})
      name rate list_of_tunnels

  let parse line =
    parse_string ~consume:All valve line |> Result.get_or_failwith
end

let input = IO.read_lines_l stdin

let parse input =
  List.map P.parse input
  |> List.fold_left
       (fun acc v ->
         let open V in
         VM.add v.name v acc )
       VM.empty

let part1 input =
  let valves = List.map P.parse input |> List.sort V.compare in
  let keys =
    List.foldi
      (fun acc i (v : V.t) ->
        let key = 1 lsl i in
        VM.add v.name key acc )
      VM.empty valves
  in
  let _adj =
    valves |> List.fold_left (fun acc (v : V.t) -> VM.add v.name v acc) VM.empty
  in
  let adj' =
    List.fold_left
      (fun acc (v : V.t) ->
        let key = VM.find v.name keys in
        let valves = List.map (fun n -> VM.find n keys) v.tunnels in
        IM.add key (v.strength, valves) acc )
      IM.empty valves
  in
  let best = Hashtbl.create (List.length valves) in
  let rec aux t states =
    if t >= timelimit then states
    else
      (* let _ = Printf.printf "@%d : %d\n" t (List.length states) in *)
      let _ = flush_all () in
      let states' =
        List.fold_left
          (fun acc (loc, is_open, pres) ->
            let k = (loc, is_open) in
            let best_pres = Hashtbl.get_or best k ~default:(-1) in
            if pres <= best_pres then acc
            else
              let strength, nearby = IM.find loc adj' in
              Hashtbl.add best k pres ;
              let acc =
                if loc land is_open = 0 && strength > 0 then
                  (loc, is_open lor loc, pres + (strength * (timelimit - t)))
                  :: acc
                else acc
              in
              let acc =
                List.fold_left
                  (fun acc loc' ->
                    let k = (loc', is_open) in
                    let pres' = Hashtbl.get_or best k ~default:(-1) in
                    if pres <= pres' then acc else (loc', is_open, pres) :: acc
                    )
                  acc nearby
              in
              acc )
          [] states
      in
      aux (t + 1) states'
  in
  aux 1 [(VM.find "AA" keys, 0, 0)]
  |> List.fold_left
       (fun acc (_, _, pressure) -> if pressure > acc then pressure else acc)
       0

let _ =
  let _ = part1 input in
  Printf.printf "part1=%d;part2=" (part1 input)
