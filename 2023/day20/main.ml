open Containers
module SM = Map.Make (String)

module Gate = struct
  type t =
    | FlipFlop of bool
    | Conjunction of (bool * int) SM.t
    | Broadcaster
    | Sink

  let of_string s =
    if String.equal s "broadcaster" then Broadcaster
    else if String.prefix ~pre:"%" s then FlipFlop false
    else Conjunction SM.empty

  let name s = if String.equal s "broadcaster" then s else String.drop 1 s
end

module State = struct
  type t = { gates : Gate.t SM.t; outputs : string list SM.t }

  let empty = { gates = SM.empty; outputs = SM.empty }

  let gate name t =
    match SM.get name t.gates with None -> Gate.Sink | Some m -> m

  let outputs name t =
    match SM.get name t.outputs with None -> [] | Some o -> o

  let patch t =
    SM.fold
      (fun name outs t ->
        List.fold_left
          (fun t o ->
            {
              t with
              gates =
                SM.update o
                  (function
                    | None -> None
                    | Some (Gate.Conjunction v) ->
                        Some (Gate.Conjunction (SM.add name (false, 0) v))
                    | Some m -> Some m)
                  t.gates;
            })
          t outs)
      t.outputs t

  let add name m outs t =
    let gates' = SM.add name m t.gates in
    let outputs' = SM.add name outs t.outputs in
    { gates = gates'; outputs = outputs' }

  let set name m t =
    let gates' = SM.add name m t.gates in
    { t with gates = gates' }

  let exec n (src, pulse, dest) t =
    let m = gate dest t in
    let outputs = outputs dest t in
    match m with
    | Gate.Sink -> ([], t)
    | Gate.Broadcaster -> (List.map (fun o -> (dest, pulse, o)) outputs, t)
    | Gate.FlipFlop v ->
        if pulse then ([], t)
        else
          let pulse' = not v in
          ( List.map (fun o -> (dest, pulse', o)) outputs,
            set dest (Gate.FlipFlop pulse') t )
    | Gate.Conjunction v ->
        let v' =
          SM.update src
            (function
              | None -> Some (pulse, if pulse then n else 0)
              | Some (prev, n') -> Some (pulse, if pulse then n else n'))
            v
        in
        let e = SM.exists (fun _ (h, _) -> not h) v' in
        ( List.map (fun o -> (dest, e, o)) outputs,
          set dest (Gate.Conjunction v') t )
end

module Input = struct
  let parse_line line =
    match String.split ~by:" -> " line with
    | [ input; outputs ] ->
        let outs = String.split ~by:", " outputs in
        (Gate.name input, Gate.of_string input, outs)
    | _ -> failwith "invalid line"

  let parse input =
    List.fold_left
      (fun acc line ->
        let n, m, o = parse_line line in
        State.add n m o acc)
      State.empty input
    |> State.patch
end

let input = IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty

let run n t =
  let rec aux pulses (highs, lows) t =
    match pulses with
    | [] -> ((highs, lows), t)
    | (src, p, dest) :: tl ->
        let generated, t' = State.exec n (src, p, dest) t in
        let dh, dl = if p then (1, 0) else (0, 1) in
        aux (List.append tl generated) (highs + dh, lows + dl) t'
  in
  aux [ ("button", false, "broadcaster") ] (0, 0) t

let start = Input.parse input

let part1 =
  let rec press n (high, low) t =
    if n = 0 then (high, low)
    else
      let (dh, dl), t' = run n t in
      press (n - 1) (high + dh, low + dl) t'
  in
  let h, l = press 1000 (0, 0) start in
  h * l

let part2 =
  let complete v = SM.for_all (fun k (_, n) -> n > 0) v in
  let rec press n t =
    let _, t' = run n t in
    match State.gate "kl" t' with
    | Gate.Conjunction v -> if complete v then v else press (n + 1) t'
    | _ -> failwith "impossible."
  in
  let s = press 1 start in

  SM.fold (fun k (_, a) acc -> Xmas.Integer.lcm acc a) s 1

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
