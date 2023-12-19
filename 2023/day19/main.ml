open Containers
module SM = Map.Make (String)

module Part = struct
  type t = { x : int; m : int; a : int; s : int }

  let sum t = t.x + t.m + t.a + t.s

  let get field t =
    match field with
    | "x" -> t.x
    | "m" -> t.m
    | "a" -> t.a
    | "s" -> t.s
    | _ -> failwith "not a field"
end

module PartRange = struct
  type t = {
    x : Xmas.Interval.t;
    m : Xmas.Interval.t;
    a : Xmas.Interval.t;
    s : Xmas.Interval.t;
  }

  let size t =
    let open Xmas.Interval in
    length t.x * length t.m * length t.a * length t.s

  let start = { x = (1, 4001); m = (1, 4001); a = (1, 4001); s = (1, 4001) }
  let empty = { x = (0, 0); m = (0, 0); a = (0, 0); s = (0, 0) }

  let is_empty t =
    let open Xmas.Interval in
    is_empty t.x || is_empty t.m || is_empty t.a || is_empty t.s

  let split field v t =
    match field with
    | "x" ->
        let lb, ub = t.x in
        ({ t with x = (lb, v) }, { t with x = (v, ub) })
    | "m" ->
        let lb, ub = t.m in
        ({ t with m = (lb, v) }, { t with m = (v, ub) })
    | "a" ->
        let lb, ub = t.a in
        ({ t with a = (lb, v) }, { t with a = (v, ub) })
    | "s" ->
        let lb, ub = t.s in
        ({ t with s = (lb, v) }, { t with s = (v, ub) })
    | _ -> failwith "not a field"
end

module Outcome = struct
  type t = Accept | Reject | Transfer of string

  let of_string s =
    match s with "A" -> Accept | "R" -> Reject | next -> Transfer next
end

module Rule = struct
  type op = LessThan of int | GreaterThan of int | All
  type t = { field : string; op : op; outcome : Outcome.t }

  let build_conditional field op v out =
    let outcome = Outcome.of_string out in
    match op with
    | '<' -> { field; op = LessThan v; outcome }
    | '>' -> { field; op = GreaterThan v; outcome }
    | _ -> failwith "invalid op"

  let build_catch_all outcome =
    { field = ""; op = All; outcome = Outcome.of_string outcome }

  let run part t =
    match t.op with
    | All -> Some t.outcome
    | LessThan v -> if Part.get t.field part < v then Some t.outcome else None
    | GreaterThan v ->
        if Part.get t.field part > v then Some t.outcome else None

  let run_range range t =
    match t.op with
    | All -> (t.outcome, range, PartRange.empty)
    | LessThan v ->
        let lower, upper = PartRange.split t.field v range in
        (t.outcome, lower, upper)
    | GreaterThan v ->
        let lower, upper = PartRange.split t.field (v + 1) range in
        (t.outcome, upper, lower)
end

module Input = struct
  let parse_workflow line =
    let open Angstrom in
    let open Xmas.Parsing in
    let id = identifier <* char '{' in

    let pred =
      lift4 Rule.build_conditional identifier any_char
        (number <* char ':')
        identifier
    in

    let catch_all = identifier >>| Rule.build_catch_all in

    let rules = sep_by1 (char ',') (pred <|> catch_all) <* char '}' in

    let all = lift2 (fun id rules -> (id, rules)) id rules in

    parse_string ~consume:All all line |> Result.get_exn

  let parse_workflows p =
    String.split_on_char '\n' p |> List.map parse_workflow |> SM.of_list

  let parse_parts p =
    let lines = String.split_on_char '\n' p in
    List.map
      (fun l ->
        Scanf.sscanf l "{x=%d,m=%d,a=%d,s=%d}" (fun x m a s ->
            Part.{ x; m; a; s }))
      lines

  let parse p =
    match String.split ~by:"\n\n" p with
    | [ workflows; parts ] -> (parse_workflows workflows, parse_parts parts)
    | _ -> failwith "invalid input"
end

let input = IO.read_all stdin |> String.trim
let workflows, parts = Input.parse input

let part1 =
  let rec run workflows wn part =
    match SM.get wn workflows with
    | None -> failwith "missing workflow name!"
    | Some wf -> (
        match List.find_map (Rule.run part) wf with
        | Some (Outcome.Transfer wn) -> run workflows wn part
        | Some Outcome.Accept -> (part, Outcome.Accept)
        | Some Outcome.Reject -> (part, Outcome.Reject)
        | _ -> failwith "no catch-all rule!")
  in
  List.map (run workflows "in") parts
  |> List.filter_map (function
       | part, Outcome.Accept -> Some part
       | _, _ -> None)
  |> List.map Part.sum |> List.fold_left ( + ) 0

let part2 =
  let rec run workflows wn pr =
    if PartRange.is_empty pr then 0
    else
      match SM.get wn workflows with
      | None -> failwith "invalid workflow name"
      | Some wfs ->
          List.fold_left
            (fun (acc, pr) wf ->
              if PartRange.is_empty pr then (acc, pr)
              else
                let out, spr, pr' = Rule.run_range pr wf in
                match out with
                | Outcome.Accept -> (acc + PartRange.size spr, pr')
                | Outcome.Reject -> (acc, pr')
                | Outcome.Transfer wn -> (acc + run workflows wn spr, pr'))
            (0, pr) wfs
          |> fst
  in
  run workflows "in" PartRange.start

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
