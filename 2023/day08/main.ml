open Containers
module SM = Map.Make (String)

module Input = struct
  let parse lines =
    match lines with
    | directions :: _ :: routes ->
        let routes =
          List.map
            (fun route ->
              Scanf.sscanf route "%[A-Z] = (%[A-Z], %[A-Z])" (fun a b c ->
                  (a, (b, c))))
            routes
          |> SM.add_list SM.empty
        in
        ( String.to_seq directions
          |> Seq.map (function 'L' -> fst | _ -> snd)
          |> Seq.cycle,
          routes )
    | _ -> failwith "impossible"
end

let input = IO.read_lines_l stdin
let steps, routes = Input.parse input

let navigate cond start =
  let _, n =
    Xmas.Seq.fold_while
      (fun (at, n) f ->
        let next = f (SM.find at routes) in
        if cond next then ((next, n + 1), `Stop) else ((next, n + 1), `Continue))
      (start, 0) steps
  in
  n

let part1 = navigate (String.equal "ZZZ") "AAA"

let part2 =
  let starts =
    SM.keys routes |> List.of_iter |> List.filter (String.ends_with ~suffix:"A")
  in
  let navigate = navigate (String.ends_with ~suffix:"Z") in
  let trips = List.map navigate starts in
  List.reduce Xmas.Integer.lcm trips |> Option.get_exn_or "impossible"

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
