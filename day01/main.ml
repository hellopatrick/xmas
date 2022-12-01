open Core

let input = In_channel.(input_lines stdin)

let part1 lines =
  let lines =
    List.map lines ~f:(fun l ->
        if String.is_empty l then 0 else Int.of_string l )
  in
  let elves = List.group lines ~break:(fun _ b -> b = 0) in
  let elves = List.filter elves ~f:(fun l -> not (List.is_empty l)) in
  let sums = List.map elves ~f:(fun l -> List.fold l ~init:0 ~f:Int.( + )) in
  List.max_elt sums ~compare:Int.compare |> Option.value ~default:0

let part2 lines =
  let lines =
    List.map lines ~f:(fun l ->
        if String.is_empty l then 0 else Int.of_string l )
  in
  let elves = List.group lines ~break:(fun _ b -> b = 0) in
  let elves = List.filter elves ~f:(fun l -> not (List.is_empty l)) in
  let sums = List.map elves ~f:(fun l -> List.fold l ~init:0 ~f:Int.( + )) in
  let sums = List.sort sums ~compare:Int.compare in
  let top3 = List.drop sums (List.length sums - 3) in
  List.fold top3 ~init:0 ~f:Int.( + )

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
