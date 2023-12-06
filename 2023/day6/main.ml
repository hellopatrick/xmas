open Containers

module Input = struct
  let parse lines =
    let times, distances =
      match lines with
      | times :: distances :: _ -> (times, distances)
      | _ -> failwith "invalid line"
    in
    let parse' line =
      List.filter_map Int.of_string @@ String.split_on_char ' ' line
    in
    List.combine (parse' times) (parse' distances)

  let parse_kerning lines =
    let parse' line =
      match String.replace ~sub:" " ~by:"" line |> String.split_on_char ':' with
      | _ :: n :: _ -> Int.of_string_exn n
      | _ -> failwith "invalid line"
    in
    match lines with
    | time :: distance :: _ -> (parse' time, parse' distance)
    | _ -> failwith "invalid line"
end

let input = IO.read_lines_l stdin

let is_winnable time distance i =
  let speed = i in
  let time_left = time - i in
  let travel_distance = speed * time_left in
  travel_distance > distance

let winnable_strategies time distance =
  let is_winnable = is_winnable time distance in
  Seq.range 0 time
  |> Seq.fold (fun acc i -> if is_winnable i then acc + 1 else acc) 0

let part1 =
  let races = Input.parse input in
  List.map (fun (t, d) -> winnable_strategies t d) races
  |> List.fold_left Int.mul 1

let part2 =
  let t, d = Input.parse_kerning input in
  winnable_strategies t d

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
