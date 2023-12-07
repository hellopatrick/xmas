open Containers

module Input = struct
  open Angstrom
  open Xmas.Parsing

  let line_parser =
    take_while (fun c -> not @@ is_digit c) *> sep_by1 whitespace number

  let parse_line line =
    line |> parse_string ~consume:All line_parser |> Result.get_or_failwith

  let parse lines =
    match lines with
    | times :: distances :: _ ->
        List.combine (parse_line times) (parse_line distances)
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

let answer races =
  List.map (fun (t, d) -> winnable_strategies t d) races
  |> List.fold_left Int.mul 1

let part1 = input |> Input.parse |> answer

let part2 =
  input |> List.map (String.replace ~sub:" " ~by:"") |> Input.parse |> answer

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
