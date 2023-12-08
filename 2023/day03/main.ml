open Containers

let input = IO.read_lines_l stdin

type t = Symbol of string | Number of string

let t_of_string s =
  match Int.of_string s with Some _ -> Number s | _ -> Symbol s

module CM = Xmas.Coordinate.Map

let re = Re.compile @@ Re.Pcre.re "([0-9]+|[^0-9^.])"

let parse acc y l =
  let groups = Re.all re l in
  let offs = List.map (fun g -> Re.Group.offset g 0) groups |> List.map fst in
  let matches = List.map (fun g -> Re.Group.get g 0) groups in
  let combo = List.combine offs matches in
  List.fold_left (fun acc (x, s) -> CM.add (x, y) (t_of_string s) acc) acc combo

let adj (x, y) id map =
  let width = String.length id in
  let neighbors =
    let xs = List.range (x - 1) (x + width) in
    List.flat_map (fun x -> [ (x, y - 1); (x, y); (x, y + 1) ]) xs
  in
  List.exists
    (fun (x, y) ->
      match CM.get (x, y) map with Some (Symbol _) -> true | _ -> false)
    neighbors

let map = List.foldi parse CM.empty input

let part1 =
  CM.fold
    (fun coord sym acc ->
      match sym with
      | Symbol _ -> acc
      | Number id ->
          if adj coord id map then acc + Int.of_string_exn id else acc)
    map 0

let gear_score map (x, y) =
  let neighbors =
    CM.fold
      (fun (x', y') sym acc ->
        match sym with
        | Symbol _ -> acc
        | Number n ->
            let w = String.length n in
            if y < y' - 1 || y > y' + 1 then acc
            else if x < x' - 1 || x > x' + w then acc
            else Int.of_string_exn n :: acc)
      map []
  in
  match neighbors with [ a; b ] -> a * b | _ -> 0

let part2 =
  CM.fold
    (fun coord sym acc ->
      match sym with Symbol "*" -> acc + gear_score map coord | _ -> acc)
    map 0

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
