open Containers

let input = IO.read_lines_l stdin

type round = { red : int; green : int; blue : int }
type t = { id : int; rounds : round list }

let merge { red = r; green = g; blue = b } { red = r'; green = g'; blue = b' } =
  { red = r + r'; green = g + g'; blue = b + b' }

module P = struct
  open Angstrom
  open Xmas.Parsing

  let id = string "Game " *> number <* string ": "
  let comma = string ", "

  let red =
    number <* string " red" >>| fun i -> { red = i; green = 0; blue = 0 }

  let green =
    number <* string " green" >>| fun i -> { red = 0; green = i; blue = 0 }

  let blue =
    number <* string " blue" >>| fun i -> { red = 0; green = 0; blue = i }

  let rounds =
    sep_by comma (red <|> green <|> blue) >>| fun r ->
    List.fold_left merge { red = 0; green = 0; blue = 0 } r

  let game =
    lift2 (fun id rounds -> { id; rounds }) id (sep_by (string "; ") rounds)

  let parse line = parse_string ~consume:All game line |> Result.get_or_failwith
end

let games = List.map P.parse input

let part1 =
  let r, g, b = (12, 13, 14) in
  let possible { rounds; _ } =
    List.for_all
      (fun { red; green; blue } -> red <= r && green <= g && blue <= b)
      rounds
  in
  List.filter possible games |> List.map (fun { id; _ } -> id) |> Xmas.Enum.sum

let minimum_viable rs =
  List.fold_left
    (fun { red = r; green = g; blue = b } { red = r'; green = g'; blue = b' } ->
      { red = max r r'; green = max g g'; blue = max b b' })
    { red = 0; green = 0; blue = 0 }
    rs

let part2 =
  List.map (fun { rounds; _ } -> minimum_viable rounds) games
  |> List.map (fun { red; green; blue } -> red * green * blue)
  |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
