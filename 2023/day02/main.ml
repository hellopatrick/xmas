open Containers

module Round = struct
  type t = { red : int; green : int; blue : int }

  let merge ?(op = Int.add) { red = r; green = g; blue = b }
      { red = r'; green = g'; blue = b' } =
    { red = op r r'; green = op g g'; blue = op b b' }

  let power { red; green; blue } = red * green * blue
end

module Game = struct
  type t = { id : int; rounds : Round.t list }

  let id { id; _ } = id
  let rounds { rounds; _ } = rounds
end

module P = struct
  open Angstrom
  open Xmas.Parsing

  let id = string "Game " *> number <* string ": "
  let comma = string ", "

  let red =
    number <* string " red" >>| fun i -> Round.{ red = i; green = 0; blue = 0 }

  let green =
    number <* string " green" >>| fun i ->
    Round.{ red = 0; green = i; blue = 0 }

  let blue =
    number <* string " blue" >>| fun i -> Round.{ red = 0; green = 0; blue = i }

  let rounds =
    sep_by comma (red <|> green <|> blue) >>| fun r ->
    List.fold_left Round.merge { red = 0; green = 0; blue = 0 } r

  let game =
    lift2
      (fun i r -> Game.{ id = i; rounds = r })
      id
      (sep_by (string "; ") rounds)

  let parse line = parse_string ~consume:All game line |> Result.get_or_failwith
end

let input = IO.read_lines_l stdin
let games = List.map P.parse input

let part1 =
  let possible Game.{ rounds; _ } =
    List.for_all
      (fun Round.{ red; green; blue } -> red <= 12 && green <= 13 && blue <= 14)
      rounds
  in
  List.filter possible games |> List.map Game.id |> Xmas.Enum.sum

let minimum_viable rs =
  List.fold_left (Round.merge ~op:max) { red = 0; green = 0; blue = 0 } rs

let part2 =
  games |> List.map Game.rounds |> List.map minimum_viable
  |> List.map Round.power |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
