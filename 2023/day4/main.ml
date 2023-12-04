open Containers

let input = IO.read_lines_l stdin

module IS = Set.Make (Int)

module Input = struct
  type t = { winning : IS.t; given : IS.t }

  let matches t = IS.cardinal @@ IS.inter t.winning t.given

  let score t =
    let m = matches t in
    if m > 0 then Int.pow 2 (m - 1) else 0

  let parse =
    let open Angstrom in
    let open Xmas.Parsing in
    let* _ =
      string "Card" *> whitespace *> number *> whitespace *> string ":"
      *> whitespace
    in
    let* winning = sep_by1 whitespace number in
    let* _ = whitespace *> string "|" *> whitespace in
    let* given = sep_by1 whitespace number in
    return { winning = IS.of_list winning; given = IS.of_list given }

  let parse_all =
    List.map (fun l ->
        l |> Angstrom.parse_string ~consume:All parse |> Result.get_exn)
end

let input = Input.parse_all input
let part1 = input |> List.map Input.score |> List.fold_left ( + ) 0

let part2 =
  let cards = Array.make (List.length input) 1 in
  List.foldi
    (fun acc i next ->
      let matches = Input.matches next in
      if matches > 0 then
        Seq.range (i + 1) (i + matches)
        |> Seq.iter (fun j -> cards.(j) <- cards.(j) + cards.(i));
      acc + cards.(i))
    0 input

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
