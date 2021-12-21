open Core

module Die = struct
  type t = int Sequence.t

  let loaded =
    Sequence.unfold ~init:1 ~f:(fun acc ->
        let acc = if acc > 100 then 1 else acc in
        Some (acc, acc + 1) )

  let dirac =
    let possible_die =
      [ (1, 1, 1)
      ; (1, 1, 2)
      ; (1, 1, 3)
      ; (1, 2, 1)
      ; (1, 2, 2)
      ; (1, 2, 3)
      ; (1, 3, 1)
      ; (1, 3, 2)
      ; (1, 3, 3)
      ; (2, 1, 1)
      ; (2, 1, 2)
      ; (2, 1, 3)
      ; (2, 2, 1)
      ; (2, 2, 2)
      ; (2, 2, 3)
      ; (2, 3, 1)
      ; (2, 3, 2)
      ; (2, 3, 3)
      ; (3, 1, 1)
      ; (3, 1, 2)
      ; (3, 1, 3)
      ; (3, 2, 1)
      ; (3, 2, 2)
      ; (3, 2, 3)
      ; (3, 3, 1)
      ; (3, 3, 2)
      ; (3, 3, 3) ]
      |> List.map ~f:(fun (x, y, z) -> x + y + z)
    in
    Sequence.of_list possible_die
end

module Board = struct
  type t = {p1: int; p2: int; size: int} [@@deriving show, hash, sexp, eq, ord]

  let init p1 p2 size = {p1; p2; size}

  let curr t player =
    match player with 1 -> t.p1 | 2 -> t.p2 | _ -> failwith "invalid player"

  let move t player steps =
    let curr = curr t player in
    let next = 1 + ((curr - 1 + steps) % t.size) in
    match player with
    | 1 ->
        (next, {t with p1= next})
    | 2 ->
        (next, {t with p2= next})
    | _ ->
        failwith "invalid player"

  include Hashable
end

module Score = struct
  type t = {p1: int; p2: int} [@@deriving show, hash, sexp, eq, ord]

  let empty = {p1= 0; p2= 0}

  let has_winner {p1; p2} goal = p1 >= goal || p2 >= goal

  let inc t player amt =
    match player with
    | 1 ->
        {t with p1= t.p1 + amt}
    | 2 ->
        {t with p2= t.p2 + amt}
    | _ ->
        failwith "invalid player"

  let loser t = Int.min t.p1 t.p2
end

let parse lines =
  match
    List.map lines ~f:(fun line ->
        Scanf.sscanf line "Player %d starting position: %d" (fun _ pos -> pos) )
  with
  | [p1; p2] ->
      (p1, p2)
  | _ ->
      failwith "wrong player count"

let p1, p2 = In_channel.(input_lines stdin) |> parse

let run scr brd die goal =
  let rec aux scr brd die rnd =
    if Score.has_winner scr goal then (scr, rnd * 3)
    else
      let player = (rnd % 2) + 1 in
      let rolls, die = Sequence.split_n die 3 in
      let steps = Xmas.Enum.sum rolls in
      let n, brd = Board.move brd player steps in
      let scr = Score.inc scr player n in
      aux scr brd die (rnd + 1)
  in
  aux scr brd die 0

let part1 =
  let scr = Score.empty in
  let brd = Board.init p1 p2 10 in
  let die = Die.loaded in
  let res, rolls = run scr brd die 1000 in
  rolls * Score.loser res

module State = struct
  type t = Score.t * Board.t * bool [@@deriving hash, sexp, ord, eq]
end

module StateHash = Hashtbl.Make (State)

let simulate scr brd die goal =
  let memo = StateHash.create () in
  let rec aux scr brd is_p1 =
    StateHash.find_or_add memo (scr, brd, is_p1) ~default:(fun () ->
        if scr.p1 >= goal then (1, 0)
        else if scr.p2 >= goal then (0, 1)
        else
          let player = if is_p1 then 1 else 2 in
          Sequence.fold die ~init:(0, 0) ~f:(fun (p1, p2) steps ->
              let n, brd = Board.move brd player steps in
              let scr = Score.inc scr player n in
              let w1, w2 = aux scr brd (not is_p1) in
              (w1 + p1, w2 + p2) ) )
  in
  aux scr brd true

let part2 =
  let scr = Score.empty in
  let brd = Board.init p1 p2 10 in
  let die = Die.dirac in
  let p1, p2 = simulate scr brd die 21 in
  Int.max p1 p2

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
