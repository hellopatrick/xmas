open Containers

module Card = struct
  type t =
    | Joker [@value 1]
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
  [@@deriving show, enum, ord, eq]

  let is_joker = equal Joker

  let of_char ?(joker = false) c =
    match c with
    | 'A' -> Ace
    | 'K' -> King
    | 'Q' -> Queen
    | 'J' -> if joker then Joker else Jack
    | 'T' -> Ten
    | _ ->
        of_enum (Char.to_int c - Char.to_int '0') |> Option.get_exn_or "invalid"
end

module Hand = struct
  type t =
    | HighCard of Card.t list
    | Pair of Card.t list
    | TwoPairs of Card.t list
    | ThreeOfAKind of Card.t list
    | FullHouse of Card.t list
    | FourOfAKind of Card.t list
    | FiveOfAKind of Card.t list
  [@@deriving show, eq, ord]

  let of_list h =
    let jokers = List.count Card.is_joker h in
    let groups =
      List.group_by ~eq:Card.equal h
      |> List.map List.length |> List.sort Int.compare
    in
    match groups with
    | [ 5 ] -> FiveOfAKind h
    | [ 1; 4 ] -> if jokers = 0 then FourOfAKind h else FiveOfAKind h
    | [ 2; 3 ] -> if jokers = 0 then FullHouse h else FiveOfAKind h
    | [ 1; 1; 3 ] -> if jokers = 0 then ThreeOfAKind h else FourOfAKind h
    | [ 1; 2; 2 ] ->
        if jokers = 0 then TwoPairs h
        else if jokers = 1 then FullHouse h
        else FourOfAKind h
    | [ 1; 1; 1; 2 ] -> if jokers = 0 then Pair h else ThreeOfAKind h
    | _ -> if jokers = 0 then HighCard h else Pair h
end

module Input = struct
  let parse ?(joker = false) line =
    let hand, bid = Scanf.sscanf line "%s %d" (fun s c -> (s, c)) in
    let hand =
      String.to_list hand |> List.map (Card.of_char ~joker) |> Hand.of_list
    in
    (hand, bid)
end

let input = IO.read_lines_l stdin

let answer hands =
  hands
  |> List.sort (fun (h, _) (h', _) -> Hand.compare h h')
  |> List.foldi (fun acc i (_, b) -> acc + ((i + 1) * b)) 0

let part1 = input |> List.map Input.parse |> answer
let part2 = input |> List.map (Input.parse ~joker:true) |> answer
let _ = Printf.printf "part1=%d;part2=%d" part1 part2
