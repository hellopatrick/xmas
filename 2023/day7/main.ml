open Containers

module Card = struct
  type t = int

  let of_char ?(joker = false) c =
    match c with
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> if joker then -1 else 11
    | 'T' -> 10
    | _ -> Char.to_int c - Char.to_int '0'
end

module Hand = struct
  let strength h =
    let jokers = List.count (fun c -> c = -1) h in
    let groups = List.group_by ~eq:Int.equal h in
    let groups = List.map List.length groups in
    let groups = List.sort Int.compare groups in
    match groups with
    | [ 5 ] -> 7
    | [ 1; 4 ] -> if jokers = 0 then 6 else 7
    | [ 2; 3 ] -> if jokers = 0 then 5 else 7
    | [ 1; 1; 3 ] -> if jokers = 0 then 4 else 6
    | [ 1; 2; 2 ] -> if jokers = 0 then 3 else if jokers = 1 then 5 else 6
    | [ 1; 1; 1; 2 ] -> if jokers = 0 then 2 else 4
    | [ 1; 1; 1; 1; 1 ] -> if jokers = 0 then 1 else 2
    | _ -> 0

  let compare h h' =
    let s = strength h in
    let s' = strength h' in
    if s > s' then 1
    else if s < s' then -1
    else
      let c = List.combine h h' in
      match List.find_opt (fun (a, b) -> not @@ Int.equal a b) c with
      | None -> 0
      | Some (a, b) -> if a > b then 1 else -1
end

module Input = struct
  let parse ?(joker = false) line =
    let hand, bid = Scanf.sscanf line "%s %d" (fun s c -> (s, c)) in
    let hand = String.to_list hand |> List.map (Card.of_char ~joker) in
    (hand, bid)
end

let input = IO.read_lines_l stdin

let answer hands =
  hands
  |> List.sort (fun (h, _) (h', _) -> Hand.compare h h')
  |> List.foldi (fun acc i (_, b) -> acc + ((i + 1) * b)) 0

let part1 = List.map Input.parse input |> answer
let part2 = List.map (Input.parse ~joker:true) input |> answer
let _ = Printf.printf "part1=%d;part2=%d" part1 part2
