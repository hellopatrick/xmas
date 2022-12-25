open Containers

module O = struct
  type t = Add of int | Mult of int | Square

  let handle i = function Add j -> i + j | Mult j -> i * j | Square -> i * i
end

type monkey = {
  name : int;
  items : int list;
  op : O.t;
  test : int;
  if_true : int;
  if_false : int;
  seen : int;
}

module P = struct
  open Angstrom
  open Xmas.Parsing

  let name = whitespace *> string "Monkey " *> number <* chomp_eol
  let comma = string ", "

  let items =
    whitespace *> string "Starting items: " *> sep_by comma number <* chomp_eol

  let add =
    whitespace *> string "Operation: new = old + " *> number
    >>| (fun l -> O.Add l)
    <* chomp_eol

  let mult =
    whitespace *> string "Operation: new = old * " *> number
    >>| (fun l -> O.Mult l)
    <* chomp_eol

  let square =
    whitespace *> string "Operation: new = old * old"
    >>| Fun.const O.Square <* chomp_eol

  let op = add <|> square <|> mult
  let test = whitespace *> string "Test: divisible by " *> number <* chomp_eol

  let if_true =
    whitespace *> string "If true: throw to monkey " *> number <* chomp_eol

  let if_false =
    whitespace *> string "If false: throw to monkey " *> number <* chomp_eol

  let monkey =
    (fun name items op test if_true if_false ->
      { name; items; op; test; if_true; if_false; seen = 0 })
    <$> name <*> items <*> op <*> test <*> if_true <*> if_false

  let parse input =
    parse_string ~consume:All (sep_by chomp_eol monkey) input
    |> Result.get_or_failwith |> Array.of_list
end

let solve monkeys rounds wd =
  let run s m =
    let len =
      List.fold_left
        (fun acc i ->
          let i' = wd @@ O.handle i m.op in
          let dest = if i' mod m.test = 0 then m.if_true else m.if_false in
          let d = s.(dest) in
          s.(dest) <- { d with items = i' :: d.items };
          acc + 1)
        0 m.items
    in
    s.(m.name) <- { m with seen = m.seen + len; items = [] }
  in
  let rec aux i state =
    if i > rounds then state
    else
      let _ = Array.iter (run state) state in
      aux (i + 1) state
  in
  aux 1 monkeys
  |> Array.fold
       (fun (m1, m2) n ->
         if n.seen > m1 then (n.seen, m1)
         else if n.seen > m2 then (m1, n.seen)
         else (m1, m2))
       (0, 0)
  |> fun (a, b) -> a * b

let part1 input =
  let monkeys = P.parse input in
  solve monkeys 20 (fun wl -> wl / 3)

let part2 input =
  let monkeys = P.parse input in
  let wd = Array.fold (fun a m -> a * m.test) 1 monkeys in
  solve monkeys 10000 (fun wl -> wl mod wd)

let _ =
  let input = IO.read_all stdin in
  Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
