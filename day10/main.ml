open Containers

let input = IO.read_lines_l stdin

module Cmd = struct
  type t = Noop | AddX of int
end

let parse input =
  let aux line =
    if String.equal line "noop" then Cmd.Noop
    else Scanf.sscanf line "addx %d" (fun x -> Cmd.AddX x)
  in
  List.map aux input

let history input =
  let cmds = parse input in
  let _, history =
    List.fold_left
      (fun (x, hist) cmd ->
        match cmd with
        | Cmd.Noop ->
            (x, x :: hist)
        | Cmd.AddX dx ->
            let x' = x + dx in
            (x', x' :: x :: hist) )
      (1, [1; 1])
      cmds
  in
  List.rev history

let h = history input

let part1 h =
  List.foldi (fun acc i s -> acc + if (i - 20) mod 40 = 0 then i * s else 0) 0 h

let part2 h =
  let _, buf =
    List.fold_left
      (fun (i, b) s ->
        let x = i mod 40 in
        if x = 0 then Buffer.add_char b '\n' ;
        let c = if Int.abs (x - s) <= 1 then '#' else ' ' in
        Buffer.add_char b c ;
        (i + 1, b) )
      (0, Buffer.create 240)
      (List.drop 1 h)
  in
  buf |> Buffer.to_seq |> String.of_seq

let _ = Printf.printf "part1=%dpart2=%s" (part1 h) (part2 h)
