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

let part1 h =
  List.foldi (fun acc i s -> acc + if (i - 20) mod 40 = 0 then i * s else 0) 0 h

let part2 h =
  let draw i x = if Int.abs (i - x) <= 1 then '#' else ' ' in
  let draw_line = Fun.compose (List.mapi draw) String.of_list in
  h |> List.drop 1 |> List.chunks 40 |> List.map draw_line |> String.concat "\n"

let _ =
  let h = history input in
  Printf.printf "part1=%dpart2=\n%s" (part1 h) (part2 h)
