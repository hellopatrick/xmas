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
      (1, [1; 1]) (* hack *)
      cmds
  in
  List.rev history |> Array.of_list

let part1 input =
  let h = history input in
  List.fold_left (fun acc i -> acc + (i * h.(i))) 0 [20; 60; 100; 140; 180; 220]

let part2 input =
  let h = history input in
  let crt = Array.init 6 (fun _ -> Array.make 40 '.') in
  for i = 1 to 240 do
    let y = (i - 1) / 40 in
    let x = (i - 1) mod 40 in
    let s = h.(i) in
    let c = if x >= s - 1 && x <= s + 1 then '#' else '.' in
    crt.(y).(x) <- c
  done ;
  Array.iter (fun l -> Array.iter print_char l ; print_newline ()) crt

let _ = Printf.printf "part1=%dpart2=\n" (part1 input)

let _ = part2 input
