open Containers

let input = IO.(read_lines_l stdin)

module Move = struct
  type t = { src : int; dst : int; amt : int }

  let parse s =
    Scanf.sscanf s "move %d from %d to %d" (fun amt src dst ->
        { src = src - 1; dst = dst - 1; amt })
end

let parse lines =
  let stacks, moves =
    List.take_drop_while (fun s -> not @@ String.is_empty s) lines
  in
  let stacks = List.map String.to_list stacks in
  let moves = List.tl moves |> List.map Move.parse in
  (Array.of_list stacks, moves)

let solve_inplace f stack moves =
  let rec aux moves =
    match moves with
    | [] -> ()
    | mv :: tl ->
        let Move.{ src; dst; amt } = mv in
        let s = stack.(src) in
        let d = stack.(dst) in
        let top, bot = List.take_drop amt s in
        stack.(src) <- bot;
        stack.(dst) <- List.concat [ f top; d ];
        aux tl
  in
  let _ = aux moves in
  stack |> Array.map List.hd |> Array.to_list |> String.of_list

let part1 input =
  let stack, moves = parse input in
  solve_inplace List.rev stack moves

let part2 input =
  let stack, moves = parse input in
  solve_inplace Fun.id stack moves

let _ = Printf.printf "part1=%s;part2=%s" (part1 input) (part2 input)
