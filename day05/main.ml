open Core

let input = In_channel.(input_lines stdin)

module Move = struct
  type t = {src: int; dst: int; amt: int}

  let parse s =
    Scanf.sscanf s "move %d from %d to %d" (fun amt src dst ->
        {src= src - 1; dst= dst - 1; amt} )
end

let parse lines =
  let stacks, moves =
    List.split_while lines ~f:(fun s -> not (String.equal s "--"))
  in
  let stacks = List.map stacks ~f:String.to_list in
  let moves = List.tl_exn moves in
  let moves = List.map moves ~f:Move.parse in
  (Array.of_list stacks, moves)

let solve_inplace f stack moves =
  let rec aux moves =
    match moves with
    | [] ->
        ()
    | mv :: tl ->
        let Move.{src; dst; amt} = mv in
        let s = stack.(src) in
        let d = stack.(dst) in
        let top, bot = List.split_n s amt in
        Array.set stack src bot ;
        Array.set stack dst (List.concat [f top; d]) ;
        aux tl
  in
  let _ = aux moves in
  Array.map stack ~f:List.hd_exn |> Array.to_list |> String.of_char_list

let part1 input =
  let stack, moves = parse input in
  solve_inplace List.rev stack moves

let part2 input =
  let stack, moves = parse input in
  solve_inplace Fn.id stack moves

let _ = Printf.printf "part1=%s;part2=%s" (part1 input) (part2 input)
