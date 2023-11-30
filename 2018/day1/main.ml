open Containers

let input = IO.read_lines_l stdin |> List.map Int.of_string_exn
let part1 = List.fold_left ( + ) 0 input

module IS = Set.Make (Int)

let part2 =
  let track (seen, freq) s =
    let next_freq = freq + s in
    if IS.mem next_freq seen then `Stop (seen, next_freq)
    else `Continue (IS.add next_freq seen, next_freq)
  in
  let input = Seq.cycle (Seq.of_list input) in
  let _, freq = Xmas.Seq.fold_until track (IS.empty, 0) input in
  freq

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
