open Containers
module IS = Set.Make (Int)

let input = IO.read_lines_l stdin |> List.map Int.of_string_exn
let part1 = List.fold_left ( + ) 0 input

let part2 =
  let track (seen, freq) s =
    let next_freq = freq + s in
    if IS.mem next_freq seen then ((seen, next_freq), `Stop)
    else ((IS.add next_freq seen, next_freq), `Continue)
  in
  let input = Seq.cycle (Seq.of_list input) in
  let _, freq = Xmas.Seq.fold_while track (IS.empty, 0) input in
  freq

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
