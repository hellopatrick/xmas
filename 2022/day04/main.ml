open Containers

let input = IO.(read_lines_l stdin)

module Range = struct
  type t = {first: int; last: int}

  let parse b = Scanf.bscanf b "%i-%i" (fun first last -> {first; last})

  let includes t s = t.first <= s.first && t.last >= s.last

  let overlap t s = t.first <= s.last && t.last >= s.first
end

let parse line =
  Scanf.sscanf line "%r,%r" Range.parse Range.parse (fun r1 r2 -> (r1, r2))

let part1 input =
  input |> List.map parse
  |> List.fold_left
       (fun acc (a, b) ->
         acc + Bool.to_int (Range.includes a b || Range.includes b a) )
       0

let part2 input =
  input |> List.map parse
  |> List.fold_left (fun acc (a, b) -> acc + Bool.to_int (Range.overlap a b)) 0

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
