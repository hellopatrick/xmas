open Core

let input = In_channel.(input_lines stdin)

module Range = struct
  type t = {s: int; e: int}

  let includes t s = t.s <= s.s && t.e >= s.e

  let overlap t s = t.s <= s.e && t.e >= s.s
end

let parse line =
  Scanf.sscanf line "%d-%d,%d-%d" (fun s1 e1 s2 e2 ->
      (Range.{s= s1; e= e1}, Range.{s= s2; e= e2}) )

let part1 input =
  input |> List.map ~f:parse
  |> List.fold ~init:0 ~f:(fun acc (a, b) ->
         acc + Bool.to_int (Range.includes a b || Range.includes b a) )

let part2 input =
  input |> List.map ~f:parse
  |> List.fold ~init:0 ~f:(fun acc (a, b) ->
         acc + Bool.to_int (Range.overlap a b) )

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
