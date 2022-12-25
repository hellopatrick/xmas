open Containers

module SNAFU = struct
  let to_rep num =
    let rec aux num digits =
      if num = 0 then digits
      else
        let num, rem = (num / 5, num mod 5) in
        match rem with
        | 0 -> aux num ('0' :: digits)
        | 1 -> aux num ('1' :: digits)
        | 2 -> aux num ('2' :: digits)
        | 3 -> aux (num + 1) ('=' :: digits)
        | 4 -> aux (num + 1) ('-' :: digits)
        | _ -> failwith "impossible"
    in
    aux num [] |> String.of_list

  let parse str =
    let digits = String.to_list str |> List.rev in
    let rec aux digits place value =
      match digits with
      | [] -> value
      | digit :: tl ->
          let v' =
            match digit with
            | '0' -> 0
            | '1' -> place
            | '2' -> 2 * place
            | '-' -> -1 * place
            | '=' -> -2 * place
            | _ -> failwith "invalid-digit"
          in
          aux tl (place * 5) (v' + value)
    in
    aux digits 1 0
end

let input = IO.read_lines_l stdin
let parse input = List.map SNAFU.parse input

let part1 input =
  let nums = parse input in
  List.fold_left ( + ) 0 nums |> SNAFU.to_rep

let _ = Printf.printf "part1=%s;part2=" (part1 input)
