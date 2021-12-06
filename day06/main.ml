open Core

let input =
  let starting_fish =
    In_channel.read_all "./input/day06.txt"
    |> String.split ~on:','
    |> List.map ~f:Int.of_string
  in
  List.init 9 ~f:(fun i -> List.count ~f:(fun j -> i = j) starting_fish)


let process fishes =
  match fishes with
  | [ zero; one; two; three; four; five; six; seven; eight ] ->
      [ one; two; three; four; five; six; seven + zero; eight; zero ]
  | _ ->
      raise (Failure "unreachable")


let rec spawn fishes n =
  if n > 0 then spawn (process fishes) (n - 1) else fishes


let part1 = spawn input 80 |> Xmas.Enum.sum

let part2 = spawn input 256 |> Xmas.Enum.sum

let _ = Printf.printf "part1=%d; part2=%d" part1 part2
