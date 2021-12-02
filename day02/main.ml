(* https://adventofcode.com/2021/day/2 *)

open Core

module Direction = struct
  type t =
    | Forward of int
    | Up of int
    | Down of int

  let of_string str =
    let parts = String.split ~on:' ' str in
    match parts with
    | [ "forward"; n ] ->
        Forward (Int.of_string n)
    | [ "up"; n ] ->
        Up (Int.of_string n)
    | [ "down"; n ] ->
        Down (Int.of_string n)
    | _ ->
        raise (Failure "unreachable")
end

let input =
  In_channel.read_lines "./input/day02.txt" |> List.map ~f:Direction.of_string


let route (x, y) = function
  | Direction.Forward n ->
      (x + n, y)
  | Direction.Up n ->
      (x, y - n)
  | Direction.Down n ->
      (x, y + n)


let part1 =
  let x, y = List.fold ~init:(0, 0) ~f:route input in
  x * y


let aim (x, y, a) = function
  | Direction.Forward n ->
      (x + n, y + (a * n), a)
  | Direction.Up n ->
      (x, y, a - n)
  | Direction.Down n ->
      (x, y, a + n)


let part2 =
  let x, y, _ = List.fold ~init:(0, 0, 0) ~f:aim input in
  x * y


let () = Printf.printf "part1=%d; part2=%d" part1 part2
