open Core

let input =
  let lines = In_channel.read_lines "./input/day03.txt" in
  let f c = if Char.equal c '0' then 0 else 1 in
  let f s = String.to_list s |> List.map ~f in
  List.map ~f lines

let bitlist_to_int =
  let f curr bit = Int.bit_or (Int.shift_left curr 1) bit in
  List.fold ~init:0 ~f

let part1 =
  let counts =
    input |> List.transpose_exn
    |> List.map
         ~f:
           (List.fold ~init:(0, 0) ~f:(fun (c0, c1) c ->
                if c = 0 then (c0 + 1, c1) else (c0, c1 + 1)))
  in
  let gamma =
    counts
    |> List.map ~f:(fun (x, y) -> if x > y then 0 else 1)
    |> bitlist_to_int
  in
  let epsilon =
    counts
    |> List.map ~f:(fun (x, y) -> if x < y then 0 else 1)
    |> bitlist_to_int
  in
  gamma * epsilon

let part2 =
  let filter ls n chooser =
    let len = List.length ls in
    let cs = List.map ~f:(fun l -> List.nth_exn l n) ls in
    let c0 = List.count ~f:(Int.equal 0) cs in
    let choice = chooser c0 len in
    List.filter ~f:(fun l -> Int.equal choice (List.nth_exn l n)) ls
  in
  let rec helper l n chooser =
    match l with
    | [] -> raise (Failure "?")
    | [ ans ] -> ans
    | _ -> helper (filter l n chooser) (n + 1) chooser
  in
  let o2 =
    helper input 0 (fun c0 len -> if c0 > len / 2 then 0 else 1)
    |> bitlist_to_int
  in
  let co2 =
    helper input 0 (fun c0 len -> if c0 <= len / 2 then 0 else 1)
    |> bitlist_to_int
  in
  o2 * co2

let () = Printf.printf "part1=%d; part2=%d" part1 part2
