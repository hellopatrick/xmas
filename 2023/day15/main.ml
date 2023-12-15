open Containers

type cmd = Set of string * int | Remove of string

let hash str =
  String.fold (fun acc c -> (acc + Char.to_int c) * 17 mod 256) 0 str

let rem lbl boxes =
  let idx = hash lbl in
  let box = Array.get boxes idx in
  let box' = List.remove_assoc ~eq:String.equal lbl box in
  Array.set boxes idx box'

let set lbl fp boxes =
  let idx = hash lbl in
  let box = Array.get boxes idx in
  let box' =
    if List.mem_assoc ~eq:String.equal lbl box then
      List.map
        (fun (l, f) -> if String.equal l lbl then (lbl, fp) else (l, f))
        box
    else List.append box [ (lbl, fp) ]
  in
  Array.set boxes idx box'

module Input = struct
  let parse str =
    match String.chop_suffix ~suf:"-" str with
    | Some lbl -> rem lbl
    | None -> Scanf.sscanf str "%[a-z]=%d" (fun lbl fp -> set lbl fp)
end

let input = IO.read_all stdin |> String.trim |> String.split_on_char ','
let part1 = List.map hash input |> List.fold_left ( + ) 0

let part2 =
  let cmds = List.map Input.parse input in
  let boxes = Array.make 256 [] in

  List.iter (fun f -> f boxes) cmds;

  Array.foldi
    (fun acc i box ->
      acc
      + List.foldi (fun acc j (_, fp) -> acc + ((i + 1) * (j + 1) * fp)) 0 box)
    0 boxes

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
