open Containers

module Input = struct
  let parse ?(repeat = 1) line =
    match String.split_on_char ' ' line with
    | [ map; guide ] ->
        let map =
          List.init repeat (fun _ -> String.to_list map)
          |> List.intersperse [ '?' ] |> List.flatten
        in
        let guide =
          String.split_on_char ',' guide
          |> List.map int_of_string |> List.repeat repeat
        in
        (map, guide)
    | _ -> failwith "invalid"
end

let cache = Hashtbl.create 10_000
let cache' = Hashtbl.create 10_000

let rec possibilities cs is =
  Hashtbl.get_or_add cache'
    ~f:(fun (cs, is) ->
      match (cs, is) with
      | [], [] -> 1
      | [], _ -> 0
      | '.' :: tl, _ -> possibilities tl is
      | '#' :: _, _ -> handle_dmg cs is
      | '?' :: tl, _ -> possibilities tl is + handle_dmg cs is
      | _ -> failwith "impossible")
    ~k:(cs, is)

and handle_dmg cs is =
  Hashtbl.get_or_add cache
    ~f:(fun (cs, is) ->
      let cl = List.length cs in
      match is with
      | [] -> 0
      | a :: tl when cl < a -> 0
      | a :: tl when List.exists (Char.equal '.') (List.take a cs) -> 0
      | a :: [] when cl = a -> 1
      | a :: _ when cl = a -> 0
      | a :: tl -> (
          match List.drop a cs with
          | '#' :: _ -> 0
          | _ :: dc -> possibilities dc tl
          | _ -> failwith "impossible"))
    ~k:(cs, is)

let input = IO.read_lines_l stdin

let part1 =
  List.map Input.parse input
  |> List.map (fun (cs, is) -> possibilities cs is)
  |> List.fold_left ( + ) 0

let part2 =
  List.map (Input.parse ~repeat:5) input
  |> List.map (fun (cs, is) -> possibilities cs is)
  |> List.fold_left ( + ) 0

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
