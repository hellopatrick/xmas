open Containers

let input = IO.(read_lines_l stdin)

let parse lines =
  let hm = Hashtbl.create 10 in
  let rec aux lines path =
    match lines with
    | [] -> hm
    | "$ ls" :: tl -> aux tl path
    | line :: tl when String.prefix line ~pre:"dir" -> aux tl path
    | line :: tl when String.prefix line ~pre:"$ cd" -> (
        let name = Scanf.sscanf line "$ cd %s" Fun.id in
        match name with
        | ".." -> aux tl (List.tl path)
        | _ -> aux tl (name :: path))
    | line :: tl ->
        let data, _ =
          Scanf.sscanf line "%d %s" (fun size name -> (size, name))
        in
        let _ =
          List.fold_right
            (fun comp acc ->
              let k = acc ^ comp in
              let _ =
                Hashtbl.update hm
                  ~f:(fun _ b ->
                    match b with None -> Some data | Some v -> Some (v + data))
                  ~k
              in
              k)
            path ""
        in
        aux tl path
  in
  aux lines []

let disk = parse input

let part1 disk =
  Hashtbl.fold
    (fun _ data acc -> if data <= 100000 then acc + data else acc)
    disk 0

let part2 disk =
  let max_space = 70000000 in
  let required_space = 30000000 in
  let used_space = Hashtbl.find disk "/" in
  let free_space = max_space - used_space in
  let needed_space = required_space - free_space in
  Hashtbl.fold
    (fun _ data acc -> if data >= needed_space && data < acc then data else acc)
    disk Int.max_int

let _ = Printf.printf "part1=%d;part2=%d" (part1 disk) (part2 disk)
