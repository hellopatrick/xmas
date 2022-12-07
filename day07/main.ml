open Core

let input = In_channel.(input_lines stdin)

module File = struct
  type t = {name: string; size: int}
end

module Dir = struct
  type t = {name: string; parent: t option; children: t list; files: File.t list}
end

let parse lines =
  let hm = Hashtbl.create (module String) in
  let rec aux lines path =
    match lines with
    | [] ->
        hm
    | "$ ls" :: tl ->
        aux tl path
    | line :: tl when String.is_prefix line ~prefix:"dir" ->
        aux tl path
    | line :: tl when String.is_prefix line ~prefix:"$ cd" -> (
        let name = Scanf.sscanf line "$ cd %s" Fn.id in
        match name with
        | ".." ->
            aux tl (List.tl_exn path)
        | _ ->
            aux tl (name :: path) )
    | line :: tl ->
        let data, _ =
          Scanf.sscanf line "%d %s" (fun size name -> (size, name))
        in
        let _ =
          List.fold_right path
            ~f:(fun comp acc ->
              let acc = comp :: acc in
              let key = String.concat ~sep:"/" acc in
              let _ =
                Hashtbl.update hm key ~f:(function
                  | None ->
                      data
                  | Some v ->
                      v + data )
              in
              acc )
            ~init:[]
        in
        aux tl path
  in
  aux lines []

let disk = parse input

let part1 disk =
  Hashtbl.fold disk ~init:0 ~f:(fun ~key:_ ~data acc ->
      if data <= 100000 then acc + data else acc )

let part2 disk =
  let max_space = 70000000 in
  let required_space = 30000000 in
  let used_space = Hashtbl.find_exn disk "/" in
  let free_space = max_space - used_space in
  let needed_space = required_space - free_space in
  Hashtbl.fold ~init:Int.max_value
    ~f:(fun ~key:_ ~data acc ->
      if data >= needed_space && data < acc then data else acc )
    disk

let _ = Printf.printf "part1=%d;part2=%d" (part1 disk) (part2 disk)
