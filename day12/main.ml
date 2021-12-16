open Core

module Cave = struct
  type t = Large of string | Small of string
  [@@deriving show, eq, ord, sexp, hash]

  type link = {start: t; finish: t}

  (* now stringable *)
  let of_string =
    let is_large_cave str = String.equal str (String.uppercase str) in
    function str when is_large_cave str -> Large str | str -> Small str

  let to_string = show

  let start = Small "start"

  let finish = Small "end"

  let is_end = equal finish

  let is_large = function Large _ -> true | _ -> false
end

module Path = struct
  type t = {start: Cave.t; finish: Cave.t}

  let of_string t =
    match String.split ~on:'-' t with
    | s :: e :: _ ->
        {start= Cave.of_string s; finish= Cave.of_string e}
    | _ ->
        failwith "unreachable"
end

module CaveSet = Set.Make (Cave)
module CaveTable = Hashtbl.Make (Cave)
module CaveMap = Map.Make (Cave)

let create_adjacency_list links =
  let table = CaveTable.create () in
  List.iter links ~f:(fun Path.{start; finish} ->
      let update_with a set =
        match set with
        | Some nodes ->
            CaveSet.add nodes a
        | None ->
            CaveSet.of_list [a]
      in
      CaveTable.update table start ~f:(update_with finish) ;
      CaveTable.update table finish ~f:(update_with start) ) ;
  table

let input =
  In_channel.input_lines In_channel.stdin |> List.map ~f:Path.of_string

let adj_list = create_adjacency_list input

let part1 =
  let dfs m =
    let find = CaveTable.find_exn m in
    let rec aux node visited path complete =
      let visited =
        if Cave.is_large node then visited else CaveSet.add visited node
      in
      if Cave.is_end node then List.rev (node :: path) :: complete
      else
        let adjacent = find node in
        let next = CaveSet.diff adjacent visited in
        if CaveSet.is_empty next then complete
        else
          let path = node :: path in
          let f acc node = aux node visited path acc in
          CaveSet.fold next ~init:complete ~f
    in
    aux Cave.start CaveSet.empty [] []
  in
  let paths = dfs adj_list in
  List.length paths

let part2 =
  let dfs m =
    let find = CaveTable.find_exn m in
    let rec aux node visited path complete =
      if Cave.is_end node then List.rev (node :: path) :: complete
      else
        let adjacent = find node in
        let visited =
          if Cave.is_large node then visited
          else
            CaveMap.update visited node ~f:(function
              | Some v ->
                  v + 1
              | None ->
                  1 )
        in
        let has_visited_twice =
          CaveMap.exists visited ~f:(fun cnt -> cnt >= 2)
        in
        let next =
          CaveSet.filter adjacent ~f:(function
            | Small "start" ->
                false
            | Small "end" ->
                true
            | Large _ ->
                true
            | node ->
                if CaveMap.mem visited node then not has_visited_twice else true )
        in
        if CaveSet.is_empty next then complete
        else
          let path = node :: path in
          let f acc node = aux node visited path acc in
          CaveSet.fold next ~init:complete ~f
    in
    aux Cave.start CaveMap.empty [] []
  in
  let paths = dfs adj_list in
  List.length paths

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
