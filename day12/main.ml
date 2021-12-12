open Core

module Cave = struct
  type t = Large of string | Small of string [@@deriving show, eq, ord]
  type link = { start : t; finish : t }

  (* now stringable *)
  let of_string =
    let is_large_cave str = String.equal str (String.uppercase str) in
    function str when is_large_cave str -> Large str | str -> Small str

  let to_string = show

  (* how to do this via Sexapble.of_stringable? *)
  let t_of_sexp sexp =
    match sexp with
    | Sexp.Atom s -> ( try of_string s with exn -> of_sexp_error_exn exn sexp)
    | Sexp.List _ ->
        of_sexp_error
          "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list"
          sexp

  let sexp_of_t t = Sexp.Atom (to_string t)

  let parse l =
    match String.split ~on:'-' l with
    | s :: e :: _ -> { start = of_string s; finish = of_string e }
    | _ -> failwith "unreachable"

  let start = Small "start"
  let finish = Small "end"
  let is_end = equal finish
  let is_large = function Large _ -> true | _ -> false
  let hash t = String.hash (to_string t)
end

module CaveSet = Set.Make (Cave)
module CaveTable = Hashtbl.Make (Cave)

let create_adjacency_list links =
  let table = CaveTable.create () in
  List.iter links ~f:(fun Cave.{ start; finish } ->
      let update_with a set =
        match set with
        | Some nodes -> CaveSet.add nodes a
        | None -> CaveSet.of_list [ a ]
      in
      CaveTable.update table start ~f:(update_with finish);
      CaveTable.update table finish ~f:(update_with start));
  table

let input = In_channel.input_lines In_channel.stdin |> List.map ~f:Cave.parse
let adj_list = create_adjacency_list input

let dfs m =
  let find = CaveTable.find_exn m in
  let rec aux node visited path complete =
    if Cave.is_end node then List.rev (node :: path) :: complete
    else
      let adjacent = find node in
      let next = CaveSet.diff adjacent visited in
      if CaveSet.is_empty next then complete
      else
        let visited =
          if Cave.is_large node then visited else CaveSet.add visited node
        in
        let f acc node = aux node visited (node :: path) acc in
        CaveSet.fold next ~init:complete ~f
  in
  aux Cave.start CaveSet.empty [] []

let paths = dfs adj_list
let part1 = List.length paths
let _ = Printf.printf "part1=%d;part2=" part1
