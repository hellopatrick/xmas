open Core

module T3 = struct
  type t = int * int * int [@@deriving show, eq, ord, sexp]

  let diff (x0, y0, z0) (x1, y1, z1) = (x0 - x1, y0 - y1, z0 - z1)

  let add (x0, y0, z0) (x1, y1, z1) = (x0 + x1, y0 + y1, z0 + z1)

  let manhattan_dist a b =
    let dx, dy, dz = diff a b in
    Int.abs dx + Int.abs dy + Int.abs dz
end

module CS = Set.Make (T3)

let show_cs t = CS.sexp_of_t t |> Sexp.to_string_hum

let parse lines =
  List.group lines ~break:(fun a b -> String.is_empty a || String.is_empty b)
  |> List.filter_map ~f:(function
       | [] | [""] ->
           None
       | header :: probes when String.is_prefix header ~prefix:"---" ->
           Some probes
       | _ ->
           failwith "unknown group" )
  |> List.map
       ~f:
         (List.map ~f:(fun line ->
              Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z)) ) )
  |> List.map ~f:CS.of_list

let rotations =
  [ (fun (x, y, z) -> (x, y, z))
  ; (fun (x, y, z) -> (x, z, -y))
  ; (fun (x, y, z) -> (x, -y, -z))
  ; (fun (x, y, z) -> (x, -z, y))
  ; (fun (x, y, z) -> (-x, y, -z))
  ; (fun (x, y, z) -> (-x, z, y))
  ; (fun (x, y, z) -> (-x, -y, z))
  ; (fun (x, y, z) -> (-x, -z, -y))
  ; (fun (x, y, z) -> (-y, x, z))
  ; (fun (x, y, z) -> (-z, x, -y))
  ; (fun (x, y, z) -> (y, x, -z))
  ; (fun (x, y, z) -> (z, x, y))
  ; (fun (x, y, z) -> (y, -x, z))
  ; (fun (x, y, z) -> (z, -x, -y))
  ; (fun (x, y, z) -> (-y, -x, -z))
  ; (fun (x, y, z) -> (-z, -x, y))
  ; (fun (x, y, z) -> (-z, y, x))
  ; (fun (x, y, z) -> (y, z, x))
  ; (fun (x, y, z) -> (z, -y, x))
  ; (fun (x, y, z) -> (-y, -z, x))
  ; (fun (x, y, z) -> (z, y, -x))
  ; (fun (x, y, z) -> (-y, z, -x))
  ; (fun (x, y, z) -> (-z, -y, -x))
  ; (fun (x, y, z) -> (y, -z, -x)) ]

module IntSet = Set.Make (Int)

let find_offset' known unknown =
  CS.find_map unknown ~f:(fun pt ->
      let possible_offsets = CS.map known ~f:(fun a -> T3.diff a pt) in
      CS.find_map possible_offsets ~f:(fun offset ->
          let transformed_unknown =
            CS.map unknown ~f:(fun pt -> T3.add pt offset)
          in
          let common = CS.inter transformed_unknown known in
          let len = CS.length common in
          if len >= 12 then Some (offset, transformed_unknown) else None ) )

let find_offset known unknown =
  CS.find_map unknown ~f:(fun pt ->
      let seq = Set.to_sequence known in
      let possible_offsets = Sequence.map seq ~f:(fun a -> T3.diff a pt) in
      Sequence.find_map possible_offsets ~f:(fun offset ->
          let transformed_unknown =
            CS.map unknown ~f:(fun pt -> T3.add pt offset)
          in
          let common = CS.inter transformed_unknown known in
          let len = CS.length common in
          if len >= 12 then Some (offset, transformed_unknown) else None ) )

let fit known_beacons unknown_scan =
  List.find_map unknown_scan ~f:(fun transformed ->
      find_offset known_beacons transformed )

let find_next_alignment known_beacons unknown_scans =
  let rec aux unknown_scans still_unknown_scans =
    match unknown_scans with
    | [] ->
        failwith "unknown_scans = empty_list"
    | attempt :: tl -> (
      match fit known_beacons attempt with
      | Some (offset, adjusted) ->
          (offset, adjusted, tl @ still_unknown_scans)
      | None ->
          aux tl (attempt :: still_unknown_scans) )
  in
  aux unknown_scans []

let rec solve offsets known_beacons unknown_scans =
  (* Printf.printf "offsets=%d known_beacons=%d unknown_scans=%d\n"
       (List.length offsets) (CS.length known_beacons)
       (List.length unknown_scans) ;
     Out_channel.(flush stdout) ; *)
  match unknown_scans with
  | [] ->
      (offsets, known_beacons)
  | _ ->
      let offset, found_beacons, leftovers =
        find_next_alignment known_beacons unknown_scans
      in
      solve (offset :: offsets) (CS.union known_beacons found_beacons) leftovers

let process = function
  | [] ->
      failwith "must have at least one reading"
  | known :: rest ->
      solve
        [(0, 0, 0)]
        known
        (* pre-compute the possible rotations *)
        (List.map rest ~f:(fun reading ->
             List.map rotations ~f:(fun rot -> CS.map reading ~f:rot) ) )

let input = Stdio.(In_channel.input_lines stdin) |> parse

let offsets, known_beacons = process input

let part1 = CS.length known_beacons

let part2 =
  List.cartesian_product offsets offsets
  |> List.map ~f:(fun (a, b) -> T3.manhattan_dist a b)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
