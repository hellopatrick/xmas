open Core

module Amphipod = struct
  type t = Amber | Bronze | Copper | Desert
  [@@deriving eq, enum, show, hash, sexp, ord]

  let cost = function
    | Amber ->
        1
    | Bronze ->
        10
    | Copper ->
        100
    | Desert ->
        1000

  let to_char = function
    | Amber ->
        'A'
    | Bronze ->
        'B'
    | Copper ->
        'C'
    | Desert ->
        'D'

  let of_char = function
    | 'A' ->
        Amber
    | 'B' ->
        Bronze
    | 'C' ->
        Copper
    | 'D' ->
        Desert
    | _ ->
        failwith "unknown amphipod"

  let idx = to_enum
end

module IntMap = struct
  include Map.Make (Int)
  include Provide_hash (Int)
end

module State = struct
  type t = {spots: Amphipod.t IntMap.t; slots: Amphipod.t list IntMap.t}
  [@@deriving eq, hash, sexp, ord]

  let show_spot t i =
    match IntMap.find t.spots i with
    | None ->
        '.'
    | Some a ->
        Amphipod.to_char a

  let show_slot t i j =
    match IntMap.find t.slots i with
    | None ->
        '?'
    | Some tl ->
        let missing = List.length tl - 4 in
        if j + missing < 0 then '.'
        else
          List.nth tl (j + missing)
          |> Option.map ~f:Amphipod.to_char
          |> Option.value ~default:'.'

  let show t =
    String.strip
      (Printf.sprintf
         {|
#############
#%c%c.%c.%c.%c.%c%c#
###%c#%c#%c#%c###
  #%c#%c#%c#%c#
  #%c#%c#%c#%c#
  #%c#%c#%c#%c#
  #########
    |}
         (show_spot t 0) (show_spot t 1) (show_spot t 2) (show_spot t 3)
         (show_spot t 4) (show_spot t 5) (show_spot t 6) (show_slot t 0 0)
         (show_slot t 1 0) (show_slot t 2 0) (show_slot t 3 0) (show_slot t 0 1)
         (show_slot t 1 1) (show_slot t 2 1) (show_slot t 3 1) (show_slot t 0 2)
         (show_slot t 1 2) (show_slot t 2 2) (show_slot t 3 2) (show_slot t 0 3)
         (show_slot t 1 3) (show_slot t 2 3) (show_slot t 3 3) )

  let final =
    { spots= IntMap.empty
    ; slots=
        IntMap.of_alist_exn
          [ (0, List.init 4 ~f:(fun _ -> Amphipod.Amber))
          ; (1, List.init 4 ~f:(fun _ -> Amphipod.Bronze))
          ; (2, List.init 4 ~f:(fun _ -> Amphipod.Copper))
          ; (3, List.init 4 ~f:(fun _ -> Amphipod.Desert)) ] }

  let is_final = equal final

  let rec can_land l a =
    match l with
    | [] ->
        true
    | hd :: tl ->
        if Amphipod.equal hd a then can_land tl a else false

  let must_check =
    [| [|(2, [1]); (4, [1; 2]); (6, [1; 2; 3]); (8, [1; 2; 3; 4])|]
     ; [|(1, []); (3, [2]); (5, [2; 3]); (7, [2; 3; 4])|]
     ; [|(1, []); (1, []); (3, [3]); (5, [3; 4])|]
     ; [|(3, [2]); (1, []); (1, []); (3, [4])|]
     ; [|(5, [2; 3]); (3, [3]); (1, []); (1, [])|]
     ; [|(7, [2; 3; 4]); (5, [3; 4]); (3, [4]); (1, [])|]
     ; [|(8, [2; 3; 4; 5]); (6, [3; 4; 5]); (4, [4; 5]); (2, [5])|] |]

  let possible_spot_moves_for_spot t i a =
    let spots = IntMap.remove t.spots i in
    let desired = Amphipod.to_enum a in
    let slot = IntMap.find_exn t.slots desired in
    if can_land slot a then
      let dist, possible_blocks = must_check.(i).(desired) in
      if List.for_all possible_blocks ~f:(fun i -> not (IntMap.mem spots i))
      then
        [ ( (dist + (4 - List.length slot)) * Amphipod.cost a
          , {spots; slots= IntMap.update t.slots desired ~f:(fun _ -> a :: slot)}
          ) ]
      else []
    else []

  let possible_spot_moves t =
    IntMap.fold t.spots ~init:[] ~f:(fun ~key:i ~data:a acc ->
        possible_spot_moves_for_spot t i a @ acc )

  let must_check' =
    [| [ (3, [0; 1])
       ; (2, [1])
       ; (2, [2])
       ; (4, [2; 3])
       ; (6, [2; 3; 4])
       ; (8, [2; 3; 4; 5])
       ; (9, [2; 3; 4; 5; 6]) ]
     ; [ (5, [0; 1; 2])
       ; (4, [1; 2])
       ; (2, [2])
       ; (2, [3])
       ; (4, [3; 4])
       ; (6, [3; 4; 5])
       ; (7, [3; 4; 5; 6]) ]
     ; [ (7, [0; 1; 2; 3])
       ; (6, [1; 2; 3])
       ; (4, [2; 3])
       ; (2, [3])
       ; (2, [4])
       ; (4, [3; 4; 5])
       ; (5, [3; 4; 5; 6]) ]
     ; [ (9, [0; 1; 2; 3; 4])
       ; (8, [1; 2; 3; 4])
       ; (6, [2; 3; 4])
       ; (4, [3; 4])
       ; (2, [4])
       ; (2, [5])
       ; (3, [5; 6]) ] |]

  let possible_slot_moves_for_slot t i =
    let slot = IntMap.find_exn t.slots i in
    match slot with
    | [] ->
        []
    | hd :: tl ->
        let desired = Amphipod.to_enum hd in
        let spots = t.spots in
        if desired = i && can_land tl hd then []
        else
          let paths_out = must_check'.(i) in
          List.filter_mapi paths_out ~f:(fun j (dist, path) ->
              if List.for_all path ~f:(fun i -> not (IntMap.mem spots i)) then
                Some
                  ( (dist + 4 - List.length slot) * Amphipod.cost hd
                  , { spots= IntMap.set spots ~key:j ~data:hd
                    ; slots= IntMap.set t.slots ~key:i ~data:tl } )
              else None )

  let possible_slot_moves t =
    IntMap.fold t.slots ~init:[] ~f:(fun ~key:i ~data acc ->
        match data with
        | [] ->
            acc
        | _ ->
            possible_slot_moves_for_slot t i @ acc )
end

let possible_next_states_memo = Hashtbl.create (module State)

let possible_next_states t =
  Hashtbl.find_or_add possible_next_states_memo t ~default:(fun () ->
      let spot_moves = State.possible_spot_moves t
      and slot_moves = State.possible_slot_moves t in
      spot_moves @ slot_moves )

module PQ = Xmas.Priority_queue

let solve initial_state =
  let memo = Hashtbl.create (module State) in
  let rec aux queue best_known_cost =
    let next = PQ.extract queue in
    match next with
    | None ->
        best_known_cost
    | Some (current_cost, state, queue) -> (
        let seen =
          Hashtbl.find memo state |> Option.value ~default:Int.max_value
        in
        if current_cost >= best_known_cost || current_cost > seen then
          aux queue best_known_cost
        else if State.is_final state then Int.min current_cost best_known_cost
        else
          match possible_next_states state with
          | [] ->
              aux queue best_known_cost
          | ns ->
              let ns =
                List.map ns ~f:(fun (cost, state) ->
                    let total_cost = current_cost + cost in
                    (total_cost, state) )
              in
              let pq =
                List.fold ns ~init:queue ~f:(fun acc (c, s) ->
                    match Hashtbl.find memo s with
                    | None ->
                        PQ.insert acc c s
                    | Some c' ->
                        if c <= c' then PQ.insert acc c s else acc )
              in
              List.iter ns ~f:(fun (current_cost, state) ->
                  Hashtbl.update memo state ~f:(function
                    | None ->
                        current_cost
                    | Some c ->
                        Int.min c current_cost ) ) ;
              aux pq best_known_cost )
  in
  let pq = PQ.empty in
  aux (PQ.insert pq 0 initial_state) Int.max_value

let input =
  In_channel.(input_lines stdin)
  |> List.map ~f:(fun line ->
         String.to_list line |> List.map ~f:Amphipod.of_char )

let initial =
  State.
    { spots= IntMap.empty
    ; slots= List.mapi input ~f:(fun i a -> (i, a)) |> IntMap.of_alist_exn }

let part2 = solve initial

let _ = Printf.printf "part2=%d" part2
