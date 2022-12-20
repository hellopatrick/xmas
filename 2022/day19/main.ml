open Containers

module Blueprint = struct
  type t =
    { num: int
    ; ore_ore: int
    ; clay_ore: int
    ; obsidian_ore: int
    ; obsidian_clay: int
    ; geode_ore: int
    ; geode_obsidian: int }

  let max_ore t =
    max t.ore_ore (max t.clay_ore (max t.obsidian_ore t.geode_ore))

  let pp t =
    Printf.printf "%d %d %d (%d,%d) (%d,%d)\n" t.num t.ore_ore t.clay_ore
      t.obsidian_ore t.obsidian_clay t.geode_ore t.geode_obsidian
end

module State = struct
  type t =
    { time_left: int
    ; ore: int
    ; clay: int
    ; obsidian: int
    ; geode: int
    ; ore_robots: int
    ; clay_robots: int
    ; obsidian_robots: int
    ; geode_robots: int }

  let pp t =
    Printf.printf "@%d (%d, %d, %d, %d) (%d, %d, %d, %d)\n" t.time_left t.ore
      t.clay t.obsidian t.geode t.ore_robots t.clay_robots t.obsidian_robots
      t.geode_robots

  let start n =
    { time_left= n
    ; ore= 0
    ; clay= 0
    ; obsidian= 0
    ; geode= 0
    ; ore_robots= 1
    ; clay_robots= 0
    ; obsidian_robots= 0
    ; geode_robots= 0 }

  let scrub t = {t with time_left= 0}

  let max_geodes n (bp : Blueprint.t) =
    let rec aux q m =
      match q with
      | [] ->
          m
      | t :: q' ->
          if t.time_left <= 0 then aux q' m
          else if
            t.geode
            + (t.time_left * t.geode_robots)
            + (t.time_left * t.time_left / 2)
            < m (* cannot catch up. *)
          then aux q' m
          else
            let t' =
              { t with
                time_left= t.time_left - 1
              ; ore= t.ore + t.ore_robots
              ; clay= t.clay + t.clay_robots
              ; obsidian= t.obsidian + t.obsidian_robots
              ; geode= t.geode + t.geode_robots }
            in
            let m' = max m t'.geode in
            let res =
              if t.ore >= bp.geode_ore && t.obsidian >= bp.geode_obsidian then
                [ { t' with
                    ore= t'.ore - bp.geode_ore
                  ; obsidian= t'.obsidian - bp.geode_obsidian
                  ; geode_robots= t'.geode_robots + 1 } ]
              else
                [ ( if
                    t.ore >= bp.obsidian_ore && t.clay >= bp.obsidian_clay
                    && t.obsidian_robots < bp.geode_obsidian
                  then
                    Some
                      { t' with
                        ore= t'.ore - bp.obsidian_ore
                      ; clay= t'.clay - bp.obsidian_clay
                      ; obsidian_robots= t'.obsidian_robots + 1 }
                  else None )
                ; ( if t.ore >= bp.clay_ore && t.clay_robots < bp.obsidian_clay
                  then
                    Some
                      { t' with
                        ore= t'.ore - bp.clay_ore
                      ; clay_robots= t'.clay_robots + 1 }
                  else None )
                ; ( if t.ore >= bp.ore_ore && t.ore_robots < Blueprint.max_ore bp
                  then
                    Some
                      { t' with
                        ore= t'.ore - bp.ore_ore
                      ; ore_robots= t'.ore_robots + 1 }
                  else None )
                ; (if t.ore < 6 then Some t' else None) ]
                |> List.filter_map Fun.id
            in
            aux (res @ q') m'
    in
    aux [start n] 0
end

module Parser = struct
  let parse line =
    Scanf.sscanf line
      "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d \
       ore. Each obsidian robot costs %d ore and %d clay. Each geode robot \
       costs %d ore and %d obsidian."
      (fun
        num
        ore_ore
        clay_ore
        obsidian_ore
        obsidian_clay
        geode_ore
        geode_obsidian
      ->
        Blueprint.
          { num
          ; ore_ore
          ; clay_ore
          ; obsidian_ore
          ; obsidian_clay
          ; geode_ore
          ; geode_obsidian } )
end

let input = IO.read_lines_l stdin

let parse input = List.map Parser.parse input

let part1 input =
  let blueprints = parse input in
  let res =
    List.map
      (fun bp ->
        let open Blueprint in
        bp.num * State.max_geodes 24 bp )
      blueprints
  in
  List.fold_left ( + ) 0 res

let part2 input =
  let blueprints = parse input |> List.take 3 in
  let res = List.map (State.max_geodes 32) blueprints in
  List.fold_left ( * ) 1 res

let _ = Printf.printf "part1=%d;part2=%d" (part1 input) (part2 input)
