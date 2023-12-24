open Containers

module V = struct
  type t = { x : float; y : float; z : float }
end

module HS = struct
  type t = { p : V.t; v : V.t }

  let m t = t.v.y /. t.v.x
  let b t = t.p.y -. (m t *. t.p.x)

  let intersect t s =
    let mt = m t and ms = m s in
    if Float.equal mt ms then None
    else
      let bt = b t and bs = b s in
      let x = (bt -. bs) /. (ms -. mt) in
      let y = (mt *. x) +. bt in
      Some (x, y)

  let time_of_x x t = (x -. t.p.x) /. t.v.x
  let show t = Printf.sprintf "%f,%f [%f,%f]" t.p.x t.p.y t.v.x t.v.y
end

module Input = struct
  let parse_line line =
    Scanf.sscanf line "%f, %f, %f @ %f, %f, %f" (fun px py pz vx vy vz ->
        HS.
          { p = V.{ x = px; y = py; z = pz }; v = V.{ x = vx; y = vy; z = vz } })

  let parse lines = List.map parse_line lines
end

let input =
  IO.read_lines_l stdin |> List.filter Xmas.Str.is_not_empty |> Input.parse

let pairs l =
  let rec aux l pairs =
    match l with
    | [] -> pairs
    | hd :: tl ->
        let p = List.map (fun c -> (c, hd)) tl in
        aux tl (p @ pairs)
  in
  aux l []

let part1 =
  let ps = pairs input in
  List.filter
    (fun (p, q) ->
      match HS.intersect p q with
      | None -> false
      | Some (x, y) ->
          if
            x >=. 200000000000000.0 && y >=. 200000000000000.0
            && x <=. 400000000000000.0 && y <=. 400000000000000.0
            && HS.time_of_x x p >=. 0.0
            && HS.time_of_x x q >=. 0.0
          then true
          else false)
    ps
  |> List.length

(* generate file for `alt-ergo`, then just add px, py, pz manually.*)
let part2 =
  let open HS in
  let file = "output.ae" in
  let oc = open_out file in

  Printf.fprintf oc "logic px, py, pz, vx, vy, vz : real\n";
  List.iteri
    (fun i t ->
      Printf.fprintf oc "logic t%03d : real\n" i;
      Printf.fprintf oc "axiom cx%03d : %f + (%f * t%03d) = px + (vx * t%03d)\n"
        i t.p.x t.v.x i i;
      Printf.fprintf oc "axiom cy%03d : %f + (%f * t%03d) = py + (vy * t%03d)\n"
        i t.p.y t.v.y i i;
      Printf.fprintf oc "axiom cz%03d : %f + (%f * t%03d) = pz + (vz * t%03d)\n"
        i t.p.z t.v.z i i;
      Printf.fprintf oc "check_sat at%03d : t%03d > 0.0\n" i i)
    (List.take 10 input);
  (* turns out don't actually need all the lines. probably something to do with planes? *)
  close_out oc
