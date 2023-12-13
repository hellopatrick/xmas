open Containers

module Input = struct
  let parse input =
    String.trim input |> String.split ~by:"\n\n"
    |> List.map @@ String.split_on_char '\n'
    |> List.map (fun ls -> List.map String.to_list ls)
end

type t = Horizontal of int | Vertical of int

let input = IO.read_all stdin

let rec is_prefix l p =
  match (l, p) with
  | [], _ -> true
  | _, [] -> true
  | l :: l', p :: p' ->
      if List.equal Char.equal l p then is_prefix l' p' else false

let solve grid ignore =
  let rec aux i =
    if i = 0 then None
    else if i = ignore then aux (i - 1)
    else
      let hd, tl = List.take_drop i grid in
      let hd = List.rev hd in
      if is_prefix hd tl then Some i else aux (i - 1)
  in
  aux (List.length grid - 1)

let find_mirror ?(ignore = (-1, -1)) grid =
  match solve grid (fst ignore) with
  | Some i -> Some (Horizontal i)
  | None -> (
      match solve (Xmas.Enum.transpose grid) (snd ignore) with
      | Some i -> Some (Vertical i)
      | None -> None)

let grids = Input.parse input

let part1 =
  grids
  |> List.filter_map find_mirror
  |> List.fold_left
       (fun acc r ->
         acc + match r with Horizontal h -> 100 * h | Vertical v -> v)
       0

let cleaned grid x y =
  let row = List.get_at_idx_exn y grid in
  let ch = List.get_at_idx_exn x row in
  let ch' = if Char.equal ch '#' then '.' else '#' in
  List.set_at_idx y (List.set_at_idx x ch' row) grid

let cleaned_grids grid =
  let h = List.length grid in
  match grid with
  | [] -> Seq.empty
  | a :: _ ->
      let w = List.length a in
      Seq.init (h * w) (fun i ->
          let y = i / w and x = i mod w in
          cleaned grid x y)

let mirror_equal a b =
  match (a, b) with
  | Horizontal h, Horizontal h' -> h = h'
  | Vertical v, Vertical v' -> v = v'
  | _, _ -> false

let part2 =
  grids
  |> List.filter_map (fun grid ->
         let prev =
           find_mirror grid
           |> Option.map (function
                | Horizontal h -> (h, -1)
                | Vertical v -> (-1, v))
           |> Option.get_exn_or "o?"
         in
         cleaned_grids grid
         |> Seq.find_map (fun grid -> find_mirror grid ~ignore:prev))
  |> List.fold_left
       (fun acc r ->
         acc + match r with Horizontal h -> 100 * h | Vertical v -> v)
       0

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
