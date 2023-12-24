open Containers

let seed = IO.read_all stdin |> String.trim
let cache = Hashtbl.create 10000

let stretch_hash k =
  let h = Digest.string (Format.sprintf "%s%d" seed k) |> Digest.to_hex in

  let rec aux h n =
    if n = 0 then h
    else
      let h' = Digest.string h |> Digest.to_hex in
      aux h' (n - 1)
  in

  aux h 2016

let hash ~stretch k =
  Hashtbl.get_or_add cache
    ~f:(fun k ->
      (if stretch then stretch_hash k
       else Digest.string (Format.sprintf "%s%d" seed k) |> Digest.to_hex)
      |> String.to_list
      |> List.group_succ ~eq:Char.equal)
    ~k

let has_chain ?(stretch = false) k c =
  hash ~stretch k
  |> List.exists (function
       | a :: _ :: _ :: _ :: _ :: _ -> Char.equal a c
       | _ -> false)

let possible_key ?(stretch = false) k =
  hash ~stretch k
  |> List.find_opt (fun chain -> List.length chain >= 3)
  |> Option.map List.hd

let part1 =
  Seq.ints 0
  |> Seq.filter_map (fun i -> possible_key i |> Option.map (fun c -> (i, c)))
  |> Seq.filter (fun (i, c) ->
         Seq.range (i + 1) (i + 1000) |> Seq.exists (fun k -> has_chain k c))
  |> Seq.drop 63 |> Seq.head_exn |> fst

let _ = Hashtbl.clear cache

let part2 =
  Seq.ints 0
  |> Seq.filter_map (fun i ->
         possible_key ~stretch:true i |> Option.map (fun c -> (i, c)))
  |> Seq.filter (fun (i, c) ->
         Seq.range (i + 1) (i + 1000)
         |> Seq.exists (fun k -> has_chain ~stretch:true k c))
  |> Seq.drop 63 |> Seq.head_exn |> fst

let _ = Printf.printf "part1 = %d ; part2 = %d" part1 part2
