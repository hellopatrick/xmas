open Core
module CharSet = Set.Make (Char)

type t =
  { hints : CharSet.t list
  ; output : CharSet.t list
  }

let to_charset str = str |> String.to_list |> CharSet.of_list

let parse_line l =
  Scanf.sscanf
    l
    "%s %s %s %s %s %s %s %s %s %s | %s %s %s %s"
    (fun i0 i1 i2 i3 i4 i5 i6 i7 i8 i9 o0 o1 o2 o3 ->
      { hints =
          [ to_charset i0
          ; to_charset i1
          ; to_charset i2
          ; to_charset i3
          ; to_charset i4
          ; to_charset i5
          ; to_charset i6
          ; to_charset i7
          ; to_charset i8
          ; to_charset i9
          ]
      ; output = [ to_charset o0; to_charset o1; to_charset o2; to_charset o3 ]
      } )


let input = In_channel.read_lines "./input/day08.txt" |> List.map ~f:parse_line

let part1 =
  let f { output; _ } =
    List.count
      ~f:(fun s ->
        let len = Set.length s in
        len = 2 || len = 3 || len = 4 || len = 7 )
      output
  in
  List.map ~f input |> Xmas.Enum.sum


module CharMap = Map.Make (Char)

let frequency_map hints =
  let f init hint =
    Set.fold
      ~init
      ~f:(fun m c ->
        CharMap.update m c ~f:(function Some cnt -> cnt + 1 | None -> 1) )
      hint
  in
  List.fold ~init:CharMap.empty ~f hints


let translate_output mapping output =
  let fingerprint =
    List.map
      ~f:(fun s -> CharSet.exists ~f:(fun c -> Char.equal c s) output)
      mapping
  in
  match fingerprint with
  | [ true; true; true; false; true; true; true ] ->
      0
  | [ false; false; true; false; false; true; false ] ->
      1
  | [ true; false; true; true; true; false; true ] ->
      2
  | [ true; false; true; true; false; true; true ] ->
      3
  | [ false; true; true; true; false; true; false ] ->
      4
  | [ true; true; false; true; false; true; true ] ->
      5
  | [ true; true; false; true; true; true; true ] ->
      6
  | [ true; false; true; false; false; true; false ] ->
      7
  | [ true; true; true; true; true; true; true ] ->
      8
  | [ true; true; true; true; false; true; true ] ->
      9
  | _ ->
      raise Xmas.Exc.Unreachable


let decode { hints; output } =
  let seven = List.find_exn ~f:(fun s -> Set.length s = 3) hints in
  let one = List.find_exn ~f:(fun s -> Set.length s = 2) hints in
  let four = List.find_exn ~f:(fun s -> Set.length s = 4) hints in
  let all = CharSet.of_list [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g' ] in
  let freq_map = frequency_map hints in
  let top_left =
    CharMap.filter freq_map ~f:(fun c -> c = 6) |> CharMap.keys |> List.hd_exn
  in
  let bottom_left =
    CharMap.filter freq_map ~f:(fun c -> c = 4) |> CharMap.keys |> List.hd_exn
  in
  let bottom_right =
    CharMap.filter freq_map ~f:(fun c -> c = 9) |> CharMap.keys |> List.hd_exn
  in
  let top = Set.diff seven one |> Set.choose_exn in
  let top_right = Set.remove one bottom_right |> Set.choose_exn in
  let middle =
    Set.diff four (CharSet.of_list [ top_left; top_right; bottom_right ])
    |> Set.choose_exn
  in
  let deduced =
    CharSet.of_list
      [ bottom_left; bottom_right; top; top_right; top_left; middle ]
  in
  let bottom = Set.diff all deduced |> Set.choose_exn in
  let translated_hints =
    [ top; top_left; top_right; middle; bottom_left; bottom_right; bottom ]
  in
  List.map ~f:(translate_output translated_hints) output
  |> List.rev
  |> List.foldi ~init:0 ~f:(fun i acc v -> acc + (Int.pow 10 i * v))


let part2 = input |> List.map ~f:decode |> Xmas.Enum.sum

let () = Printf.printf "part1=%d; part2=%d" part1 part2