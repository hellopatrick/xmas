open Core
module T2 = Tuple.Comparable (Int) (Int)

module Image = struct
  include T2.Map

  let to_ppm t file =
    let file = Out_channel.create file in
    let (x0, y0), _ = min_elt_exn t in
    let (x1, y1), _ = max_elt_exn t in
    let xs = List.range ~start:`inclusive ~stop:`inclusive x0 x1 in
    let ys = List.range ~start:`inclusive ~stop:`inclusive y0 y1 in
    let width = x1 - x0 in
    let height = y1 - y0 in
    Printf.fprintf file "P3\n%d %d\n255\n" width height ;
    List.iter ys ~f:(fun y ->
        List.iter xs ~f:(fun x ->
            let px = find_exn t (x, y) in
            let px = if px then "255 255 255" else "0 0 0" in
            Printf.fprintf file "%s " px ) ;
        Printf.fprintf file "\n" )
end

type t = {image: bool Image.t; algorithm: bool array; default: bool}

let parse lines =
  let parse_algorithm alg =
    alg |> String.to_list
    |> List.map ~f:(function '#' -> true | _ -> false)
    |> Array.of_list
  in
  let parse_image img =
    let image = Image.empty in
    List.foldi img ~init:image ~f:(fun i acc row ->
        String.foldi row ~init:acc ~f:(fun j acc c ->
            match c with
            | '#' ->
                Image.add_exn acc ~key:(j, i) ~data:true
            | _ ->
                Image.add_exn acc ~key:(j, i) ~data:false ) )
  in
  match lines with
  | algorithm :: _ :: image ->
      { algorithm= parse_algorithm algorithm
      ; image= parse_image image
      ; default= false }
  | _ ->
      failwith "unsupported input"

let t = In_channel.(input_lines stdin) |> parse

let neighbors (x, y) =
  [ (x - 1, y - 1)
  ; (x, y - 1)
  ; (x + 1, y - 1)
  ; (x - 1, y)
  ; (x, y)
  ; (x + 1, y)
  ; (x - 1, y + 1)
  ; (x, y + 1)
  ; (x + 1, y + 1) ]

let paint alg img default pt =
  let ns = neighbors pt in
  let pxs =
    List.map ns ~f:(fun pt -> Image.find img pt |> Option.value ~default)
  in
  let pos =
    List.fold pxs ~init:0 ~f:(fun acc bit ->
        let acc = Int.shift_left acc 1 in
        if bit then Int.bit_or acc 1 else acc )
  in
  alg.(pos)

let apply alg img default =
  let (x0, y0), _ = Image.min_elt_exn img in
  let (x1, y1), _ = Image.max_elt_exn img in
  let xmin, ymin, xmax, ymax = (x0 - 1, y0 - 1, x1 + 1, y1 + 1) in
  let xs = List.range ~start:`inclusive ~stop:`inclusive xmin xmax in
  let ys = List.range ~start:`inclusive ~stop:`inclusive ymin ymax in
  List.fold xs ~init:img ~f:(fun acc x ->
      List.fold ys ~init:acc ~f:(fun acc y ->
          let px = paint alg img default (x, y) in
          Image.update acc (x, y) ~f:(fun _ -> px) ) )

let modify alg default = if default then alg.(511) else alg.(0)

let show_image img =
  let (x0, y0), _ = Image.min_elt_exn img in
  let (x1, y1), _ = Image.max_elt_exn img in
  let xs = List.range ~start:`inclusive ~stop:`inclusive x0 x1 in
  let ys = List.range ~start:`inclusive ~stop:`inclusive y0 y1 in
  List.iter ys ~f:(fun y ->
      List.iter xs ~f:(fun x ->
          let px = Image.find_exn img (x, y) in
          let b = if px then '#' else '.' in
          Printf.printf "%c" b ) ;
      Printf.printf "\n" )

let step t =
  let image = apply t.algorithm t.image t.default in
  let new_default = modify t.algorithm t.default in
  {t with image; default= new_default}

let rec run t n = if n = 0 then t else run (step t) (n - 1)

let part1 =
  let {image; _} = run t 2 in
  Image.count image ~f:(fun v -> v)

let part2 =
  let {image; _} = run t 50 in
  (* Image.to_ppm image "test.ppm" ; *)
  Image.count image ~f:(fun v -> v)

let _ = Printf.printf "part1=%d;part2=%d" part1 part2
