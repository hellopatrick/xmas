let[@inline] sum lst = List.fold_left ( + ) 0 lst

let rec choose k l =
  if k = 0 then [ [] ]
  else
    let len = List.length l in
    if len < k then []
    else if k = len then [ l ]
    else
      match l with
      | h :: t ->
          let starting_with_h =
            List.map (fun sublist -> h :: sublist) (choose (pred k) t)
          in
          let not_starting_with_h = choose k t in
          starting_with_h @ not_starting_with_h
      | [] -> assert false

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))
