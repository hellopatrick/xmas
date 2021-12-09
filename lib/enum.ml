open Base

let rec window n = function
  | [] ->
      []
  | _ :: tl as l ->
      if List.length l >= n then List.take l n :: window n tl else []


let sum = List.fold ~init:0 ~f:Int.( + )

let all l ~f =
  let rec aux = function
    | hd :: tl ->
        if f hd then aux tl else false
    | _ ->
        true
  in
  aux l
