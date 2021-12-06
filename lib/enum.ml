open Base

let rec window n = function
  | [] ->
      []
  | _ :: tl as l ->
      if List.length l >= n then List.take l n :: window n tl else []


let sum = List.fold ~init:0 ~f:Int.( + )
