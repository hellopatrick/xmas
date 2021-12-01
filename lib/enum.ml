open Base

let rec window n l =
  match l with
  | [] ->
      []
  | _ :: tl ->
      if List.length l >= n then List.take l n :: window n tl else []
