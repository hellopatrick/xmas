open Base

let rec window n = function
  | [] ->
      []
  | _ :: tl as l ->
      if List.length l >= n then List.take l n :: window n tl else []
