type priority = int

type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

let empty = Empty

let rec insert queue prio elt =
  match queue with
  | Empty ->
      Node (prio, elt, Empty, Empty)
  | Node (p, e, left, right) ->
      if prio <= p then Node (prio, elt, insert right p e, left)
      else Node (p, e, insert right prio elt, left)

let rec contains queue elt =
  match queue with
  | Empty ->
      false
  | Node (_, e, left, right) ->
      if e == elt then true else contains left elt || contains right elt

let rec remove_top = function
  | Empty ->
      Empty
  | Node (_, _, left, Empty) ->
      left
  | Node (_, _, Empty, right) ->
      right
  | Node
      ( _
      , _
      , (Node (lprio, lelt, _, _) as left)
      , (Node (rprio, relt, _, _) as right) ) ->
      if lprio <= rprio then Node (lprio, lelt, remove_top left, right)
      else Node (rprio, relt, left, remove_top right)

let extract = function
  | Empty ->
      None
  | Node (prio, elt, _, _) as queue ->
      Some (prio, elt, remove_top queue)
