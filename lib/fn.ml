let[@inline] compose f g x = g (f x)
let ( % ) = compose
