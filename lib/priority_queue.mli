type priority = int
type 'a t

val empty : 'a t
val contains : 'a t -> 'a -> bool
val insert : 'a t -> priority -> 'a -> 'a t
val extract : 'a t -> (priority * 'a * 'a t) option
