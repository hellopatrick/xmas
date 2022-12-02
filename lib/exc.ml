exception Unreachable of string

let unreachable s = raise (Unreachable s)