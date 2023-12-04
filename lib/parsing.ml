open Angstrom

let is_eol = function '\n' | '\r' -> true | _ -> false
let is_whitespace = function ' ' | '\t' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let number = take_while1 is_digit >>| int_of_string
let whitespace = take_while is_whitespace
let ws = whitespace
let chomp_eol = take_till is_eol <* end_of_line
