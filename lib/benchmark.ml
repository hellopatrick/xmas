open Core

let time name f =
  let s = Time_ns.now () in
  let r = f () in
  let e = Time_ns.now () in
  let dt = Time_ns.diff e s in
  Printf.printf name r;
  Printf.printf " [time=%dms]" (Time_ns.Span.to_int_ms dt);
  Printf.printf "\n";
  r
