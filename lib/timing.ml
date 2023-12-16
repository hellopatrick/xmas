let watch f =
  let t = Sys.time () in
  let res = f () in
  Printf.printf "Execution time: %fs\n" (Sys.time () -. t);
  res
