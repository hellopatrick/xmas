let ( |? ) maybe default =
  match maybe with Some v -> v | None -> Lazy.force default
