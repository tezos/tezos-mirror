let _ =
  let x = Some 1 in
  match[@time.duration pattern_matching] x with Some a -> a | None -> 2
