let make_progress_printer total message =
  let counter = ref 1 in
  fun () ->
    Printf.eprintf "\r%s %d/%d%!" message !counter total ;
    incr counter
