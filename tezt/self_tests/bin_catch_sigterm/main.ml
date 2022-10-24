let on_sigterm _ = print_endline "received SIGTERM, ignoring"

let () = Sys.set_signal Sys.sigterm (Signal_handle on_sigterm)

let () =
  print_endline "waiting..." ;
  Unix.sleep 3
