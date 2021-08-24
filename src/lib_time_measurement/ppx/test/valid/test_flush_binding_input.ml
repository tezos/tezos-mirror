let _ =
  Lwt.return 1 >>= fun x1 ->
  Lwt.return 2 >>= fun x2 ->
  (Lwt.return 3 [@time.flush]) >>= fun x3 ->
  Lwt.return 4 >>= fun x4 ->
  Lwt.return 5 >>= fun x5 -> Lwt.return (x1 + x2 + x3 + x4 + x5)
