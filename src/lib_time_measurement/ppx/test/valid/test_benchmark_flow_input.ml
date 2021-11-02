let _ =
  (Lwt.return 1 [@time.duration_lwt step1]) >>= fun x1 ->
  (Lwt.return 2 [@time.duration_lwt step2]) >>= fun x2 ->
  (Lwt.return 3 [@time.duration_lwt step3]) >>= fun x3 ->
  Lwt.return (4 [@time.duration step4]) >>= fun x4 ->
  Lwt.return (5 [@time.duration step5]) >>= fun x5 ->
  (Lwt.return (x1 + x2 + x3 + x4 + x5) [@time.duration step6] [@time.flush])
