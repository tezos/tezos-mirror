let _ =
  let open Lwt.Syntax in
  let* x1 = (Lwt.return 1 [@time.duration_lwt step1]) in
  let* x2 = (Lwt.return 2 [@time.duration_lwt step2]) in
  let* x3 = (Lwt.return 3 [@time.duration_lwt step3]) in
  let* x4 = Lwt.return (4 [@time.duration step4]) in
  let* x5 = Lwt.return (5 [@time.duration step5]) in
  (Lwt.return (x1 + x2 + x3 + x4 + x5) [@time.duration step6] [@time.flush])
