let _ =
  let open Lwt.Syntax in
  let* x1 =
    Lwt.bind (Lwt.return 1) (fun __flush__id__0 ->
        Lwt.map
          (fun () -> __flush__id__0)
          (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
  in
  let* x2 =
    Lwt.bind (Lwt.return 2) (fun __flush__id__1 ->
        Lwt.map
          (fun () -> __flush__id__1)
          (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
  in
  let* x3 =
    Lwt.bind (Lwt.return 3) (fun __flush__id__2 ->
        Lwt.map
          (fun () -> __flush__id__2)
          (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
  in
  let* x4 =
    Lwt.bind (Lwt.return 4) (fun __flush__id__3 ->
        Lwt.map
          (fun () -> __flush__id__3)
          (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
  in
  let* x5 =
    Lwt.bind (Lwt.return 5) (fun __flush__id__4 ->
        Lwt.map
          (fun () -> __flush__id__4)
          (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
  in
  Lwt.return (x1 + x2 + x3 + x4 + x5)
