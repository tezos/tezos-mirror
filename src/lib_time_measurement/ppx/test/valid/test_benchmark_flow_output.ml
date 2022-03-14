let _ =
  let open Lwt.Syntax in
  let* x1 =
    Tezos_time_measurement_runtime.Default.Time_measurement.duration_lwt
      ("step1", [])
      (fun () -> Lwt.return 1)
  in
  let* x2 =
    Tezos_time_measurement_runtime.Default.Time_measurement.duration_lwt
      ("step2", [])
      (fun () -> Lwt.return 2)
  in
  let* x3 =
    Tezos_time_measurement_runtime.Default.Time_measurement.duration_lwt
      ("step3", [])
      (fun () -> Lwt.return 3)
  in
  let* x4 =
    Lwt.return
      (Tezos_time_measurement_runtime.Default.Time_measurement.duration
         ("step4", [])
         (fun () -> 4))
  in
  let* x5 =
    Lwt.return
      (Tezos_time_measurement_runtime.Default.Time_measurement.duration
         ("step5", [])
         (fun () -> 5))
  in
  Lwt.bind
    (Tezos_time_measurement_runtime.Default.Time_measurement.duration
       ("step6", [])
       (fun () -> Lwt.return (x1 + x2 + x3 + x4 + x5)))
    (fun __flush__id__0 ->
      Lwt.map
        (fun () -> __flush__id__0)
        (Tezos_time_measurement_runtime.Default.Time_measurement.flush ()))
