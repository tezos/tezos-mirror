let _ =
  let open Lwt.Syntax in
    let* x1 = Lwt.return 1
     in
    let* x2 = Lwt.return 2
     in
    let* x3 =
      Lwt.bind (Lwt.return 3)
        (fun __flush__id__0 ->
           Lwt.map (fun () -> __flush__id__0)
             (Tezos_time_measurement_runtime.Default.Time_measurement.flush
                ()))
     in
    let* x4 = Lwt.return 4
     in let* x5 = Lwt.return 5
         in Lwt.return ((((x1 + x2) + x3) + x4) + x5)
