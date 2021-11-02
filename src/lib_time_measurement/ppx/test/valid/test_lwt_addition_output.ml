let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.duration_lwt
    ("addition", [])
    (fun () -> Lwt.return (1 + 3))
