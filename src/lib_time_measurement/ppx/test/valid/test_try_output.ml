let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.duration
    ("try_with", []) (fun () -> try 1 with | _ -> 2)
