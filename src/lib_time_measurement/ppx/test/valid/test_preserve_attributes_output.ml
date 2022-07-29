let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.duration
    ("coucou", []) (fun () -> (("coucou")[@attr1 ][@attr2 4 * 2]))
