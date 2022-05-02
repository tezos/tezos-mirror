let f x y z = (x + y) + z
let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.timestamp_pre
    ("fun_app", ["123"]) (fun () -> f 1 2 3)
