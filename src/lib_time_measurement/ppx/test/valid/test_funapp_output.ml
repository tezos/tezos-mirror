let f x y z = (x + y) + z
let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.duration
    ("fun_app", []) (fun () -> f 1 2 3)
