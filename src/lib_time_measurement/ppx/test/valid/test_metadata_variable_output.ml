let f x y z = (x + y) + z
let l = ["1"; "2"; "3"]
let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.timestamp_pre
    ("fun_app", l) (fun () -> f 1 2 3)
