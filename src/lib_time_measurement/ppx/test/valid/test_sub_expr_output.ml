let _ =
  Tezos_time_measurement_runtime.Default.Time_measurement.duration
    ("expr", [])
    (fun () ->
      let x =
        Tezos_time_measurement_runtime.Default.Time_measurement.duration
          ("sub_expr", [])
          (fun () -> "coucou")
      in
      x)
