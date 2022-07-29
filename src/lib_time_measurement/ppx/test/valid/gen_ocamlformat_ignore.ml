let () =
  Utils.test_files "."
  |> List.filter (fun f -> not (Filename.check_suffix f "_input.ml"))
  |> List.iter print_endline
