open Opentelemetry_client_ocurl

let test_urls ~name config =
  Printf.printf "--- %s ---\n" name;
  Printf.printf "url_traces = %s\n" config.Config.url_traces;
  Printf.printf "url_metrics = %s\n" config.Config.url_metrics;
  Printf.printf "url_logs = %s\n" config.Config.url_logs;
  print_endline "------\n"

let default_url () =
  let config = Config.make () in
  test_urls ~name:"default_url" config

let base_url_from_config () =
  let config = Config.make ~url:"http://localhost:3000" () in
  test_urls ~name:"base_url_from_config" config

let base_url_from_env () =
  Unix.putenv "OTEL_EXPORTER_OTLP_ENDPOINT" "http://localhost:5000";
  let config = Config.make () in
  test_urls ~name:"base_url_from_env" config

let base_url_from_both_config_and_env () =
  (* url from env should take precedence *)
  Unix.putenv "OTEL_EXPORTER_OTLP_ENDPOINT" "http://localhost:5000";
  let config = Config.make ~url:"http://localhost:3000" () in
  test_urls ~name:"base_url_from_both_config_and_env" config

let override_trace_url_from_config () =
  let config =
    Config.make ~url:"http://localhost:3000"
      ~url_traces:"http://localhost:3001/send/traces" ()
  in
  test_urls ~name:"override_trace_url_from_config" config

let override_trace_url_from_env () =
  Unix.putenv "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
    "http://localhost:3001/send/traces";
  let config = Config.make () in
  test_urls ~name:"override_trace_url_from_env" config

let override_trace_url_from_both_config_and_env () =
  Unix.putenv "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
    "http://localhost:3001/send/traces/env";
  let config =
    Config.make ~url_traces:"http://localhost:3001/send/traces/config" ()
  in
  test_urls ~name:"override_trace_url_from_both_config_and_env" config

let set_all_in_config () =
  let config =
    Config.make ~url_traces:"http://localhost:3001/send/traces"
      ~url_metrics:"http://localhost:3002/send/metrics"
      ~url_logs:"http://localhost:3003/send/logs" ()
  in
  test_urls ~name:"set_all_in_config" config

let set_all_in_env () =
  Unix.putenv "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
    "http://localhost:3001/send/traces";
  Unix.putenv "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"
    "http://localhost:3002/send/metrics";
  Unix.putenv "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"
    "http://localhost:3003/send/logs";
  let config = Config.make () in
  test_urls ~name:"set_all_in_env" config

let remove_trailing_slash_config () =
  let config = Config.make ~url:"http://localhost:3000/" () in
  test_urls ~name:"remove_trailing_slash_config" config

let remove_trailing_slash_env () =
  Unix.putenv "OTEL_EXPORTER_OTLP_ENDPOINT" "http://localhost:3000/";
  let config = Config.make () in
  test_urls ~name:"remove_trailing_slash_env" config

let () = default_url ()

let () = base_url_from_config ()

let () = base_url_from_env ()

let () = base_url_from_both_config_and_env ()

let () = override_trace_url_from_config ()

let () = override_trace_url_from_env ()

let () = override_trace_url_from_both_config_and_env ()

let () = set_all_in_config ()

let () = set_all_in_env ()

let () = remove_trailing_slash_config ()

let () = remove_trailing_slash_env ()
