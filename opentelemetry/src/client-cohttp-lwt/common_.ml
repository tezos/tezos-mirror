module Atomic = Opentelemetry_atomic.Atomic

let[@inline] ( let@ ) f x = f x

let spf = Printf.sprintf

let tid () = Thread.id @@ Thread.self ()

let debug_ =
  ref
    (match Sys.getenv_opt "OTEL_OCAML_DEBUG" with
    | Some ("1" | "true") -> true
    | _ -> false)

let default_url = "http://localhost:4318"

let make_get_from_env env_name =
  let value = ref None in
  fun () ->
    match !value with
    | None ->
      value := Sys.getenv_opt env_name;
      !value
    | Some value -> Some value

let get_url_from_env = make_get_from_env "OTEL_EXPORTER_OTLP_ENDPOINT"

let get_url_traces_from_env =
  make_get_from_env "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"

let get_url_metrics_from_env =
  make_get_from_env "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT"

let get_url_logs_from_env = make_get_from_env "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT"

let remove_trailing_slash url =
  if url <> "" && String.get url (String.length url - 1) = '/' then
    String.sub url 0 (String.length url - 1)
  else
    url

let parse_headers s =
  let parse_header s =
    match String.split_on_char '=' s with
    | [ key; value ] -> key, value
    | _ -> failwith "Unexpected format for header"
  in
  String.split_on_char ',' s |> List.map parse_header

let default_headers = []

let headers =
  ref
    (try parse_headers (Sys.getenv "OTEL_EXPORTER_OTLP_HEADERS")
     with _ -> default_headers)

let get_headers () = !headers

let set_headers s = headers := s
