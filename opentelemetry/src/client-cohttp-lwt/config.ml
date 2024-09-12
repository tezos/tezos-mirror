open Common_

type t = {
  debug: bool;
  url_traces: string;
  url_metrics: string;
  url_logs: string;
  headers: (string * string) list;
  batch_traces: int option;
  batch_metrics: int option;
  batch_logs: int option;
  batch_timeout_ms: int;
}

let pp out self : unit =
  let ppiopt = Format.pp_print_option Format.pp_print_int in
  let pp_header ppf (a, b) = Format.fprintf ppf "@[%s: @,%s@]@." a b in
  let ppheaders = Format.pp_print_list pp_header in
  let {
    debug;
    url_traces;
    url_metrics;
    url_logs;
    headers;
    batch_traces;
    batch_metrics;
    batch_logs;
    batch_timeout_ms;
  } =
    self
  in
  Format.fprintf out
    "{@[ debug=%B;@ url_traces=%S;@ url_metrics=%S;@ url_logs=%S;@ \
     headers=%a;@ batch_traces=%a;@ batch_metrics=%a;@ batch_logs=%a;@ \
     batch_timeout_ms=%d; @]}"
    debug url_traces url_metrics url_logs ppheaders headers ppiopt batch_traces
    ppiopt batch_metrics ppiopt batch_logs batch_timeout_ms

let make ?(debug = !debug_) ?url ?url_traces ?url_metrics ?url_logs
    ?(headers = get_headers ()) ?(batch_traces = Some 400)
    ?(batch_metrics = Some 20) ?(batch_logs = Some 400)
    ?(batch_timeout_ms = 500) () : t =
  let url_traces, url_metrics, url_logs =
    let base_url =
      let base_url =
        match get_url_from_env () with
        | None -> Option.value url ~default:default_url
        | Some url -> remove_trailing_slash url
      in
      remove_trailing_slash base_url
    in
    let url_traces =
      match get_url_traces_from_env () with
      | None -> Option.value url_traces ~default:(base_url ^ "/v1/traces")
      | Some url -> url
    in
    let url_metrics =
      match get_url_metrics_from_env () with
      | None -> Option.value url_metrics ~default:(base_url ^ "/v1/metrics")
      | Some url -> url
    in
    let url_logs =
      match get_url_logs_from_env () with
      | None -> Option.value url_logs ~default:(base_url ^ "/v1/logs")
      | Some url -> url
    in
    url_traces, url_metrics, url_logs
  in
  {
    debug;
    url_traces;
    url_metrics;
    url_logs;
    headers;
    batch_traces;
    batch_metrics;
    batch_timeout_ms;
    batch_logs;
  }
