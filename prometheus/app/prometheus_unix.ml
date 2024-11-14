open Prometheus

module Metrics = struct
  let namespace = "prometheus"

  let subsystem = "logs"

  let inc_messages =
    let help = "Total number of messages logged" in
    let c =
      Counter.v_labels
        ~label_names:["level"; "src"]
        ~help
        ~namespace
        ~subsystem
        "messages_total"
    in
    fun lvl src ->
      let lvl = Logs.level_to_string (Some lvl) in
      Counter.inc_one @@ Counter.labels c [lvl; src]
end

module Unix_runtime = struct
  let start_time = Unix.gettimeofday ()

  let simple_metric ~metric_type ~help name fn =
    let info =
      {MetricInfo.name = MetricName.v name; help; metric_type; label_names = []}
    in
    let collect () = LabelSetMap.singleton [] [Sample_set.sample (fn ())] in
    (info, collect)

  let process_start_time_seconds =
    simple_metric
      ~metric_type:Counter
      "process_start_time_seconds"
      (fun () -> start_time)
      ~help:"Start time of the process since unix epoch in seconds."

  let metrics = [process_start_time_seconds]
end

type config = int option

module Server = Prometheus_app.Cohttp (Cohttp_lwt_unix.Server)

let serve = function
  | None -> []
  | Some port ->
      let mode = `TCP (`Port port) in
      let callback = Server.callback in
      let thread =
        Cohttp_lwt_unix.Server.create
          ~mode
          (Cohttp_lwt_unix.Server.make ~callback ())
      in
      [thread]

let listen_prometheus =
  let open! Cmdliner in
  let doc =
    Arg.info
      ~docs:"MONITORING OPTIONS"
      ~docv:"PORT"
      ~doc:"Port on which to provide Prometheus metrics over HTTP."
      ["listen-prometheus"]
  in
  Arg.(value @@ opt (some int) None doc)

let opts = listen_prometheus

let () =
  let add (info, collector) =
    CollectorRegistry.(register default) info collector
  in
  List.iter add Unix_runtime.metrics

module Logging = struct
  let inc_counter = Metrics.inc_messages

  let pp_timestamp f x =
    let open Unix in
    let tm = localtime x in
    Fmt.pf
      f
      "%04d-%02d-%02d %02d:%02d.%02d"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      tm.tm_mday
      tm.tm_hour
      tm.tm_min
      tm.tm_sec

  let reporter formatter =
    let report src level ~over k msgf =
      let k _ =
        over () ;
        k ()
      in
      let src = Logs.Src.name src in
      Metrics.inc_messages level src ;
      msgf @@ fun ?header ?tags:_ fmt ->
      Fmt.kpf
        k
        formatter
        ("%a %a %a @[" ^^ fmt ^^ "@]@.")
        pp_timestamp
        (Unix.gettimeofday ())
        Fmt.(styled `Magenta string)
        (Printf.sprintf "%14s" src)
        Logs_fmt.pp_header
        (level, header)
    in
    {Logs.report}

  let set_level (src, level) =
    let rec aux = function
      | [] ->
          Logs.warn (fun f ->
              f "set_level: logger %S not registered; ignoring" src)
      | x :: _ when Logs.Src.name x = src -> Logs.Src.set_level x (Some level)
      | _ :: xs -> aux xs
    in
    aux (Logs.Src.list ())

  let init ?(default_level = Logs.Info) ?(levels = []) ?(formatter = Fmt.stderr)
      () =
    Fmt_tty.setup_std_outputs () ;
    Logs.set_reporter (reporter formatter) ;
    Logs.set_level (Some default_level) ;
    List.iter set_level levels
end
