module T = Opentelemetry_lwt
module Atomic = Opentelemetry_atomic.Atomic
open Lwt.Syntax

let spf = Printf.sprintf

let ( let@ ) f x = f x

let sleep_inner = ref 0.1

let sleep_outer = ref 2.0

let n_jobs = ref 1

let num_sleep = Atomic.make 0

let stress_alloc_ = ref true

let stop = Atomic.make false

let num_tr = Atomic.make 0

let run_job () : unit Lwt.t =
  let i = ref 0 in
  while%lwt not @@ Atomic.get stop do
    let@ scope =
      Atomic.incr num_tr;
      T.Trace.with_ ~kind:T.Span.Span_kind_producer "loop.outer"
        ~attrs:[ "i", `Int !i ]
    in

    for%lwt j = 0 to 4 do
      (* parent scope is found via thread local storage *)
      let@ scope =
        Atomic.incr num_tr;
        T.Trace.with_ ~scope ~kind:T.Span.Span_kind_internal
          ~attrs:[ "j", `Int j ]
          "loop.inner"
      in

      let* () = Lwt_unix.sleep !sleep_outer in
      Atomic.incr num_sleep;

      T.Logs.(
        emit
          [
            make_strf ~trace_id:scope.trace_id ~span_id:scope.span_id
              ~severity:Severity_number_info "inner at %d" j;
          ]);

      incr i;

      try%lwt
        Atomic.incr num_tr;
        let@ scope =
          T.Trace.with_ ~kind:T.Span.Span_kind_internal ~scope "alloc"
        in
        (* allocate some stuff *)
        if !stress_alloc_ then (
          let _arr = Sys.opaque_identity @@ Array.make (25 * 25551) 42.0 in
          ignore _arr
        );

        let* () = Lwt_unix.sleep !sleep_inner in
        Atomic.incr num_sleep;

        if j = 4 && !i mod 13 = 0 then failwith "oh no";

        (* simulate a failure *)
        T.Trace.add_event scope (fun () -> T.Event.make "done with alloc");
        Lwt.return ()
      with Failure _ -> Lwt.return ()
    done
  done

let run () : unit Lwt.t =
  T.GC_metrics.basic_setup ();

  T.Metrics_callbacks.register (fun () ->
      T.Metrics.
        [
          sum ~name:"num-sleep" ~is_monotonic:true
            [ int (Atomic.get num_sleep) ];
        ]);

  let n_jobs = max 1 !n_jobs in
  Printf.printf "run %d jobs\n%!" n_jobs;

  let jobs = Array.init n_jobs (fun _ -> run_job ()) |> Array.to_list in
  Lwt.join jobs

let () =
  Sys.catch_break true;
  T.Globals.service_name := "t1";
  T.Globals.service_namespace := Some "ocaml-otel.test";
  let ts_start = Unix.gettimeofday () in

  let debug = ref false in
  let batch_traces = ref 400 in
  let batch_metrics = ref 3 in
  let opts =
    [
      "--debug", Arg.Bool (( := ) debug), " enable debug output";
      ( "--stress-alloc",
        Arg.Bool (( := ) stress_alloc_),
        " perform heavy allocs in inner loop" );
      "--batch-traces", Arg.Int (( := ) batch_traces), " size of traces batch";
      ( "--batch-metrics",
        Arg.Int (( := ) batch_metrics),
        " size of metrics batch" );
      "--sleep-inner", Arg.Set_float sleep_inner, " sleep (in s) in inner loop";
      "--sleep-outer", Arg.Set_float sleep_outer, " sleep (in s) in outer loop";
      "-j", Arg.Set_int n_jobs, " number of parallel jobs";
    ]
    |> Arg.align
  in

  Arg.parse opts (fun _ -> ()) "emit1 [opt]*";

  let some_if_nzero r =
    if !r > 0 then
      Some !r
    else
      None
  in
  let config =
    Opentelemetry_client_cohttp_lwt.Config.make ~debug:!debug
      ~batch_traces:(some_if_nzero batch_traces)
      ~batch_metrics:(some_if_nzero batch_metrics)
      ()
  in
  Format.printf "@[<2>sleep outer: %.3fs,@ sleep inner: %.3fs,@ config: %a@]@."
    !sleep_outer !sleep_inner Opentelemetry_client_cohttp_lwt.Config.pp config;

  let@ () =
    Fun.protect ~finally:(fun () ->
        let elapsed = Unix.gettimeofday () -. ts_start in
        let n_per_sec = float (Atomic.get num_tr) /. elapsed in
        Printf.printf "\ndone. %d spans in %.4fs (%.4f/s)\n%!"
          (Atomic.get num_tr) elapsed n_per_sec)
  in
  Opentelemetry_client_cohttp_lwt.with_setup ~stop ~config () @@ fun () ->
  Lwt_main.run @@ run ()
