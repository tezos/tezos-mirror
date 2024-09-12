(*
   https://github.com/open-telemetry/oteps/blob/main/text/0035-opentelemetry-protocol.md
   https://github.com/open-telemetry/oteps/blob/main/text/0099-otlp-http.md
 *)

module OT = Opentelemetry
module Config = Config
open Opentelemetry
include Common_

let needs_gc_metrics = Atomic.make false

let last_gc_metrics = Atomic.make (Mtime_clock.now ())

let timeout_gc_metrics = Mtime.Span.(20 * s)

let gc_metrics = ref []
(* side channel for GC, appended to {!E_metrics}'s data *)

(* capture current GC metrics if {!needs_gc_metrics} is true,
   or it has been a long time since the last GC metrics collection,
   and push them into {!gc_metrics} for later collection *)
let sample_gc_metrics_if_needed () =
  let now = Mtime_clock.now () in
  let alarm = Atomic.compare_and_set needs_gc_metrics true false in
  let timeout () =
    let elapsed = Mtime.span now (Atomic.get last_gc_metrics) in
    Mtime.Span.compare elapsed timeout_gc_metrics > 0
  in
  if alarm || timeout () then (
    Atomic.set last_gc_metrics now;
    let l =
      OT.Metrics.make_resource_metrics
        ~attrs:(Opentelemetry.GC_metrics.get_runtime_attributes ())
      @@ Opentelemetry.GC_metrics.get_metrics ()
    in
    gc_metrics := l :: !gc_metrics
  )

type error =
  [ `Status of int * Opentelemetry.Proto.Status.status
  | `Failure of string
  | `Sysbreak
  ]

let n_errors = Atomic.make 0

let n_dropped = Atomic.make 0

let report_err_ = function
  | `Sysbreak -> Printf.eprintf "opentelemetry: ctrl-c captured, stopping\n%!"
  | `Failure msg ->
    Format.eprintf "@[<2>opentelemetry: export failed: %s@]@." msg
  | `Status (code, { Opentelemetry.Proto.Status.code = scode; message; details })
    ->
    let pp_details out l =
      List.iter
        (fun s -> Format.fprintf out "%S;@ " (Bytes.unsafe_to_string s))
        l
    in
    Format.eprintf
      "@[<2>opentelemetry: export failed with@ http code=%d@ status \
       {@[code=%ld;@ message=%S;@ details=[@[%a@]]@]}@]@."
      code scode
      (Bytes.unsafe_to_string message)
      pp_details details

module Httpc : sig
  type t

  val create : unit -> t

  val send :
    t ->
    url:string ->
    decode:[ `Dec of Pbrt.Decoder.t -> 'a | `Ret of 'a ] ->
    string ->
    ('a, error) result Lwt.t

  val cleanup : t -> unit
end = struct
  open Opentelemetry.Proto
  open Lwt.Syntax
  module Httpc = Cohttp_lwt_unix.Client

  type t = unit

  let create () : t = ()

  let cleanup _self = ()

  (* send the content to the remote endpoint/path *)
  let send (_self : t) ~url ~decode (bod : string) : ('a, error) result Lwt.t =
    let uri = Uri.of_string url in

    let open Cohttp in
    let headers = Header.(add_list (init ()) !headers) in
    let headers =
      Header.(add headers "Content-Type" "application/x-protobuf")
    in

    let body = Cohttp_lwt.Body.of_string bod in

    let* r =
      try%lwt
        let+ r = Httpc.post ~headers ~body uri in
        Ok r
      with e -> Lwt.return @@ Error e
    in
    match r with
    | Error e ->
      let err =
        `Failure
          (spf "sending signals via http POST to %S\nfailed with:\n%s" url
             (Printexc.to_string e))
      in
      Lwt.return @@ Error err
    | Ok (resp, body) ->
      let* body = Cohttp_lwt.Body.to_string body in
      let code = Response.status resp |> Code.code_of_status in
      if not (Code.is_error code) then (
        match decode with
        | `Ret x -> Lwt.return @@ Ok x
        | `Dec f ->
          let dec = Pbrt.Decoder.of_string body in
          let r =
            try Ok (f dec)
            with e ->
              let bt = Printexc.get_backtrace () in
              Error
                (`Failure
                  (spf "decoding failed with:\n%s\n%s" (Printexc.to_string e) bt))
          in
          Lwt.return r
      ) else (
        let dec = Pbrt.Decoder.of_string body in

        let r =
          try
            let status = Status.decode_pb_status dec in
            Error (`Status (code, status))
          with e ->
            let bt = Printexc.get_backtrace () in
            Error
              (`Failure
                (spf
                   "httpc: decoding of status (url=%S, code=%d) failed with:\n\
                    %s\n\
                    status: %S\n\
                    %s"
                   url code (Printexc.to_string e) body bt))
        in
        Lwt.return r
      )
end

(** Batch of resources to be pushed later.

    This type is thread-safe. *)
module Batch : sig
  type 'a t

  val push' : 'a t -> 'a -> unit

  val pop_if_ready : ?force:bool -> now:Mtime.t -> 'a t -> 'a list option
  (** Is the batch ready to be emitted? If batching is disabled,
      this is true as soon as {!is_empty} is false. If a timeout is provided
      for this batch, then it will be ready if an element has been in it
      for at least the timeout.
      @param now passed to implement timeout *)

  val make : ?batch:int -> ?timeout:Mtime.span -> unit -> 'a t
  (** Create a new batch *)
end = struct
  type 'a t = {
    mutable size: int;
    mutable q: 'a list;
    batch: int option;
    high_watermark: int;
    timeout: Mtime.span option;
    mutable start: Mtime.t;
  }

  let make ?batch ?timeout () : _ t =
    Option.iter (fun b -> assert (b > 0)) batch;
    let high_watermark = Option.fold ~none:100 ~some:(fun x -> x * 10) batch in
    {
      size = 0;
      start = Mtime_clock.now ();
      q = [];
      batch;
      timeout;
      high_watermark;
    }

  let timeout_expired_ ~now self : bool =
    match self.timeout with
    | Some t ->
      let elapsed = Mtime.span now self.start in
      Mtime.Span.compare elapsed t >= 0
    | None -> false

  let is_full_ self : bool =
    match self.batch with
    | None -> self.size > 0
    | Some b -> self.size >= b

  let pop_if_ready ?(force = false) ~now (self : _ t) : _ list option =
    if self.size > 0 && (force || is_full_ self || timeout_expired_ ~now self)
    then (
      let l = self.q in
      self.q <- [];
      self.size <- 0;
      assert (l <> []);
      Some l
    ) else
      None

  let push (self : _ t) x : bool =
    if self.size >= self.high_watermark then (
      (* drop this to prevent queue from growing too fast *)
      Atomic.incr n_dropped;
      true
    ) else (
      if self.size = 0 && Option.is_some self.timeout then
        (* current batch starts now *)
        self.start <- Mtime_clock.now ();

      (* add to queue *)
      self.size <- 1 + self.size;
      self.q <- x :: self.q;
      let ready = is_full_ self in
      ready
    )

  let push' self x = ignore (push self x : bool)
end

(** An emitter. This is used by {!Backend} below to forward traces/metrics/…
    from the program to whatever collector client we have. *)
module type EMITTER = sig
  open Opentelemetry.Proto

  val push_trace : Trace.resource_spans list -> unit

  val push_metrics : Metrics.resource_metrics list -> unit

  val push_logs : Logs.resource_logs list -> unit

  val set_on_tick_callbacks : (unit -> unit) AList.t -> unit

  val tick : unit -> unit

  val cleanup : unit -> unit
end

(* make an emitter.

   exceptions inside should be caught, see
   https://opentelemetry.io/docs/reference/specification/error-handling/ *)
let mk_emitter ~stop ~(config : Config.t) () : (module EMITTER) =
  let open Proto in
  let open Lwt.Syntax in
  (* local helpers *)
  let open struct
    let timeout =
      if config.batch_timeout_ms > 0 then
        Some Mtime.Span.(config.batch_timeout_ms * ms)
      else
        None

    let batch_traces : Trace.resource_spans list Batch.t =
      Batch.make ?batch:config.batch_traces ?timeout ()

    let batch_metrics : Metrics.resource_metrics list Batch.t =
      Batch.make ?batch:config.batch_metrics ?timeout ()

    let batch_logs : Logs.resource_logs list Batch.t =
      Batch.make ?batch:config.batch_logs ?timeout ()

    let on_tick_cbs_ = Atomic.make (AList.make ())

    let set_on_tick_callbacks = Atomic.set on_tick_cbs_

    let send_http_ (httpc : Httpc.t) encoder ~url ~encode x : unit Lwt.t =
      Pbrt.Encoder.reset encoder;
      encode x encoder;
      let data = Pbrt.Encoder.to_string encoder in
      let* r = Httpc.send httpc ~url ~decode:(`Ret ()) data in
      match r with
      | Ok () -> Lwt.return ()
      | Error `Sysbreak ->
        Printf.eprintf "ctrl-c captured, stopping\n%!";
        Atomic.set stop true;
        Lwt.return ()
      | Error err ->
        (* TODO: log error _via_ otel? *)
        Atomic.incr n_errors;
        report_err_ err;
        (* avoid crazy error loop *)
        Lwt_unix.sleep 3.

    let send_metrics_http curl encoder (l : Metrics.resource_metrics list list)
        =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Metrics_service.default_export_metrics_service_request
          ~resource_metrics:l ()
      in
      let url = config.Config.url_metrics in
      send_http_ curl encoder ~url
        ~encode:Metrics_service.encode_pb_export_metrics_service_request x

    let send_traces_http curl encoder (l : Trace.resource_spans list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Trace_service.default_export_trace_service_request ~resource_spans:l ()
      in
      let url = config.Config.url_traces in
      send_http_ curl encoder ~url
        ~encode:Trace_service.encode_pb_export_trace_service_request x

    let send_logs_http curl encoder (l : Logs.resource_logs list list) =
      let l = List.fold_left (fun acc l -> List.rev_append l acc) [] l in
      let x =
        Logs_service.default_export_logs_service_request ~resource_logs:l ()
      in
      let url = config.Config.url_logs in
      send_http_ curl encoder ~url
        ~encode:Logs_service.encode_pb_export_logs_service_request x

    (* emit metrics, if the batch is full or timeout lapsed *)
    let emit_metrics_maybe ~now ?force httpc encoder : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_metrics with
      | None -> Lwt.return false
      | Some l ->
        let batch = !gc_metrics :: l in
        gc_metrics := [];
        let+ () = send_metrics_http httpc encoder batch in
        true

    let emit_traces_maybe ~now ?force httpc encoder : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_traces with
      | None -> Lwt.return false
      | Some l ->
        let+ () = send_traces_http httpc encoder l in
        true

    let emit_logs_maybe ~now ?force httpc encoder : bool Lwt.t =
      match Batch.pop_if_ready ?force ~now batch_logs with
      | None -> Lwt.return false
      | Some l ->
        let+ () = send_logs_http httpc encoder l in
        true

    let[@inline] guard_exn_ where f =
      try f ()
      with e ->
        let bt = Printexc.get_backtrace () in
        Printf.eprintf
          "opentelemetry-curl: uncaught exception in %s: %s\n%s\n%!" where
          (Printexc.to_string e) bt

    let emit_all_force (httpc : Httpc.t) encoder : unit Lwt.t =
      let now = Mtime_clock.now () in
      let+ (_ : bool) = emit_traces_maybe ~now ~force:true httpc encoder
      and+ (_ : bool) = emit_logs_maybe ~now ~force:true httpc encoder
      and+ (_ : bool) = emit_metrics_maybe ~now ~force:true httpc encoder in
      ()

    let tick_common_ () =
      if !debug_ then Printf.eprintf "tick (from %d)\n%!" (tid ());
      sample_gc_metrics_if_needed ();
      List.iter
        (fun f ->
          try f ()
          with e ->
            Printf.eprintf "on tick callback raised: %s\n"
              (Printexc.to_string e))
        (AList.get @@ Atomic.get on_tick_cbs_);
      ()

    (* thread that calls [tick()] regularly, to help enforce timeouts *)
    let setup_ticker_thread ~tick ~finally () =
      let rec tick_thread () =
        if Atomic.get stop then (
          finally ();
          Lwt.return ()
        ) else
          let* () = Lwt_unix.sleep 0.5 in
          let* () = tick () in
          tick_thread ()
      in
      Lwt.async tick_thread
  end in
  let httpc = Httpc.create () in
  let encoder = Pbrt.Encoder.create () in

  let module M = struct
    (* we make sure that this is thread-safe, even though we don't have a
       background thread. There can still be a ticker thread, and there
       can also be several user threads that produce spans and call
       the emit functions. *)

    let push_trace e =
      let@ () = guard_exn_ "push trace" in
      Batch.push' batch_traces e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_traces_maybe ~now httpc encoder in
          ())

    let push_metrics e =
      let@ () = guard_exn_ "push metrics" in
      sample_gc_metrics_if_needed ();
      Batch.push' batch_metrics e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_metrics_maybe ~now httpc encoder in
          ())

    let push_logs e =
      let@ () = guard_exn_ "push logs" in
      Batch.push' batch_logs e;
      let now = Mtime_clock.now () in
      Lwt.async (fun () ->
          let+ (_ : bool) = emit_logs_maybe ~now httpc encoder in
          ())

    let set_on_tick_callbacks = set_on_tick_callbacks

    let tick_ () =
      tick_common_ ();
      sample_gc_metrics_if_needed ();
      let now = Mtime_clock.now () in
      let+ (_ : bool) = emit_traces_maybe ~now httpc encoder
      and+ (_ : bool) = emit_logs_maybe ~now httpc encoder
      and+ (_ : bool) = emit_metrics_maybe ~now httpc encoder in
      ()

    let () = setup_ticker_thread ~tick:tick_ ~finally:ignore ()

    (* if called in a blocking context: work in the background *)
    let tick () = Lwt.async tick_

    let cleanup () =
      if !debug_ then Printf.eprintf "opentelemetry: exiting…\n%!";
      Lwt.async (fun () ->
          let* () = emit_all_force httpc encoder in
          Httpc.cleanup httpc;
          Lwt.return ())
  end in
  (module M)

module Backend
    (Arg : sig
      val stop : bool Atomic.t

      val config : Config.t
    end)
    () : Opentelemetry.Collector.BACKEND = struct
  include (val mk_emitter ~stop:Arg.stop ~config:Arg.config ())

  open Opentelemetry.Proto
  open Opentelemetry.Collector

  let send_trace : Trace.resource_spans list sender =
    {
      send =
        (fun l ~ret ->
          (if !debug_ then
             let@ () = Lock.with_lock in
             Format.eprintf "send spans %a@."
               (Format.pp_print_list Trace.pp_resource_spans)
               l);
          push_trace l;
          ret ());
    }

  let last_sent_metrics = Atomic.make (Mtime_clock.now ())

  let timeout_sent_metrics = Mtime.Span.(5 * s)
  (* send metrics from time to time *)

  let signal_emit_gc_metrics () =
    if !debug_ then
      Printf.eprintf "opentelemetry: emit GC metrics requested\n%!";
    Atomic.set needs_gc_metrics true

  let additional_metrics () : Metrics.resource_metrics list =
    (* add exporter metrics to the lot? *)
    let last_emit = Atomic.get last_sent_metrics in
    let now = Mtime_clock.now () in
    let add_own_metrics =
      let elapsed = Mtime.span last_emit now in
      Mtime.Span.compare elapsed timeout_sent_metrics > 0
    in

    (* there is a possible race condition here, as several threads might update
       metrics at the same time. But that's harmless. *)
    if add_own_metrics then (
      Atomic.set last_sent_metrics now;
      let open OT.Metrics in
      [
        make_resource_metrics
          [
            sum ~name:"otel.export.dropped" ~is_monotonic:true
              [
                int
                  ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                  ~now:(Mtime.to_uint64_ns now) (Atomic.get n_dropped);
              ];
            sum ~name:"otel.export.errors" ~is_monotonic:true
              [
                int
                  ~start_time_unix_nano:(Mtime.to_uint64_ns last_emit)
                  ~now:(Mtime.to_uint64_ns now) (Atomic.get n_errors);
              ];
          ];
      ]
    ) else
      []

  let send_metrics : Metrics.resource_metrics list sender =
    {
      send =
        (fun m ~ret ->
          (if !debug_ then
             let@ () = Lock.with_lock in
             Format.eprintf "send metrics %a@."
               (Format.pp_print_list Metrics.pp_resource_metrics)
               m);

          let m = List.rev_append (additional_metrics ()) m in
          push_metrics m;
          ret ());
    }

  let send_logs : Logs.resource_logs list sender =
    {
      send =
        (fun m ~ret ->
          (if !debug_ then
             let@ () = Lock.with_lock in
             Format.eprintf "send logs %a@."
               (Format.pp_print_list Logs.pp_resource_logs)
               m);

          push_logs m;
          ret ());
    }
end

let create_backend ?(stop = Atomic.make false) ?(config = Config.make ()) () =
  debug_ := config.debug;

  let module B =
    Backend
      (struct
        let stop = stop

        let config = config
      end)
      ()
  in
  (module B : OT.Collector.BACKEND)

let setup_ ?stop ?config () =
  let backend = create_backend ?stop ?config () in
  let (module B : OT.Collector.BACKEND) = backend in
  OT.Collector.set_backend backend;
  B.cleanup

let setup ?stop ?config ?(enable = true) () =
  if enable then (
    let cleanup = setup_ ?stop ?config () in
    at_exit cleanup
  )

let with_setup ?stop ?(config = Config.make ()) ?(enable = true) () f =
  if enable then (
    let cleanup = setup_ ?stop ~config () in
    Fun.protect ~finally:cleanup f
  ) else
    f ()
