type t = private {
  debug: bool;
  url_traces: string;  (** Url to send traces *)
  url_metrics: string;  (** Url to send metrics*)
  url_logs: string;  (** Url to send logs *)
  headers: (string * string) list;
      (** API headers sent to the endpoint. Default is none or
      "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
  batch_traces: int option;
      (** Batch traces? If [Some i], then this produces batches of (at most)
      [i] items. If [None], there is no batching.

      Note that traces and metrics are batched separately.
      Default [Some 400].
  *)
  batch_metrics: int option;
      (** Batch metrics? If [Some i], then this produces batches of (at most)
      [i] items. If [None], there is no batching.

      Note that traces and metrics are batched separately.
      Default [None].
  *)
  batch_logs: int option;
      (** Batch logs? See {!batch_metrics} for details.
      Default [Some 400] *)
  batch_timeout_ms: int;
      (** Number of milliseconds after which we will emit a batch, even
      incomplete.
      Note that the batch might take longer than that, because this is
      only checked when a new event occurs. Default 500. *)
}
(** Configuration.

  To build one, use {!make} below. This might be extended with more
  fields in the future. *)

val make :
  ?debug:bool ->
  ?url:string ->
  ?url_traces:string ->
  ?url_metrics:string ->
  ?url_logs:string ->
  ?headers:(string * string) list ->
  ?batch_traces:int option ->
  ?batch_metrics:int option ->
  ?batch_logs:int option ->
  ?batch_timeout_ms:int ->
  unit ->
  t
(** Make a configuration.

   @param thread if true and [bg_threads] is not provided, we will pick a number
   of bg threads. Otherwise the number of [bg_threads] superseeds this option.

   @param url base url used to construct per-signal urls. Per-signal url options take precedence over this base url.
   Default is "http://localhost:4318", or "OTEL_EXPORTER_OTLP_ENDPOINT" if set.
 
   Example of constructed per-signal urls with the base url http://localhost:4318
   - Traces: http://localhost:4318/v1/traces
   - Metrics: http://localhost:4318/v1/metrics
   - Logs: http://localhost:4318/v1/logs
 
   Use per-signal url options if different urls are needed for each signal type.
 
   @param url_traces url to send traces, or "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT" if set.
   The url is used as-is without any modification.
 
   @param url_metrics url to send metrics, or "OTEL_EXPORTER_OTLP_METRICS_ENDPOINT" if set.
   The url is used as-is without any modification.
 
   @param url_logs url to send logs, or "OTEL_EXPORTER_OTLP_LOGS_ENDPOINT" if set.
   The url is used as-is without any modification.

 *)

val pp : Format.formatter -> t -> unit
