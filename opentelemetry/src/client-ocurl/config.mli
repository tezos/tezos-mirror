(** Configuration for the ocurl backend *)

type t = private {
  debug: bool;
  url_traces: string;  (** Url to send traces *)
  url_metrics: string;  (** Url to send metrics*)
  url_logs: string;  (** Url to send logs *)
  headers: (string * string) list;
      (** API headers sent to the endpoint. Default is none or
      "OTEL_EXPORTER_OTLP_HEADERS" if set. *)
  batch_timeout_ms: int;
      (** Number of milliseconds after which we will emit a batch, even
      incomplete.
      Note that the batch might take longer than that, because this is
      only checked when a new event occurs or when a tick
      is emitted. Default 2_000. *)
  bg_threads: int;
      (** Are there background threads, and how many? Default [4].
            This will be adjusted to be at least [1] and at most [32]. *)
  ticker_thread: bool;
      (** If true, start a thread that regularly checks if signals should
          be sent to the collector. Default [true] *)
  ticker_interval_ms: int;
      (** Interval for ticker thread, in milliseconds. This is
          only useful if [ticker_thread] is [true].
          This will be clamped between [2 ms] and some longer
          interval (maximum [60s] currently).
      Default 500.
      @since 0.7 *)
  self_trace: bool;
      (** If true, the OTEL library will also emit its own spans. Default [false].
          @since 0.7 *)
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
  ?batch_timeout_ms:int ->
  ?bg_threads:int ->
  ?ticker_thread:bool ->
  ?ticker_interval_ms:int ->
  ?self_trace:bool ->
  unit ->
  t
(** Make a configuration.

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
