(** Report metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - This module is intended to be used by applications that export Prometheus metrics.
      Libraries should only link against the `Prometheus` module.

    - This module automatically initialises itself and registers some standard collectors relating to
      GC statistics, as recommended by Prometheus.

    - This extends [Prometheus_app] with support for cmdliner option parsing, a server pre-configured
      for Unix, and a start-time metric that uses [Unix.gettimeofday].
 *)

type config

(** [serve config] starts a Cohttp server according to config.
    It returns a singleton list containing the thread to monitor,
    or an empty list if no server is configured. *)
val serve : config -> unit Lwt.t list

(** [opts] is the extra command-line options to offer Prometheus
    monitoring. *)
val opts : config Cmdliner.Term.t

(** Report metrics for messages logged. *)
module Logging : sig
  (** Initialise the Logs library with a reporter that reports prometheus metrics too.
      The reporter is configured to log to stderr and the log messages include a
      timestamp and the event's source.

      A server will typically use the following code to initialise logging:
      {[
      let () = Prometheus_app.Logging.init ()
      ]}

      Or:
      {[
      let () =
        Prometheus_unix.Logging.init ()
          ~default_level:Logs.Debug
          ~levels:[
            "cohttp.lwt.io", Logs.Info;
          ]
      ]}
      @param default_level The default log-level to use (default {!Logs.Info}).
      @param levels Provides levels for specific log sources.
      @param formatter A custom formatter (default {!Fmt.stderr}). *)
  val init :
    ?default_level:Logs.level ->
    ?levels:(string * Logs.level) list ->
    ?formatter:Format.formatter ->
    unit ->
    unit

  (** [inc_counter level src] increments the count of messages logged by [src] at [level].
      The reporter installed by [init] calls this automatically, but you might want to
      use this if you use your own reporter instead. *)
  val inc_counter : Logs.level -> string -> unit
end
