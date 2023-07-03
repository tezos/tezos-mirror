module Metrics : sig
  val durations : string -> Prometheus.Summary.t
end

val sql : string -> (unit -> 'a Lwt.t) -> 'a Lwt.t
