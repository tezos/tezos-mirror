(** Collect metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with Lwt threads, NOT with native threading.

    - This library is intended to be a dependency of any library that might need to report metrics,
      even though many applications will not enable it. Therefore it should have minimal dependencies.
*)

type metric_type =
  | Counter
  | Gauge
  | Summary
  | Histogram

module type NAME = sig
  type t = private string

  val v : string -> t
  (** Raises an exception if the name is not valid. *)

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end
(** A string that meets some additional requirements. *)

module MetricName : NAME
(** A valid name for a metric. *)

module LabelName  : NAME
(** A valid name for a label. *)

module MetricInfo : sig
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }
end
(** Metadata about a metric. *)

module LabelSetMap : Asetmap.Map.S with type key = string list
(** A map indexed by a set of labels. *)

module MetricFamilyMap : Asetmap.Map.S with type key = MetricInfo.t
(** A map indexed by metric families. *)

module Sample_set : sig
  type sample = {
    ext : string;               (** An extension to append to the base metric name. *)
    value : float;
    bucket : (LabelName.t * float) option;   (** The "le" or "quantile" label and value, if any. *)
  }

  type t = sample list
  (** A collection of values that together represent a single sample.
      For a counter, each reading is just a single value, but more complex types
      require multiple values.
      For example, a "summary" sample set contains "_sum" and "_count" values.
   *)

  val sample : ?ext:string -> ?bucket:(LabelName.t * float) -> float -> sample
end

module CollectorRegistry : sig
  type t
  (** A collection of metrics to be monitored. *)

  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t
  (** The result of reading a set of metrics. *)

  val create : unit -> t
  (** [create ()] is a fresh registry. This is mostly useful for testing. *)

  val default : t
  (** The default registry. *)

  val collect : t -> snapshot Lwt.t
  (** Read the current value of each metric. *)

  val register : t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t) -> unit
  (** [register t metric collector] adds [metric] to the set of metrics being collected.
      It will call [collector ()] to collect the values each time [collect] is called. *)

  val register_lwt : t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t Lwt.t) -> unit
  (** [register_lwt t metric collector] is the same as [register t metrics collector]
      but [collector] returns [Sample_set.t LabelSetMap.t Lwt.t]. *)

  val register_pre_collect : t -> (unit -> unit) -> unit
  (** [register_pre_collect t fn] arranges for [fn ()] to be called at the start
      of each collection. This is useful if one expensive call provides
      information about multiple metrics. *)

  val register_pre_collect_lwt : t -> (unit -> unit Lwt.t) -> unit
  (** [register_pre_collect t fn] same as [register_pre_collect] but [fn] returns [unit Lwt.t]. *)
end
(** A collection of metric reporters. Usually, only {!CollectorRegistry.default} is used. *)

module type METRIC = sig
  type family
  (** A collection of metrics that are the same except for their labels.
      e.g. "Number of HTTP responses" *)

  type t
  (** A particular metric.
      e.g. "Number of HTTP responses with code=404" *)

  val v_labels : label_names:string list -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> family
  (** [v_labels ~label_names ~help ~namespace ~subsystem name] is a family of metrics with full name
      [namespace_subsystem_name] and documentation string [help]. Each metric in the family will provide
      a value for each of the labels.
      The new family is registered with [registry] (default: {!CollectorRegistry.default}). *)

  val labels : family -> string list -> t
  (** [labels family label_values] is the metric in [family] with these values for the labels.
      The order of the values must be the same as the order of the [label_names] passed to [v_labels];
      you may wish to write a wrapper function with labelled arguments to avoid mistakes.
      If this is called multiple times with the same set of values, the existing metric will be returned. *)

  val v_label : label_name:string -> ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> (string -> t)
  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)

  val v : ?registry:CollectorRegistry.t -> help:string -> ?namespace:string -> ?subsystem:string -> string -> t
  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
end
(** Operations common to all types of metric. *)

module Counter : sig
  include METRIC
  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases [t] by [v], which must be non-negative. *)
end
(** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)

module Gauge : sig
  include METRIC

  val inc_one : t -> unit
  val inc : t -> float -> unit
  (** [inc t v] increases the current value of the guage by [v]. *)

  val dec_one : t -> unit
  val dec : t -> float -> unit
  (** [dec t v] decreases the current value of the guage by [v]. *)

  val set : t -> float -> unit
  (** [set t v] sets the current value of the guage to [v]. *)

  val track_inprogress : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [track_inprogress t f] increases the value of the gauge by one while [f ()] is running. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      increases the metric by the difference.
  *)
end
(** A gauge is a metric that represents a single numerical value that can arbitrarily go up and down. *)

module Summary : sig
  include METRIC

  val observe : t -> float -> unit
  (** [observe t v] increases the total by [v] and the count by one. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
end
(** A summary is a metric that records both the number of readings and their total.
    This allows calculating the average. *)

module Histogram_spec : sig
  type t

  val of_linear : float -> float -> int -> t
  (** [of_linear start interval count] will return a histogram type with
      [count] buckets with values starting at [start] and [interval] apart:
      [(start, start+interval, start + (2 * interval), ... start + ((count-1) * interval), infinity)].
      [count] does not include the infinity bucket.
  *)

  val of_exponential : float -> float -> int -> t
  (** [of_exponential start factor count] will return a histogram type with
      [count] buckets with values starting at [start] and every next item [previous*factor].
      [count] does not include the infinity bucket.
  *)

  val of_list : float list -> t
  (** [of_list [0.5; 1.]] will return a histogram with buckets [0.5;1.;infinity]. *)
end

module type HISTOGRAM = sig
  include METRIC

  val observe : t -> float -> unit
  (** [observe t v] adds one to the appropriate bucket for v and adds v to the sum. *)

  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
end

module Histogram (Buckets : sig val spec : Histogram_spec.t end) : HISTOGRAM

module DefaultHistogram : HISTOGRAM
(** A histogram configured with reasonable defaults for measuring network request times in seconds. *)
