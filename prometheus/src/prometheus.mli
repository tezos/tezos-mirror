(** Collect metrics for Prometheus.
    See: https://prometheus.io/

    Notes:

    - The Prometheus docs require that client libraries are thread-safe. We interpret this to mean safe
      with Lwt threads, NOT with native threading.

    - This library is intended to be a dependency of any library that might need to report metrics,
      even though many applications will not enable it. Therefore it should have minimal dependencies.
*)

type metric_type = Counter | Gauge | Summary | Histogram

(** A string that meets some additional requirements. *)
module type NAME = sig
  type t = private string

  (** Raises an exception if the name is not valid. *)
  val v : string -> t

  val pp : Format.formatter -> t -> unit

  val compare : t -> t -> int
end

(** A valid name for a metric. *)
module MetricName : NAME

(** A valid name for a label. *)
module LabelName : NAME

(** Metadata about a metric. *)
module MetricInfo : sig
  type t = {
    name : MetricName.t;
    metric_type : metric_type;
    help : string;
    label_names : LabelName.t list;
  }

  (** [v ~help ~label_names ~metric_type ~namespace ~subsystem name] creates
      metric metadata with full name [namespace_subsystem_name] and
      documentation string [help].

      @param help Documentation string describing the metric.
      @param label_names Names of labels for this metric (default: [[]]).
      @param metric_type The type of metric (Counter, Gauge, Summary, or
        Histogram).
      @param namespace Optional prefix for the metric name.
      @param subsystem Optional prefix between namespace and name.
  *)
  val v :
    help:string ->
    ?label_names:LabelName.t list ->
    metric_type:metric_type ->
    ?namespace:string ->
    ?subsystem:string ->
    string ->
    t
end

(** A map indexed by a set of labels. *)
module LabelSetMap : Asetmap.Map.S with type key = string list

(** A map indexed by metric families. *)
module MetricFamilyMap : Asetmap.Map.S with type key = MetricInfo.t

module Sample_set : sig
  type sample = {
    ext : string;  (** An extension to append to the base metric name. *)
    value : float;
    bucket : (LabelName.t * float) option;
        (** The "le" or "quantile" label and value, if any. *)
  }

  (** A collection of values that together represent a single sample.
      For a counter, each reading is just a single value, but more complex types
      require multiple values.
      For example, a "summary" sample set contains "_sum" and "_count" values.
   *)
  type t = sample list

  val sample : ?ext:string -> ?bucket:LabelName.t * float -> float -> sample
end

(** A collection of metric reporters. Usually, only {!CollectorRegistry.default} is used. *)
module CollectorRegistry : sig
  (** A collection of metrics to be monitored. *)
  type t

  (** The result of reading a set of metrics. *)
  type snapshot = Sample_set.t LabelSetMap.t MetricFamilyMap.t

  (** [create ()] is a fresh registry. This is mostly useful for testing. *)
  val create : unit -> t

  (** The default registry. *)
  val default : t

  (** Read the current value of each metric. *)
  val collect : t -> snapshot Lwt.t

  (** [register t metric collector] adds [metric] to the set of metrics being collected.
      It will call [collector ()] to collect the values each time [collect] is called. *)
  val register :
    t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t) -> unit

  (** [register_lwt t metric collector] is the same as [register t metrics collector]
      but [collector] returns [Sample_set.t LabelSetMap.t Lwt.t]. *)
  val register_lwt :
    t -> MetricInfo.t -> (unit -> Sample_set.t LabelSetMap.t Lwt.t) -> unit

  (** [register_pre_collect t fn] arranges for [fn ()] to be called at the start
      of each collection. This is useful if one expensive call provides
      information about multiple metrics. *)
  val register_pre_collect : t -> (unit -> unit) -> unit

  (** [register_pre_collect t fn] same as [register_pre_collect] but [fn] returns [unit Lwt.t]. *)
  val register_pre_collect_lwt : t -> (unit -> unit Lwt.t) -> unit
end

(** Operations common to all types of metric. *)
module type METRIC = sig
  (** A collection of metrics that are the same except for their labels.
      e.g. "Number of HTTP responses" *)
  type family

  (** A particular metric.
      e.g. "Number of HTTP responses with code=404" *)
  type t

  (** [v_labels ~label_names ~help ~namespace ~subsystem name] is a family of metrics with full name
      [namespace_subsystem_name] and documentation string [help]. Each metric in the family will provide
      a value for each of the labels.
      The new family is registered with [registry] (default: {!CollectorRegistry.default}). *)
  val v_labels :
    label_names:string list ->
    ?registry:CollectorRegistry.t ->
    help:string ->
    ?namespace:string ->
    ?subsystem:string ->
    string ->
    family

  (** [labels family label_values] is the metric in [family] with these values for the labels.
      The order of the values must be the same as the order of the [label_names] passed to [v_labels];
      you may wish to write a wrapper function with labelled arguments to avoid mistakes.
      If this is called multiple times with the same set of values, the existing metric will be returned. *)
  val labels : family -> string list -> t

  (** [v_label] is a convenience wrapper around [v_labels] for the case where there is a single label.
      The result is a function from the single label's value to the metric. *)
  val v_label :
    label_name:string ->
    ?registry:CollectorRegistry.t ->
    help:string ->
    ?namespace:string ->
    ?subsystem:string ->
    string ->
    string ->
    t

  (** [v] is a convenience wrapper around [v_labels] for the case where there are no labels. *)
  val v :
    ?registry:CollectorRegistry.t ->
    help:string ->
    ?namespace:string ->
    ?subsystem:string ->
    string ->
    t

  (** [clear] will clean every metric that was sent. *)
  val clear : family -> unit

  (** [clear_specific t labels] will clear a specific metric [t] that matches the given
      [labels]. *)
  val clear_specific : family -> string list -> unit
end

(** A counter is a cumulative metric that represents a single numerical value that only ever goes up. *)
module Counter : sig
  include METRIC

  val inc_one : t -> unit

  (** [inc t v] increases [t] by [v], which must be non-negative. *)
  val inc : t -> float -> unit

  (** [set t v] sets the current value of the counter to [v]. *)
  val set : t -> float -> unit
end

(** A gauge is a metric that represents a single numerical value that can arbitrarily go up and down. *)
module Gauge : sig
  include METRIC

  val read : t -> float

  val inc_one : t -> unit

  (** [inc t v] increases the current value of the guage by [v]. *)
  val inc : t -> float -> unit

  val dec_one : t -> unit

  (** [dec t v] decreases the current value of the guage by [v]. *)
  val dec : t -> float -> unit

  (** [set t v] sets the current value of the guage to [v]. *)
  val set : t -> float -> unit

  (** [track_inprogress t f] increases the value of the gauge by one while [f ()] is running. *)
  val track_inprogress : t -> (unit -> 'a Lwt.t) -> 'a Lwt.t

  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      increases the metric by the difference.
  *)
  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

(** A summary is a metric that records both the number of readings and their total.
    This allows calculating the average. *)
module Summary : sig
  include METRIC

  (** [observe ?n t v] increases the total by [v] and the count by [n] (default: by one). *)
  val observe : ?n:float -> t -> float -> unit

  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module Histogram_spec : sig
  type t

  (** [of_linear start interval count] will return a histogram type with
      [count] buckets with values starting at [start] and [interval] apart:
      [(start, start+interval, start + (2 * interval), ... start + ((count-1) * interval), infinity)].
      [count] does not include the infinity bucket.
  *)
  val of_linear : float -> float -> int -> t

  (** [of_exponential start factor count] will return a histogram type with
      [count] buckets with values starting at [start] and every next item [previous*factor].
      [count] does not include the infinity bucket.
  *)
  val of_exponential : float -> float -> int -> t

  (** [of_list [0.5; 1.]] will return a histogram with buckets [0.5;1.;infinity]. *)
  val of_list : float list -> t
end

module type HISTOGRAM = sig
  include METRIC

  (** [observe t v] adds one to the appropriate bucket for v and adds v to the sum. *)
  val observe : t -> float -> unit

  (** [time t gettime f] calls [gettime ()] before and after executing [f ()] and
      observes the difference. *)
  val time : t -> (unit -> float) -> (unit -> 'a Lwt.t) -> 'a Lwt.t
end

module Histogram (Buckets : sig
  val spec : Histogram_spec.t
end) : HISTOGRAM

(** A histogram configured with reasonable defaults for measuring network request times in seconds. *)
module DefaultHistogram : HISTOGRAM
