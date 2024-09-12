
(** Code for metrics.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/metrics/v1/metrics.proto", do not edit *)



(** {2 Types} *)

type exemplar_value =
  | As_double of float
  | As_int of int64

and exemplar = {
  filtered_attributes : Common.key_value list;
  time_unix_nano : int64;
  value : exemplar_value;
  span_id : bytes;
  trace_id : bytes;
}

type number_data_point_value =
  | As_double of float
  | As_int of int64

and number_data_point = {
  attributes : Common.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  value : number_data_point_value;
  exemplars : exemplar list;
  flags : int32;
}

type gauge = {
  data_points : number_data_point list;
}

type aggregation_temporality =
  | Aggregation_temporality_unspecified 
  | Aggregation_temporality_delta 
  | Aggregation_temporality_cumulative 

type sum = {
  data_points : number_data_point list;
  aggregation_temporality : aggregation_temporality;
  is_monotonic : bool;
}

type histogram_data_point = {
  attributes : Common.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float option;
  bucket_counts : int64 list;
  explicit_bounds : float list;
  exemplars : exemplar list;
  flags : int32;
  min : float option;
  max : float option;
}

type histogram = {
  data_points : histogram_data_point list;
  aggregation_temporality : aggregation_temporality;
}

type exponential_histogram_data_point_buckets = {
  offset : int32;
  bucket_counts : int64 list;
}

type exponential_histogram_data_point = {
  attributes : Common.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float option;
  scale : int32;
  zero_count : int64;
  positive : exponential_histogram_data_point_buckets option;
  negative : exponential_histogram_data_point_buckets option;
  flags : int32;
  exemplars : exemplar list;
  min : float option;
  max : float option;
  zero_threshold : float;
}

type exponential_histogram = {
  data_points : exponential_histogram_data_point list;
  aggregation_temporality : aggregation_temporality;
}

type summary_data_point_value_at_quantile = {
  quantile : float;
  value : float;
}

type summary_data_point = {
  attributes : Common.key_value list;
  start_time_unix_nano : int64;
  time_unix_nano : int64;
  count : int64;
  sum : float;
  quantile_values : summary_data_point_value_at_quantile list;
  flags : int32;
}

type summary = {
  data_points : summary_data_point list;
}

type metric_data =
  | Gauge of gauge
  | Sum of sum
  | Histogram of histogram
  | Exponential_histogram of exponential_histogram
  | Summary of summary

and metric = {
  name : string;
  description : string;
  unit_ : string;
  data : metric_data;
}

type scope_metrics = {
  scope : Common.instrumentation_scope option;
  metrics : metric list;
  schema_url : string;
}

type resource_metrics = {
  resource : Resource.resource option;
  scope_metrics : scope_metrics list;
  schema_url : string;
}

type metrics_data = {
  resource_metrics : resource_metrics list;
}

type data_point_flags =
  | Data_point_flags_do_not_use 
  | Data_point_flags_no_recorded_value_mask 


(** {2 Basic values} *)

val default_exemplar_value : unit -> exemplar_value
(** [default_exemplar_value ()] is the default value for type [exemplar_value] *)

val default_exemplar : 
  ?filtered_attributes:Common.key_value list ->
  ?time_unix_nano:int64 ->
  ?value:exemplar_value ->
  ?span_id:bytes ->
  ?trace_id:bytes ->
  unit ->
  exemplar
(** [default_exemplar ()] is the default value for type [exemplar] *)

val default_number_data_point_value : unit -> number_data_point_value
(** [default_number_data_point_value ()] is the default value for type [number_data_point_value] *)

val default_number_data_point : 
  ?attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?value:number_data_point_value ->
  ?exemplars:exemplar list ->
  ?flags:int32 ->
  unit ->
  number_data_point
(** [default_number_data_point ()] is the default value for type [number_data_point] *)

val default_gauge : 
  ?data_points:number_data_point list ->
  unit ->
  gauge
(** [default_gauge ()] is the default value for type [gauge] *)

val default_aggregation_temporality : unit -> aggregation_temporality
(** [default_aggregation_temporality ()] is the default value for type [aggregation_temporality] *)

val default_sum : 
  ?data_points:number_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  ?is_monotonic:bool ->
  unit ->
  sum
(** [default_sum ()] is the default value for type [sum] *)

val default_histogram_data_point : 
  ?attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float option ->
  ?bucket_counts:int64 list ->
  ?explicit_bounds:float list ->
  ?exemplars:exemplar list ->
  ?flags:int32 ->
  ?min:float option ->
  ?max:float option ->
  unit ->
  histogram_data_point
(** [default_histogram_data_point ()] is the default value for type [histogram_data_point] *)

val default_histogram : 
  ?data_points:histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  histogram
(** [default_histogram ()] is the default value for type [histogram] *)

val default_exponential_histogram_data_point_buckets : 
  ?offset:int32 ->
  ?bucket_counts:int64 list ->
  unit ->
  exponential_histogram_data_point_buckets
(** [default_exponential_histogram_data_point_buckets ()] is the default value for type [exponential_histogram_data_point_buckets] *)

val default_exponential_histogram_data_point : 
  ?attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float option ->
  ?scale:int32 ->
  ?zero_count:int64 ->
  ?positive:exponential_histogram_data_point_buckets option ->
  ?negative:exponential_histogram_data_point_buckets option ->
  ?flags:int32 ->
  ?exemplars:exemplar list ->
  ?min:float option ->
  ?max:float option ->
  ?zero_threshold:float ->
  unit ->
  exponential_histogram_data_point
(** [default_exponential_histogram_data_point ()] is the default value for type [exponential_histogram_data_point] *)

val default_exponential_histogram : 
  ?data_points:exponential_histogram_data_point list ->
  ?aggregation_temporality:aggregation_temporality ->
  unit ->
  exponential_histogram
(** [default_exponential_histogram ()] is the default value for type [exponential_histogram] *)

val default_summary_data_point_value_at_quantile : 
  ?quantile:float ->
  ?value:float ->
  unit ->
  summary_data_point_value_at_quantile
(** [default_summary_data_point_value_at_quantile ()] is the default value for type [summary_data_point_value_at_quantile] *)

val default_summary_data_point : 
  ?attributes:Common.key_value list ->
  ?start_time_unix_nano:int64 ->
  ?time_unix_nano:int64 ->
  ?count:int64 ->
  ?sum:float ->
  ?quantile_values:summary_data_point_value_at_quantile list ->
  ?flags:int32 ->
  unit ->
  summary_data_point
(** [default_summary_data_point ()] is the default value for type [summary_data_point] *)

val default_summary : 
  ?data_points:summary_data_point list ->
  unit ->
  summary
(** [default_summary ()] is the default value for type [summary] *)

val default_metric_data : unit -> metric_data
(** [default_metric_data ()] is the default value for type [metric_data] *)

val default_metric : 
  ?name:string ->
  ?description:string ->
  ?unit_:string ->
  ?data:metric_data ->
  unit ->
  metric
(** [default_metric ()] is the default value for type [metric] *)

val default_scope_metrics : 
  ?scope:Common.instrumentation_scope option ->
  ?metrics:metric list ->
  ?schema_url:string ->
  unit ->
  scope_metrics
(** [default_scope_metrics ()] is the default value for type [scope_metrics] *)

val default_resource_metrics : 
  ?resource:Resource.resource option ->
  ?scope_metrics:scope_metrics list ->
  ?schema_url:string ->
  unit ->
  resource_metrics
(** [default_resource_metrics ()] is the default value for type [resource_metrics] *)

val default_metrics_data : 
  ?resource_metrics:resource_metrics list ->
  unit ->
  metrics_data
(** [default_metrics_data ()] is the default value for type [metrics_data] *)

val default_data_point_flags : unit -> data_point_flags
(** [default_data_point_flags ()] is the default value for type [data_point_flags] *)


(** {2 Make functions} *)


val make_exemplar : 
  filtered_attributes:Common.key_value list ->
  time_unix_nano:int64 ->
  value:exemplar_value ->
  span_id:bytes ->
  trace_id:bytes ->
  unit ->
  exemplar
(** [make_exemplar … ()] is a builder for type [exemplar] *)


val make_number_data_point : 
  attributes:Common.key_value list ->
  start_time_unix_nano:int64 ->
  time_unix_nano:int64 ->
  value:number_data_point_value ->
  exemplars:exemplar list ->
  flags:int32 ->
  unit ->
  number_data_point
(** [make_number_data_point … ()] is a builder for type [number_data_point] *)

val make_gauge : 
  data_points:number_data_point list ->
  unit ->
  gauge
(** [make_gauge … ()] is a builder for type [gauge] *)


val make_sum : 
  data_points:number_data_point list ->
  aggregation_temporality:aggregation_temporality ->
  is_monotonic:bool ->
  unit ->
  sum
(** [make_sum … ()] is a builder for type [sum] *)

val make_histogram_data_point : 
  attributes:Common.key_value list ->
  start_time_unix_nano:int64 ->
  time_unix_nano:int64 ->
  count:int64 ->
  ?sum:float option ->
  bucket_counts:int64 list ->
  explicit_bounds:float list ->
  exemplars:exemplar list ->
  flags:int32 ->
  ?min:float option ->
  ?max:float option ->
  unit ->
  histogram_data_point
(** [make_histogram_data_point … ()] is a builder for type [histogram_data_point] *)

val make_histogram : 
  data_points:histogram_data_point list ->
  aggregation_temporality:aggregation_temporality ->
  unit ->
  histogram
(** [make_histogram … ()] is a builder for type [histogram] *)

val make_exponential_histogram_data_point_buckets : 
  offset:int32 ->
  bucket_counts:int64 list ->
  unit ->
  exponential_histogram_data_point_buckets
(** [make_exponential_histogram_data_point_buckets … ()] is a builder for type [exponential_histogram_data_point_buckets] *)

val make_exponential_histogram_data_point : 
  attributes:Common.key_value list ->
  start_time_unix_nano:int64 ->
  time_unix_nano:int64 ->
  count:int64 ->
  ?sum:float option ->
  scale:int32 ->
  zero_count:int64 ->
  ?positive:exponential_histogram_data_point_buckets option ->
  ?negative:exponential_histogram_data_point_buckets option ->
  flags:int32 ->
  exemplars:exemplar list ->
  ?min:float option ->
  ?max:float option ->
  zero_threshold:float ->
  unit ->
  exponential_histogram_data_point
(** [make_exponential_histogram_data_point … ()] is a builder for type [exponential_histogram_data_point] *)

val make_exponential_histogram : 
  data_points:exponential_histogram_data_point list ->
  aggregation_temporality:aggregation_temporality ->
  unit ->
  exponential_histogram
(** [make_exponential_histogram … ()] is a builder for type [exponential_histogram] *)

val make_summary_data_point_value_at_quantile : 
  quantile:float ->
  value:float ->
  unit ->
  summary_data_point_value_at_quantile
(** [make_summary_data_point_value_at_quantile … ()] is a builder for type [summary_data_point_value_at_quantile] *)

val make_summary_data_point : 
  attributes:Common.key_value list ->
  start_time_unix_nano:int64 ->
  time_unix_nano:int64 ->
  count:int64 ->
  sum:float ->
  quantile_values:summary_data_point_value_at_quantile list ->
  flags:int32 ->
  unit ->
  summary_data_point
(** [make_summary_data_point … ()] is a builder for type [summary_data_point] *)

val make_summary : 
  data_points:summary_data_point list ->
  unit ->
  summary
(** [make_summary … ()] is a builder for type [summary] *)


val make_metric : 
  name:string ->
  description:string ->
  unit_:string ->
  data:metric_data ->
  unit ->
  metric
(** [make_metric … ()] is a builder for type [metric] *)

val make_scope_metrics : 
  ?scope:Common.instrumentation_scope option ->
  metrics:metric list ->
  schema_url:string ->
  unit ->
  scope_metrics
(** [make_scope_metrics … ()] is a builder for type [scope_metrics] *)

val make_resource_metrics : 
  ?resource:Resource.resource option ->
  scope_metrics:scope_metrics list ->
  schema_url:string ->
  unit ->
  resource_metrics
(** [make_resource_metrics … ()] is a builder for type [resource_metrics] *)

val make_metrics_data : 
  resource_metrics:resource_metrics list ->
  unit ->
  metrics_data
(** [make_metrics_data … ()] is a builder for type [metrics_data] *)



(** {2 Formatters} *)

val pp_exemplar_value : Format.formatter -> exemplar_value -> unit 
(** [pp_exemplar_value v] formats v *)

val pp_exemplar : Format.formatter -> exemplar -> unit 
(** [pp_exemplar v] formats v *)

val pp_number_data_point_value : Format.formatter -> number_data_point_value -> unit 
(** [pp_number_data_point_value v] formats v *)

val pp_number_data_point : Format.formatter -> number_data_point -> unit 
(** [pp_number_data_point v] formats v *)

val pp_gauge : Format.formatter -> gauge -> unit 
(** [pp_gauge v] formats v *)

val pp_aggregation_temporality : Format.formatter -> aggregation_temporality -> unit 
(** [pp_aggregation_temporality v] formats v *)

val pp_sum : Format.formatter -> sum -> unit 
(** [pp_sum v] formats v *)

val pp_histogram_data_point : Format.formatter -> histogram_data_point -> unit 
(** [pp_histogram_data_point v] formats v *)

val pp_histogram : Format.formatter -> histogram -> unit 
(** [pp_histogram v] formats v *)

val pp_exponential_histogram_data_point_buckets : Format.formatter -> exponential_histogram_data_point_buckets -> unit 
(** [pp_exponential_histogram_data_point_buckets v] formats v *)

val pp_exponential_histogram_data_point : Format.formatter -> exponential_histogram_data_point -> unit 
(** [pp_exponential_histogram_data_point v] formats v *)

val pp_exponential_histogram : Format.formatter -> exponential_histogram -> unit 
(** [pp_exponential_histogram v] formats v *)

val pp_summary_data_point_value_at_quantile : Format.formatter -> summary_data_point_value_at_quantile -> unit 
(** [pp_summary_data_point_value_at_quantile v] formats v *)

val pp_summary_data_point : Format.formatter -> summary_data_point -> unit 
(** [pp_summary_data_point v] formats v *)

val pp_summary : Format.formatter -> summary -> unit 
(** [pp_summary v] formats v *)

val pp_metric_data : Format.formatter -> metric_data -> unit 
(** [pp_metric_data v] formats v *)

val pp_metric : Format.formatter -> metric -> unit 
(** [pp_metric v] formats v *)

val pp_scope_metrics : Format.formatter -> scope_metrics -> unit 
(** [pp_scope_metrics v] formats v *)

val pp_resource_metrics : Format.formatter -> resource_metrics -> unit 
(** [pp_resource_metrics v] formats v *)

val pp_metrics_data : Format.formatter -> metrics_data -> unit 
(** [pp_metrics_data v] formats v *)

val pp_data_point_flags : Format.formatter -> data_point_flags -> unit 
(** [pp_data_point_flags v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_exemplar_value : exemplar_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_exemplar_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exemplar : exemplar -> Pbrt.Encoder.t -> unit
(** [encode_pb_exemplar v encoder] encodes [v] with the given [encoder] *)

val encode_pb_number_data_point_value : number_data_point_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_number_data_point_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_number_data_point : number_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_number_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_gauge : gauge -> Pbrt.Encoder.t -> unit
(** [encode_pb_gauge v encoder] encodes [v] with the given [encoder] *)

val encode_pb_aggregation_temporality : aggregation_temporality -> Pbrt.Encoder.t -> unit
(** [encode_pb_aggregation_temporality v encoder] encodes [v] with the given [encoder] *)

val encode_pb_sum : sum -> Pbrt.Encoder.t -> unit
(** [encode_pb_sum v encoder] encodes [v] with the given [encoder] *)

val encode_pb_histogram_data_point : histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_histogram : histogram -> Pbrt.Encoder.t -> unit
(** [encode_pb_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram_data_point_buckets : exponential_histogram_data_point_buckets -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram_data_point_buckets v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram_data_point : exponential_histogram_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_exponential_histogram : exponential_histogram -> Pbrt.Encoder.t -> unit
(** [encode_pb_exponential_histogram v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary_data_point_value_at_quantile : summary_data_point_value_at_quantile -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary_data_point_value_at_quantile v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary_data_point : summary_data_point -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary_data_point v encoder] encodes [v] with the given [encoder] *)

val encode_pb_summary : summary -> Pbrt.Encoder.t -> unit
(** [encode_pb_summary v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metric_data : metric_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_metric_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metric : metric -> Pbrt.Encoder.t -> unit
(** [encode_pb_metric v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_metrics : scope_metrics -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_metrics : resource_metrics -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_metrics v encoder] encodes [v] with the given [encoder] *)

val encode_pb_metrics_data : metrics_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_metrics_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_data_point_flags : data_point_flags -> Pbrt.Encoder.t -> unit
(** [encode_pb_data_point_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_exemplar_value : Pbrt.Decoder.t -> exemplar_value
(** [decode_pb_exemplar_value decoder] decodes a [exemplar_value] binary value from [decoder] *)

val decode_pb_exemplar : Pbrt.Decoder.t -> exemplar
(** [decode_pb_exemplar decoder] decodes a [exemplar] binary value from [decoder] *)

val decode_pb_number_data_point_value : Pbrt.Decoder.t -> number_data_point_value
(** [decode_pb_number_data_point_value decoder] decodes a [number_data_point_value] binary value from [decoder] *)

val decode_pb_number_data_point : Pbrt.Decoder.t -> number_data_point
(** [decode_pb_number_data_point decoder] decodes a [number_data_point] binary value from [decoder] *)

val decode_pb_gauge : Pbrt.Decoder.t -> gauge
(** [decode_pb_gauge decoder] decodes a [gauge] binary value from [decoder] *)

val decode_pb_aggregation_temporality : Pbrt.Decoder.t -> aggregation_temporality
(** [decode_pb_aggregation_temporality decoder] decodes a [aggregation_temporality] binary value from [decoder] *)

val decode_pb_sum : Pbrt.Decoder.t -> sum
(** [decode_pb_sum decoder] decodes a [sum] binary value from [decoder] *)

val decode_pb_histogram_data_point : Pbrt.Decoder.t -> histogram_data_point
(** [decode_pb_histogram_data_point decoder] decodes a [histogram_data_point] binary value from [decoder] *)

val decode_pb_histogram : Pbrt.Decoder.t -> histogram
(** [decode_pb_histogram decoder] decodes a [histogram] binary value from [decoder] *)

val decode_pb_exponential_histogram_data_point_buckets : Pbrt.Decoder.t -> exponential_histogram_data_point_buckets
(** [decode_pb_exponential_histogram_data_point_buckets decoder] decodes a [exponential_histogram_data_point_buckets] binary value from [decoder] *)

val decode_pb_exponential_histogram_data_point : Pbrt.Decoder.t -> exponential_histogram_data_point
(** [decode_pb_exponential_histogram_data_point decoder] decodes a [exponential_histogram_data_point] binary value from [decoder] *)

val decode_pb_exponential_histogram : Pbrt.Decoder.t -> exponential_histogram
(** [decode_pb_exponential_histogram decoder] decodes a [exponential_histogram] binary value from [decoder] *)

val decode_pb_summary_data_point_value_at_quantile : Pbrt.Decoder.t -> summary_data_point_value_at_quantile
(** [decode_pb_summary_data_point_value_at_quantile decoder] decodes a [summary_data_point_value_at_quantile] binary value from [decoder] *)

val decode_pb_summary_data_point : Pbrt.Decoder.t -> summary_data_point
(** [decode_pb_summary_data_point decoder] decodes a [summary_data_point] binary value from [decoder] *)

val decode_pb_summary : Pbrt.Decoder.t -> summary
(** [decode_pb_summary decoder] decodes a [summary] binary value from [decoder] *)

val decode_pb_metric_data : Pbrt.Decoder.t -> metric_data
(** [decode_pb_metric_data decoder] decodes a [metric_data] binary value from [decoder] *)

val decode_pb_metric : Pbrt.Decoder.t -> metric
(** [decode_pb_metric decoder] decodes a [metric] binary value from [decoder] *)

val decode_pb_scope_metrics : Pbrt.Decoder.t -> scope_metrics
(** [decode_pb_scope_metrics decoder] decodes a [scope_metrics] binary value from [decoder] *)

val decode_pb_resource_metrics : Pbrt.Decoder.t -> resource_metrics
(** [decode_pb_resource_metrics decoder] decodes a [resource_metrics] binary value from [decoder] *)

val decode_pb_metrics_data : Pbrt.Decoder.t -> metrics_data
(** [decode_pb_metrics_data decoder] decodes a [metrics_data] binary value from [decoder] *)

val decode_pb_data_point_flags : Pbrt.Decoder.t -> data_point_flags
(** [decode_pb_data_point_flags decoder] decodes a [data_point_flags] binary value from [decoder] *)
