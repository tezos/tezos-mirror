[@@@ocaml.warning "-27-30-39"]

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

let rec default_exemplar_value () : exemplar_value = As_double (0.)

and default_exemplar 
  ?filtered_attributes:((filtered_attributes:Common.key_value list) = [])
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?value:((value:exemplar_value) = As_double (0.))
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  () : exemplar  = {
  filtered_attributes;
  time_unix_nano;
  value;
  span_id;
  trace_id;
}

let rec default_number_data_point_value () : number_data_point_value = As_double (0.)

and default_number_data_point 
  ?attributes:((attributes:Common.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?value:((value:number_data_point_value) = As_double (0.))
  ?exemplars:((exemplars:exemplar list) = [])
  ?flags:((flags:int32) = 0l)
  () : number_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  value;
  exemplars;
  flags;
}

let rec default_gauge 
  ?data_points:((data_points:number_data_point list) = [])
  () : gauge  = {
  data_points;
}

let rec default_aggregation_temporality () = (Aggregation_temporality_unspecified:aggregation_temporality)

let rec default_sum 
  ?data_points:((data_points:number_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  ?is_monotonic:((is_monotonic:bool) = false)
  () : sum  = {
  data_points;
  aggregation_temporality;
  is_monotonic;
}

let rec default_histogram_data_point 
  ?attributes:((attributes:Common.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float option) = None)
  ?bucket_counts:((bucket_counts:int64 list) = [])
  ?explicit_bounds:((explicit_bounds:float list) = [])
  ?exemplars:((exemplars:exemplar list) = [])
  ?flags:((flags:int32) = 0l)
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  () : histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  bucket_counts;
  explicit_bounds;
  exemplars;
  flags;
  min;
  max;
}

let rec default_histogram 
  ?data_points:((data_points:histogram_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  () : histogram  = {
  data_points;
  aggregation_temporality;
}

let rec default_exponential_histogram_data_point_buckets 
  ?offset:((offset:int32) = 0l)
  ?bucket_counts:((bucket_counts:int64 list) = [])
  () : exponential_histogram_data_point_buckets  = {
  offset;
  bucket_counts;
}

let rec default_exponential_histogram_data_point 
  ?attributes:((attributes:Common.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float option) = None)
  ?scale:((scale:int32) = 0l)
  ?zero_count:((zero_count:int64) = 0L)
  ?positive:((positive:exponential_histogram_data_point_buckets option) = None)
  ?negative:((negative:exponential_histogram_data_point_buckets option) = None)
  ?flags:((flags:int32) = 0l)
  ?exemplars:((exemplars:exemplar list) = [])
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  ?zero_threshold:((zero_threshold:float) = 0.)
  () : exponential_histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  scale;
  zero_count;
  positive;
  negative;
  flags;
  exemplars;
  min;
  max;
  zero_threshold;
}

let rec default_exponential_histogram 
  ?data_points:((data_points:exponential_histogram_data_point list) = [])
  ?aggregation_temporality:((aggregation_temporality:aggregation_temporality) = default_aggregation_temporality ())
  () : exponential_histogram  = {
  data_points;
  aggregation_temporality;
}

let rec default_summary_data_point_value_at_quantile 
  ?quantile:((quantile:float) = 0.)
  ?value:((value:float) = 0.)
  () : summary_data_point_value_at_quantile  = {
  quantile;
  value;
}

let rec default_summary_data_point 
  ?attributes:((attributes:Common.key_value list) = [])
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?count:((count:int64) = 0L)
  ?sum:((sum:float) = 0.)
  ?quantile_values:((quantile_values:summary_data_point_value_at_quantile list) = [])
  ?flags:((flags:int32) = 0l)
  () : summary_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  quantile_values;
  flags;
}

let rec default_summary 
  ?data_points:((data_points:summary_data_point list) = [])
  () : summary  = {
  data_points;
}

let rec default_metric_data () : metric_data = Gauge (default_gauge ())

and default_metric 
  ?name:((name:string) = "")
  ?description:((description:string) = "")
  ?unit_:((unit_:string) = "")
  ?data:((data:metric_data) = Gauge (default_gauge ()))
  () : metric  = {
  name;
  description;
  unit_;
  data;
}

let rec default_scope_metrics 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ?metrics:((metrics:metric list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_metrics  = {
  scope;
  metrics;
  schema_url;
}

let rec default_resource_metrics 
  ?resource:((resource:Resource.resource option) = None)
  ?scope_metrics:((scope_metrics:scope_metrics list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_metrics  = {
  resource;
  scope_metrics;
  schema_url;
}

let rec default_metrics_data 
  ?resource_metrics:((resource_metrics:resource_metrics list) = [])
  () : metrics_data  = {
  resource_metrics;
}

let rec default_data_point_flags () = (Data_point_flags_do_not_use:data_point_flags)

type exemplar_mutable = {
  mutable filtered_attributes : Common.key_value list;
  mutable time_unix_nano : int64;
  mutable value : exemplar_value;
  mutable span_id : bytes;
  mutable trace_id : bytes;
}

let default_exemplar_mutable () : exemplar_mutable = {
  filtered_attributes = [];
  time_unix_nano = 0L;
  value = As_double (0.);
  span_id = Bytes.create 0;
  trace_id = Bytes.create 0;
}

type number_data_point_mutable = {
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable value : number_data_point_value;
  mutable exemplars : exemplar list;
  mutable flags : int32;
}

let default_number_data_point_mutable () : number_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  value = As_double (0.);
  exemplars = [];
  flags = 0l;
}

type gauge_mutable = {
  mutable data_points : number_data_point list;
}

let default_gauge_mutable () : gauge_mutable = {
  data_points = [];
}

type sum_mutable = {
  mutable data_points : number_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
  mutable is_monotonic : bool;
}

let default_sum_mutable () : sum_mutable = {
  data_points = [];
  aggregation_temporality = default_aggregation_temporality ();
  is_monotonic = false;
}

type histogram_data_point_mutable = {
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable bucket_counts : int64 list;
  mutable explicit_bounds : float list;
  mutable exemplars : exemplar list;
  mutable flags : int32;
  mutable min : float option;
  mutable max : float option;
}

let default_histogram_data_point_mutable () : histogram_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = None;
  bucket_counts = [];
  explicit_bounds = [];
  exemplars = [];
  flags = 0l;
  min = None;
  max = None;
}

type histogram_mutable = {
  mutable data_points : histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

let default_histogram_mutable () : histogram_mutable = {
  data_points = [];
  aggregation_temporality = default_aggregation_temporality ();
}

type exponential_histogram_data_point_buckets_mutable = {
  mutable offset : int32;
  mutable bucket_counts : int64 list;
}

let default_exponential_histogram_data_point_buckets_mutable () : exponential_histogram_data_point_buckets_mutable = {
  offset = 0l;
  bucket_counts = [];
}

type exponential_histogram_data_point_mutable = {
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float option;
  mutable scale : int32;
  mutable zero_count : int64;
  mutable positive : exponential_histogram_data_point_buckets option;
  mutable negative : exponential_histogram_data_point_buckets option;
  mutable flags : int32;
  mutable exemplars : exemplar list;
  mutable min : float option;
  mutable max : float option;
  mutable zero_threshold : float;
}

let default_exponential_histogram_data_point_mutable () : exponential_histogram_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = None;
  scale = 0l;
  zero_count = 0L;
  positive = None;
  negative = None;
  flags = 0l;
  exemplars = [];
  min = None;
  max = None;
  zero_threshold = 0.;
}

type exponential_histogram_mutable = {
  mutable data_points : exponential_histogram_data_point list;
  mutable aggregation_temporality : aggregation_temporality;
}

let default_exponential_histogram_mutable () : exponential_histogram_mutable = {
  data_points = [];
  aggregation_temporality = default_aggregation_temporality ();
}

type summary_data_point_value_at_quantile_mutable = {
  mutable quantile : float;
  mutable value : float;
}

let default_summary_data_point_value_at_quantile_mutable () : summary_data_point_value_at_quantile_mutable = {
  quantile = 0.;
  value = 0.;
}

type summary_data_point_mutable = {
  mutable attributes : Common.key_value list;
  mutable start_time_unix_nano : int64;
  mutable time_unix_nano : int64;
  mutable count : int64;
  mutable sum : float;
  mutable quantile_values : summary_data_point_value_at_quantile list;
  mutable flags : int32;
}

let default_summary_data_point_mutable () : summary_data_point_mutable = {
  attributes = [];
  start_time_unix_nano = 0L;
  time_unix_nano = 0L;
  count = 0L;
  sum = 0.;
  quantile_values = [];
  flags = 0l;
}

type summary_mutable = {
  mutable data_points : summary_data_point list;
}

let default_summary_mutable () : summary_mutable = {
  data_points = [];
}

type metric_mutable = {
  mutable name : string;
  mutable description : string;
  mutable unit_ : string;
  mutable data : metric_data;
}

let default_metric_mutable () : metric_mutable = {
  name = "";
  description = "";
  unit_ = "";
  data = Gauge (default_gauge ());
}

type scope_metrics_mutable = {
  mutable scope : Common.instrumentation_scope option;
  mutable metrics : metric list;
  mutable schema_url : string;
}

let default_scope_metrics_mutable () : scope_metrics_mutable = {
  scope = None;
  metrics = [];
  schema_url = "";
}

type resource_metrics_mutable = {
  mutable resource : Resource.resource option;
  mutable scope_metrics : scope_metrics list;
  mutable schema_url : string;
}

let default_resource_metrics_mutable () : resource_metrics_mutable = {
  resource = None;
  scope_metrics = [];
  schema_url = "";
}

type metrics_data_mutable = {
  mutable resource_metrics : resource_metrics list;
}

let default_metrics_data_mutable () : metrics_data_mutable = {
  resource_metrics = [];
}


(** {2 Make functions} *)


let rec make_exemplar 
  ~(filtered_attributes:Common.key_value list)
  ~(time_unix_nano:int64)
  ~(value:exemplar_value)
  ~(span_id:bytes)
  ~(trace_id:bytes)
  () : exemplar  = {
  filtered_attributes;
  time_unix_nano;
  value;
  span_id;
  trace_id;
}


let rec make_number_data_point 
  ~(attributes:Common.key_value list)
  ~(start_time_unix_nano:int64)
  ~(time_unix_nano:int64)
  ~(value:number_data_point_value)
  ~(exemplars:exemplar list)
  ~(flags:int32)
  () : number_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  value;
  exemplars;
  flags;
}

let rec make_gauge 
  ~(data_points:number_data_point list)
  () : gauge  = {
  data_points;
}


let rec make_sum 
  ~(data_points:number_data_point list)
  ~(aggregation_temporality:aggregation_temporality)
  ~(is_monotonic:bool)
  () : sum  = {
  data_points;
  aggregation_temporality;
  is_monotonic;
}

let rec make_histogram_data_point 
  ~(attributes:Common.key_value list)
  ~(start_time_unix_nano:int64)
  ~(time_unix_nano:int64)
  ~(count:int64)
  ?sum:((sum:float option) = None)
  ~(bucket_counts:int64 list)
  ~(explicit_bounds:float list)
  ~(exemplars:exemplar list)
  ~(flags:int32)
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  () : histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  bucket_counts;
  explicit_bounds;
  exemplars;
  flags;
  min;
  max;
}

let rec make_histogram 
  ~(data_points:histogram_data_point list)
  ~(aggregation_temporality:aggregation_temporality)
  () : histogram  = {
  data_points;
  aggregation_temporality;
}

let rec make_exponential_histogram_data_point_buckets 
  ~(offset:int32)
  ~(bucket_counts:int64 list)
  () : exponential_histogram_data_point_buckets  = {
  offset;
  bucket_counts;
}

let rec make_exponential_histogram_data_point 
  ~(attributes:Common.key_value list)
  ~(start_time_unix_nano:int64)
  ~(time_unix_nano:int64)
  ~(count:int64)
  ?sum:((sum:float option) = None)
  ~(scale:int32)
  ~(zero_count:int64)
  ?positive:((positive:exponential_histogram_data_point_buckets option) = None)
  ?negative:((negative:exponential_histogram_data_point_buckets option) = None)
  ~(flags:int32)
  ~(exemplars:exemplar list)
  ?min:((min:float option) = None)
  ?max:((max:float option) = None)
  ~(zero_threshold:float)
  () : exponential_histogram_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  scale;
  zero_count;
  positive;
  negative;
  flags;
  exemplars;
  min;
  max;
  zero_threshold;
}

let rec make_exponential_histogram 
  ~(data_points:exponential_histogram_data_point list)
  ~(aggregation_temporality:aggregation_temporality)
  () : exponential_histogram  = {
  data_points;
  aggregation_temporality;
}

let rec make_summary_data_point_value_at_quantile 
  ~(quantile:float)
  ~(value:float)
  () : summary_data_point_value_at_quantile  = {
  quantile;
  value;
}

let rec make_summary_data_point 
  ~(attributes:Common.key_value list)
  ~(start_time_unix_nano:int64)
  ~(time_unix_nano:int64)
  ~(count:int64)
  ~(sum:float)
  ~(quantile_values:summary_data_point_value_at_quantile list)
  ~(flags:int32)
  () : summary_data_point  = {
  attributes;
  start_time_unix_nano;
  time_unix_nano;
  count;
  sum;
  quantile_values;
  flags;
}

let rec make_summary 
  ~(data_points:summary_data_point list)
  () : summary  = {
  data_points;
}


let rec make_metric 
  ~(name:string)
  ~(description:string)
  ~(unit_:string)
  ~(data:metric_data)
  () : metric  = {
  name;
  description;
  unit_;
  data;
}

let rec make_scope_metrics 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ~(metrics:metric list)
  ~(schema_url:string)
  () : scope_metrics  = {
  scope;
  metrics;
  schema_url;
}

let rec make_resource_metrics 
  ?resource:((resource:Resource.resource option) = None)
  ~(scope_metrics:scope_metrics list)
  ~(schema_url:string)
  () : resource_metrics  = {
  resource;
  scope_metrics;
  schema_url;
}

let rec make_metrics_data 
  ~(resource_metrics:resource_metrics list)
  () : metrics_data  = {
  resource_metrics;
}


[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_exemplar_value fmt (v:exemplar_value) =
  match v with
  | As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_exemplar fmt (v:exemplar) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "filtered_attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.filtered_attributes;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" pp_exemplar_value fmt v.value;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_number_data_point_value fmt (v:number_data_point_value) =
  match v with
  | As_double x -> Format.fprintf fmt "@[<hv2>As_double(@,%a)@]" Pbrt.Pp.pp_float x
  | As_int x -> Format.fprintf fmt "@[<hv2>As_int(@,%a)@]" Pbrt.Pp.pp_int64 x

and pp_number_data_point fmt (v:number_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "value" pp_number_data_point_value fmt v.value;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_gauge fmt (v:gauge) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_aggregation_temporality fmt (v:aggregation_temporality) =
  match v with
  | Aggregation_temporality_unspecified -> Format.fprintf fmt "Aggregation_temporality_unspecified"
  | Aggregation_temporality_delta -> Format.fprintf fmt "Aggregation_temporality_delta"
  | Aggregation_temporality_cumulative -> Format.fprintf fmt "Aggregation_temporality_cumulative"

let rec pp_sum fmt (v:sum) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_number_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
    Pbrt.Pp.pp_record_field ~first:false "is_monotonic" Pbrt.Pp.pp_bool fmt v.is_monotonic;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram_data_point fmt (v:histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.sum;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.bucket_counts;
    Pbrt.Pp.pp_record_field ~first:false "explicit_bounds" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.explicit_bounds;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    Pbrt.Pp.pp_record_field ~first:false "min" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.min;
    Pbrt.Pp.pp_record_field ~first:false "max" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.max;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram fmt (v:histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_histogram_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point_buckets fmt (v:exponential_histogram_data_point_buckets) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "offset" Pbrt.Pp.pp_int32 fmt v.offset;
    Pbrt.Pp.pp_record_field ~first:false "bucket_counts" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.bucket_counts;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram_data_point fmt (v:exponential_histogram_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.sum;
    Pbrt.Pp.pp_record_field ~first:false "scale" Pbrt.Pp.pp_int32 fmt v.scale;
    Pbrt.Pp.pp_record_field ~first:false "zero_count" Pbrt.Pp.pp_int64 fmt v.zero_count;
    Pbrt.Pp.pp_record_field ~first:false "positive" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.positive;
    Pbrt.Pp.pp_record_field ~first:false "negative" (Pbrt.Pp.pp_option pp_exponential_histogram_data_point_buckets) fmt v.negative;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    Pbrt.Pp.pp_record_field ~first:false "exemplars" (Pbrt.Pp.pp_list pp_exemplar) fmt v.exemplars;
    Pbrt.Pp.pp_record_field ~first:false "min" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.min;
    Pbrt.Pp.pp_record_field ~first:false "max" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.max;
    Pbrt.Pp.pp_record_field ~first:false "zero_threshold" Pbrt.Pp.pp_float fmt v.zero_threshold;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_exponential_histogram fmt (v:exponential_histogram) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_exponential_histogram_data_point) fmt v.data_points;
    Pbrt.Pp.pp_record_field ~first:false "aggregation_temporality" pp_aggregation_temporality fmt v.aggregation_temporality;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point_value_at_quantile fmt (v:summary_data_point_value_at_quantile) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "quantile" Pbrt.Pp.pp_float fmt v.quantile;
    Pbrt.Pp.pp_record_field ~first:false "value" Pbrt.Pp.pp_float fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_data_point fmt (v:summary_data_point) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "count" Pbrt.Pp.pp_int64 fmt v.count;
    Pbrt.Pp.pp_record_field ~first:false "sum" Pbrt.Pp.pp_float fmt v.sum;
    Pbrt.Pp.pp_record_field ~first:false "quantile_values" (Pbrt.Pp.pp_list pp_summary_data_point_value_at_quantile) fmt v.quantile_values;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary fmt (v:summary) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "data_points" (Pbrt.Pp.pp_list pp_summary_data_point) fmt v.data_points;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metric_data fmt (v:metric_data) =
  match v with
  | Gauge x -> Format.fprintf fmt "@[<hv2>Gauge(@,%a)@]" pp_gauge x
  | Sum x -> Format.fprintf fmt "@[<hv2>Sum(@,%a)@]" pp_sum x
  | Histogram x -> Format.fprintf fmt "@[<hv2>Histogram(@,%a)@]" pp_histogram x
  | Exponential_histogram x -> Format.fprintf fmt "@[<hv2>Exponential_histogram(@,%a)@]" pp_exponential_histogram x
  | Summary x -> Format.fprintf fmt "@[<hv2>Summary(@,%a)@]" pp_summary x

and pp_metric fmt (v:metric) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~first:false "description" Pbrt.Pp.pp_string fmt v.description;
    Pbrt.Pp.pp_record_field ~first:false "unit_" Pbrt.Pp.pp_string fmt v.unit_;
    Pbrt.Pp.pp_record_field ~first:false "data" pp_metric_data fmt v.data;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_metrics fmt (v:scope_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "metrics" (Pbrt.Pp.pp_list pp_metric) fmt v.metrics;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_metrics fmt (v:resource_metrics) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_metrics" (Pbrt.Pp.pp_list pp_scope_metrics) fmt v.scope_metrics;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_metrics_data fmt (v:metrics_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list pp_resource_metrics) fmt v.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_data_point_flags fmt (v:data_point_flags) =
  match v with
  | Data_point_flags_do_not_use -> Format.fprintf fmt "Data_point_flags_do_not_use"
  | Data_point_flags_no_recorded_value_mask -> Format.fprintf fmt "Data_point_flags_no_recorded_value_mask"

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_exemplar_value (v:exemplar_value) encoder = 
  begin match v with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end

and encode_pb_exemplar (v:exemplar) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.filtered_attributes encoder;
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  begin match v.value with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end;
  Pbrt.Encoder.bytes v.span_id encoder;
  Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bytes v.trace_id encoder;
  Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_number_data_point_value (v:number_data_point_value) encoder = 
  begin match v with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end

and encode_pb_number_data_point (v:number_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  begin match v.value with
  | As_double x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | As_int x ->
    Pbrt.Encoder.int64_as_bits64 x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bits64 encoder; 
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  Pbrt.Encoder.int32_as_varint v.flags encoder;
  Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  ()

let rec encode_pb_gauge (v:gauge) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_number_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  ()

let rec encode_pb_aggregation_temporality (v:aggregation_temporality) encoder =
  match v with
  | Aggregation_temporality_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Aggregation_temporality_delta -> Pbrt.Encoder.int_as_varint 1 encoder
  | Aggregation_temporality_cumulative -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_sum (v:sum) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_number_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  encode_pb_aggregation_temporality v.aggregation_temporality encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.bool v.is_monotonic encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  ()

let rec encode_pb_histogram_data_point (v:histogram_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.count encoder;
  Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  begin match v.sum with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.int64_as_bits64 x encoder;
    ) lst encoder;
  ) v.bucket_counts encoder;
  Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) lst encoder;
  ) v.explicit_bounds encoder;
  Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  Pbrt.Encoder.int32_as_varint v.flags encoder;
  Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  begin match v.min with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  begin match v.max with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 12 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_histogram (v:histogram) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_histogram_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  encode_pb_aggregation_temporality v.aggregation_temporality encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  ()

let rec encode_pb_exponential_histogram_data_point_buckets (v:exponential_histogram_data_point_buckets) encoder = 
  Pbrt.Encoder.int32_as_zigzag v.offset encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.nested (fun lst encoder ->
    Pbrt.List_util.rev_iter_with (fun x encoder -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) lst encoder;
  ) v.bucket_counts encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_exponential_histogram_data_point (v:exponential_histogram_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.count encoder;
  Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  begin match v.sum with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  Pbrt.Encoder.int32_as_zigzag v.scale encoder;
  Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_bits64 v.zero_count encoder;
  Pbrt.Encoder.key 7 Pbrt.Bits64 encoder; 
  begin match v.positive with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point_buckets x encoder;
    Pbrt.Encoder.key 8 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  begin match v.negative with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point_buckets x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.Encoder.int32_as_varint v.flags encoder;
  Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_exemplar x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  ) v.exemplars encoder;
  begin match v.min with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 12 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  begin match v.max with
  | Some x -> 
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 13 Pbrt.Bits64 encoder; 
  | None -> ();
  end;
  Pbrt.Encoder.float_as_bits64 v.zero_threshold encoder;
  Pbrt.Encoder.key 14 Pbrt.Bits64 encoder; 
  ()

let rec encode_pb_exponential_histogram (v:exponential_histogram) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_exponential_histogram_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  encode_pb_aggregation_temporality v.aggregation_temporality encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  ()

let rec encode_pb_summary_data_point_value_at_quantile (v:summary_data_point_value_at_quantile) encoder = 
  Pbrt.Encoder.float_as_bits64 v.quantile encoder;
  Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.float_as_bits64 v.value encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  ()

let rec encode_pb_summary_data_point (v:summary_data_point) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
  Pbrt.Encoder.key 2 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 3 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.count encoder;
  Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.float_as_bits64 v.sum encoder;
  Pbrt.Encoder.key 5 Pbrt.Bits64 encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_summary_data_point_value_at_quantile x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.quantile_values encoder;
  Pbrt.Encoder.int32_as_varint v.flags encoder;
  Pbrt.Encoder.key 8 Pbrt.Varint encoder; 
  ()

let rec encode_pb_summary (v:summary) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_summary_data_point x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.data_points encoder;
  ()

let rec encode_pb_metric_data (v:metric_data) encoder = 
  begin match v with
  | Gauge x ->
    Pbrt.Encoder.nested encode_pb_gauge x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Sum x ->
    Pbrt.Encoder.nested encode_pb_sum x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  | Histogram x ->
    Pbrt.Encoder.nested encode_pb_histogram x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | Exponential_histogram x ->
    Pbrt.Encoder.nested encode_pb_exponential_histogram x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  | Summary x ->
    Pbrt.Encoder.nested encode_pb_summary x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  end

and encode_pb_metric (v:metric) encoder = 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.description encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.unit_ encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  begin match v.data with
  | Gauge x ->
    Pbrt.Encoder.nested encode_pb_gauge x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Sum x ->
    Pbrt.Encoder.nested encode_pb_sum x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  | Histogram x ->
    Pbrt.Encoder.nested encode_pb_histogram x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  | Exponential_histogram x ->
    Pbrt.Encoder.nested encode_pb_exponential_histogram x encoder;
    Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  | Summary x ->
    Pbrt.Encoder.nested encode_pb_summary x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  end;
  ()

let rec encode_pb_scope_metrics (v:scope_metrics) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_metric x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.metrics encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_resource_metrics (v:resource_metrics) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_scope_metrics x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_metrics encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_metrics_data (v:metrics_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_resource_metrics x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_metrics encoder;
  ()

let rec encode_pb_data_point_flags (v:data_point_flags) encoder =
  match v with
  | Data_point_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Data_point_flags_no_recorded_value_mask -> Pbrt.Encoder.int_as_varint 1 encoder

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_exemplar_value d = 
  let rec loop () = 
    let ret:exemplar_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "exemplar_value"
      | Some (3, _) -> (As_double (Pbrt.Decoder.float_as_bits64 d) : exemplar_value) 
      | Some (6, _) -> (As_int (Pbrt.Decoder.int64_as_bits64 d) : exemplar_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_exemplar d =
  let v = default_exemplar_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.filtered_attributes <- List.rev v.filtered_attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.filtered_attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.filtered_attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.value <- As_double (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(3)" pk
    | Some (6, Pbrt.Bits64) -> begin
      v.value <- As_int (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(6)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exemplar), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    filtered_attributes = v.filtered_attributes;
    time_unix_nano = v.time_unix_nano;
    value = v.value;
    span_id = v.span_id;
    trace_id = v.trace_id;
  } : exemplar)

let rec decode_pb_number_data_point_value d = 
  let rec loop () = 
    let ret:number_data_point_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "number_data_point_value"
      | Some (4, _) -> (As_double (Pbrt.Decoder.float_as_bits64 d) : number_data_point_value) 
      | Some (6, _) -> (As_int (Pbrt.Decoder.int64_as_bits64 d) : number_data_point_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_number_data_point d =
  let v = default_number_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.value <- As_double (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(4)" pk
    | Some (6, Pbrt.Bits64) -> begin
      v.value <- As_int (Pbrt.Decoder.int64_as_bits64 d);
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(6)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(5)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(number_data_point), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    attributes = v.attributes;
    start_time_unix_nano = v.start_time_unix_nano;
    time_unix_nano = v.time_unix_nano;
    value = v.value;
    exemplars = v.exemplars;
    flags = v.flags;
  } : number_data_point)

let rec decode_pb_gauge d =
  let v = default_gauge_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_pb_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(gauge), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    data_points = v.data_points;
  } : gauge)

let rec decode_pb_aggregation_temporality d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Aggregation_temporality_unspecified:aggregation_temporality)
  | 1 -> (Aggregation_temporality_delta:aggregation_temporality)
  | 2 -> (Aggregation_temporality_cumulative:aggregation_temporality)
  | _ -> Pbrt.Decoder.malformed_variant "aggregation_temporality"

let rec decode_pb_sum d =
  let v = default_sum_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_pb_number_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_pb_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.is_monotonic <- Pbrt.Decoder.bool d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(sum), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    data_points = v.data_points;
    aggregation_temporality = v.aggregation_temporality;
    is_monotonic = v.is_monotonic;
  } : sum)

let rec decode_pb_histogram_data_point d =
  let v = default_histogram_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.explicit_bounds <- List.rev v.explicit_bounds;
      v.bucket_counts <- List.rev v.bucket_counts;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (9, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(9)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.bucket_counts <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_bits64 d)::l) [] d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.explicit_bounds <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(8)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(10)" pk
    | Some (11, Pbrt.Bits64) -> begin
      v.min <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(11)" pk
    | Some (12, Pbrt.Bits64) -> begin
      v.max <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_data_point), field(12)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    attributes = v.attributes;
    start_time_unix_nano = v.start_time_unix_nano;
    time_unix_nano = v.time_unix_nano;
    count = v.count;
    sum = v.sum;
    bucket_counts = v.bucket_counts;
    explicit_bounds = v.explicit_bounds;
    exemplars = v.exemplars;
    flags = v.flags;
    min = v.min;
    max = v.max;
  } : histogram_data_point)

let rec decode_pb_histogram d =
  let v = default_histogram_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_pb_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_pb_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    data_points = v.data_points;
    aggregation_temporality = v.aggregation_temporality;
  } : histogram)

let rec decode_pb_exponential_histogram_data_point_buckets d =
  let v = default_exponential_histogram_data_point_buckets_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.bucket_counts <- List.rev v.bucket_counts;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.offset <- Pbrt.Decoder.int32_as_zigzag d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point_buckets), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.bucket_counts <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point_buckets), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    offset = v.offset;
    bucket_counts = v.bucket_counts;
  } : exponential_histogram_data_point_buckets)

let rec decode_pb_exponential_histogram_data_point d =
  let v = default_exponential_histogram_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.exemplars <- List.rev v.exemplars;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(1)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.scale <- Pbrt.Decoder.int32_as_zigzag d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(6)" pk
    | Some (7, Pbrt.Bits64) -> begin
      v.zero_count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.positive <- Some (decode_pb_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.negative <- Some (decode_pb_exponential_histogram_data_point_buckets (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.exemplars <- (decode_pb_exemplar (Pbrt.Decoder.nested d)) :: v.exemplars;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(11)" pk
    | Some (12, Pbrt.Bits64) -> begin
      v.min <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(12)" pk
    | Some (13, Pbrt.Bits64) -> begin
      v.max <- Some (Pbrt.Decoder.float_as_bits64 d);
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(13)" pk
    | Some (14, Pbrt.Bits64) -> begin
      v.zero_threshold <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram_data_point), field(14)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    attributes = v.attributes;
    start_time_unix_nano = v.start_time_unix_nano;
    time_unix_nano = v.time_unix_nano;
    count = v.count;
    sum = v.sum;
    scale = v.scale;
    zero_count = v.zero_count;
    positive = v.positive;
    negative = v.negative;
    flags = v.flags;
    exemplars = v.exemplars;
    min = v.min;
    max = v.max;
    zero_threshold = v.zero_threshold;
  } : exponential_histogram_data_point)

let rec decode_pb_exponential_histogram d =
  let v = default_exponential_histogram_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_pb_exponential_histogram_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.aggregation_temporality <- decode_pb_aggregation_temporality d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(exponential_histogram), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    data_points = v.data_points;
    aggregation_temporality = v.aggregation_temporality;
  } : exponential_histogram)

let rec decode_pb_summary_data_point_value_at_quantile d =
  let v = default_summary_data_point_value_at_quantile_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.quantile <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point_value_at_quantile), field(1)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.value <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point_value_at_quantile), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    quantile = v.quantile;
    value = v.value;
  } : summary_data_point_value_at_quantile)

let rec decode_pb_summary_data_point d =
  let v = default_summary_data_point_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.quantile_values <- List.rev v.quantile_values;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(7)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.count <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.quantile_values <- (decode_pb_summary_data_point_value_at_quantile (Pbrt.Decoder.nested d)) :: v.quantile_values;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(6)" pk
    | Some (8, Pbrt.Varint) -> begin
      v.flags <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_data_point), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    attributes = v.attributes;
    start_time_unix_nano = v.start_time_unix_nano;
    time_unix_nano = v.time_unix_nano;
    count = v.count;
    sum = v.sum;
    quantile_values = v.quantile_values;
    flags = v.flags;
  } : summary_data_point)

let rec decode_pb_summary d =
  let v = default_summary_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.data_points <- List.rev v.data_points;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.data_points <- (decode_pb_summary_data_point (Pbrt.Decoder.nested d)) :: v.data_points;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    data_points = v.data_points;
  } : summary)

let rec decode_pb_metric_data d = 
  let rec loop () = 
    let ret:metric_data = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "metric_data"
      | Some (5, _) -> (Gauge (decode_pb_gauge (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (7, _) -> (Sum (decode_pb_sum (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (9, _) -> (Histogram (decode_pb_histogram (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (10, _) -> (Exponential_histogram (decode_pb_exponential_histogram (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (11, _) -> (Summary (decode_pb_summary (Pbrt.Decoder.nested d)) : metric_data) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_metric d =
  let v = default_metric_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.description <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.unit_ <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.data <- Gauge (decode_pb_gauge (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(5)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.data <- Sum (decode_pb_sum (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(7)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.data <- Histogram (decode_pb_histogram (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.data <- Exponential_histogram (decode_pb_exponential_histogram (Pbrt.Decoder.nested d));
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.data <- Summary (decode_pb_summary (Pbrt.Decoder.nested d));
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metric), field(11)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    name = v.name;
    description = v.description;
    unit_ = v.unit_;
    data = v.data;
  } : metric)

let rec decode_pb_scope_metrics d =
  let v = default_scope_metrics_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.metrics <- List.rev v.metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.scope <- Some (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.metrics <- (decode_pb_metric (Pbrt.Decoder.nested d)) :: v.metrics;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_metrics), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    scope = v.scope;
    metrics = v.metrics;
    schema_url = v.schema_url;
  } : scope_metrics)

let rec decode_pb_resource_metrics d =
  let v = default_resource_metrics_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.scope_metrics <- List.rev v.scope_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.scope_metrics <- (decode_pb_scope_metrics (Pbrt.Decoder.nested d)) :: v.scope_metrics;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_metrics), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource = v.resource;
    scope_metrics = v.scope_metrics;
    schema_url = v.schema_url;
  } : resource_metrics)

let rec decode_pb_metrics_data d =
  let v = default_metrics_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_metrics <- List.rev v.resource_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_metrics <- (decode_pb_resource_metrics (Pbrt.Decoder.nested d)) :: v.resource_metrics;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(metrics_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_metrics = v.resource_metrics;
  } : metrics_data)

let rec decode_pb_data_point_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Data_point_flags_do_not_use:data_point_flags)
  | 1 -> (Data_point_flags_no_recorded_value_mask:data_point_flags)
  | _ -> Pbrt.Decoder.malformed_variant "data_point_flags"
