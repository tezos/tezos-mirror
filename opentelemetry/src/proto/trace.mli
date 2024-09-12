
(** Code for trace.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/trace/v1/trace.proto", do not edit *)



(** {2 Types} *)

type span_span_kind =
  | Span_kind_unspecified 
  | Span_kind_internal 
  | Span_kind_server 
  | Span_kind_client 
  | Span_kind_producer 
  | Span_kind_consumer 

type span_event = {
  time_unix_nano : int64;
  name : string;
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
}

type span_link = {
  trace_id : bytes;
  span_id : bytes;
  trace_state : string;
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
}

type status_status_code =
  | Status_code_unset 
  | Status_code_ok 
  | Status_code_error 

type status = {
  message : string;
  code : status_status_code;
}

type span = {
  trace_id : bytes;
  span_id : bytes;
  trace_state : string;
  parent_span_id : bytes;
  name : string;
  kind : span_span_kind;
  start_time_unix_nano : int64;
  end_time_unix_nano : int64;
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
  events : span_event list;
  dropped_events_count : int32;
  links : span_link list;
  dropped_links_count : int32;
  status : status option;
}

type scope_spans = {
  scope : Common.instrumentation_scope option;
  spans : span list;
  schema_url : string;
}

type resource_spans = {
  resource : Resource.resource option;
  scope_spans : scope_spans list;
  schema_url : string;
}

type traces_data = {
  resource_spans : resource_spans list;
}


(** {2 Basic values} *)

val default_span_span_kind : unit -> span_span_kind
(** [default_span_span_kind ()] is the default value for type [span_span_kind] *)

val default_span_event : 
  ?time_unix_nano:int64 ->
  ?name:string ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  span_event
(** [default_span_event ()] is the default value for type [span_event] *)

val default_span_link : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  span_link
(** [default_span_link ()] is the default value for type [span_link] *)

val default_status_status_code : unit -> status_status_code
(** [default_status_status_code ()] is the default value for type [status_status_code] *)

val default_status : 
  ?message:string ->
  ?code:status_status_code ->
  unit ->
  status
(** [default_status ()] is the default value for type [status] *)

val default_span : 
  ?trace_id:bytes ->
  ?span_id:bytes ->
  ?trace_state:string ->
  ?parent_span_id:bytes ->
  ?name:string ->
  ?kind:span_span_kind ->
  ?start_time_unix_nano:int64 ->
  ?end_time_unix_nano:int64 ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?events:span_event list ->
  ?dropped_events_count:int32 ->
  ?links:span_link list ->
  ?dropped_links_count:int32 ->
  ?status:status option ->
  unit ->
  span
(** [default_span ()] is the default value for type [span] *)

val default_scope_spans : 
  ?scope:Common.instrumentation_scope option ->
  ?spans:span list ->
  ?schema_url:string ->
  unit ->
  scope_spans
(** [default_scope_spans ()] is the default value for type [scope_spans] *)

val default_resource_spans : 
  ?resource:Resource.resource option ->
  ?scope_spans:scope_spans list ->
  ?schema_url:string ->
  unit ->
  resource_spans
(** [default_resource_spans ()] is the default value for type [resource_spans] *)

val default_traces_data : 
  ?resource_spans:resource_spans list ->
  unit ->
  traces_data
(** [default_traces_data ()] is the default value for type [traces_data] *)


(** {2 Make functions} *)


val make_span_event : 
  time_unix_nano:int64 ->
  name:string ->
  attributes:Common.key_value list ->
  dropped_attributes_count:int32 ->
  unit ->
  span_event
(** [make_span_event … ()] is a builder for type [span_event] *)

val make_span_link : 
  trace_id:bytes ->
  span_id:bytes ->
  trace_state:string ->
  attributes:Common.key_value list ->
  dropped_attributes_count:int32 ->
  unit ->
  span_link
(** [make_span_link … ()] is a builder for type [span_link] *)


val make_status : 
  message:string ->
  code:status_status_code ->
  unit ->
  status
(** [make_status … ()] is a builder for type [status] *)

val make_span : 
  trace_id:bytes ->
  span_id:bytes ->
  trace_state:string ->
  parent_span_id:bytes ->
  name:string ->
  kind:span_span_kind ->
  start_time_unix_nano:int64 ->
  end_time_unix_nano:int64 ->
  attributes:Common.key_value list ->
  dropped_attributes_count:int32 ->
  events:span_event list ->
  dropped_events_count:int32 ->
  links:span_link list ->
  dropped_links_count:int32 ->
  ?status:status option ->
  unit ->
  span
(** [make_span … ()] is a builder for type [span] *)

val make_scope_spans : 
  ?scope:Common.instrumentation_scope option ->
  spans:span list ->
  schema_url:string ->
  unit ->
  scope_spans
(** [make_scope_spans … ()] is a builder for type [scope_spans] *)

val make_resource_spans : 
  ?resource:Resource.resource option ->
  scope_spans:scope_spans list ->
  schema_url:string ->
  unit ->
  resource_spans
(** [make_resource_spans … ()] is a builder for type [resource_spans] *)

val make_traces_data : 
  resource_spans:resource_spans list ->
  unit ->
  traces_data
(** [make_traces_data … ()] is a builder for type [traces_data] *)


(** {2 Formatters} *)

val pp_span_span_kind : Format.formatter -> span_span_kind -> unit 
(** [pp_span_span_kind v] formats v *)

val pp_span_event : Format.formatter -> span_event -> unit 
(** [pp_span_event v] formats v *)

val pp_span_link : Format.formatter -> span_link -> unit 
(** [pp_span_link v] formats v *)

val pp_status_status_code : Format.formatter -> status_status_code -> unit 
(** [pp_status_status_code v] formats v *)

val pp_status : Format.formatter -> status -> unit 
(** [pp_status v] formats v *)

val pp_span : Format.formatter -> span -> unit 
(** [pp_span v] formats v *)

val pp_scope_spans : Format.formatter -> scope_spans -> unit 
(** [pp_scope_spans v] formats v *)

val pp_resource_spans : Format.formatter -> resource_spans -> unit 
(** [pp_resource_spans v] formats v *)

val pp_traces_data : Format.formatter -> traces_data -> unit 
(** [pp_traces_data v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_span_span_kind : span_span_kind -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_span_kind v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span_event : span_event -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_event v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span_link : span_link -> Pbrt.Encoder.t -> unit
(** [encode_pb_span_link v encoder] encodes [v] with the given [encoder] *)

val encode_pb_status_status_code : status_status_code -> Pbrt.Encoder.t -> unit
(** [encode_pb_status_status_code v encoder] encodes [v] with the given [encoder] *)

val encode_pb_status : status -> Pbrt.Encoder.t -> unit
(** [encode_pb_status v encoder] encodes [v] with the given [encoder] *)

val encode_pb_span : span -> Pbrt.Encoder.t -> unit
(** [encode_pb_span v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_spans : scope_spans -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_spans v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_spans : resource_spans -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_spans v encoder] encodes [v] with the given [encoder] *)

val encode_pb_traces_data : traces_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_traces_data v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_span_span_kind : Pbrt.Decoder.t -> span_span_kind
(** [decode_pb_span_span_kind decoder] decodes a [span_span_kind] binary value from [decoder] *)

val decode_pb_span_event : Pbrt.Decoder.t -> span_event
(** [decode_pb_span_event decoder] decodes a [span_event] binary value from [decoder] *)

val decode_pb_span_link : Pbrt.Decoder.t -> span_link
(** [decode_pb_span_link decoder] decodes a [span_link] binary value from [decoder] *)

val decode_pb_status_status_code : Pbrt.Decoder.t -> status_status_code
(** [decode_pb_status_status_code decoder] decodes a [status_status_code] binary value from [decoder] *)

val decode_pb_status : Pbrt.Decoder.t -> status
(** [decode_pb_status decoder] decodes a [status] binary value from [decoder] *)

val decode_pb_span : Pbrt.Decoder.t -> span
(** [decode_pb_span decoder] decodes a [span] binary value from [decoder] *)

val decode_pb_scope_spans : Pbrt.Decoder.t -> scope_spans
(** [decode_pb_scope_spans decoder] decodes a [scope_spans] binary value from [decoder] *)

val decode_pb_resource_spans : Pbrt.Decoder.t -> resource_spans
(** [decode_pb_resource_spans decoder] decodes a [resource_spans] binary value from [decoder] *)

val decode_pb_traces_data : Pbrt.Decoder.t -> traces_data
(** [decode_pb_traces_data decoder] decodes a [traces_data] binary value from [decoder] *)
