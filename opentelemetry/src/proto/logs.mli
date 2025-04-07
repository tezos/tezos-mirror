
(** Code for logs.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/logs/v1/logs.proto", do not edit *)



(** {2 Types} *)

type severity_number =
  | Severity_number_unspecified 
  | Severity_number_trace 
  | Severity_number_trace2 
  | Severity_number_trace3 
  | Severity_number_trace4 
  | Severity_number_debug 
  | Severity_number_debug2 
  | Severity_number_debug3 
  | Severity_number_debug4 
  | Severity_number_info 
  | Severity_number_info2 
  | Severity_number_info3 
  | Severity_number_info4 
  | Severity_number_warn 
  | Severity_number_warn2 
  | Severity_number_warn3 
  | Severity_number_warn4 
  | Severity_number_error 
  | Severity_number_error2 
  | Severity_number_error3 
  | Severity_number_error4 
  | Severity_number_fatal 
  | Severity_number_fatal2 
  | Severity_number_fatal3 
  | Severity_number_fatal4 

type log_record = {
  time_unix_nano : int64;
  observed_time_unix_nano : int64;
  severity_number : severity_number;
  severity_text : string;
  body : Common.any_value option;
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
  flags : int32;
  trace_id : bytes;
  span_id : bytes;
}

type scope_logs = {
  scope : Common.instrumentation_scope option;
  log_records : log_record list;
  schema_url : string;
}

type resource_logs = {
  resource : Resource.resource option;
  scope_logs : scope_logs list;
  schema_url : string;
}

type logs_data = {
  resource_logs : resource_logs list;
}

type log_record_flags =
  | Log_record_flags_do_not_use 
  | Log_record_flags_trace_flags_mask 


(** {2 Basic values} *)

val default_severity_number : unit -> severity_number
(** [default_severity_number ()] is the default value for type [severity_number] *)

val default_log_record : 
  ?time_unix_nano:int64 ->
  ?observed_time_unix_nano:int64 ->
  ?severity_number:severity_number ->
  ?severity_text:string ->
  ?body:Common.any_value option ->
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  ?flags:int32 ->
  ?trace_id:bytes ->
  ?span_id:bytes ->
  unit ->
  log_record
(** [default_log_record ()] is the default value for type [log_record] *)

val default_scope_logs : 
  ?scope:Common.instrumentation_scope option ->
  ?log_records:log_record list ->
  ?schema_url:string ->
  unit ->
  scope_logs
(** [default_scope_logs ()] is the default value for type [scope_logs] *)

val default_resource_logs : 
  ?resource:Resource.resource option ->
  ?scope_logs:scope_logs list ->
  ?schema_url:string ->
  unit ->
  resource_logs
(** [default_resource_logs ()] is the default value for type [resource_logs] *)

val default_logs_data : 
  ?resource_logs:resource_logs list ->
  unit ->
  logs_data
(** [default_logs_data ()] is the default value for type [logs_data] *)

val default_log_record_flags : unit -> log_record_flags
(** [default_log_record_flags ()] is the default value for type [log_record_flags] *)


(** {2 Make functions} *)


val make_log_record : 
  time_unix_nano:int64 ->
  observed_time_unix_nano:int64 ->
  severity_number:severity_number ->
  severity_text:string ->
  ?body:Common.any_value option ->
  attributes:Common.key_value list ->
  dropped_attributes_count:int32 ->
  flags:int32 ->
  trace_id:bytes ->
  span_id:bytes ->
  unit ->
  log_record
(** [make_log_record … ()] is a builder for type [log_record] *)

val make_scope_logs : 
  ?scope:Common.instrumentation_scope option ->
  log_records:log_record list ->
  schema_url:string ->
  unit ->
  scope_logs
(** [make_scope_logs … ()] is a builder for type [scope_logs] *)

val make_resource_logs : 
  ?resource:Resource.resource option ->
  scope_logs:scope_logs list ->
  schema_url:string ->
  unit ->
  resource_logs
(** [make_resource_logs … ()] is a builder for type [resource_logs] *)

val make_logs_data : 
  resource_logs:resource_logs list ->
  unit ->
  logs_data
(** [make_logs_data … ()] is a builder for type [logs_data] *)



(** {2 Formatters} *)

val pp_severity_number : Format.formatter -> severity_number -> unit 
(** [pp_severity_number v] formats v *)

val pp_log_record : Format.formatter -> log_record -> unit 
(** [pp_log_record v] formats v *)

val pp_scope_logs : Format.formatter -> scope_logs -> unit 
(** [pp_scope_logs v] formats v *)

val pp_resource_logs : Format.formatter -> resource_logs -> unit 
(** [pp_resource_logs v] formats v *)

val pp_logs_data : Format.formatter -> logs_data -> unit 
(** [pp_logs_data v] formats v *)

val pp_log_record_flags : Format.formatter -> log_record_flags -> unit 
(** [pp_log_record_flags v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_severity_number : severity_number -> Pbrt.Encoder.t -> unit
(** [encode_pb_severity_number v encoder] encodes [v] with the given [encoder] *)

val encode_pb_log_record : log_record -> Pbrt.Encoder.t -> unit
(** [encode_pb_log_record v encoder] encodes [v] with the given [encoder] *)

val encode_pb_scope_logs : scope_logs -> Pbrt.Encoder.t -> unit
(** [encode_pb_scope_logs v encoder] encodes [v] with the given [encoder] *)

val encode_pb_resource_logs : resource_logs -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource_logs v encoder] encodes [v] with the given [encoder] *)

val encode_pb_logs_data : logs_data -> Pbrt.Encoder.t -> unit
(** [encode_pb_logs_data v encoder] encodes [v] with the given [encoder] *)

val encode_pb_log_record_flags : log_record_flags -> Pbrt.Encoder.t -> unit
(** [encode_pb_log_record_flags v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_severity_number : Pbrt.Decoder.t -> severity_number
(** [decode_pb_severity_number decoder] decodes a [severity_number] binary value from [decoder] *)

val decode_pb_log_record : Pbrt.Decoder.t -> log_record
(** [decode_pb_log_record decoder] decodes a [log_record] binary value from [decoder] *)

val decode_pb_scope_logs : Pbrt.Decoder.t -> scope_logs
(** [decode_pb_scope_logs decoder] decodes a [scope_logs] binary value from [decoder] *)

val decode_pb_resource_logs : Pbrt.Decoder.t -> resource_logs
(** [decode_pb_resource_logs decoder] decodes a [resource_logs] binary value from [decoder] *)

val decode_pb_logs_data : Pbrt.Decoder.t -> logs_data
(** [decode_pb_logs_data decoder] decodes a [logs_data] binary value from [decoder] *)

val decode_pb_log_record_flags : Pbrt.Decoder.t -> log_record_flags
(** [decode_pb_log_record_flags decoder] decodes a [log_record_flags] binary value from [decoder] *)
