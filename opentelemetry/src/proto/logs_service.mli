
(** Code for logs_service.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/collector/logs/v1/logs_service.proto", do not edit *)



(** {2 Types} *)

type export_logs_service_request = {
  resource_logs : Logs.resource_logs list;
}

type export_logs_partial_success = {
  rejected_log_records : int64;
  error_message : string;
}

type export_logs_service_response = {
  partial_success : export_logs_partial_success option;
}


(** {2 Basic values} *)

val default_export_logs_service_request : 
  ?resource_logs:Logs.resource_logs list ->
  unit ->
  export_logs_service_request
(** [default_export_logs_service_request ()] is the default value for type [export_logs_service_request] *)

val default_export_logs_partial_success : 
  ?rejected_log_records:int64 ->
  ?error_message:string ->
  unit ->
  export_logs_partial_success
(** [default_export_logs_partial_success ()] is the default value for type [export_logs_partial_success] *)

val default_export_logs_service_response : 
  ?partial_success:export_logs_partial_success option ->
  unit ->
  export_logs_service_response
(** [default_export_logs_service_response ()] is the default value for type [export_logs_service_response] *)


(** {2 Make functions} *)

val make_export_logs_service_request : 
  resource_logs:Logs.resource_logs list ->
  unit ->
  export_logs_service_request
(** [make_export_logs_service_request … ()] is a builder for type [export_logs_service_request] *)

val make_export_logs_partial_success : 
  rejected_log_records:int64 ->
  error_message:string ->
  unit ->
  export_logs_partial_success
(** [make_export_logs_partial_success … ()] is a builder for type [export_logs_partial_success] *)

val make_export_logs_service_response : 
  ?partial_success:export_logs_partial_success option ->
  unit ->
  export_logs_service_response
(** [make_export_logs_service_response … ()] is a builder for type [export_logs_service_response] *)


(** {2 Formatters} *)

val pp_export_logs_service_request : Format.formatter -> export_logs_service_request -> unit 
(** [pp_export_logs_service_request v] formats v *)

val pp_export_logs_partial_success : Format.formatter -> export_logs_partial_success -> unit 
(** [pp_export_logs_partial_success v] formats v *)

val pp_export_logs_service_response : Format.formatter -> export_logs_service_response -> unit 
(** [pp_export_logs_service_response v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_export_logs_service_request : export_logs_service_request -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_logs_service_request v encoder] encodes [v] with the given [encoder] *)

val encode_pb_export_logs_partial_success : export_logs_partial_success -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_logs_partial_success v encoder] encodes [v] with the given [encoder] *)

val encode_pb_export_logs_service_response : export_logs_service_response -> Pbrt.Encoder.t -> unit
(** [encode_pb_export_logs_service_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_export_logs_service_request : Pbrt.Decoder.t -> export_logs_service_request
(** [decode_pb_export_logs_service_request decoder] decodes a [export_logs_service_request] binary value from [decoder] *)

val decode_pb_export_logs_partial_success : Pbrt.Decoder.t -> export_logs_partial_success
(** [decode_pb_export_logs_partial_success decoder] decodes a [export_logs_partial_success] binary value from [decoder] *)

val decode_pb_export_logs_service_response : Pbrt.Decoder.t -> export_logs_service_response
(** [decode_pb_export_logs_service_response decoder] decodes a [export_logs_service_response] binary value from [decoder] *)
