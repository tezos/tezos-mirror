
(** Code for resource.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/resource/v1/resource.proto", do not edit *)



(** {2 Types} *)

type resource = {
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
}


(** {2 Basic values} *)

val default_resource : 
  ?attributes:Common.key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  resource
(** [default_resource ()] is the default value for type [resource] *)


(** {2 Make functions} *)

val make_resource : 
  attributes:Common.key_value list ->
  dropped_attributes_count:int32 ->
  unit ->
  resource
(** [make_resource … ()] is a builder for type [resource] *)


(** {2 Formatters} *)

val pp_resource : Format.formatter -> resource -> unit 
(** [pp_resource v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_resource : resource -> Pbrt.Encoder.t -> unit
(** [encode_pb_resource v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_resource : Pbrt.Decoder.t -> resource
(** [decode_pb_resource decoder] decodes a [resource] binary value from [decoder] *)
