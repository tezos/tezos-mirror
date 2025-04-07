
(** Code for common.proto *)

(* generated from "../../vendor/opentelemetry-proto/opentelemetry/proto/common/v1/common.proto", do not edit *)



(** {2 Types} *)

type any_value =
  | String_value of string
  | Bool_value of bool
  | Int_value of int64
  | Double_value of float
  | Array_value of array_value
  | Kvlist_value of key_value_list
  | Bytes_value of bytes

and array_value = {
  values : any_value list;
}

and key_value_list = {
  values : key_value list;
}

and key_value = {
  key : string;
  value : any_value option;
}

type instrumentation_scope = {
  name : string;
  version : string;
  attributes : key_value list;
  dropped_attributes_count : int32;
}


(** {2 Basic values} *)

val default_any_value : unit -> any_value
(** [default_any_value ()] is the default value for type [any_value] *)

val default_array_value : 
  ?values:any_value list ->
  unit ->
  array_value
(** [default_array_value ()] is the default value for type [array_value] *)

val default_key_value_list : 
  ?values:key_value list ->
  unit ->
  key_value_list
(** [default_key_value_list ()] is the default value for type [key_value_list] *)

val default_key_value : 
  ?key:string ->
  ?value:any_value option ->
  unit ->
  key_value
(** [default_key_value ()] is the default value for type [key_value] *)

val default_instrumentation_scope : 
  ?name:string ->
  ?version:string ->
  ?attributes:key_value list ->
  ?dropped_attributes_count:int32 ->
  unit ->
  instrumentation_scope
(** [default_instrumentation_scope ()] is the default value for type [instrumentation_scope] *)


(** {2 Make functions} *)


val make_array_value : 
  values:any_value list ->
  unit ->
  array_value
(** [make_array_value … ()] is a builder for type [array_value] *)

val make_key_value_list : 
  values:key_value list ->
  unit ->
  key_value_list
(** [make_key_value_list … ()] is a builder for type [key_value_list] *)

val make_key_value : 
  key:string ->
  ?value:any_value option ->
  unit ->
  key_value
(** [make_key_value … ()] is a builder for type [key_value] *)

val make_instrumentation_scope : 
  name:string ->
  version:string ->
  attributes:key_value list ->
  dropped_attributes_count:int32 ->
  unit ->
  instrumentation_scope
(** [make_instrumentation_scope … ()] is a builder for type [instrumentation_scope] *)


(** {2 Formatters} *)

val pp_any_value : Format.formatter -> any_value -> unit 
(** [pp_any_value v] formats v *)

val pp_array_value : Format.formatter -> array_value -> unit 
(** [pp_array_value v] formats v *)

val pp_key_value_list : Format.formatter -> key_value_list -> unit 
(** [pp_key_value_list v] formats v *)

val pp_key_value : Format.formatter -> key_value -> unit 
(** [pp_key_value v] formats v *)

val pp_instrumentation_scope : Format.formatter -> instrumentation_scope -> unit 
(** [pp_instrumentation_scope v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_any_value : any_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_any_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_array_value : array_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_array_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_key_value_list : key_value_list -> Pbrt.Encoder.t -> unit
(** [encode_pb_key_value_list v encoder] encodes [v] with the given [encoder] *)

val encode_pb_key_value : key_value -> Pbrt.Encoder.t -> unit
(** [encode_pb_key_value v encoder] encodes [v] with the given [encoder] *)

val encode_pb_instrumentation_scope : instrumentation_scope -> Pbrt.Encoder.t -> unit
(** [encode_pb_instrumentation_scope v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_any_value : Pbrt.Decoder.t -> any_value
(** [decode_pb_any_value decoder] decodes a [any_value] binary value from [decoder] *)

val decode_pb_array_value : Pbrt.Decoder.t -> array_value
(** [decode_pb_array_value decoder] decodes a [array_value] binary value from [decoder] *)

val decode_pb_key_value_list : Pbrt.Decoder.t -> key_value_list
(** [decode_pb_key_value_list decoder] decodes a [key_value_list] binary value from [decoder] *)

val decode_pb_key_value : Pbrt.Decoder.t -> key_value
(** [decode_pb_key_value decoder] decodes a [key_value] binary value from [decoder] *)

val decode_pb_instrumentation_scope : Pbrt.Decoder.t -> instrumentation_scope
(** [decode_pb_instrumentation_scope decoder] decodes a [instrumentation_scope] binary value from [decoder] *)
