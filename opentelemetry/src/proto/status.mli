
(** Code for status.proto *)

(* generated from "status.proto", do not edit *)



(** {2 Types} *)

type status = {
  code : int32;
  message : bytes;
  details : bytes list;
}


(** {2 Basic values} *)

val default_status : 
  ?code:int32 ->
  ?message:bytes ->
  ?details:bytes list ->
  unit ->
  status
(** [default_status ()] is the default value for type [status] *)


(** {2 Make functions} *)

val make_status : 
  code:int32 ->
  message:bytes ->
  details:bytes list ->
  unit ->
  status
(** [make_status … ()] is a builder for type [status] *)


(** {2 Formatters} *)

val pp_status : Format.formatter -> status -> unit 
(** [pp_status v] formats v *)


(** {2 Protobuf Encoding} *)

val encode_pb_status : status -> Pbrt.Encoder.t -> unit
(** [encode_pb_status v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_pb_status : Pbrt.Decoder.t -> status
(** [decode_pb_status decoder] decodes a [status] binary value from [decoder] *)
