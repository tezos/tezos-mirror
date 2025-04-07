[@@@ocaml.warning "-27-30-39"]

type status = {
  code : int32;
  message : bytes;
  details : bytes list;
}

let rec default_status 
  ?code:((code:int32) = 0l)
  ?message:((message:bytes) = Bytes.create 0)
  ?details:((details:bytes list) = [])
  () : status  = {
  code;
  message;
  details;
}

type status_mutable = {
  mutable code : int32;
  mutable message : bytes;
  mutable details : bytes list;
}

let default_status_mutable () : status_mutable = {
  code = 0l;
  message = Bytes.create 0;
  details = [];
}


(** {2 Make functions} *)

let rec make_status 
  ~(code:int32)
  ~(message:bytes)
  ~(details:bytes list)
  () : status  = {
  code;
  message;
  details;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_status fmt (v:status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "code" Pbrt.Pp.pp_int32 fmt v.code;
    Pbrt.Pp.pp_record_field ~first:false "message" Pbrt.Pp.pp_bytes fmt v.message;
    Pbrt.Pp.pp_record_field ~first:false "details" (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes) fmt v.details;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_status (v:status) encoder = 
  Pbrt.Encoder.int32_as_varint v.code encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.bytes v.message encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.bytes x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.details encoder;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_status d =
  let v = default_status_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.details <- List.rev v.details;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.code <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.message <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.details <- (Pbrt.Decoder.bytes d) :: v.details;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    code = v.code;
    message = v.message;
    details = v.details;
  } : status)
