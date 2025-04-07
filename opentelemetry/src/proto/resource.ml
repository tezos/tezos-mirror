[@@@ocaml.warning "-27-30-39"]

type resource = {
  attributes : Common.key_value list;
  dropped_attributes_count : int32;
}

let rec default_resource 
  ?attributes:((attributes:Common.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : resource  = {
  attributes;
  dropped_attributes_count;
}

type resource_mutable = {
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_resource_mutable () : resource_mutable = {
  attributes = [];
  dropped_attributes_count = 0l;
}


(** {2 Make functions} *)

let rec make_resource 
  ~(attributes:Common.key_value list)
  ~(dropped_attributes_count:int32)
  () : resource  = {
  attributes;
  dropped_attributes_count;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_resource fmt (v:resource) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_resource (v:resource) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_resource d =
  let v = default_resource_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
  } : resource)
