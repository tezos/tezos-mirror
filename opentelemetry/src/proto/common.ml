[@@@ocaml.warning "-27-30-39"]

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

let rec default_any_value () : any_value = String_value ("")

and default_array_value 
  ?values:((values:any_value list) = [])
  () : array_value  = {
  values;
}

and default_key_value_list 
  ?values:((values:key_value list) = [])
  () : key_value_list  = {
  values;
}

and default_key_value 
  ?key:((key:string) = "")
  ?value:((value:any_value option) = None)
  () : key_value  = {
  key;
  value;
}

let rec default_instrumentation_scope 
  ?name:((name:string) = "")
  ?version:((version:string) = "")
  ?attributes:((attributes:key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : instrumentation_scope  = {
  name;
  version;
  attributes;
  dropped_attributes_count;
}

type array_value_mutable = {
  mutable values : any_value list;
}

let default_array_value_mutable () : array_value_mutable = {
  values = [];
}

type key_value_list_mutable = {
  mutable values : key_value list;
}

let default_key_value_list_mutable () : key_value_list_mutable = {
  values = [];
}

type key_value_mutable = {
  mutable key : string;
  mutable value : any_value option;
}

let default_key_value_mutable () : key_value_mutable = {
  key = "";
  value = None;
}

type instrumentation_scope_mutable = {
  mutable name : string;
  mutable version : string;
  mutable attributes : key_value list;
  mutable dropped_attributes_count : int32;
}

let default_instrumentation_scope_mutable () : instrumentation_scope_mutable = {
  name = "";
  version = "";
  attributes = [];
  dropped_attributes_count = 0l;
}


(** {2 Make functions} *)


let rec make_array_value 
  ~(values:any_value list)
  () : array_value  = {
  values;
}

and make_key_value_list 
  ~(values:key_value list)
  () : key_value_list  = {
  values;
}

and make_key_value 
  ~(key:string)
  ?value:((value:any_value option) = None)
  () : key_value  = {
  key;
  value;
}

let rec make_instrumentation_scope 
  ~(name:string)
  ~(version:string)
  ~(attributes:key_value list)
  ~(dropped_attributes_count:int32)
  () : instrumentation_scope  = {
  name;
  version;
  attributes;
  dropped_attributes_count;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_any_value fmt (v:any_value) =
  match v with
  | String_value x -> Format.fprintf fmt "@[<hv2>String_value(@,%a)@]" Pbrt.Pp.pp_string x
  | Bool_value x -> Format.fprintf fmt "@[<hv2>Bool_value(@,%a)@]" Pbrt.Pp.pp_bool x
  | Int_value x -> Format.fprintf fmt "@[<hv2>Int_value(@,%a)@]" Pbrt.Pp.pp_int64 x
  | Double_value x -> Format.fprintf fmt "@[<hv2>Double_value(@,%a)@]" Pbrt.Pp.pp_float x
  | Array_value x -> Format.fprintf fmt "@[<hv2>Array_value(@,%a)@]" pp_array_value x
  | Kvlist_value x -> Format.fprintf fmt "@[<hv2>Kvlist_value(@,%a)@]" pp_key_value_list x
  | Bytes_value x -> Format.fprintf fmt "@[<hv2>Bytes_value(@,%a)@]" Pbrt.Pp.pp_bytes x

and pp_array_value fmt (v:array_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_any_value) fmt v.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value_list fmt (v:key_value_list) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "values" (Pbrt.Pp.pp_list pp_key_value) fmt v.values;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_key_value fmt (v:key_value) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "key" Pbrt.Pp.pp_string fmt v.key;
    Pbrt.Pp.pp_record_field ~first:false "value" (Pbrt.Pp.pp_option pp_any_value) fmt v.value;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_instrumentation_scope fmt (v:instrumentation_scope) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~first:false "version" Pbrt.Pp.pp_string fmt v.version;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_any_value (v:any_value) encoder = 
  begin match v with
  | String_value x ->
    Pbrt.Encoder.string x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | Bool_value x ->
    Pbrt.Encoder.bool x encoder;
    Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  | Int_value x ->
    Pbrt.Encoder.int64_as_varint x encoder;
    Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  | Double_value x ->
    Pbrt.Encoder.float_as_bits64 x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bits64 encoder; 
  | Array_value x ->
    Pbrt.Encoder.nested encode_pb_array_value x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | Kvlist_value x ->
    Pbrt.Encoder.nested encode_pb_key_value_list x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  | Bytes_value x ->
    Pbrt.Encoder.bytes x encoder;
    Pbrt.Encoder.key 7 Pbrt.Bytes encoder; 
  end

and encode_pb_array_value (v:array_value) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_any_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.values encoder;
  ()

and encode_pb_key_value_list (v:key_value_list) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_key_value x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.values encoder;
  ()

and encode_pb_key_value (v:key_value) encoder = 
  Pbrt.Encoder.string v.key encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  begin match v.value with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_any_value x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_instrumentation_scope (v:instrumentation_scope) encoder = 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.version encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_key_value x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_any_value d = 
  let rec loop () = 
    let ret:any_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "any_value"
      | Some (1, _) -> (String_value (Pbrt.Decoder.string d) : any_value) 
      | Some (2, _) -> (Bool_value (Pbrt.Decoder.bool d) : any_value) 
      | Some (3, _) -> (Int_value (Pbrt.Decoder.int64_as_varint d) : any_value) 
      | Some (4, _) -> (Double_value (Pbrt.Decoder.float_as_bits64 d) : any_value) 
      | Some (5, _) -> (Array_value (decode_pb_array_value (Pbrt.Decoder.nested d)) : any_value) 
      | Some (6, _) -> (Kvlist_value (decode_pb_key_value_list (Pbrt.Decoder.nested d)) : any_value) 
      | Some (7, _) -> (Bytes_value (Pbrt.Decoder.bytes d) : any_value) 
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_pb_array_value d =
  let v = default_array_value_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.values <- List.rev v.values;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.values <- (decode_pb_any_value (Pbrt.Decoder.nested d)) :: v.values;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(array_value), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    values = v.values;
  } : array_value)

and decode_pb_key_value_list d =
  let v = default_key_value_list_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.values <- List.rev v.values;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.values <- (decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.values;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value_list), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    values = v.values;
  } : key_value_list)

and decode_pb_key_value d =
  let v = default_key_value_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.key <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.value <- Some (decode_pb_any_value (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(key_value), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    key = v.key;
    value = v.value;
  } : key_value)

let rec decode_pb_instrumentation_scope d =
  let v = default_instrumentation_scope_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.version <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.attributes <- (decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(instrumentation_scope), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    name = v.name;
    version = v.version;
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
  } : instrumentation_scope)
