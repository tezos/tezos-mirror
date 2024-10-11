[@@@ocaml.warning "-27-30-39"]

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

let rec default_span_span_kind () = (Span_kind_unspecified:span_span_kind)

let rec default_span_event 
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?name:((name:string) = "")
  ?attributes:((attributes:Common.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : span_event  = {
  time_unix_nano;
  name;
  attributes;
  dropped_attributes_count;
}

let rec default_span_link 
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_state:((trace_state:string) = "")
  ?attributes:((attributes:Common.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  () : span_link  = {
  trace_id;
  span_id;
  trace_state;
  attributes;
  dropped_attributes_count;
}

let rec default_status_status_code () = (Status_code_unset:status_status_code)

let rec default_status 
  ?message:((message:string) = "")
  ?code:((code:status_status_code) = default_status_status_code ())
  () : status  = {
  message;
  code;
}

let rec default_span 
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  ?trace_state:((trace_state:string) = "")
  ?parent_span_id:((parent_span_id:bytes) = Bytes.create 0)
  ?name:((name:string) = "")
  ?kind:((kind:span_span_kind) = default_span_span_kind ())
  ?start_time_unix_nano:((start_time_unix_nano:int64) = 0L)
  ?end_time_unix_nano:((end_time_unix_nano:int64) = 0L)
  ?attributes:((attributes:Common.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  ?events:((events:span_event list) = [])
  ?dropped_events_count:((dropped_events_count:int32) = 0l)
  ?links:((links:span_link list) = [])
  ?dropped_links_count:((dropped_links_count:int32) = 0l)
  ?status:((status:status option) = None)
  () : span  = {
  trace_id;
  span_id;
  trace_state;
  parent_span_id;
  name;
  kind;
  start_time_unix_nano;
  end_time_unix_nano;
  attributes;
  dropped_attributes_count;
  events;
  dropped_events_count;
  links;
  dropped_links_count;
  status;
}

let rec default_scope_spans 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ?spans:((spans:span list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_spans  = {
  scope;
  spans;
  schema_url;
}

let rec default_resource_spans 
  ?resource:((resource:Resource.resource option) = None)
  ?scope_spans:((scope_spans:scope_spans list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_spans  = {
  resource;
  scope_spans;
  schema_url;
}

let rec default_traces_data 
  ?resource_spans:((resource_spans:resource_spans list) = [])
  () : traces_data  = {
  resource_spans;
}

type span_event_mutable = {
  mutable time_unix_nano : int64;
  mutable name : string;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_span_event_mutable () : span_event_mutable = {
  time_unix_nano = 0L;
  name = "";
  attributes = [];
  dropped_attributes_count = 0l;
}

type span_link_mutable = {
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
}

let default_span_link_mutable () : span_link_mutable = {
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
  trace_state = "";
  attributes = [];
  dropped_attributes_count = 0l;
}

type status_mutable = {
  mutable message : string;
  mutable code : status_status_code;
}

let default_status_mutable () : status_mutable = {
  message = "";
  code = default_status_status_code ();
}

type span_mutable = {
  mutable trace_id : bytes;
  mutable span_id : bytes;
  mutable trace_state : string;
  mutable parent_span_id : bytes;
  mutable name : string;
  mutable kind : span_span_kind;
  mutable start_time_unix_nano : int64;
  mutable end_time_unix_nano : int64;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable events : span_event list;
  mutable dropped_events_count : int32;
  mutable links : span_link list;
  mutable dropped_links_count : int32;
  mutable status : status option;
}

let default_span_mutable () : span_mutable = {
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
  trace_state = "";
  parent_span_id = Bytes.create 0;
  name = "";
  kind = default_span_span_kind ();
  start_time_unix_nano = 0L;
  end_time_unix_nano = 0L;
  attributes = [];
  dropped_attributes_count = 0l;
  events = [];
  dropped_events_count = 0l;
  links = [];
  dropped_links_count = 0l;
  status = None;
}

type scope_spans_mutable = {
  mutable scope : Common.instrumentation_scope option;
  mutable spans : span list;
  mutable schema_url : string;
}

let default_scope_spans_mutable () : scope_spans_mutable = {
  scope = None;
  spans = [];
  schema_url = "";
}

type resource_spans_mutable = {
  mutable resource : Resource.resource option;
  mutable scope_spans : scope_spans list;
  mutable schema_url : string;
}

let default_resource_spans_mutable () : resource_spans_mutable = {
  resource = None;
  scope_spans = [];
  schema_url = "";
}

type traces_data_mutable = {
  mutable resource_spans : resource_spans list;
}

let default_traces_data_mutable () : traces_data_mutable = {
  resource_spans = [];
}


(** {2 Make functions} *)


let rec make_span_event 
  ~(time_unix_nano:int64)
  ~(name:string)
  ~(attributes:Common.key_value list)
  ~(dropped_attributes_count:int32)
  () : span_event  = {
  time_unix_nano;
  name;
  attributes;
  dropped_attributes_count;
}

let rec make_span_link 
  ~(trace_id:bytes)
  ~(span_id:bytes)
  ~(trace_state:string)
  ~(attributes:Common.key_value list)
  ~(dropped_attributes_count:int32)
  () : span_link  = {
  trace_id;
  span_id;
  trace_state;
  attributes;
  dropped_attributes_count;
}


let rec make_status 
  ~(message:string)
  ~(code:status_status_code)
  () : status  = {
  message;
  code;
}

let rec make_span 
  ~(trace_id:bytes)
  ~(span_id:bytes)
  ~(trace_state:string)
  ~(parent_span_id:bytes)
  ~(name:string)
  ~(kind:span_span_kind)
  ~(start_time_unix_nano:int64)
  ~(end_time_unix_nano:int64)
  ~(attributes:Common.key_value list)
  ~(dropped_attributes_count:int32)
  ~(events:span_event list)
  ~(dropped_events_count:int32)
  ~(links:span_link list)
  ~(dropped_links_count:int32)
  ?status:((status:status option) = None)
  () : span  = {
  trace_id;
  span_id;
  trace_state;
  parent_span_id;
  name;
  kind;
  start_time_unix_nano;
  end_time_unix_nano;
  attributes;
  dropped_attributes_count;
  events;
  dropped_events_count;
  links;
  dropped_links_count;
  status;
}

let rec make_scope_spans 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ~(spans:span list)
  ~(schema_url:string)
  () : scope_spans  = {
  scope;
  spans;
  schema_url;
}

let rec make_resource_spans 
  ?resource:((resource:Resource.resource option) = None)
  ~(scope_spans:scope_spans list)
  ~(schema_url:string)
  () : resource_spans  = {
  resource;
  scope_spans;
  schema_url;
}

let rec make_traces_data 
  ~(resource_spans:resource_spans list)
  () : traces_data  = {
  resource_spans;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_span_span_kind fmt (v:span_span_kind) =
  match v with
  | Span_kind_unspecified -> Format.fprintf fmt "Span_kind_unspecified"
  | Span_kind_internal -> Format.fprintf fmt "Span_kind_internal"
  | Span_kind_server -> Format.fprintf fmt "Span_kind_server"
  | Span_kind_client -> Format.fprintf fmt "Span_kind_client"
  | Span_kind_producer -> Format.fprintf fmt "Span_kind_producer"
  | Span_kind_consumer -> Format.fprintf fmt "Span_kind_consumer"

let rec pp_span_event fmt (v:span_event) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span_link fmt (v:span_link) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.trace_state;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_status_status_code fmt (v:status_status_code) =
  match v with
  | Status_code_unset -> Format.fprintf fmt "Status_code_unset"
  | Status_code_ok -> Format.fprintf fmt "Status_code_ok"
  | Status_code_error -> Format.fprintf fmt "Status_code_error"

let rec pp_status fmt (v:status) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "message" Pbrt.Pp.pp_string fmt v.message;
    Pbrt.Pp.pp_record_field ~first:false "code" pp_status_status_code fmt v.code;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_span fmt (v:span) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
    Pbrt.Pp.pp_record_field ~first:false "trace_state" Pbrt.Pp.pp_string fmt v.trace_state;
    Pbrt.Pp.pp_record_field ~first:false "parent_span_id" Pbrt.Pp.pp_bytes fmt v.parent_span_id;
    Pbrt.Pp.pp_record_field ~first:false "name" Pbrt.Pp.pp_string fmt v.name;
    Pbrt.Pp.pp_record_field ~first:false "kind" pp_span_span_kind fmt v.kind;
    Pbrt.Pp.pp_record_field ~first:false "start_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.start_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "end_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.end_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    Pbrt.Pp.pp_record_field ~first:false "events" (Pbrt.Pp.pp_list pp_span_event) fmt v.events;
    Pbrt.Pp.pp_record_field ~first:false "dropped_events_count" Pbrt.Pp.pp_int32 fmt v.dropped_events_count;
    Pbrt.Pp.pp_record_field ~first:false "links" (Pbrt.Pp.pp_list pp_span_link) fmt v.links;
    Pbrt.Pp.pp_record_field ~first:false "dropped_links_count" Pbrt.Pp.pp_int32 fmt v.dropped_links_count;
    Pbrt.Pp.pp_record_field ~first:false "status" (Pbrt.Pp.pp_option pp_status) fmt v.status;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_spans fmt (v:scope_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "spans" (Pbrt.Pp.pp_list pp_span) fmt v.spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_spans fmt (v:resource_spans) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_spans" (Pbrt.Pp.pp_list pp_scope_spans) fmt v.scope_spans;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_traces_data fmt (v:traces_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list pp_resource_spans) fmt v.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_span_span_kind (v:span_span_kind) encoder =
  match v with
  | Span_kind_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Span_kind_internal -> Pbrt.Encoder.int_as_varint 1 encoder
  | Span_kind_server -> Pbrt.Encoder.int_as_varint 2 encoder
  | Span_kind_client -> Pbrt.Encoder.int_as_varint 3 encoder
  | Span_kind_producer -> Pbrt.Encoder.int_as_varint 4 encoder
  | Span_kind_consumer -> Pbrt.Encoder.int_as_varint 5 encoder

let rec encode_pb_span_event (v:span_event) encoder = 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 4 Pbrt.Varint encoder; 
  ()

let rec encode_pb_span_link (v:span_link) encoder = 
  Pbrt.Encoder.bytes v.trace_id encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bytes v.span_id encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.trace_state encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 5 Pbrt.Varint encoder; 
  ()

let rec encode_pb_status_status_code (v:status_status_code) encoder =
  match v with
  | Status_code_unset -> Pbrt.Encoder.int_as_varint (0) encoder
  | Status_code_ok -> Pbrt.Encoder.int_as_varint 1 encoder
  | Status_code_error -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_pb_status (v:status) encoder = 
  Pbrt.Encoder.string v.message encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  encode_pb_status_status_code v.code encoder;
  Pbrt.Encoder.key 3 Pbrt.Varint encoder; 
  ()

let rec encode_pb_span (v:span) encoder = 
  Pbrt.Encoder.bytes v.trace_id encoder;
  Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bytes v.span_id encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.trace_state encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bytes v.parent_span_id encoder;
  Pbrt.Encoder.key 4 Pbrt.Bytes encoder; 
  Pbrt.Encoder.string v.name encoder;
  Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  encode_pb_span_span_kind v.kind encoder;
  Pbrt.Encoder.key 6 Pbrt.Varint encoder; 
  Pbrt.Encoder.int64_as_bits64 v.start_time_unix_nano encoder;
  Pbrt.Encoder.key 7 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.end_time_unix_nano encoder;
  Pbrt.Encoder.key 8 Pbrt.Bits64 encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 10 Pbrt.Varint encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_span_event x encoder;
    Pbrt.Encoder.key 11 Pbrt.Bytes encoder; 
  ) v.events encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_events_count encoder;
  Pbrt.Encoder.key 12 Pbrt.Varint encoder; 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_span_link x encoder;
    Pbrt.Encoder.key 13 Pbrt.Bytes encoder; 
  ) v.links encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_links_count encoder;
  Pbrt.Encoder.key 14 Pbrt.Varint encoder; 
  begin match v.status with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_status x encoder;
    Pbrt.Encoder.key 15 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

let rec encode_pb_scope_spans (v:scope_spans) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_span x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.spans encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_resource_spans (v:resource_spans) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_scope_spans x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_spans encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_traces_data (v:traces_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_resource_spans x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_spans encoder;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_span_span_kind d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Span_kind_unspecified:span_span_kind)
  | 1 -> (Span_kind_internal:span_span_kind)
  | 2 -> (Span_kind_server:span_span_kind)
  | 3 -> (Span_kind_client:span_span_kind)
  | 4 -> (Span_kind_producer:span_span_kind)
  | 5 -> (Span_kind_consumer:span_span_kind)
  | _ -> Pbrt.Decoder.malformed_variant "span_span_kind"

let rec decode_pb_span_event d =
  let v = default_span_event_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_event), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    time_unix_nano = v.time_unix_nano;
    name = v.name;
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
  } : span_event)

let rec decode_pb_span_link d =
  let v = default_span_link_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.trace_state <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(4)" pk
    | Some (5, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span_link), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    trace_id = v.trace_id;
    span_id = v.span_id;
    trace_state = v.trace_state;
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
  } : span_link)

let rec decode_pb_status_status_code d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Status_code_unset:status_status_code)
  | 1 -> (Status_code_ok:status_status_code)
  | 2 -> (Status_code_error:status_status_code)
  | _ -> Pbrt.Decoder.malformed_variant "status_status_code"

let rec decode_pb_status d =
  let v = default_status_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (2, Pbrt.Bytes) -> begin
      v.message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.code <- decode_pb_status_status_code d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(status), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    message = v.message;
    code = v.code;
  } : status)

let rec decode_pb_span d =
  let v = default_span_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.links <- List.rev v.links;
      v.events <- List.rev v.events;
      v.attributes <- List.rev v.attributes;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.trace_state <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.parent_span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(5)" pk
    | Some (6, Pbrt.Varint) -> begin
      v.kind <- decode_pb_span_span_kind d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(6)" pk
    | Some (7, Pbrt.Bits64) -> begin
      v.start_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(7)" pk
    | Some (8, Pbrt.Bits64) -> begin
      v.end_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(9)" pk
    | Some (10, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.events <- (decode_pb_span_event (Pbrt.Decoder.nested d)) :: v.events;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(11)" pk
    | Some (12, Pbrt.Varint) -> begin
      v.dropped_events_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(12)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.links <- (decode_pb_span_link (Pbrt.Decoder.nested d)) :: v.links;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(13)" pk
    | Some (14, Pbrt.Varint) -> begin
      v.dropped_links_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(14)" pk
    | Some (15, Pbrt.Bytes) -> begin
      v.status <- Some (decode_pb_status (Pbrt.Decoder.nested d));
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(span), field(15)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    trace_id = v.trace_id;
    span_id = v.span_id;
    trace_state = v.trace_state;
    parent_span_id = v.parent_span_id;
    name = v.name;
    kind = v.kind;
    start_time_unix_nano = v.start_time_unix_nano;
    end_time_unix_nano = v.end_time_unix_nano;
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
    events = v.events;
    dropped_events_count = v.dropped_events_count;
    links = v.links;
    dropped_links_count = v.dropped_links_count;
    status = v.status;
  } : span)

let rec decode_pb_scope_spans d =
  let v = default_scope_spans_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.spans <- List.rev v.spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.scope <- Some (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.spans <- (decode_pb_span (Pbrt.Decoder.nested d)) :: v.spans;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    scope = v.scope;
    spans = v.spans;
    schema_url = v.schema_url;
  } : scope_spans)

let rec decode_pb_resource_spans d =
  let v = default_resource_spans_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.scope_spans <- List.rev v.scope_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.scope_spans <- (decode_pb_scope_spans (Pbrt.Decoder.nested d)) :: v.scope_spans;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_spans), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource = v.resource;
    scope_spans = v.scope_spans;
    schema_url = v.schema_url;
  } : resource_spans)

let rec decode_pb_traces_data d =
  let v = default_traces_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_spans <- List.rev v.resource_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_spans <- (decode_pb_resource_spans (Pbrt.Decoder.nested d)) :: v.resource_spans;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(traces_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_spans = v.resource_spans;
  } : traces_data)
