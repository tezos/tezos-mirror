[@@@ocaml.warning "-27-30-39"]

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

let rec default_severity_number () = (Severity_number_unspecified:severity_number)

let rec default_log_record 
  ?time_unix_nano:((time_unix_nano:int64) = 0L)
  ?observed_time_unix_nano:((observed_time_unix_nano:int64) = 0L)
  ?severity_number:((severity_number:severity_number) = default_severity_number ())
  ?severity_text:((severity_text:string) = "")
  ?body:((body:Common.any_value option) = None)
  ?attributes:((attributes:Common.key_value list) = [])
  ?dropped_attributes_count:((dropped_attributes_count:int32) = 0l)
  ?flags:((flags:int32) = 0l)
  ?trace_id:((trace_id:bytes) = Bytes.create 0)
  ?span_id:((span_id:bytes) = Bytes.create 0)
  () : log_record  = {
  time_unix_nano;
  observed_time_unix_nano;
  severity_number;
  severity_text;
  body;
  attributes;
  dropped_attributes_count;
  flags;
  trace_id;
  span_id;
}

let rec default_scope_logs 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ?log_records:((log_records:log_record list) = [])
  ?schema_url:((schema_url:string) = "")
  () : scope_logs  = {
  scope;
  log_records;
  schema_url;
}

let rec default_resource_logs 
  ?resource:((resource:Resource.resource option) = None)
  ?scope_logs:((scope_logs:scope_logs list) = [])
  ?schema_url:((schema_url:string) = "")
  () : resource_logs  = {
  resource;
  scope_logs;
  schema_url;
}

let rec default_logs_data 
  ?resource_logs:((resource_logs:resource_logs list) = [])
  () : logs_data  = {
  resource_logs;
}

let rec default_log_record_flags () = (Log_record_flags_do_not_use:log_record_flags)

type log_record_mutable = {
  mutable time_unix_nano : int64;
  mutable observed_time_unix_nano : int64;
  mutable severity_number : severity_number;
  mutable severity_text : string;
  mutable body : Common.any_value option;
  mutable attributes : Common.key_value list;
  mutable dropped_attributes_count : int32;
  mutable flags : int32;
  mutable trace_id : bytes;
  mutable span_id : bytes;
}

let default_log_record_mutable () : log_record_mutable = {
  time_unix_nano = 0L;
  observed_time_unix_nano = 0L;
  severity_number = default_severity_number ();
  severity_text = "";
  body = None;
  attributes = [];
  dropped_attributes_count = 0l;
  flags = 0l;
  trace_id = Bytes.create 0;
  span_id = Bytes.create 0;
}

type scope_logs_mutable = {
  mutable scope : Common.instrumentation_scope option;
  mutable log_records : log_record list;
  mutable schema_url : string;
}

let default_scope_logs_mutable () : scope_logs_mutable = {
  scope = None;
  log_records = [];
  schema_url = "";
}

type resource_logs_mutable = {
  mutable resource : Resource.resource option;
  mutable scope_logs : scope_logs list;
  mutable schema_url : string;
}

let default_resource_logs_mutable () : resource_logs_mutable = {
  resource = None;
  scope_logs = [];
  schema_url = "";
}

type logs_data_mutable = {
  mutable resource_logs : resource_logs list;
}

let default_logs_data_mutable () : logs_data_mutable = {
  resource_logs = [];
}


(** {2 Make functions} *)


let rec make_log_record 
  ~(time_unix_nano:int64)
  ~(observed_time_unix_nano:int64)
  ~(severity_number:severity_number)
  ~(severity_text:string)
  ?body:((body:Common.any_value option) = None)
  ~(attributes:Common.key_value list)
  ~(dropped_attributes_count:int32)
  ~(flags:int32)
  ~(trace_id:bytes)
  ~(span_id:bytes)
  () : log_record  = {
  time_unix_nano;
  observed_time_unix_nano;
  severity_number;
  severity_text;
  body;
  attributes;
  dropped_attributes_count;
  flags;
  trace_id;
  span_id;
}

let rec make_scope_logs 
  ?scope:((scope:Common.instrumentation_scope option) = None)
  ~(log_records:log_record list)
  ~(schema_url:string)
  () : scope_logs  = {
  scope;
  log_records;
  schema_url;
}

let rec make_resource_logs 
  ?resource:((resource:Resource.resource option) = None)
  ~(scope_logs:scope_logs list)
  ~(schema_url:string)
  () : resource_logs  = {
  resource;
  scope_logs;
  schema_url;
}

let rec make_logs_data 
  ~(resource_logs:resource_logs list)
  () : logs_data  = {
  resource_logs;
}


[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_severity_number fmt (v:severity_number) =
  match v with
  | Severity_number_unspecified -> Format.fprintf fmt "Severity_number_unspecified"
  | Severity_number_trace -> Format.fprintf fmt "Severity_number_trace"
  | Severity_number_trace2 -> Format.fprintf fmt "Severity_number_trace2"
  | Severity_number_trace3 -> Format.fprintf fmt "Severity_number_trace3"
  | Severity_number_trace4 -> Format.fprintf fmt "Severity_number_trace4"
  | Severity_number_debug -> Format.fprintf fmt "Severity_number_debug"
  | Severity_number_debug2 -> Format.fprintf fmt "Severity_number_debug2"
  | Severity_number_debug3 -> Format.fprintf fmt "Severity_number_debug3"
  | Severity_number_debug4 -> Format.fprintf fmt "Severity_number_debug4"
  | Severity_number_info -> Format.fprintf fmt "Severity_number_info"
  | Severity_number_info2 -> Format.fprintf fmt "Severity_number_info2"
  | Severity_number_info3 -> Format.fprintf fmt "Severity_number_info3"
  | Severity_number_info4 -> Format.fprintf fmt "Severity_number_info4"
  | Severity_number_warn -> Format.fprintf fmt "Severity_number_warn"
  | Severity_number_warn2 -> Format.fprintf fmt "Severity_number_warn2"
  | Severity_number_warn3 -> Format.fprintf fmt "Severity_number_warn3"
  | Severity_number_warn4 -> Format.fprintf fmt "Severity_number_warn4"
  | Severity_number_error -> Format.fprintf fmt "Severity_number_error"
  | Severity_number_error2 -> Format.fprintf fmt "Severity_number_error2"
  | Severity_number_error3 -> Format.fprintf fmt "Severity_number_error3"
  | Severity_number_error4 -> Format.fprintf fmt "Severity_number_error4"
  | Severity_number_fatal -> Format.fprintf fmt "Severity_number_fatal"
  | Severity_number_fatal2 -> Format.fprintf fmt "Severity_number_fatal2"
  | Severity_number_fatal3 -> Format.fprintf fmt "Severity_number_fatal3"
  | Severity_number_fatal4 -> Format.fprintf fmt "Severity_number_fatal4"

let rec pp_log_record fmt (v:log_record) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "time_unix_nano" Pbrt.Pp.pp_int64 fmt v.time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "observed_time_unix_nano" Pbrt.Pp.pp_int64 fmt v.observed_time_unix_nano;
    Pbrt.Pp.pp_record_field ~first:false "severity_number" pp_severity_number fmt v.severity_number;
    Pbrt.Pp.pp_record_field ~first:false "severity_text" Pbrt.Pp.pp_string fmt v.severity_text;
    Pbrt.Pp.pp_record_field ~first:false "body" (Pbrt.Pp.pp_option Common.pp_any_value) fmt v.body;
    Pbrt.Pp.pp_record_field ~first:false "attributes" (Pbrt.Pp.pp_list Common.pp_key_value) fmt v.attributes;
    Pbrt.Pp.pp_record_field ~first:false "dropped_attributes_count" Pbrt.Pp.pp_int32 fmt v.dropped_attributes_count;
    Pbrt.Pp.pp_record_field ~first:false "flags" Pbrt.Pp.pp_int32 fmt v.flags;
    Pbrt.Pp.pp_record_field ~first:false "trace_id" Pbrt.Pp.pp_bytes fmt v.trace_id;
    Pbrt.Pp.pp_record_field ~first:false "span_id" Pbrt.Pp.pp_bytes fmt v.span_id;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_scope_logs fmt (v:scope_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "scope" (Pbrt.Pp.pp_option Common.pp_instrumentation_scope) fmt v.scope;
    Pbrt.Pp.pp_record_field ~first:false "log_records" (Pbrt.Pp.pp_list pp_log_record) fmt v.log_records;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_resource_logs fmt (v:resource_logs) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource" (Pbrt.Pp.pp_option Resource.pp_resource) fmt v.resource;
    Pbrt.Pp.pp_record_field ~first:false "scope_logs" (Pbrt.Pp.pp_list pp_scope_logs) fmt v.scope_logs;
    Pbrt.Pp.pp_record_field ~first:false "schema_url" Pbrt.Pp.pp_string fmt v.schema_url;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_logs_data fmt (v:logs_data) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list pp_resource_logs) fmt v.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_log_record_flags fmt (v:log_record_flags) =
  match v with
  | Log_record_flags_do_not_use -> Format.fprintf fmt "Log_record_flags_do_not_use"
  | Log_record_flags_trace_flags_mask -> Format.fprintf fmt "Log_record_flags_trace_flags_mask"

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_severity_number (v:severity_number) encoder =
  match v with
  | Severity_number_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Severity_number_trace -> Pbrt.Encoder.int_as_varint 1 encoder
  | Severity_number_trace2 -> Pbrt.Encoder.int_as_varint 2 encoder
  | Severity_number_trace3 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Severity_number_trace4 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Severity_number_debug -> Pbrt.Encoder.int_as_varint 5 encoder
  | Severity_number_debug2 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Severity_number_debug3 -> Pbrt.Encoder.int_as_varint 7 encoder
  | Severity_number_debug4 -> Pbrt.Encoder.int_as_varint 8 encoder
  | Severity_number_info -> Pbrt.Encoder.int_as_varint 9 encoder
  | Severity_number_info2 -> Pbrt.Encoder.int_as_varint 10 encoder
  | Severity_number_info3 -> Pbrt.Encoder.int_as_varint 11 encoder
  | Severity_number_info4 -> Pbrt.Encoder.int_as_varint 12 encoder
  | Severity_number_warn -> Pbrt.Encoder.int_as_varint 13 encoder
  | Severity_number_warn2 -> Pbrt.Encoder.int_as_varint 14 encoder
  | Severity_number_warn3 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Severity_number_warn4 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Severity_number_error -> Pbrt.Encoder.int_as_varint 17 encoder
  | Severity_number_error2 -> Pbrt.Encoder.int_as_varint 18 encoder
  | Severity_number_error3 -> Pbrt.Encoder.int_as_varint 19 encoder
  | Severity_number_error4 -> Pbrt.Encoder.int_as_varint 20 encoder
  | Severity_number_fatal -> Pbrt.Encoder.int_as_varint 21 encoder
  | Severity_number_fatal2 -> Pbrt.Encoder.int_as_varint 22 encoder
  | Severity_number_fatal3 -> Pbrt.Encoder.int_as_varint 23 encoder
  | Severity_number_fatal4 -> Pbrt.Encoder.int_as_varint 24 encoder

let rec encode_pb_log_record (v:log_record) encoder = 
  Pbrt.Encoder.int64_as_bits64 v.time_unix_nano encoder;
  Pbrt.Encoder.key 1 Pbrt.Bits64 encoder; 
  Pbrt.Encoder.int64_as_bits64 v.observed_time_unix_nano encoder;
  Pbrt.Encoder.key 11 Pbrt.Bits64 encoder; 
  encode_pb_severity_number v.severity_number encoder;
  Pbrt.Encoder.key 2 Pbrt.Varint encoder; 
  Pbrt.Encoder.string v.severity_text encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  begin match v.body with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_any_value x encoder;
    Pbrt.Encoder.key 5 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Common.encode_pb_key_value x encoder;
    Pbrt.Encoder.key 6 Pbrt.Bytes encoder; 
  ) v.attributes encoder;
  Pbrt.Encoder.int32_as_varint v.dropped_attributes_count encoder;
  Pbrt.Encoder.key 7 Pbrt.Varint encoder; 
  Pbrt.Encoder.int32_as_bits32 v.flags encoder;
  Pbrt.Encoder.key 8 Pbrt.Bits32 encoder; 
  Pbrt.Encoder.bytes v.trace_id encoder;
  Pbrt.Encoder.key 9 Pbrt.Bytes encoder; 
  Pbrt.Encoder.bytes v.span_id encoder;
  Pbrt.Encoder.key 10 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_scope_logs (v:scope_logs) encoder = 
  begin match v.scope with
  | Some x -> 
    Pbrt.Encoder.nested Common.encode_pb_instrumentation_scope x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_log_record x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.log_records encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_resource_logs (v:resource_logs) encoder = 
  begin match v.resource with
  | Some x -> 
    Pbrt.Encoder.nested Resource.encode_pb_resource x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_scope_logs x encoder;
    Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ) v.scope_logs encoder;
  Pbrt.Encoder.string v.schema_url encoder;
  Pbrt.Encoder.key 3 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_logs_data (v:logs_data) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested encode_pb_resource_logs x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_logs encoder;
  ()

let rec encode_pb_log_record_flags (v:log_record_flags) encoder =
  match v with
  | Log_record_flags_do_not_use -> Pbrt.Encoder.int_as_varint (0) encoder
  | Log_record_flags_trace_flags_mask -> Pbrt.Encoder.int_as_varint 255 encoder

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_severity_number d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Severity_number_unspecified:severity_number)
  | 1 -> (Severity_number_trace:severity_number)
  | 2 -> (Severity_number_trace2:severity_number)
  | 3 -> (Severity_number_trace3:severity_number)
  | 4 -> (Severity_number_trace4:severity_number)
  | 5 -> (Severity_number_debug:severity_number)
  | 6 -> (Severity_number_debug2:severity_number)
  | 7 -> (Severity_number_debug3:severity_number)
  | 8 -> (Severity_number_debug4:severity_number)
  | 9 -> (Severity_number_info:severity_number)
  | 10 -> (Severity_number_info2:severity_number)
  | 11 -> (Severity_number_info3:severity_number)
  | 12 -> (Severity_number_info4:severity_number)
  | 13 -> (Severity_number_warn:severity_number)
  | 14 -> (Severity_number_warn2:severity_number)
  | 15 -> (Severity_number_warn3:severity_number)
  | 16 -> (Severity_number_warn4:severity_number)
  | 17 -> (Severity_number_error:severity_number)
  | 18 -> (Severity_number_error2:severity_number)
  | 19 -> (Severity_number_error3:severity_number)
  | 20 -> (Severity_number_error4:severity_number)
  | 21 -> (Severity_number_fatal:severity_number)
  | 22 -> (Severity_number_fatal2:severity_number)
  | 23 -> (Severity_number_fatal3:severity_number)
  | 24 -> (Severity_number_fatal4:severity_number)
  | _ -> Pbrt.Decoder.malformed_variant "severity_number"

let rec decode_pb_log_record d =
  let v = default_log_record_mutable () in
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
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(1)" pk
    | Some (11, Pbrt.Bits64) -> begin
      v.observed_time_unix_nano <- Pbrt.Decoder.int64_as_bits64 d;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(11)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.severity_number <- decode_pb_severity_number d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.severity_text <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(3)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.body <- Some (Common.decode_pb_any_value (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.attributes <- (Common.decode_pb_key_value (Pbrt.Decoder.nested d)) :: v.attributes;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(6)" pk
    | Some (7, Pbrt.Varint) -> begin
      v.dropped_attributes_count <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(7)" pk
    | Some (8, Pbrt.Bits32) -> begin
      v.flags <- Pbrt.Decoder.int32_as_bits32 d;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.trace_id <- Pbrt.Decoder.bytes d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.span_id <- Pbrt.Decoder.bytes d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_record), field(10)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    time_unix_nano = v.time_unix_nano;
    observed_time_unix_nano = v.observed_time_unix_nano;
    severity_number = v.severity_number;
    severity_text = v.severity_text;
    body = v.body;
    attributes = v.attributes;
    dropped_attributes_count = v.dropped_attributes_count;
    flags = v.flags;
    trace_id = v.trace_id;
    span_id = v.span_id;
  } : log_record)

let rec decode_pb_scope_logs d =
  let v = default_scope_logs_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.log_records <- List.rev v.log_records;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.scope <- Some (Common.decode_pb_instrumentation_scope (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.log_records <- (decode_pb_log_record (Pbrt.Decoder.nested d)) :: v.log_records;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(scope_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    scope = v.scope;
    log_records = v.log_records;
    schema_url = v.schema_url;
  } : scope_logs)

let rec decode_pb_resource_logs d =
  let v = default_resource_logs_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.scope_logs <- List.rev v.scope_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource <- Some (Resource.decode_pb_resource (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.scope_logs <- (decode_pb_scope_logs (Pbrt.Decoder.nested d)) :: v.scope_logs;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.schema_url <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_logs), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource = v.resource;
    scope_logs = v.scope_logs;
    schema_url = v.schema_url;
  } : resource_logs)

let rec decode_pb_logs_data d =
  let v = default_logs_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_logs <- List.rev v.resource_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_logs <- (decode_pb_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(logs_data), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_logs = v.resource_logs;
  } : logs_data)

let rec decode_pb_log_record_flags d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Log_record_flags_do_not_use:log_record_flags)
  | 255 -> (Log_record_flags_trace_flags_mask:log_record_flags)
  | _ -> Pbrt.Decoder.malformed_variant "log_record_flags"
