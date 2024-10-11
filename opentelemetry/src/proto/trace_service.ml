[@@@ocaml.warning "-27-30-39"]

type export_trace_service_request = {
  resource_spans : Trace.resource_spans list;
}

type export_trace_partial_success = {
  rejected_spans : int64;
  error_message : string;
}

type export_trace_service_response = {
  partial_success : export_trace_partial_success option;
}

let rec default_export_trace_service_request 
  ?resource_spans:((resource_spans:Trace.resource_spans list) = [])
  () : export_trace_service_request  = {
  resource_spans;
}

let rec default_export_trace_partial_success 
  ?rejected_spans:((rejected_spans:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_trace_partial_success  = {
  rejected_spans;
  error_message;
}

let rec default_export_trace_service_response 
  ?partial_success:((partial_success:export_trace_partial_success option) = None)
  () : export_trace_service_response  = {
  partial_success;
}

type export_trace_service_request_mutable = {
  mutable resource_spans : Trace.resource_spans list;
}

let default_export_trace_service_request_mutable () : export_trace_service_request_mutable = {
  resource_spans = [];
}

type export_trace_partial_success_mutable = {
  mutable rejected_spans : int64;
  mutable error_message : string;
}

let default_export_trace_partial_success_mutable () : export_trace_partial_success_mutable = {
  rejected_spans = 0L;
  error_message = "";
}

type export_trace_service_response_mutable = {
  mutable partial_success : export_trace_partial_success option;
}

let default_export_trace_service_response_mutable () : export_trace_service_response_mutable = {
  partial_success = None;
}


(** {2 Make functions} *)

let rec make_export_trace_service_request 
  ~(resource_spans:Trace.resource_spans list)
  () : export_trace_service_request  = {
  resource_spans;
}

let rec make_export_trace_partial_success 
  ~(rejected_spans:int64)
  ~(error_message:string)
  () : export_trace_partial_success  = {
  rejected_spans;
  error_message;
}

let rec make_export_trace_service_response 
  ?partial_success:((partial_success:export_trace_partial_success option) = None)
  () : export_trace_service_response  = {
  partial_success;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_export_trace_service_request fmt (v:export_trace_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_spans" (Pbrt.Pp.pp_list Trace.pp_resource_spans) fmt v.resource_spans;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_trace_partial_success fmt (v:export_trace_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_spans" Pbrt.Pp.pp_int64 fmt v.rejected_spans;
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.error_message;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_trace_service_response fmt (v:export_trace_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_trace_partial_success) fmt v.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_export_trace_service_request (v:export_trace_service_request) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Trace.encode_pb_resource_spans x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_spans encoder;
  ()

let rec encode_pb_export_trace_partial_success (v:export_trace_partial_success) encoder = 
  Pbrt.Encoder.int64_as_varint v.rejected_spans encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_export_trace_service_response (v:export_trace_service_response) encoder = 
  begin match v.partial_success with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_export_trace_partial_success x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_export_trace_service_request d =
  let v = default_export_trace_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_spans <- List.rev v.resource_spans;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_spans <- (Trace.decode_pb_resource_spans (Pbrt.Decoder.nested d)) :: v.resource_spans;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_spans = v.resource_spans;
  } : export_trace_service_request)

let rec decode_pb_export_trace_partial_success d =
  let v = default_export_trace_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_spans <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    rejected_spans = v.rejected_spans;
    error_message = v.error_message;
  } : export_trace_partial_success)

let rec decode_pb_export_trace_service_response d =
  let v = default_export_trace_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_pb_export_trace_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_trace_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    partial_success = v.partial_success;
  } : export_trace_service_response)
