[@@@ocaml.warning "-27-30-39"]

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

let rec default_export_logs_service_request 
  ?resource_logs:((resource_logs:Logs.resource_logs list) = [])
  () : export_logs_service_request  = {
  resource_logs;
}

let rec default_export_logs_partial_success 
  ?rejected_log_records:((rejected_log_records:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_logs_partial_success  = {
  rejected_log_records;
  error_message;
}

let rec default_export_logs_service_response 
  ?partial_success:((partial_success:export_logs_partial_success option) = None)
  () : export_logs_service_response  = {
  partial_success;
}

type export_logs_service_request_mutable = {
  mutable resource_logs : Logs.resource_logs list;
}

let default_export_logs_service_request_mutable () : export_logs_service_request_mutable = {
  resource_logs = [];
}

type export_logs_partial_success_mutable = {
  mutable rejected_log_records : int64;
  mutable error_message : string;
}

let default_export_logs_partial_success_mutable () : export_logs_partial_success_mutable = {
  rejected_log_records = 0L;
  error_message = "";
}

type export_logs_service_response_mutable = {
  mutable partial_success : export_logs_partial_success option;
}

let default_export_logs_service_response_mutable () : export_logs_service_response_mutable = {
  partial_success = None;
}


(** {2 Make functions} *)

let rec make_export_logs_service_request 
  ~(resource_logs:Logs.resource_logs list)
  () : export_logs_service_request  = {
  resource_logs;
}

let rec make_export_logs_partial_success 
  ~(rejected_log_records:int64)
  ~(error_message:string)
  () : export_logs_partial_success  = {
  rejected_log_records;
  error_message;
}

let rec make_export_logs_service_response 
  ?partial_success:((partial_success:export_logs_partial_success option) = None)
  () : export_logs_service_response  = {
  partial_success;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_export_logs_service_request fmt (v:export_logs_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_logs" (Pbrt.Pp.pp_list Logs.pp_resource_logs) fmt v.resource_logs;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_logs_partial_success fmt (v:export_logs_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_log_records" Pbrt.Pp.pp_int64 fmt v.rejected_log_records;
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.error_message;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_logs_service_response fmt (v:export_logs_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_logs_partial_success) fmt v.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_export_logs_service_request (v:export_logs_service_request) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Logs.encode_pb_resource_logs x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_logs encoder;
  ()

let rec encode_pb_export_logs_partial_success (v:export_logs_partial_success) encoder = 
  Pbrt.Encoder.int64_as_varint v.rejected_log_records encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_export_logs_service_response (v:export_logs_service_response) encoder = 
  begin match v.partial_success with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_export_logs_partial_success x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_export_logs_service_request d =
  let v = default_export_logs_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_logs <- List.rev v.resource_logs;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_logs <- (Logs.decode_pb_resource_logs (Pbrt.Decoder.nested d)) :: v.resource_logs;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_logs = v.resource_logs;
  } : export_logs_service_request)

let rec decode_pb_export_logs_partial_success d =
  let v = default_export_logs_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_log_records <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    rejected_log_records = v.rejected_log_records;
    error_message = v.error_message;
  } : export_logs_partial_success)

let rec decode_pb_export_logs_service_response d =
  let v = default_export_logs_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_pb_export_logs_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_logs_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    partial_success = v.partial_success;
  } : export_logs_service_response)
