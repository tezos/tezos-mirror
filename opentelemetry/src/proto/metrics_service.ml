[@@@ocaml.warning "-27-30-39"]

type export_metrics_service_request = {
  resource_metrics : Metrics.resource_metrics list;
}

type export_metrics_partial_success = {
  rejected_data_points : int64;
  error_message : string;
}

type export_metrics_service_response = {
  partial_success : export_metrics_partial_success option;
}

let rec default_export_metrics_service_request 
  ?resource_metrics:((resource_metrics:Metrics.resource_metrics list) = [])
  () : export_metrics_service_request  = {
  resource_metrics;
}

let rec default_export_metrics_partial_success 
  ?rejected_data_points:((rejected_data_points:int64) = 0L)
  ?error_message:((error_message:string) = "")
  () : export_metrics_partial_success  = {
  rejected_data_points;
  error_message;
}

let rec default_export_metrics_service_response 
  ?partial_success:((partial_success:export_metrics_partial_success option) = None)
  () : export_metrics_service_response  = {
  partial_success;
}

type export_metrics_service_request_mutable = {
  mutable resource_metrics : Metrics.resource_metrics list;
}

let default_export_metrics_service_request_mutable () : export_metrics_service_request_mutable = {
  resource_metrics = [];
}

type export_metrics_partial_success_mutable = {
  mutable rejected_data_points : int64;
  mutable error_message : string;
}

let default_export_metrics_partial_success_mutable () : export_metrics_partial_success_mutable = {
  rejected_data_points = 0L;
  error_message = "";
}

type export_metrics_service_response_mutable = {
  mutable partial_success : export_metrics_partial_success option;
}

let default_export_metrics_service_response_mutable () : export_metrics_service_response_mutable = {
  partial_success = None;
}


(** {2 Make functions} *)

let rec make_export_metrics_service_request 
  ~(resource_metrics:Metrics.resource_metrics list)
  () : export_metrics_service_request  = {
  resource_metrics;
}

let rec make_export_metrics_partial_success 
  ~(rejected_data_points:int64)
  ~(error_message:string)
  () : export_metrics_partial_success  = {
  rejected_data_points;
  error_message;
}

let rec make_export_metrics_service_response 
  ?partial_success:((partial_success:export_metrics_partial_success option) = None)
  () : export_metrics_service_response  = {
  partial_success;
}

[@@@ocaml.warning "-27-30-39"]

(** {2 Formatters} *)

let rec pp_export_metrics_service_request fmt (v:export_metrics_service_request) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "resource_metrics" (Pbrt.Pp.pp_list Metrics.pp_resource_metrics) fmt v.resource_metrics;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_metrics_partial_success fmt (v:export_metrics_partial_success) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "rejected_data_points" Pbrt.Pp.pp_int64 fmt v.rejected_data_points;
    Pbrt.Pp.pp_record_field ~first:false "error_message" Pbrt.Pp.pp_string fmt v.error_message;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_export_metrics_service_response fmt (v:export_metrics_service_response) = 
  let pp_i fmt () =
    Pbrt.Pp.pp_record_field ~first:true "partial_success" (Pbrt.Pp.pp_option pp_export_metrics_partial_success) fmt v.partial_success;
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Encoding} *)

let rec encode_pb_export_metrics_service_request (v:export_metrics_service_request) encoder = 
  Pbrt.List_util.rev_iter_with (fun x encoder -> 
    Pbrt.Encoder.nested Metrics.encode_pb_resource_metrics x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  ) v.resource_metrics encoder;
  ()

let rec encode_pb_export_metrics_partial_success (v:export_metrics_partial_success) encoder = 
  Pbrt.Encoder.int64_as_varint v.rejected_data_points encoder;
  Pbrt.Encoder.key 1 Pbrt.Varint encoder; 
  Pbrt.Encoder.string v.error_message encoder;
  Pbrt.Encoder.key 2 Pbrt.Bytes encoder; 
  ()

let rec encode_pb_export_metrics_service_response (v:export_metrics_service_response) encoder = 
  begin match v.partial_success with
  | Some x -> 
    Pbrt.Encoder.nested encode_pb_export_metrics_partial_success x encoder;
    Pbrt.Encoder.key 1 Pbrt.Bytes encoder; 
  | None -> ();
  end;
  ()

[@@@ocaml.warning "-27-30-39"]

(** {2 Protobuf Decoding} *)

let rec decode_pb_export_metrics_service_request d =
  let v = default_export_metrics_service_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.resource_metrics <- List.rev v.resource_metrics;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.resource_metrics <- (Metrics.decode_pb_resource_metrics (Pbrt.Decoder.nested d)) :: v.resource_metrics;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_service_request), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    resource_metrics = v.resource_metrics;
  } : export_metrics_service_request)

let rec decode_pb_export_metrics_partial_success d =
  let v = default_export_metrics_partial_success_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.rejected_data_points <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_partial_success), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.error_message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_partial_success), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    rejected_data_points = v.rejected_data_points;
    error_message = v.error_message;
  } : export_metrics_partial_success)

let rec decode_pb_export_metrics_service_response d =
  let v = default_export_metrics_service_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.partial_success <- Some (decode_pb_export_metrics_partial_success (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(export_metrics_service_response), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    partial_success = v.partial_success;
  } : export_metrics_service_response)
