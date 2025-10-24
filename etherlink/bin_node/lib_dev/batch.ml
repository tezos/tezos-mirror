(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc
open Rpc_encodings

(* The node can either take a single request or multiple requests at
   once. *)
type 'a batched_request = Singleton of 'a | Batch of 'a list

let batch_encoding kind =
  Data_encoding.(
    union
      [
        case
          ~title:"singleton"
          (Tag 0)
          kind
          (function Singleton i -> Some i | _ -> None)
          (fun i -> Singleton i);
        case
          ~title:"batch"
          (Tag 1)
          (list kind)
          (function Batch i -> Some i | _ -> None)
          (fun i -> Batch i);
      ])

let dispatch_service ~path =
  Service.post_service
    ~query:Query.empty
    ~input:JSONRPC.request_encoding
    ~output:JSONRPC.response_encoding
    path

let dispatch_batch_service ~path =
  Service.post_service
    ~description:"JSONRPC endpoint"
    ~query:Query.empty
    ~input:(batch_encoding JSONRPC.request_encoding)
    ~output:(batch_encoding JSONRPC.response_encoding)
    path

let call (type input output)
    (module R : Rpc_encodings.METHOD
      with type input = input
       and type output = output) ~keep_alive ~timeout ~evm_node_endpoint
    (input : input) =
  let open Lwt_result_syntax in
  let* response =
    Rollup_services.call_service
      ~keep_alive
      ~timeout
      ~base:evm_node_endpoint
      (dispatch_batch_service ~path:Resto.Path.root)
      ()
      ()
      (Singleton
         JSONRPC.
           {
             method_ = R.method_;
             parameters =
               Some (Data_encoding.Json.construct R.input_encoding input);
             id = Some (random_id ());
           })
  in
  match response with
  | Singleton {value = Ok value; _} | Batch [{value = Ok value; _}] ->
      return (Data_encoding.Json.destruct R.output_encoding value)
  | Singleton {value = Error err; _} | Batch [{value = Error err; _}] ->
      failwith
        "Request failed with error %s"
        Data_encoding.Json.(to_string (construct JSONRPC.error_encoding err))
  | Batch l ->
      failwith "request: unexpected number of responses (%d)" List.(length l)
