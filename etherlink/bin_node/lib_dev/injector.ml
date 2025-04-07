(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Batch
open Rpc_encodings

let construct_rpc_call ~method_ ~input_encoding parameters =
  JSONRPC.
    {
      method_;
      parameters = Some (Data_encoding.Json.construct input_encoding parameters);
      id = Some (random_id ());
    }

let call_rpc_service ~keep_alive ~base ~path request output_encoding =
  let open Lwt_result_syntax in
  let* response =
    let open Rollup_services in
    call_service
      ~keep_alive
      ~base
      (dispatch_batch_service ~path)
      ()
      ()
      (Singleton request)
  in
  match response with
  | Singleton {value = Ok json; id = _} ->
      return (Ok (Data_encoding.Json.destruct output_encoding json))
  | Singleton {value = Error err; id = _} | Batch [{value = Error err; id = _}]
    ->
      return (Error err.message)
  | Batch _ ->
      return
        (Error
           "Upstream endpoint returned an inconsistent response (more than one \
            result)")

let send_raw_transaction_request raw_tx =
  construct_rpc_call
    ~method_:Send_raw_transaction.method_
    ~input_encoding:Send_raw_transaction.input_encoding
    (Ethereum_types.hex_encode_string raw_tx)

let inject_transaction_request tx_object raw_tx =
  construct_rpc_call
    ~method_:Inject_transaction.method_
    ~input_encoding:Inject_transaction.input_encoding
    (tx_object, raw_tx)

let send_raw_transaction ~keep_alive ~base ~raw_tx =
  call_rpc_service
    ~keep_alive
    ~base
    ~path:Resto.Path.root
    (send_raw_transaction_request raw_tx)
    Send_raw_transaction.output_encoding

let inject_transaction ~keep_alive ~base ~tx_object ~raw_tx =
  call_rpc_service
    ~keep_alive
    ~base
    ~path:Resto.Path.(root / "private")
    (inject_transaction_request tx_object raw_tx)
    Inject_transaction.output_encoding
