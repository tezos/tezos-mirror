(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let send_raw_transaction_method txn =
  let open Rpc_encodings in
  let message = Ethereum_types.hex_encode_string txn in
  JSONRPC.
    {
      method_ = Send_raw_transaction.method_;
      parameters =
        Some
          (Data_encoding.Json.construct
             Send_raw_transaction.input_encoding
             message);
      id = None;
    }

let send_raw_transaction ~keep_alive ~base raw_txn =
  let open Lwt_result_syntax in
  let* response =
    let open Rollup_services in
    call_service
      ~keep_alive
      ~base
      (Services.dispatch_service ~path:Resto.Path.root)
      ()
      ()
      (Singleton (send_raw_transaction_method raw_txn))
  in

  match response with
  | Singleton {value = Ok json; id = _} ->
      let open Rpc_encodings in
      let hash =
        Data_encoding.Json.destruct Send_raw_transaction.output_encoding json
      in
      return (Ok hash)
  | Singleton {value = Error err; id = _} | Batch [{value = Error err; id = _}]
    ->
      return (Error err.message)
  | Batch _ ->
      (* should not be possible, we consider it failed *)
      return
        (Error
           "Upstream EVM endpoint returned an inconsistent response (more than \
            one result)")
