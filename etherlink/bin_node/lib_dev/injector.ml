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

let call_rpc_service ~keep_alive ~timeout ~base ~path request output_encoding =
  let open Lwt_result_syntax in
  let* response =
    let open Rollup_services in
    call_service
      ~keep_alive
      ~timeout
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

let call_singleton_request (type input output) ~keep_alive ~timeout ~base
    (module Method_ : Rpc_encodings.METHOD
      with type input = input
       and type output = output) request =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.root
    request
    Method_.output_encoding

let send_raw_transaction_request raw_tx =
  construct_rpc_call
    ~method_:Send_raw_transaction.method_
    ~input_encoding:Send_raw_transaction.input_encoding
    (Ethereum_types.hex_encode_string raw_tx)

let send_raw_transaction_sync_request raw_tx timeout block_parameter =
  construct_rpc_call
    ~method_:Send_raw_transaction_sync.method_
    ~input_encoding:Send_raw_transaction_sync.input_encoding
    (raw_tx, timeout, block_parameter)

let inject_transaction_request tx_object raw_tx wait_confirmation =
  construct_rpc_call
    ~method_:Inject_transaction.method_
    ~input_encoding:Inject_transaction.input_encoding
    (tx_object, raw_tx, wait_confirmation)

let inject_tezlink_operation_request op raw_op =
  construct_rpc_call
    ~method_:Inject_tezlink_operation.method_
    ~input_encoding:Inject_tezlink_operation.input_encoding
    (op, raw_op)

let send_raw_transaction ~keep_alive ~timeout ~base ~raw_tx =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.root
    (send_raw_transaction_request raw_tx)
    Send_raw_transaction.output_encoding

let send_raw_transaction_sync ~keep_alive ~timeout ~base ~raw_tx
    ~internal_timeout ~block_parameter =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.root
    (send_raw_transaction_sync_request raw_tx internal_timeout block_parameter)
    Send_raw_transaction_sync.output_encoding

let inject_transaction ~keep_alive ~timeout ~base ~tx_object ~raw_tx
    ~wait_confirmation =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.(root / "private")
    (inject_transaction_request tx_object raw_tx wait_confirmation)
    Inject_transaction.output_encoding

let inject_tezlink_operation ~keep_alive ~timeout ~base ~op ~raw_op =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.(root / "private")
    (inject_tezlink_operation_request op raw_op)
    Inject_tezlink_operation.output_encoding

let get_transaction_count_request address block_param =
  construct_rpc_call
    ~method_:Get_transaction_count.method_
    ~input_encoding:Get_transaction_count.input_encoding
    (address, block_param)

let get_transaction_count ~keep_alive ~timeout ~base address block_param =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.root
    (get_transaction_count_request address block_param)
    Get_transaction_count.output_encoding

let get_transaction_by_hash_request tx_hash =
  construct_rpc_call
    ~method_:Get_transaction_by_hash.method_
    ~input_encoding:Get_transaction_by_hash.input_encoding
    tx_hash

let get_transaction_by_hash ~keep_alive ~timeout ~base tx_hash =
  call_rpc_service
    ~keep_alive
    ~timeout
    ~base
    ~path:Resto.Path.root
    (get_transaction_by_hash_request tx_hash)
    Get_transaction_by_hash.output_encoding
