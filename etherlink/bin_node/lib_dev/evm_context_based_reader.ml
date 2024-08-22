(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type state = Evm_state.t

let get_state ?(block = Ethereum_types.Block_parameter.(Block_parameter Latest))
    () =
  Evm_context.get_evm_state block

let read state path =
  let open Lwt_result_syntax in
  let*! res = Evm_state.inspect state path in
  return res

let subkeys state path =
  let open Lwt_result_syntax in
  let*! res = Evm_state.subkeys state path in
  return res

let simulate_and_read state ~input =
  let open Lwt_result_syntax in
  let* raw_insights = Evm_context.execute_and_inspect state input in
  match raw_insights with
  | [Some bytes] -> return bytes
  | _ -> Error_monad.failwith "Invalid insights format"
