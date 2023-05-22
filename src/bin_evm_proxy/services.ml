(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_rpc
open Rpc_encodings

let version_service =
  Service.get_service
    ~description:"version"
    ~query:Query.empty
    ~output:Data_encoding.string
    Path.(root / "version")

let version dir =
  Directory.register0 dir version_service (fun () () ->
      Lwt.return_ok Tezos_version.Bin_version.version_string)

(* The proxy server can either take a single request or multiple requests at
   once. *)
type 'a request = Singleton of 'a | Batch of 'a list

let request_encoding kind =
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

let dispatch_service =
  Service.post_service
    ~query:Query.empty
    ~input:(request_encoding Input.encoding)
    ~output:(request_encoding Output.encoding)
    Path.(root)

let get_block ~full_transaction_object block_param
    (module Rollup_node_rpc : Rollup_node.S) =
  match block_param with
  | Ethereum_types.(Hash_param (Block_height n)) ->
      Rollup_node_rpc.nth_block ~full_transaction_object n
  | Latest | Earliest | Pending ->
      Rollup_node_rpc.current_block ~full_transaction_object

let dispatch_input
    ((module Rollup_node_rpc : Rollup_node.S), smart_rollup_address) (input, id)
    =
  let open Lwt_result_syntax in
  let* output =
    match input with
    | Accounts.Input _ -> return (Accounts.Output (Ok []))
    | Network_id.Input _ -> return (Network_id.Output (Ok Mockup.net_version))
    | Chain_id.Input _ -> return (Chain_id.Output (Ok Mockup.chain_id))
    | Get_balance.Input (Some (address, _block_param)) ->
        let* balance = Rollup_node_rpc.balance address in
        return (Get_balance.Output (Ok balance))
    | Block_number.Input _ ->
        let* block_number = Rollup_node_rpc.current_block_number () in
        return (Block_number.Output (Ok block_number))
    | Get_block_by_number.Input (Some (block_param, full_transaction_object)) ->
        let* block =
          get_block
            ~full_transaction_object
            block_param
            (module Rollup_node_rpc)
        in
        return (Get_block_by_number.Output (Ok block))
    | Get_block_by_number.Input None ->
        return (Get_block_by_number.Output (Ok Mockup.(block ())))
    | Get_block_by_hash.Input _ ->
        return (Get_block_by_hash.Output (Ok Mockup.(block ())))
    | Get_code.Input (Some (address, _)) ->
        let* code = Rollup_node_rpc.code address in
        return (Get_code.Output (Ok code))
    | Gas_price.Input _ -> return (Gas_price.Output (Ok Mockup.gas_price))
    | Get_transaction_count.Input (Some (address, _)) ->
        let* nonce = Rollup_node_rpc.nonce address in
        return (Get_transaction_count.Output (Ok nonce))
    | Get_transaction_receipt.Input (Some tx_hash) ->
        let* receipt =
          let*! res = Rollup_node_rpc.transaction_receipt tx_hash in
          match res with Ok x -> return_some x | Error _ -> return_none
        in
        return (Get_transaction_receipt.Output (Ok receipt))
    | Get_transaction_by_hash.Input (Some tx_hash) ->
        let* transaction_object = Rollup_node_rpc.transaction_object tx_hash in
        return (Get_transaction_by_hash.Output (Ok transaction_object))
    | Send_raw_transaction.Input (Some tx_raw) ->
        let* tx_hash =
          Rollup_node_rpc.inject_raw_transaction ~smart_rollup_address tx_raw
        in
        return (Send_raw_transaction.Output (Ok tx_hash))
    | Send_transaction.Input _ ->
        return (Send_transaction.Output (Ok Mockup.transaction_hash))
    | Eth_call.Input _ -> return (Eth_call.Output (Ok Mockup.call))
    | Get_estimate_gas.Input _ ->
        return (Get_estimate_gas.Output (Ok Mockup.gas_price))
    | Txpool_content.Input _ ->
        let* txpool = Rollup_node_rpc.txpool () in
        return (Txpool_content.Output (Ok txpool))
    | _ -> Error_monad.failwith "Unsupported method\n%!"
  in
  return (output, id)

let dispatch ctx dir =
  Directory.register0 dir dispatch_service (fun () input ->
      let open Lwt_result_syntax in
      match input with
      | Singleton input ->
          let+ output = dispatch_input ctx input in
          Singleton output
      | Batch inputs ->
          let+ outputs = List.map_es (dispatch_input ctx) inputs in
          Batch outputs)

let directory (rollup_node_config : (module Rollup_node.S) * string) =
  Directory.empty |> version |> dispatch rollup_node_config
