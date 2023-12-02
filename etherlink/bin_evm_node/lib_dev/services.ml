(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

let client_version =
  Format.sprintf
    "%s/%s-%s/%s/ocamlc.%s"
    "octez-evm-node"
    (Tezos_version.Version.to_string
       Tezos_version_value.Current_git_info.version)
    Tezos_version_value.Current_git_info.abbreviated_commit_hash
    Stdlib.Sys.os_type
    Stdlib.Sys.ocaml_version

let version dir =
  Directory.register0 dir version_service (fun () () ->
      Lwt.return_ok client_version)

(* The node can either take a single request or multiple requests at
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

let get_block_by_number ~full_transaction_object block_param
    (module Rollup_node_rpc : Services_backend_sig.S) =
  match block_param with
  | Ethereum_types.(Hash_param (Block_height n)) ->
      Rollup_node_rpc.nth_block ~full_transaction_object n
  | Latest | Earliest | Pending ->
      Rollup_node_rpc.current_block ~full_transaction_object

let get_transaction_from_index block index
    (module Rollup_node_rpc : Services_backend_sig.S) =
  let open Lwt_result_syntax in
  match block.Ethereum_types.transactions with
  | TxHash l -> (
      match List.nth_opt l index with
      | None -> return_none
      | Some hash -> Rollup_node_rpc.transaction_object hash)
  | TxFull l -> return @@ List.nth_opt l index

let block_transaction_count block =
  Ethereum_types.quantity_of_z @@ Z.of_int
  @@
  match block.Ethereum_types.transactions with
  | TxHash l -> List.length l
  | TxFull l -> List.length l

let dispatch_input (config : 'a Configuration.t)
    ((module Rollup_node_rpc : Services_backend_sig.S), _) (input, id) =
  let open Lwt_result_syntax in
  let dispatch_input_aux : type w. w input -> w output tzresult Lwt.t = function
    (* INTERNAL RPCs *)
    | Kernel_version.Input _ ->
        let* kernel_version = Rollup_node_rpc.kernel_version () in
        return (Kernel_version.Output (Ok kernel_version))
    (* ETHEREUM JSON-RPC API *)
    | Accounts.Input _ -> return (Accounts.Output (Ok []))
    | Network_id.Input _ ->
        let* (Qty chain_id) = Rollup_node_rpc.chain_id () in
        let net_version = Z.to_string chain_id in
        return (Network_id.Output (Ok net_version))
    | Chain_id.Input _ ->
        let* chain_id = Rollup_node_rpc.chain_id () in
        return (Chain_id.Output (Ok chain_id))
    | Get_balance.Input (Some (address, _block_param)) ->
        let* balance = Rollup_node_rpc.balance address in
        return (Get_balance.Output (Ok balance))
    | Get_storage_at.Input (Some (address, position, _block_param)) ->
        let* value = Rollup_node_rpc.storage_at address position in
        return (Get_storage_at.Output (Ok value))
    | Block_number.Input _ ->
        let* block_number = Rollup_node_rpc.current_block_number () in
        return (Block_number.Output (Ok block_number))
    | Get_block_by_number.Input (Some (block_param, full_transaction_object)) ->
        let* block =
          get_block_by_number
            ~full_transaction_object
            block_param
            (module Rollup_node_rpc)
        in
        return (Get_block_by_number.Output (Ok block))
    | Get_block_by_hash.Input (Some (block_hash, full_transaction_object)) ->
        let* block =
          Rollup_node_rpc.block_by_hash ~full_transaction_object block_hash
        in
        return (Get_block_by_hash.Output (Ok block))
    | Get_code.Input (Some (address, _)) ->
        let* code = Rollup_node_rpc.code address in
        return (Get_code.Output (Ok code))
    | Gas_price.Input _ ->
        let* base_fee = Rollup_node_rpc.base_fee_per_gas () in
        return (Gas_price.Output (Ok base_fee))
    | Get_transaction_count.Input (Some (address, _)) ->
        let* nonce = Tx_pool.nonce address in
        return (Get_transaction_count.Output (Ok nonce))
    | Get_block_transaction_count_by_hash.Input (Some block_hash) ->
        let* block =
          Rollup_node_rpc.block_by_hash
            ~full_transaction_object:false
            block_hash
        in
        return
          (Get_block_transaction_count_by_hash.Output
             (Ok (block_transaction_count block)))
    | Get_block_transaction_count_by_number.Input (Some block_param) ->
        let* block =
          get_block_by_number
            ~full_transaction_object:false
            block_param
            (module Rollup_node_rpc)
        in
        return
          (Get_block_transaction_count_by_number.Output
             (Ok (block_transaction_count block)))
    | Get_uncle_count_by_block_hash.Input (Some _block_hash) ->
        (* A block cannot have uncles. *)
        return (Get_uncle_count_by_block_hash.Output (Ok (Qty Z.zero)))
    | Get_uncle_count_by_block_number.Input (Some _block_param) ->
        (* A block cannot have uncles. *)
        return (Get_uncle_count_by_block_number.Output (Ok (Qty Z.zero)))
    | Get_transaction_receipt.Input (Some tx_hash) ->
        let* receipt = Rollup_node_rpc.transaction_receipt tx_hash in
        return (Get_transaction_receipt.Output (Ok receipt))
    | Get_transaction_by_hash.Input (Some tx_hash) ->
        let* transaction_object = Rollup_node_rpc.transaction_object tx_hash in
        return (Get_transaction_by_hash.Output (Ok transaction_object))
    | Get_transaction_by_block_hash_and_index.Input
        (Some (block_hash, Qty index)) ->
        let* block =
          Rollup_node_rpc.block_by_hash
            ~full_transaction_object:false
            block_hash
        in
        let* transaction_object =
          get_transaction_from_index
            block
            (Z.to_int index)
            (module Rollup_node_rpc)
        in
        return
          (Get_transaction_by_block_hash_and_index.Output
             (Ok transaction_object))
    | Get_transaction_by_block_number_and_index.Input
        (Some (block_number, Qty index)) ->
        let* block =
          get_block_by_number
            ~full_transaction_object:false
            block_number
            (module Rollup_node_rpc)
        in
        let* transaction_object =
          get_transaction_from_index
            block
            (Z.to_int index)
            (module Rollup_node_rpc)
        in
        return
          (Get_transaction_by_block_number_and_index.Output
             (Ok transaction_object))
    | Get_uncle_by_block_hash_and_index.Input (Some (_hash, _index)) ->
        (* A block cannot have uncles. *)
        return (Get_uncle_by_block_hash_and_index.Output (Ok None))
    | Get_uncle_by_block_number_and_index.Input (Some (_number, _index)) ->
        (* A block cannot have uncles. *)
        return (Get_uncle_by_block_number_and_index.Output (Ok None))
    | Send_raw_transaction.Input (Some tx_raw) -> (
        let* tx_hash = Tx_pool.add (Ethereum_types.hex_to_bytes tx_raw) in
        match tx_hash with
        | Ok tx_hash -> return (Send_raw_transaction.Output (Ok tx_hash))
        | Error reason ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/6229 *)
            return
              (Send_raw_transaction.Output
                 (Error {code = -32000; message = reason; data = None}))
        (* By default, the current dispatch handles the inputs *))
    | Eth_call.Input (Some (call, _)) ->
        let* call_result = Rollup_node_rpc.simulate_call call in
        return (Eth_call.Output (Ok call_result))
    | Get_estimate_gas.Input (Some (call, _)) ->
        let* gas = Rollup_node_rpc.estimate_gas call in
        return (Get_estimate_gas.Output (Ok gas))
    | Txpool_content.Input _ ->
        return
          (Txpool_content.Output
             (Ok
                Ethereum_types.
                  {pending = AddressMap.empty; queued = AddressMap.empty}))
    | Web3_clientVersion.Input _ ->
        return (Web3_clientVersion.Output (Ok client_version))
    | Web3_sha3.Input (Some data) ->
        let open Ethereum_types in
        let (Hex h) = data in
        let bytes = Hex.to_bytes_exn (`Hex h) in
        let hash_bytes = Tezos_crypto.Hacl.Hash.Keccak_256.digest bytes in
        let hash = Hex.of_bytes hash_bytes |> Hex.show in
        return (Web3_sha3.Output (Ok (Hash (Hex hash))))
    | Get_logs.Input (Some filter) ->
        let+ logs =
          Filter_helpers.get_logs
            config.log_filter
            (module Rollup_node_rpc)
            filter
        in
        Get_logs.Output (Ok logs)
    | _ -> Error_monad.failwith "Unsupported method\n%!"
  in
  let* output = dispatch_input_aux input in
  if config.verbose then
    Data_encoding.Json.construct Output.encoding (Output.Box output, id)
    |> Data_encoding.Json.to_string |> Printf.printf "%s\n%!" ;
  return (output, id)

let dispatch config ctx dir =
  Directory.register0 dir dispatch_service (fun () input ->
      let open Lwt_result_syntax in
      match input with
      | Singleton (Box input, rpc) ->
          let+ output, rpc = dispatch_input config ctx (input, rpc) in
          Singleton (Output.Box output, rpc)
      | Batch inputs ->
          let+ outputs =
            List.map_es
              (fun (Input.Box input, rpc) ->
                let+ output, rpc = dispatch_input config ctx (input, rpc) in
                (Output.Box output, rpc))
              inputs
          in
          Batch outputs)

let directory config
    ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address) =
  Directory.empty |> version
  |> dispatch
       config
       ((module Rollup_node_rpc : Services_backend_sig.S), smart_rollup_address)
