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

let dispatch_service =
  Service.post_service
    ~query:Query.empty
    ~input:Input.encoding
    ~output:Output.encoding
    Path.(root)

(** Mocked values used until we can retrieve the specific values from an EVM kernel. *)
module Mock = struct
  open Ethereum_types

  module Conv = struct
    let pow_18 = String.make 18 '0'

    (** WEI is the smallest denomination of ether.
        1 ETH = 1,000,000,000,000,000,000 WEI (10^18). *)
    let to_wei x = Z.of_string @@ Int.to_string x ^ pow_18
  end

  let hash_f = hash_of_string

  let qty_f = quantity_of_z

  (* Default chain_id for ethereum custom networks with Ganache. *)
  let id = Z.of_int 1337

  let chain_id = qty_f id

  let net_version = Z.to_string id

  let block_height_counter = ref 0

  let block_height () = block_height_of_z @@ Z.of_int !block_height_counter

  let balance = qty_f @@ Conv.to_wei 1000

  let code = hash_f "0x"

  (* Gas limit must be at least 21000 *)
  let gas_price = qty_f @@ Z.of_int 21000

  let transaction_counter = ref 0

  let transaction_count () = qty_f @@ Z.of_int !transaction_counter

  (* Transaction's hash must be an alphanumeric 66 utf8 byte
     hex (chars: a-fA-F) string *)
  let transaction_hash =
    hash_f @@ "f837c23ac7150b486be21fc00e3e2ad118e12bec1e2bca401b999f544eabc402"

  let block_hash =
    Block_hash
      "d28d009fef5019bd9b353d7d9d881bde4870d3c5e418b1faf05fd9f7540994d8"

  let block () =
    {
      number = Some (block_height ());
      hash = Some block_hash;
      parent = Block_hash (String.make 32 'a');
      nonce = hash_f @@ String.make 8 'a';
      sha3Uncles = hash_f @@ String.make 32 'a';
      logsBloom = Some (hash_f @@ String.make 256 'a');
      transactionRoot = hash_f @@ String.make 32 'a';
      stateRoot = hash_f @@ String.make 32 'a';
      receiptRoot = hash_f @@ String.make 32 'a';
      miner = hash_f @@ String.make 20 'b';
      difficulty = qty_f Z.one;
      totalDifficulty = qty_f Z.one;
      extraData = "";
      size = qty_f @@ Z.of_int 12345;
      gasLimit = qty_f @@ Z.of_int 1111111;
      gasUsed = qty_f Z.zero;
      timestamp = qty_f Z.zero;
      transactions = [transaction_hash];
      uncles = [];
    }

  let transaction_receipt () =
    {
      transactionHash = transaction_hash;
      transactionIndex = qty_f Z.zero;
      blockHash = block_hash;
      blockNumber = qty_f @@ Z.of_int !block_height_counter;
      from = address_of_string "6F4d14B90C48bEFb49CA3fe6663dEC70731A8bC7";
      to_ = address_of_string "A5A5bf58c7Dc91cBE5005A7E5c6314998Eda479E";
      contractAddress = None;
      cumulativeGasUsed = gas_price;
      effectiveGasPrice = gas_price;
      gasUsed = gas_price;
      logs = [];
      logsBloom = hash_f @@ String.make 256 'a';
      type_ = qty_f Z.zero;
      status = qty_f Z.one;
    }

  let call = hash_f "0x"
end

(** [inject_raw_transaction rollup_node_rpc ~smart_rollup_address tx_raw]
    crafts the hash of [tx_raw] and sends to the injector a message consisting
    of:
    - First 20 bytes: [smart_rollup_address].
    - Following 32 bytes: crafted transaction hash.
    - Remaining bytes: [tx_raw] in binary format.
*)
let inject_raw_transaction (module Rollup_node_rpc : Rollup_node.S)
    ~smart_rollup_address tx_raw =
  let open Lwt_result_syntax in
  let tx_hash = Tx_hash.hash_to_string (Ethereum_types.hash_to_string tx_raw) in
  let tx_raw = Ethereum_types.hash_to_bytes tx_raw in
  let tx = smart_rollup_address ^ tx_hash ^ tx_raw in
  let* () = Rollup_node_rpc.inject_raw_transaction tx in
  return (Ethereum_types.Hash Hex.(of_string tx_hash |> show))

let get_block ~full_transaction_object block_param
    (module Rollup_node_rpc : Rollup_node.S) =
  match block_param with
  | Ethereum_types.(Hash_param (Block_height n)) ->
      Rollup_node_rpc.nth_block ~full_transaction_object n
  | Latest | Earliest | Pending ->
      Rollup_node_rpc.current_block ~full_transaction_object

let dispatch (rollup_node_config : ((module Rollup_node.S) * string) option) dir
    =
  Directory.register0 dir dispatch_service (fun () (input, id) ->
      let open Lwt_result_syntax in
      let* output =
        match input with
        | Accounts.Input _ -> return (Accounts.Output (Ok []))
        | Network_id.Input _ -> return (Network_id.Output (Ok Mock.net_version))
        | Chain_id.Input _ -> return (Chain_id.Output (Ok Mock.chain_id))
        | Get_balance.Input (Some (address, _block_param)) ->
            let* balance =
              match rollup_node_config with
              | Some ((module Rollup_node_rpc), _smart_rollup_address) ->
                  let* balance = Rollup_node_rpc.balance address in
                  return balance
              | None -> return Mock.balance
            in
            return (Get_balance.Output (Ok balance))
        | Block_number.Input _ ->
            let* block_number =
              match rollup_node_config with
              | Some ((module Rollup_node_rpc), _smart_rollup_address) ->
                  Rollup_node_rpc.current_block_number ()
              | None -> return @@ Mock.block_height ()
            in
            return (Block_number.Output (Ok block_number))
        | Get_block_by_number.Input
            (Some (block_param, full_transaction_object)) ->
            let* block =
              match rollup_node_config with
              | Some (rollup_node_rpc, _smart_rollup_address) ->
                  get_block ~full_transaction_object block_param rollup_node_rpc
              | None -> return @@ Mock.block ()
            in
            return (Get_block_by_number.Output (Ok block))
        | Get_block_by_number.Input None ->
            return (Get_block_by_number.Output (Ok (Mock.block ())))
        | Get_block_by_hash.Input _ ->
            return (Get_block_by_hash.Output (Ok (Mock.block ())))
        | Get_code.Input _ -> return (Get_code.Output (Ok Mock.code))
        | Gas_price.Input _ -> return (Gas_price.Output (Ok Mock.gas_price))
        | Get_transaction_count.Input (Some (address, _)) ->
            let* nonce =
              match rollup_node_config with
              | Some ((module Rollup_node_rpc), _smart_rollup_address) ->
                  Rollup_node_rpc.nonce address
              | None -> return (Mock.transaction_count ())
            in
            return (Get_transaction_count.Output (Ok nonce))
        | Get_transaction_receipt.Input (Some tx_hash) ->
            let* receipt =
              match rollup_node_config with
              | Some ((module Rollup_node_rpc), _smart_rollup_address) -> (
                  let*! res = Rollup_node_rpc.transaction_receipt tx_hash in
                  match res with
                  | Ok x -> return_some x
                  | Error _ -> return_none)
              | None -> return_some (Mock.transaction_receipt ())
            in
            return (Get_transaction_receipt.Output (Ok receipt))
        | Send_raw_transaction.Input (Some tx_raw) ->
            let* tx_hash =
              match rollup_node_config with
              | Some (rollup_node_rpc, smart_rollup_address) ->
                  inject_raw_transaction
                    rollup_node_rpc
                    ~smart_rollup_address
                    tx_raw
              | None ->
                  incr Mock.block_height_counter ;
                  incr Mock.transaction_counter ;

                  return Mock.transaction_hash
            in
            return (Send_raw_transaction.Output (Ok tx_hash))
        | Send_transaction.Input _ ->
            return (Send_transaction.Output (Ok Mock.transaction_hash))
        | Eth_call.Input _ -> return (Eth_call.Output (Ok Mock.call))
        | Get_estimate_gas.Input _ ->
            return (Get_estimate_gas.Output (Ok Mock.gas_price))
        | _ -> Error_monad.failwith "Unsupported method\n%!"
      in
      return (output, id))

let directory (rollup_node_config : ((module Rollup_node.S) * string) option) =
  Directory.empty |> version |> dispatch rollup_node_config
