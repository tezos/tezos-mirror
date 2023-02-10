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
  let chain_id = qty_f (Z.of_int 1337)

  let block_height = block_height_of_z Z.zero

  let balance = qty_f @@ Conv.to_wei 1000

  let code = hash_f @@ String.make 100 'a'

  (* Gas limit must be at least 21000 *)
  let gas_price = qty_f @@ Z.of_int 21000

  let transaction_count = qty_f Z.zero

  (* Transaction's hash must be an alphanumeric 66 utf8 byte
     hex (chars: a-fA-F) string *)
  let transaction_hash = hash_f @@ "0x" ^ String.make 64 'a'

  let block =
    {
      number = Some (block_height_of_z Z.zero);
      hash = Some (block_hash_of_string @@ String.make 32 'a');
      parent = block_hash_of_string (String.make 32 'a');
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
      transaction = [];
      uncles = [];
    }

  let transaction_receipt =
    {
      transactionHash = transaction_hash;
      transactionIndex = qty_f Z.one;
      blockHash = block_hash_of_string @@ String.make 32 'a';
      blockNumber = qty_f Z.one;
      from = Address "0x6F4d14B90C48bEFb49CA3fe6663dEC70731A8bC7";
      to_ = Address "0xA5A5bf58c7Dc91cBE5005A7E5c6314998Eda479E";
      cumulativeGasUsed = gas_price;
      effectiveGasPrice = gas_price;
      gasUsed = gas_price;
      logs = [];
      logsBloom = hash_f @@ String.make 256 'a';
      type_ = hash_f "0x00";
      status = qty_f Z.one;
      root = hash_f @@ String.make 32 'a';
    }

  let call = hash_f "0x"
end

let dispatch dir =
  Directory.register0 dir dispatch_service (fun () (input, id) ->
      let open Lwt_result_syntax in
      let* output =
        match input with
        | Accounts.Input _ -> return (Accounts.Output (Ok []))
        | Network_id.Input _ -> return (Network_id.Output (Ok Mock.chain_id))
        | Chain_id.Input _ -> return (Chain_id.Output (Ok Mock.chain_id))
        | Get_balance.Input _ -> return (Get_balance.Output (Ok Mock.balance))
        | Block_number.Input _ ->
            return (Block_number.Output (Ok Mock.block_height))
        | Get_block_by_number.Input _ ->
            return (Get_block_by_number.Output (Ok Mock.block))
        | Get_code.Input _ -> return (Get_code.Output (Ok Mock.code))
        | Gas_price.Input _ -> return (Gas_price.Output (Ok Mock.gas_price))
        | Get_transaction_count.Input _ ->
            return (Get_transaction_count.Output (Ok Mock.transaction_count))
        | Get_transaction_receipt.Input _ ->
            return
              (Get_transaction_receipt.Output (Ok Mock.transaction_receipt))
        | Send_raw_transaction.Input _ ->
            return (Send_raw_transaction.Output (Ok Mock.transaction_hash))
        | Send_transaction.Input _ ->
            return (Send_transaction.Output (Ok Mock.transaction_hash))
        | Eth_call.Input _ -> return (Eth_call.Output (Ok Mock.call))
        | Get_estimate_gas.Input _ ->
            return (Get_estimate_gas.Output (Ok Mock.gas_price))
        | _ -> Error_monad.failwith "Unsupported method\n%!"
      in
      return (output, id))

let directory = Directory.empty |> version |> dispatch
