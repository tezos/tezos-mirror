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
      Format.printf "Version\n%!" ;
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

  let hash_f = hash_of_string

  let qty_f = quantity_of_z

  (* Default chain_id for ethereum custom networks with Ganache. *)
  let chain_id = qty_f (Z.of_int 1337)

  let block_height = block_height_of_z Z.zero

  let balance = qty_f @@ Z.of_int64 Int64.max_int

  let gas_price = qty_f Z.one

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
        | Gas_price.Input _ -> return (Gas_price.Output (Ok Mock.gas_price))
        | Get_transaction_count.Input _ ->
            return (Get_transaction_count.Output (Ok Mock.transaction_count))
        | Send_raw_transaction.Input _ ->
            return (Send_raw_transaction.Output (Ok Mock.transaction_hash))
        | _ -> Error_monad.failwith "Unsupported method\n%!"
      in
      return (output, id))

let directory = Directory.empty |> version |> dispatch
