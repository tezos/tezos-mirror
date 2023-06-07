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

type transaction_object = {
  blockHash : string;
  blockNumber : int32;
  from : string;
  gas : int32;
  gasPrice : int32;
  hash : string;
  input : string option;
  nonce : int32;
  to_ : string option;
  transactionIndex : int32;
  value : Wei.t;
  v : int32;
  r : string;
  s : string;
}

let transaction_object_of_json json =
  let open JSON in
  {
    blockHash = json |-> "blockHash" |> as_string;
    blockNumber = json |-> "blockNumber" |> as_int32;
    from = json |-> "from" |> as_string;
    gas = json |-> "gas" |> as_int32;
    gasPrice = json |-> "gasPrice" |> as_int32;
    hash = json |-> "hash" |> as_string;
    input = json |-> "input" |> as_string_opt;
    nonce = json |-> "nonce" |> as_int32;
    to_ = json |-> "to" |> as_string_opt;
    transactionIndex = json |-> "transactionIndex" |> as_int32;
    value = json |-> "value" |> as_string |> Wei.of_string;
    v = json |-> "v" |> as_int32;
    r = json |-> "r" |> as_string;
    s = json |-> "s" |> as_string;
  }

(* as per https://web3js.readthedocs.io/en/v1.2.9/web3-eth.html#gettransactionreceipt *)
type transaction_receipt = {
  status : bool;
  blockHash : string;
  blockNumber : int32;
  transactionHash : string;
  transactionIndex : int32;
  from : string;
  to_ : string option;
  contractAddress : string option;
  cumulativeGasUsed : int32;
  gasUsed : int32;
  logs : string list;
}

let transaction_receipt_of_json json =
  let open JSON in
  {
    status = json |-> "status" |> as_bool;
    blockHash = json |-> "blockHash" |> as_string;
    blockNumber = json |-> "blockNumber" |> as_int32;
    transactionHash = json |-> "transactionHash" |> as_string;
    transactionIndex = json |-> "transactionIndex" |> as_int32;
    from = json |-> "from" |> as_string;
    to_ = json |-> "to" |> as_string_opt;
    contractAddress = json |-> "contractAddress" |> as_string_opt;
    cumulativeGasUsed = json |-> "cumulativeGasUsed" |> as_int32;
    gasUsed = json |-> "gasUsed" |> as_int32;
    logs = json |-> "logs" |> as_list |> List.map as_string;
  }
