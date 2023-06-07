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

type t = {
  number : int32;
  hash : string option;
  parent : string;
  nonce : string;
  sha3Uncles : string;
  logsBloom : string option;
  transactionRoot : string;
  stateRoot : string;
  receiptRoot : string;
  miner : string;
  difficulty : int64;
  totalDifficulty : int64;
  extraData : string;
  size : int32;
  gasLimit : int32;
  gasUsed : int32;
  timestamp : int32;
  transactions : string list;
  uncles : string list;
}

let of_json json =
  let open JSON in
  {
    number = json |-> "number" |> as_int32;
    hash = json |-> "hash" |> as_string_opt;
    parent = json |-> "parent" |> as_string;
    nonce = json |-> "nonce" |> as_string;
    sha3Uncles = json |-> "sha3Uncles" |> as_string;
    logsBloom = json |-> "logsBloom" |> as_string_opt;
    transactionRoot = json |-> "transactionRoot" |> as_string;
    stateRoot = json |-> "stateRoot" |> as_string;
    receiptRoot = json |-> "receiptRoot" |> as_string;
    miner = json |-> "miner" |> as_string;
    difficulty = json |-> "difficulty" |> as_int64;
    totalDifficulty = json |-> "totalDifficulty" |> as_int64;
    extraData = json |-> "extraData" |> as_string;
    size = json |-> "size" |> as_int32;
    gasLimit = json |-> "gasLimit" |> as_int32;
    gasUsed = json |-> "gasUsed" |> as_int32;
    timestamp = json |-> "timestamp" |> as_int32;
    transactions = json |-> "transactions" |> as_list |> List.map as_string;
    uncles = json |-> "uncles" |> as_list |> List.map as_string;
  }
