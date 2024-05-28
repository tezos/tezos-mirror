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

(** Blocks can contain either only hashes or full transaction objects. As the
    empty list case cannot be discriminated between the two types, `Empty` acts
    as a wildcard for the empty transaction list. *)
type transactions =
  | Hash of string list
  | Full of Transaction.transaction_object list
  | Empty

(** Simplified Ethereum block representation. *)
type t = {
  number : int32;
  hash : string;
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
  gasLimit : int64;
  gasUsed : int64;
  timestamp : int32;
  transactions : transactions;
  uncles : string list;
}

(** Extracts a block {!t} from a JSON. *)
val of_json : JSON.t -> t
