(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let message :
    Raw_context.t ->
    Tx_rollup_message_repr.t ->
    (Raw_context.t * Tx_rollup_message_hash_repr.t) tzresult =
 fun ctxt input ->
  Tx_rollup_gas.hash
    ~hash_f:Tx_rollup_message_hash_repr.hash_bytes
    ctxt
    Tx_rollup_message_repr.encoding
    input

let message_result :
    Raw_context.t ->
    Tx_rollup_message_result_repr.t ->
    (Raw_context.t * Tx_rollup_message_result_hash_repr.t) tzresult =
 fun ctxt input ->
  Tx_rollup_gas.hash
    ~hash_f:Tx_rollup_message_result_hash_repr.hash_bytes
    ctxt
    Tx_rollup_message_result_repr.encoding
    input

let compact_commitment :
    Raw_context.t ->
    Tx_rollup_commitment_repr.Compact.t ->
    (Raw_context.t * Tx_rollup_commitment_repr.Hash.t) tzresult =
 fun ctxt input ->
  Tx_rollup_gas.hash
    ~hash_f:Tx_rollup_commitment_repr.Hash.hash_bytes
    ctxt
    Tx_rollup_commitment_repr.Compact.encoding
    input

let withdraw_list :
    Raw_context.t ->
    Tx_rollup_withdraw_repr.t list ->
    (Raw_context.t * Tx_rollup_withdraw_list_hash_repr.t) tzresult =
 fun ctxt input ->
  Tx_rollup_gas.hash
    ~hash_f:Tx_rollup_withdraw_list_hash_repr.hash_bytes
    ctxt
    (Data_encoding.list Tx_rollup_withdraw_repr.encoding)
    input
