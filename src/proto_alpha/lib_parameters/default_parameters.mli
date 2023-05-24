(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol.Alpha_context

val constants_mainnet : Constants.Parametric.t

val constants_sandbox : Constants.Parametric.t

val constants_test : Constants.Parametric.t

val test_commitments : Commitment.t list lazy_t

val make_bootstrap_account :
  Signature.public_key_hash
  * Signature.public_key
  * Tez.t
  * Signature.public_key_hash option
  * Signature.public_key option ->
  Parameters.bootstrap_account

val parameters_of_constants :
  ?bootstrap_accounts:Parameters.bootstrap_account list ->
  ?bootstrap_contracts:Parameters.bootstrap_contract list ->
  ?bootstrap_smart_rollups:Parameters.bootstrap_smart_rollup list ->
  ?commitments:Commitment.t list ->
  Constants.Parametric.t ->
  Parameters.t

val json_of_parameters : Parameters.t -> Data_encoding.json
