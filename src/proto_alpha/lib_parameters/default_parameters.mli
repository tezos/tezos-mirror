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

(** Concrete constant values available for various test setups.

    See {!Protocol.Constants_parametric_repr} for documentation on
    constant management and instructions on how to add/modify a
    constant.

    See [default_parameters.ml] for documentation on individual
    constants.
*)

open Protocol.Alpha_context

(** The same constants as on mainnet, to be used in tests when we need
    to simulate mainnet conditions.

    Note that this does not control constant values on mainnet; it
    merely mirrors them. Mainnet constant values are set in
    {!Protocol.Raw_context.prepare_first_block} (and therefore cannot
    be modified without impacting the protocol hash). Any update to
    constant values there should be propagated here. *)
val constants_mainnet : Constants.Parametric.t

(** Default constant values for sandbox mode. *)
val constants_sandbox : Constants.Parametric.t

(** Default constant values to use in most tests.

    For instance, [blocks_per_cycle] is much lower so that we may
    observe cycle transitions without needing to bake a prohibitively
    large number of blocks. *)
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

val json_of_parameters :
  ?chain_id:Chain_id.t -> Parameters.t -> Data_encoding.json

module Internal_for_tests : sig
  val bootstrap_balance : Tez.t

  val make_sc_rollup_parameter :
    dal_activation_level:Raw_level.t ->
    dal_attested_slots_validity_lag:int ->
    int ->
    Constants.Parametric.sc_rollup
end
