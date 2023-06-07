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

(** This module defines protocol parameters, i.e. constants regulating the
    behaviour of the blockchain under the protocol. *)

(** An implict contract (account) initially existing on a chain since genesis. *)
type bootstrap_account = {
  public_key_hash : Signature.Public_key_hash.t;
  public_key : Signature.Public_key.t option;
  amount : Tez_repr.t;
  delegate_to : Signature.Public_key_hash.t option;
  consensus_key : Signature.Public_key.t option;
}

(** An originated contract initially existing on a chain since genesis. *)
type bootstrap_contract = {
  delegate : Signature.Public_key_hash.t option;
  amount : Tez_repr.t;
  script : Script_repr.t;
}

(** Protocol parameters define some constants regulating behaviour of the
    chain. *)
type t = {
  bootstrap_accounts : bootstrap_account list;
  bootstrap_contracts : bootstrap_contract list;
  commitments : Commitment_repr.t list;
  constants : Constants_parametric_repr.t;
  security_deposit_ramp_up_cycles : int option;
  no_reward_cycles : int option;
}

val bootstrap_account_encoding : bootstrap_account Data_encoding.t

val encoding : t Data_encoding.t

val check_params : t -> unit tzresult
