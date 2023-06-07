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

(** This module is responsible for maintaining the
    {!Storage.Contract.Frozen_deposits_pseudotokens} and
    {!Storage.Contract.Costaking_pseudotokens} tables. *)

(** [credit_frozen_deposits_pseudotokens_for_tez_amount ctxt delegate tez_amount]
    increases [delegate]'s stake pseudotokens by an amount [pa] corresponding to
    [tez_amount] multiplied by the current rate of the delegate's frozen
    deposits pseudotokens per tez.
    The function also returns [pa].

    This function must be called on "stake" before transferring tez to
    [delegate]'s frozen deposits. *)
val credit_frozen_deposits_pseudotokens_for_tez_amount :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t ->
  (Raw_context.t * Staking_pseudotoken_repr.t) tzresult Lwt.t

(** [credit_costaking_pseudotokens ctxt contract p_amount] increases
    [contract]'s costaking pseudotokens balance by [p_amount]. *)
val credit_costaking_pseudotokens :
  Raw_context.t ->
  Contract_repr.t ->
  Staking_pseudotoken_repr.t ->
  Raw_context.t tzresult Lwt.t
