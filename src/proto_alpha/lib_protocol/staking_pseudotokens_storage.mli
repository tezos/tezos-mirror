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

(** [costaking_balance_as_tez ctxt ~contract ~delegate] returns [contract]'s
    current costaking balance converted into tez using [delegate] frozen
    deposits tez/pseudotokens rate.
    
    The given [delegate] should be [contract]'s delegate. Otherwise the given
    [Tez.t] amount will not make sense. *)
val costaking_balance_as_tez :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.Public_key_hash.t ->
  Tez_repr.t tzresult Lwt.t

(** [stake ctxt ~contract ~delegate amount] credits the [contract]'s
    costaking pseudotokens and the [delagte]'s frozen deposits pseudotokens by
    an amount of pseudotokens corresponding to [amount] using [delegate]'s 
    frozen deposits pseudotokens/tez rate. 
    
    This function must be called on "stake" **before** transferring tez to
    [delegate]'s frozen deposits. 
    
    [delegate] must be [contract]'s delegate. *)
val stake :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.Public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [request_unstake ctxt ~contract ~delegate amount] debits the [contract]'s
    costaking pseudotokens and the [delegate]'s frozen deposits pseudotokens by
    an amount of pseudotokens corresponding to [amount] using [delegate]'s
    frozen deposits pseudotokens/tez rate capped by [contract]'s costaking
    pseudotokens balance.

    It returns the tez amount corresponding to the debited pseudotokens.

    Resulting context do not make sense if [delegate] is not [contract]'s
    delegate. *)
val request_unstake :
  Raw_context.t ->
  contract:Contract_repr.t ->
  delegate:Signature.Public_key_hash.t ->
  Tez_repr.t ->
  (Raw_context.t * Tez_repr.t) tzresult Lwt.t
