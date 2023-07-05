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

(* Invariant 1:
     For {!Storage.Contract.Frozen_deposits_pseudotokens}, a missing key is
     equivalent to a value of [0], in which case it is also equivalent to having
     the same amount of tokens in the balances
     {!Storage.Contract.Frozen_deposits_pseudotokens} and
     {!Storage.Contract.Frozen_deposits}.

     This is ensured by:
       - checking that the result of
         {!Storage.Contract.Frozen_deposits_pseudotokens.find} is always matched
         with [Some v when Staking_pseudotoken_repr.(v <> zero)];
       - and that there is no call to
         {!Storage.Contract.Frozen_deposits_pseudotokens.get}.


   Invariant 2:
     All delegates with non-zero frozen deposits tez have:
       - either their costaking balance pseudotokens initialized (possibly to
         zero) and their frozen deposits pseudotokens initialized to non-zero;
       - or have no costakers, in which case their costaking balance and
         frozen deposits pseudotokens are assumed to be equal to their frozen
         deposits tez.


   Invariant 3:
     For a given delegate, their frozen deposits pseudotokens equal the sum of
     all costaking pseudotokens of their delegators (including the delegate
     itself).

     It is ensured by:
       - {credit_costaking_pseudotokens} always called with (the pseudotokens
         result of) {credit_frozen_deposits_pseudotokens_for_tez_amount} in
         {stake};
       - {dedit_costaking_pseudotokens} always called with (the same value as)
         {debit_frozen_deposits_pseudotokens} in {request_unstake}.
*)

(** [init_delegate_pseudotokens_from_frozen_deposits_balance ctxt contract]
    initializes [contract]'s frozen deposits pseudotokens and costaking
    pseudotokens usings [contract]'s current frozen deposits tez.

    This function must be called whenever a contract's frozen deposits tez are
    initialized (see invariant above). *)
val init_delegate_pseudotokens_from_frozen_deposits_balance :
  Raw_context.t -> Contract_repr.t -> Raw_context.t tzresult Lwt.t

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
