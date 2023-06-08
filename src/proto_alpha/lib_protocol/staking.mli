(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Alpha_context

(** [stake ctxt ~sender ~delegate amount] add [amount] as [sender]'s stake
    to [delegate]. *)
val stake :
  context ->
  sender:public_key_hash ->
  delegate:public_key_hash ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [request_unstake ctxt ~sender_contract ~delegate amount] records a request
    from [sender_contract] to unstake [amount] from [delegate]. *)
val request_unstake :
  context ->
  sender_contract:Contract.t ->
  delegate:public_key_hash ->
  Tez.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [request_full_unstake ctxt ~sender_contract] records a request from
    [sender_contract] to unstake everything from their delegate. *)
val request_full_unstake :
  context ->
  sender_contract:Contract.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t

(** [finalize_unstake ctxt contract] performs the finalization of all unstake
    requests from [contract] that can be finalized.
    An unstake request can be finalized if it is old enough, specifically the
    requested amount must not be at stake anymore and must not be slashable
    anymore, i.e. after [preserved_cycles + max_slashing_period] after the
    request.
    Amounts are transferred from the [contract]'s delegate (at request time)
    unstaked frozen deposits to [contract]'s spendable balance, minus slashing
    the requested stake undergone in between. *)
val finalize_unstake :
  context -> Contract.t -> (context * Receipt.balance_updates) tzresult Lwt.t

(** [punish_delegate ctxt delegate level mistake ~rewarded] slashes [delegate]
    for a [mistake] at [level] and rewards [rewarded]. *)
val punish_delegate :
  context ->
  public_key_hash ->
  Level.t ->
  [`Double_baking | `Double_endorsing] ->
  rewarded:Contract.t ->
  (context * Receipt.balance_updates) tzresult Lwt.t
