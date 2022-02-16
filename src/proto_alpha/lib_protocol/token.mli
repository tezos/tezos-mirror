(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** The aim of this module is to manage operations involving tokens such as
    minting, transferring, and burning. Every constructor of the types [source],
    [container], or [sink] represents a kind of account that holds a given (or
    possibly infinite) amount of tokens.

    Tokens can be transferred from a [source] to a [sink]. To uniformly handle
    all cases, special constructors of sources and sinks may be used. For
    example, the source [`Minted] is used to express a transfer of minted tokens
    to a destination, and the sink [`Burned] is used to express the action of
    burning a given amount of tokens taken from a source. Thanks to uniformity,
    it is easier to track transfers of tokens throughout the protocol by running
    [grep -R "Token.transfer" src/proto_alpha]. *)

(** [container] is the type of token holders with finite capacity, and whose assets
    are contained in the context. Let [d] be a delegate. Be aware that transferring
    to/from [`Delegate_balance d] will not update [d]'s stake, while transferring
    to/from [`Contract (Contract_repr.implicit_contract d)] will update [d]'s
    stake. *)
type container =
  [ `Contract of Contract_repr.t
  | `Collected_commitments of Blinded_public_key_hash.t
  | `Delegate_balance of Signature.Public_key_hash.t
  | `Frozen_deposits of Signature.Public_key_hash.t
  | `Block_fees ]

(** [infinite_source] defines types of tokens provides which are considered to be
 ** of infinite capacity. *)
type infinite_source =
  [ `Invoice
  | `Bootstrap
  | `Initial_commitments
  | `Revelation_rewards
  | `Double_signing_evidence_rewards
  | `Endorsing_rewards
  | `Baking_rewards
  | `Baking_bonuses
  | `Minted
  | `Liquidity_baking_subsidies ]

(** [source] is the type of token providers. Token providers that are not
    containers are considered to have infinite capacity. *)
type source = [infinite_source | container]

type infinite_sink =
  [ `Storage_fees
  | `Double_signing_punishments
  | `Lost_endorsing_rewards of Signature.Public_key_hash.t * bool * bool
  | `Burned ]

(** [sink] is the type of token receivers. Token receivers that are not
    containers are considered to have infinite capacity. *)
type sink = [infinite_sink | container]

(** [allocated ctxt container] returns true if [balance ctxt container] is
    guaranteed not to fail, and returns false when [balance ctxt container] may
    fail. *)
val allocated : Raw_context.t -> container -> bool tzresult Lwt.t

(** [balance ctxt container] returns the balance associated to the token holder,
    may fail if [allocated ctxt container] returns [false].
    Returns an error with the message "get_balance" if [container] refers to an
    originated contract that is not allocated.
    Returns a {!Storage_Error Missing_key} error if [container] is of the form
    [`Delegate_balance pkh], where [pkh] refers to an implicit contract that is
    not allocated. *)
val balance : Raw_context.t -> container -> Tez_repr.t tzresult Lwt.t

(** [transfer_n ?origin ctxt sources dest] transfers [amount] Tez from [src] to
    [dest] for each [(src, amount)] pair in [sources], and returns a new
    context, and the list of corresponding balance updates. The function behaves
    as though [transfer src dest amount] was invoked for each pair
    [(src, amount)] in [sources], however a single balance update is generated
    for the total amount transferred to [dest].
    When [sources] is an empty list, the function does nothing to the context,
    and returns an empty list of balance updates. *)
val transfer_n :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  ([< source] * Tez_repr.t) list ->
  [< sink] ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [transfer ?origin ctxt src dest amount] transfers [amount] Tez from source
    [src] to destination [dest], and returns a new context, and the list of
    corresponding balance updates tagged with [origin]. By default, [~origin] is
    set to [Receipt_repr.Block_application].
    Returns {!Storage_Error Missing_key} if [src] refers to a contract that is
    not allocated.
    Returns a [Balance_too_low] error if [src] refers to a contract whose
    balance is less than [amount].
    Returns a [Subtraction_underflow] error if [src] refers to a source that is
    not a contract and whose balance is less than [amount].
    Returns a [Empty_implicit_delegated_contract] error if [src] is an
    implicit contract that delegates to a different contract, and whose balance
    is equal to [amount].
    Returns a [Non_existing_contract] error if
    [dest] refers to an originated contract that is not allocated.
    Returns a [Non_existing_contract] error if [amount <> Tez_repr.zero], and
    [dest] refers to an originated contract that is not allocated.
    Returns a [Addition_overflow] error if [dest] refers to a sink whose balance
    is greater than [Int64.max - amount].
    Returns a [Wrong_level] error if [src] or [dest] refer to a level that is
    not the current level. *)
val transfer :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  [< source] ->
  [< sink] ->
  Tez_repr.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t
