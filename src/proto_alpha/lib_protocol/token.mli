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
    minting, transferring, and burning. Every constructor of the types [giver],
    [container], or [receiver] represents a kind of account that holds a given (or
    possibly infinite) amount of tokens.

    Tokens can be transferred from a [giver] to a [receiver]. To uniformly handle
    all cases, special constructors of givers and receivers may be used. For
    example, the giver [`Minted] is used to express a transfer of minted tokens
    to a receiver, and the receiver [`Burned] is used to express the action of
    burning a given amount of tokens taken from a giver. Thanks to uniformity,
    it is easier to track transfers of tokens throughout the protocol by running
    [grep -R "Token.transfer" src/proto_alpha].

    For backward compatibility purpose, an ANTI-PATTERN is used to redistribute
    slashing to denunciator; this redistribution technic should not be mimicked
    if it can be avoided (see https://gitlab.com/tezos/tezos/-/issues/4787).
    The anti-pattern works as follows:
    The part of slashed amounts that goes to the author of the denunciation are
    not directly distributed to him. Tokens are transferred to a burning sink,
    then minted from an infinite source ( see `Double_signing_punishments,
    and `Sc_rollup_refutation_rewards ).
    Again, this is an ANTI-PATTERN that should not be mimicked.
*)

(** [container] is the type of token holders with finite capacity, and whose assets
    are contained in the context. An account may have several token holders,
    that can serve as sources and/or sinks.
    For example, an implicit account [d] serving as a delegate has a token holder
    for its own spendable balance, and another token holder for its frozen deposits.
*)

type container =
  [ `Contract of Contract_repr.t
    (** Implicit account's or Originated contract's spendable balance *)
  | `Collected_commitments of Blinded_public_key_hash.t
    (** Pre-funded account waiting for the commited pkh and activation code to
        be revealed to unlock the funds *)
  | `Frozen_deposits of Signature.Public_key_hash.t
    (** Frozen tokens of a delegate for consensus security deposits *)
  | `Block_fees  (** Current block's fees collection *)
  | `Frozen_bonds of Contract_repr.t * Bond_id_repr.t
    (** Frozen tokens of a contract for bond deposits (currently used by rollups) *)
  ]

(** [infinite_source] defines types of tokens providers which are considered to be
 ** of infinite capacity. *)
type infinite_source =
  [ `Invoice
    (** Tokens minted during a protocol upgrade,
        typically to fund the development of some part of the amendment. *)
  | `Bootstrap  (** Bootstrap accounts funding *)
  | `Initial_commitments
    (** Funding of Genesis' prefunded accounts requiring an activation *)
  | `Revelation_rewards  (** Seed nonce revelation rewards *)
  | `Endorsing_rewards  (** Consensus endorsing rewards *)
  | `Baking_rewards  (** Consensus baking fixed rewards *)
  | `Baking_bonuses  (** Consensus baking variable bonus *)
  | `Minted  (** Generic source for test purpose *)
  | `Liquidity_baking_subsidies  (** Subsidy for liquidity-baking contract *)
  | `Sc_rollup_refutation_rewards
    (** Sc_rollup refutation rewards (slashing redistribution) *) ]

(** [giver] is the type of token providers. Token providers that are not
    containers are considered to have infinite capacity. *)
type giver = [infinite_source | container]

type infinite_sink =
  [ `Storage_fees  (** Fees burnt to compensate storage usage *)
  | `Double_signing_punishments  (** Consensus slashing *)
  | `Lost_endorsing_rewards of Signature.Public_key_hash.t * bool * bool
    (** Consensus rewards not distributed because the participation of the delegate was too low. *)
  | `Sc_rollup_refutation_punishments  (** Smart rollups refutation slashing *)
  | `Burned  (** Generic sink mainly for test purpose *) ]

(** [receiver] is the type of token receivers. Token receivers that are not
    containers are considered to have infinite capacity. *)
type receiver = [infinite_sink | container]

(** [allocated ctxt container] returns a new context because of possible access
    to carbonated data, and a boolean that is [true] when
    [balance ctxt container] is guaranteed not to fail, and [false] when
    [balance ctxt container] may fail. *)
val allocated :
  Raw_context.t -> container -> (Raw_context.t * bool) tzresult Lwt.t

(** [balance ctxt container] returns a new context because of an access to
    carbonated data, and the balance associated to the token holder.
    This function may fail if [allocated ctxt container] returns [false].
    Returns an error with the message "get_balance" if [container] refers to an
    originated contract that is not allocated. *)
val balance :
  Raw_context.t -> container -> (Raw_context.t * Tez_repr.t) tzresult Lwt.t

(** [transfer_n ?origin ctxt givers receiver] transfers [amount] Tez from [giver] to
    [receiver] for each [(giver, amount)] pair in [givers], and returns a new
    context, and the list of corresponding balance updates. The function behaves
    as though [transfer ?origin ctxt giver receiver amount] was invoked for each pair
    [(giver, amount)] in [givers], however a single balance update is generated
    for the total amount transferred to [receiver].
    When [givers] is an empty list, the function does nothing to the context,
    and returns an empty list of balance updates. *)
val transfer_n :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  ([< giver] * Tez_repr.t) list ->
  [< receiver] ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t

(** [transfer ?origin ctxt giver receiver amount] transfers [amount] Tez from giver
    [giver] to receiver [receiver], and returns a new context, and the list of
    corresponding balance updates tagged with [origin]. By default, [~origin] is
    set to [Receipt_repr.Block_application].
    Returns {!Storage_Error Missing_key} if [giver] refers to a contract that is
    not allocated.
    Returns a [Balance_too_low] error if [giver] refers to a contract whose
    balance is less than [amount].
    Returns a [Subtraction_underflow] error if [giver] is
    not a contract and its balance is less than [amount].
    Returns a [Empty_implicit_delegated_contract] error if [giver] is an
    implicit contract that delegates to a different contract, and whose balance
    is equal to [amount].
    Returns a [Non_existing_contract] error if
    [receiver] refers to an originated contract that is not allocated.
    Returns a [Non_existing_contract] error if [amount <> Tez_repr.zero], and
    [receiver] refers to an originated contract that is not allocated.
    Returns a [Addition_overflow] error if [receiver] refers to a receiver whose balance
    is greater than [Int64.max - amount].
    Returns a [Wrong_level] error if [src] or [receiver] refer to a level that is
    not the current level. *)
val transfer :
  ?origin:Receipt_repr.update_origin ->
  Raw_context.t ->
  [< giver] ->
  [< receiver] ->
  Tez_repr.t ->
  (Raw_context.t * Receipt_repr.balance_updates) tzresult Lwt.t
