(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** Places where tez can be found in the ledger's state. *)
type balance =
  | Contract of Contract_repr.t
  | Block_fees
  | Deposits of Signature.Public_key_hash.t
  | Unstaked_deposits of Signature.Public_key_hash.t * Cycle_repr.t
  | Nonce_revelation_rewards
  | Endorsing_rewards
  | Baking_rewards
  | Baking_bonuses
  | Storage_fees
  | Double_signing_punishments
  | Lost_endorsing_rewards of Signature.Public_key_hash.t * bool * bool
  | Liquidity_baking_subsidies
  | Burned
  | Commitments of Blinded_public_key_hash.t
  | Bootstrap
  | Invoice
  | Initial_commitments
  | Minted
  | Frozen_bonds of Contract_repr.t * Bond_id_repr.t
  | Sc_rollup_refutation_punishments
  | Sc_rollup_refutation_rewards

(** Compares two balances. *)
val compare_balance : balance -> balance -> int

(** A credit or debit of tez to a balance. *)
type balance_update = Debited of Tez_repr.t | Credited of Tez_repr.t

(** An origin of a balance update *)
type update_origin =
  | Block_application  (** Update from a block application *)
  | Protocol_migration  (** Update from a protocol migration *)
  | Subsidy  (** Update from an inflationary subsidy  *)
  | Simulation  (** Simulation of an operation **)

(** Compares two origins. *)
val compare_update_origin : update_origin -> update_origin -> int

(** A list of balance updates. Duplicates may happen.
    For example, an entry of the form [(Rewards (b,c), Credited am, ...)]
    indicates that the balance of frozen rewards has been increased by [am]
    for baker [b] and cycle [c]. *)
type balance_updates = (balance * balance_update * update_origin) list

(** The property [Json.destruct (Json.construct balance_updates) = balance_updates]
    does not always hold for [balance_updates_encoding] when [balance_updates]
    contains entries of the form [(_, _ Tez_repr.zero, _)]. This is because the
    [balance_update] [(_ Tez_repr.zero)] always decodes into [(Credited Tez_repr.zero)]. *)
val balance_updates_encoding : balance_updates Data_encoding.t

(** Group updates by (balance x origin), and remove zero-valued balances. *)
val group_balance_updates : balance_updates -> balance_updates tzresult
