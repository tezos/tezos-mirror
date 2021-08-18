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
  | Rewards of Signature.Public_key_hash.t * Cycle_repr.t
  | Fees of Signature.Public_key_hash.t * Cycle_repr.t
  | Deposits of Signature.Public_key_hash.t * Cycle_repr.t

(** A credit or debit of tez to a balance. *)
type balance_update = Debited of Tez_repr.t | Credited of Tez_repr.t

(** An origin of a balance update *)
type update_origin =
  | Block_application  (** Update from a block application *)
  | Protocol_migration  (** Update from a protocol migration *)
  | Subsidy  (** Update from an inflationary subsidy  *)

(** A list of balance updates. Duplicates may happen. *)
type balance_updates = (balance * balance_update * update_origin) list

val balance_updates_encoding : balance_updates Data_encoding.t

(** Remove zero-valued balances from a list of updates. *)
val cleanup_balance_updates : balance_updates -> balance_updates
