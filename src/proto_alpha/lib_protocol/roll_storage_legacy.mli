(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Metastate AG <contact@metastate.ch>                    *)
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

(**
   Basic roll manipulation.

   The storage related to roll (i.e. `Storage.Roll`) is not used outside of
   this module. And, this interface enforces the invariant that a roll is
   always either in the limbo list or owned by a delegate.
*)

type error +=
  | (* `Permanent *) Consume_roll_change
  | (* `Permanent *) No_roll_for_delegate
  | (* `Permanent *) No_stake_snapshot_for_cycle of Cycle_repr.t
  | (* `Permanent *) Unregistered_delegate of Signature.Public_key_hash.t

(**
   [fold ctxt f init] folds [f] on the list of all rolls from [Roll_repr.first]
   to [Storage.Next.Roll] of the context [ctxt]. Only rolls which have owners
   are considered, rolls without owners are skipped. The first parameter of [f]
   is a roll [r], the second parameter of [f] is the owner of [r], and the last
   parameter is the initial value of the accumulator.
*)
val fold :
  Raw_context.t ->
  f:(Roll_repr_legacy.roll -> Signature.Public_key.t -> 'a -> 'a tzresult Lwt.t) ->
  'a ->
  'a tzresult Lwt.t

module Delegate : sig
  val is_inactive :
    Raw_context.t -> Signature.Public_key_hash.t -> bool tzresult Lwt.t

  (**
     [add_amount ctxt dlg am] performs the following actions:

     1. if the delegate [dlg] is inactive, increase its change [chg] by [am],
     2. if the [dlg] is active, update [dlg]'s number of rolls [nr], and change
        [chg] so that [dlg]'s number of tokens is increased by [am], and equal
        to [nr * tokens_per_roll + chg], where [chg < tokens_per_roll].
  *)
  val add_amount :
    Raw_context.t ->
    Signature.Public_key_hash.t ->
    Tez_repr.t ->
    Raw_context.t tzresult Lwt.t

  (**
     [remove_amount ctxt dlg am] performs the following actions:

     1. if the delegate [dlg] is inactive, decrease its change [chg] by [am],
     2. if the [dlg] is active, update [dlg]'s number of rolls [nr], and change
     [chg] so that [dlg]'s number of tokens is decreased by [am], and equal to
     [nr * tokens_per_roll + chg], where [chg < tokens_per_roll].
  *)
  val remove_amount :
    Raw_context.t ->
    Signature.Public_key_hash.t ->
    Tez_repr.t ->
    Raw_context.t tzresult Lwt.t

  (**
     [set_inactive ctxt dlg] renders delegate [dlg] inactive and performs the
     following actions:

     1. empty the list of rolls of [dlg],
     2. increase the change of [dlg] by [nr * tokens_per_roll], where [nr] is
        [dlg]'s number of rolls prior to inactivation.
  *)
  val set_inactive :
    Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

  (**
     If the delegate [dlg] is already active then [set_active ctxt dlg]
     performs the following sequence of actions:

     1. if the delegate is not scheduled to become inactive, then schedule the
        delegate to become inactive after [(preserved_cycles * 2) + 1] cycles,
     2. if the delegate is already scheduled to become inactive at cycle [ic],
        then re-schedule it to become inactive at cycle
        [max ic (cc + preserved_cycles + 1)], where [cc] is the current cycle.

     If [dlg] is inactive then this function puts [dlg] in active state and
     performs the following actions:

     1. if [dlg] is not scheduled to become inactive, schedule [dlg] to become
        inactive after [(preserved_cycles * 2) + 1] cycles,
     2. if the [dlg] is already scheduled to become inactive at cycle [ic],
        then re-schedule it to become inactive at cycle
        [max ic (cc + (preserved_cycles * 2) + 1)], where [cc] is the current
        cycle,
     3. dispatch [dlg]'s change [chg] into [nr] rolls of size [tokens_per_roll]
        so that the total amount managed by [dlg] is unchanged and equal to
        [(nr * tokens_per_roll) + chg], where [chg < tokens_per_roll].
  *)
  val set_active :
    Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t
end

module Contract : sig
  (**
     Calls [Delegate.add_amount ctxt contract am] if a delegate is associated
     to [contract], or returns unchanged [ctxt] otherwise.
  *)
  val add_amount :
    Raw_context.t ->
    Contract_repr.t ->
    Tez_repr.t ->
    Raw_context.t tzresult Lwt.t

  (**
     Calls [Delegate.remove_amount ctxt contract am] if a delegate is associated
     to [contract], or returns unchanged [ctxt] otherwise.
  *)
  val remove_amount :
    Raw_context.t ->
    Contract_repr.t ->
    Tez_repr.t ->
    Raw_context.t tzresult Lwt.t
end

(**
   [delegate_pubkey ctxt delegate] returns the public key of
   [delegate] found in context [ctxt] if there exists a registered
   contract.
*)
val delegate_pubkey :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t

(**
   [get_change ctxt delegate] returns the amount of change held by
   [delegate] in context [ctxt]. The change is the part of the staking
   balance of a delegate that is not part of a roll, i.e., the amount
   of staking balance (smaller than the value of a roll) not being
   taken into account for baking rights computation.
*)
val get_change :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

(**
   [get_contract_delegate ctxt contract] returns the public key hash
   of the delegate whose contract is [contract] in context [ctxt].
*)
val get_contract_delegate :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t option tzresult Lwt.t
