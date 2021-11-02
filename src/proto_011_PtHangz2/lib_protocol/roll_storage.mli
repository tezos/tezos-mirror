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
  | (* `Permanent *) No_roll_snapshot_for_cycle of Cycle_repr.t
  | (* `Permanent *) Unregistered_delegate of Signature.Public_key_hash.t

(**
   [init ctxt] returns a new context initialized from [ctxt] where the next
   roll to be allocated is the first roll, i.e.
   [(Storage.Roll.Next.get ctxt) = Roll_repr.first].
   This function returns a [{!Storage_error Existing_key}] error if the context
   has already been initialized.
*)
val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

(**
   [init_first_cycles ctxt] computes a new context from [ctxt] where the store
   has been prepared to save roll snapshots for all cycles from [0] to
   [Constants.preserved_cycles + 2]:

   1. rolls for all cycles in the interval [(0, preserved_cycles)] are frozen
      (after taking a snapshot),
   2. a snapshot is taken for rolls of cycle [preserved_cycles + 1],
   3. rolls for cycle [preserved_cycles + 2] are ready for a snapshot, i.e. the
      necessary storage has been prepared.
*)
val init_first_cycles : Raw_context.t -> Raw_context.t tzresult Lwt.t

(**
   [cycle_end ctxt last_cycle] returns a new context after applying the
   end-of-cycle bookkeeping to [ctxt]:

   1. clears cycle [c = (last_cycle - preserved_cycles)] if [last_cycle >=
      preserved_cycles] (this amounts to deleting the only snapshot left after
      the freezing of [c]),
   2. freezes snapshot rolls for the cycle
      [(last_cycle + preserved_cycles + 1)] (this amounts to removing all
      snapshots for the cycle, except one randomly selected for computing
      baking rights),
   3. makes cycle [(last_cycle + preserved_cycles + 2)] ready for snapshot.
*)
val cycle_end : Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

(**
   [snapshot_rolls ctxt] creates roll snapshots for cycle
   [c = level + preserved_cycles + 2]. The returned context is such that:

   1. the snapshot index associated to cycle [c] is incremented,
   2. the rolls' owners are copied and associated to the snapshot id
      [(c,index)] (where [index] is the current snapshot index of cycle [c]),
   3. the last roll for cycle [c], and snapshot [index] is set to be the next
      roll of [ctxt].
*)
val snapshot_rolls : Raw_context.t -> Raw_context.t tzresult Lwt.t

(**
   [fold ctxt f init] folds [f] on the list of all rolls from [Roll_repr.first]
   to [Storage.Next.Roll] of the context [ctxt]. Only rolls which have owners
   are considered, rolls without owners are skipped. The first parameter of [f]
   is a roll [r], the second parameter of [f] is the owner of [r], and the last
   parameter is the initial value of the accumulator.
*)
val fold :
  Raw_context.t ->
  f:(Roll_repr.roll -> Signature.Public_key.t -> 'a -> 'a tzresult Lwt.t) ->
  'a ->
  'a tzresult Lwt.t

(**
   May return a [No_roll_snapshot_for_cycle] error.
*)
val baking_rights_owner :
  Raw_context.t ->
  Level_repr.t ->
  priority:int ->
  Signature.Public_key.t tzresult Lwt.t

(**
   May return a [No_roll_snapshot_for_cycle] error.
*)
val endorsement_rights_owner :
  Raw_context.t ->
  Level_repr.t ->
  slot:int ->
  Signature.Public_key.t tzresult Lwt.t

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
   [count_rolls ctxt delegate] returns the number of rolls held by
   [delegate] in context [ctxt].
*)
val count_rolls :
  Raw_context.t -> Signature.Public_key_hash.t -> int tzresult Lwt.t

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
   [update_tokens_per_roll ctxt am] performs the following actions:
   
   1. set the constant [tokens_per_roll] to [am],
   2. if the constant was increased by [tpram], then add the amount
      [nr * tpram] to each delegate, where [nr] is the delegate's
      number of rolls,
   3. if the constant was instead decreased by [tpram], then remove
      the amount [nr * tpram] from all delegates.
*)
val update_tokens_per_roll :
  Raw_context.t -> Tez_repr.t -> Raw_context.t tzresult Lwt.t

(**
   [get_contract_delegate ctxt contract] returns the public key hash
   of the delegate whose contract is [contract] in context [ctxt].
*)
val get_contract_delegate :
  Raw_context.t ->
  Contract_repr.t ->
  Signature.Public_key_hash.t option tzresult Lwt.t
