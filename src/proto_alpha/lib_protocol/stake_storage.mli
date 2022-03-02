(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This library provides basic operations (accessors and setters) on
    staking tokens. *)

module Delegate_sampler_state : sig
  val init :
    Raw_context.t ->
    Cycle_repr.t ->
    Storage.Delegate_sampler_state.value ->
    Raw_context.t tzresult Lwt.t

  val get :
    Raw_context.t ->
    Cycle_repr.t ->
    Storage.Delegate_sampler_state.value tzresult Lwt.t

  val remove_existing :
    Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t
end

val remove_stake :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

val add_stake :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Tez_repr.t ->
  Raw_context.t tzresult Lwt.t

val deactivate_only_call_from_delegate_storage :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t Lwt.t

val activate_only_call_from_delegate_storage :
  Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t tzresult Lwt.t

val get_staking_balance :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

val snapshot : Raw_context.t -> Raw_context.t tzresult Lwt.t

val compute_snapshot_index :
  Raw_context.t -> Cycle_repr.t -> max_snapshot_index:int -> int tzresult Lwt.t

val select_distribution_for_cycle_do_not_call_except_for_migration :
  Raw_context.t ->
  Cycle_repr.t ->
  (Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t) ->
  Raw_context.t tzresult Lwt.t

val clear_cycle : Raw_context.t -> Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val init_first_cycles :
  Raw_context.t ->
  (Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t) ->
  Raw_context.t tzresult Lwt.t

val fold :
  Raw_context.t ->
  f:(Signature.Public_key_hash.t * Tez_repr.t -> 'a -> 'a tzresult Lwt.t) ->
  order:[`Sorted | `Undefined] ->
  'a ->
  'a tzresult Lwt.t

val select_new_distribution_at_cycle_end :
  Raw_context.t ->
  new_cycle:Cycle_repr.t ->
  (Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t tzresult Lwt.t) ->
  Raw_context.t tzresult Lwt.t

val clear_at_cycle_end :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val get :
  Raw_context.t -> Signature.Public_key_hash.t -> Tez_repr.t tzresult Lwt.t

val fold_on_active_delegates_with_rolls :
  Raw_context.t ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:(Signature.Public_key_hash.t -> unit -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

val get_selected_distribution :
  Raw_context.t ->
  Cycle_repr.t ->
  (Signature.Public_key_hash.t * Tez_repr.t) list tzresult Lwt.t

val find_selected_distribution :
  Raw_context.t ->
  Cycle_repr.t ->
  (Signature.Public_key_hash.t * Tez_repr.t) list option tzresult Lwt.t

(** Copy the stake distribution for the current cycle (from
   [Storage.Stake.Selected_distribution_for_cycle]) in the raw
   context. *)
val prepare_stake_distribution : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [get_total_active_stake ctxt cycle] retrieves the amount in Tez of the
    active stake at [cycle] from [ctxt]. *)
val get_total_active_stake :
  Raw_context.t -> Cycle_repr.t -> Tez_repr.t tzresult Lwt.t

(** [sampler_for_cycle ctxt cycle] reads the sampler for [cycle] from
    [ctxt] if it has been previously inited. Otherwise it initializes
    the sampler and caches it in [ctxt] with
    [Raw_context.set_sampler_for_cycle]. *)
val sampler_for_cycle :
  Raw_context.t ->
  Cycle_repr.t ->
  (Raw_context.t
  * Seed_repr.seed
  * (Signature.public_key * Signature.public_key_hash) Sampler.t)
  tzresult
  Lwt.t
