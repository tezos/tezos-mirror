(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module defines RPC services to access the information associated to
    delegates (who they are, their delegators, their different kinds of balances, their activity, etc.).
*)

open Alpha_context

type error += (* `Temporary *) Not_registered of Signature.Public_key_hash.t

val list :
  'a #RPC_context.simple ->
  'a ->
  ?active:bool ->
  ?inactive:bool ->
  ?with_minimal_stake:bool ->
  ?without_minimal_stake:bool ->
  unit ->
  Signature.Public_key_hash.t list shell_tzresult Lwt.t

type consensus_key = {
  consensus_key_pkh : Signature.Public_key_hash.t;
  consensus_key_pk : Signature.Public_key.t;
}

type consensus_keys_info = {
  active : consensus_key;
  pendings : (Cycle.t * consensus_key) list;
}

type info = {
  full_balance : Tez.t;  (** Balance + Frozen balance *)
  current_frozen_deposits : Tez.t;
  frozen_deposits : Tez.t;
  staking_balance : Tez.t;
  frozen_deposits_limit : Tez.t option;
  delegated_contracts : Contract.t list;
  delegated_balance : Tez.t;
  min_delegated_in_current_cycle : Tez.t;
  total_delegated_stake : Tez.t;
  staking_denominator : Staking_pseudotoken.t;
  deactivated : bool;
  grace_period : Cycle.t;
  pending_denunciations : bool;
  voting_info : Vote.delegate_info;
  active_consensus_key : Signature.Public_key_hash.t;
  pending_consensus_keys : (Cycle.t * Signature.Public_key_hash.t) list;
}

type deposit_per_cycle = {cycle : Cycle.t; deposit : Tez.t}

val deposit_per_cycle_encoding : deposit_per_cycle Data_encoding.t

val info_encoding : info Data_encoding.t

val info :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  info shell_tzresult Lwt.t

val full_balance :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val current_frozen_deposits :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val frozen_deposits :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val unstaked_frozen_deposits :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  deposit_per_cycle list shell_tzresult Lwt.t

val staking_balance :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val frozen_deposits_limit :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t option shell_tzresult Lwt.t

val delegated_contracts :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Contract.t list shell_tzresult Lwt.t

val delegated_balance :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Tez.t shell_tzresult Lwt.t

val total_delegated_stake :
  'a #RPC_context.simple -> 'a -> public_key_hash -> Tez.t shell_tzresult Lwt.t

val staking_denominator :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Staking_pseudotoken.t shell_tzresult Lwt.t

val deactivated :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  bool shell_tzresult Lwt.t

val grace_period :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  Cycle.t shell_tzresult Lwt.t

val current_voting_power :
  'a #RPC_context.simple -> 'a -> public_key_hash -> int64 shell_tzresult Lwt.t

val voting_power :
  'a #RPC_context.simple -> 'a -> public_key_hash -> int64 shell_tzresult Lwt.t

val current_baking_power :
  'a #RPC_context.simple -> 'a -> public_key_hash -> int64 shell_tzresult Lwt.t

val voting_info :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Vote.delegate_info shell_tzresult Lwt.t

val consensus_key :
  'a #RPC_context.simple ->
  'a ->
  Signature.Public_key_hash.t ->
  consensus_keys_info shell_tzresult Lwt.t

val participation :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Delegate.For_RPC.participation_info shell_tzresult Lwt.t

val active_staking_parameters :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Staking_parameters_repr.t shell_tzresult Lwt.t

val pending_staking_parameters :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  (Cycle.t * Staking_parameters_repr.t) list shell_tzresult Lwt.t

val pending_denunciations :
  'a #RPC_context.simple ->
  'a ->
  public_key_hash ->
  Denunciations_repr.t shell_tzresult Lwt.t

val estimated_shared_pending_slashed_amount :
  'a #RPC_context.simple -> 'a -> public_key_hash -> Tez.t shell_tzresult Lwt.t

val register : unit -> unit
