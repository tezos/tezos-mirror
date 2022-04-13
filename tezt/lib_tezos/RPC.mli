(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** In all RPCs, default [chain] is "main" and default [block] is
   "head~2" to pick the finalized branch for Tenderbake. *)

(** {2 Shell RPCs} *)

(** Call RPC /network/connections if [peer_id] is [None].
    Call RPC /network/connections/[peer_id] otherwise. *)
val get_connections :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?peer_id:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /network/greylist/ips *)
val get_greylist_ips : ?hooks:Process.hooks -> Client.t -> JSON.t Lwt.t

(** Call RPC /chain/[chain]/chain_id *)
val get_chain_id :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block] *)
val get_block :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/hash *)
val get_block_hash :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  string Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/metadata *)
val get_block_metadata :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain] *)
val force_bootstrapped :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?bootstrapped:bool ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/is_bootstrapped *)
val is_bootstrapped :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/levels/checkpoint *)
val get_checkpoint :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/levels/savepoint *)
val get_savepoint :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/levels/caboose *)
val get_caboose :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /injection/operation *)
val inject_operation :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?async:bool ->
  data:JSON.u ->
  Client.t ->
  JSON.t Runnable.process

(** Call RPC /private/injection/operation *)
val private_inject_operation :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?async:bool ->
  data:JSON.u ->
  Client.t ->
  JSON.t Runnable.process

(** Call RPC /injection/block *)
val inject_block :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Run [tezos-client rpc /chains/<chain>/blocks/<block>/header/protocol/raw]. *)
val raw_protocol_data :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  string Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/header/protocol_data *)
val get_protocol_data :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?offset:int ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/head~[offset]/hash where default [offset] is [2]. *)
val get_branch :
  ?offset:int ->
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/operations *)
val get_operations :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/pending_operations *)
val get_mempool_pending_operations :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?version:string ->
  ?applied:bool ->
  ?branch_delayed:bool ->
  ?branch_refused:bool ->
  ?refused:bool ->
  ?outdated:bool ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/request_operations *)
val mempool_request_operations :
  ?endpoint:Client.endpoint ->
  ?chain:string ->
  ?peer:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/ban_operation *)
val mempool_ban_operation :
  ?endpoint:Client.endpoint ->
  ?chain:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/unban_operation *)
val mempool_unban_operation :
  ?endpoint:Client.endpoint ->
  ?chain:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chains/[chain]/mempool/unban_all_operations *)
val mempool_unban_all_operations :
  ?endpoint:Client.endpoint -> ?chain:string -> Client.t -> JSON.t Lwt.t

(** Call RPC GET /chains/[chain]/mempool/filter *)
val get_mempool_filter :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?include_default:bool ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC POST /chains/[chain]/mempool/filter *)
val post_mempool_filter :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/preapply/block *)
val preapply_block :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/forge/operations *)
val post_forge_operations :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/scripts/run_operation *)
val post_run_operation :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/scripts/simulate_operation *)
val post_simulate_operation :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  data:JSON.u ->
  Client.t ->
  JSON.t Lwt.t

(** {2 Protocol RPCs} *)

type ctxt_type = Bytes | Json

(** Call RPC /chain/[chain]/blocks/[block]/context/raw/[ctxt_type]/[value_path]
*)
val get_context_value :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?ctxt_type:ctxt_type ->
  value_path:string list ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/context/constants *)
val get_constants :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/context/constants/errors *)
val get_constants_errors :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/baking_rights *)
val get_baking_rights :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?delegate:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/current_level *)
val get_current_level :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?offset:int ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/endorsing_rights *)
val get_endorsing_rights :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?delegate:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /chain/[chain]/blocks/[block]/helpers/levels_in_current_cycle *)
val get_levels_in_current_cycle :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  Client.t ->
  JSON.t Lwt.t

(** Call RPC /workers/chain_validators/[chain]/ddb *)
val get_ddb :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  Client.t ->
  JSON.t Lwt.t

module Big_maps : sig
  (** Call RPC /chain/[chain]/blocks/[block]/context/big_maps/[id]/[key_hash] *)
  val get :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    id:string ->
    key_hash:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/big_maps/[big_map_id]?offset=[int]&length=[int] *)
  val get_all :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    big_map_id:string ->
    ?offset:int ->
    ?length:int ->
    Client.t ->
    JSON.t Runnable.process
end

module Contracts : sig
  (** Common protocol RPSs for contracts (i.e. under [/contracts]). *)

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts *)
  val get_all :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates *)
  val get_all_delegates :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    string list Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id] *)
  val get :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/balance *)
  val get_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/frozen_bonds *)
  val get_frozen_bonds :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/balance_and_frozen_bonds *)
  val get_balance_and_frozen_bonds :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/big_map_get *)
  val big_map_get :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    data:JSON.u ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/counter *)
  val get_counter :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/delegate *)
  val get_delegate :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/entrypoints *)
  val get_entrypoints :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/manager_key *)
  val get_manager_key :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/script *)
  val get_script :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/contracts/[contract_id]/storage *)
  val get_storage :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    contract_id:string ->
    Client.t ->
    JSON.t Runnable.process
end

module Delegates : sig
  (** Common protocol RPSs for delegates (i.e. under [/delegates]). *)

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates *)
  val get_all :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    string list Lwt.t

  (** Same as [get_all], but do not wait for the process to exit. *)
  val spawn_get_all :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh] *)
  val get :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get], but do not wait for the process to exit. *)
  val spawn_get :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/balance *)
  val get_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_balance], but do not wait for the process to exit. *)
  val spawn_get_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/full_balance *)
  val get_full_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_full_balance], but do not wait for the process to exit. *)
  val spawn_get_full_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/frozen_deposits *)
  val get_frozen_deposits :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_frozen_deposits], but do not wait for the process to exit. *)
  val spawn_get_frozen_deposits :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/deactivated *)
  val get_deactivated :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_deactivated], but do not wait for the process to exit. *)
  val spawn_get_deactivated :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/delegated_balance *)
  val get_delegated_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_delegated_balance], but do not wait for the process to exit. *)
  val spawn_get_delegated_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/delegated_contracts *)
  val get_delegated_contracts :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_delegated_contracts], but do not wait for the process to exit. *)
  val spawn_get_delegated_contracts :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/frozen_balance *)
  val get_frozen_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_frozen_balance], but do not wait for the process to exit. *)
  val spawn_get_frozen_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/frozen_balance_by_cycle *)
  val get_frozen_balance_by_cycle :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_frozen_balance_by_cycle], but do not wait for the process to exit. *)
  val spawn_get_frozen_balance_by_cycle :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/grace_period *)
  val get_grace_period :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_grace_period], but do not wait for the process to exit. *)
  val spawn_get_grace_period :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/staking_balance *)
  val get_staking_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_staking_balance], but do not wait for the process to exit. *)
  val spawn_get_staking_balance :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/delegates/[pkh]/voting_power *)
  val get_voting_power :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Same as [get_voting_power], but do not wait for the process to exit. *)
  val spawn_get_voting_power :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    pkh:string ->
    Client.t ->
    Process.t
end

module Votes : sig
  (** Common protocol RPSs for votes (i.e. under [/votes]). *)

  (** Call RPC /chain/[chain]/blocks/[block]/votes/ballot_list *)
  val get_ballot_list :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/ballots *)
  val get_ballots :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/current_proposal *)
  val get_current_proposal :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/current_quorum *)
  val get_current_quorum :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/listings *)
  val get_listings :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/proposals *)
  val get_proposals :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/current_period *)
  val get_current_period :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/successor_period *)
  val get_successor_period :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/votes/total_voting_power *)
  val get_total_voting_power :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t
end

module Script_cache : sig
  (** Call RPC /chain/[chain]/blocks/[block]/context/cache/contracts/all *)
  val get_cached_contracts :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t
end

module Tx_rollup : sig
  (** Call RPC /chain/[chain]/blocks/[block]/context/tx_rollup/[tx_rollup_id]/state *)
  val get_state :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    rollup:string ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/tx_rollup/[tx_rollup_id]/inbox/[level] *)
  val get_inbox :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    rollup:string ->
    level:int ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/tx_rollup/[rollup_hash]/commitment/[level] *)
  val get_commitment :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    rollup:string ->
    level:int ->
    Client.t ->
    JSON.t Runnable.process

  (** Call RPC /chain/[chain]/blocks/[block]/context/[rollup_hash]/pending_bonded_commitments *)
  val get_pending_bonded_commitments :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    rollup:string ->
    pkh:string ->
    Client.t ->
    JSON.t Runnable.process

  module Forge : sig
    module Inbox : sig
      val message_hash :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process

      val merkle_tree_hash :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process

      val merkle_tree_path :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process
    end

    module Commitment : sig
      val merkle_tree_hash :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process

      val merkle_tree_path :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process

      val message_result_hash :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process
    end

    module Withdraw : sig
      val withdraw_list_hash :
        ?endpoint:Client.endpoint ->
        ?hooks:Process.hooks ->
        ?chain:string ->
        ?block:string ->
        data:JSON.u ->
        Client.t ->
        JSON.t Runnable.process
    end
  end
end

module Sc_rollup : sig
  (** Call RPC /chain/[chain]/blocks/[block]/context/sc_rollup *)
  val list :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/sc_rollup/[rollup_hash]/state *)
  val get_inbox :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    sc_rollup_address:string ->
    Client.t ->
    JSON.t Lwt.t

  (** Call RPC /chain/[chain]/blocks/[block]/context/sc_rollup/[rollup_hash]/initial_level *)
  val get_initial_level :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    sc_rollup_address:string ->
    Client.t ->
    JSON.t Lwt.t
end

val raw_bytes :
  ?endpoint:Client.endpoint ->
  ?hooks:Process.hooks ->
  ?chain:string ->
  ?block:string ->
  ?path:string list ->
  Client.t ->
  JSON.t Lwt.t

module Curl : sig
  (** [get ()] returns [Some curl] where [curl ~url] returns the raw response obtained
      by curl when requesting [url]. Returns [None] if [curl] cannot be found. *)
  val get : unit -> (url:string -> JSON.t Lwt.t) option Lwt.t

  (** [post data] returns [Some curl] where [curl ~url data] returns the raw
      response obtained by curl when posting the data to [url]. Returns [None] if
      [curl] cannot be found. *)
  val post : unit -> (url:string -> JSON.t -> JSON.t Lwt.t) option Lwt.t
end
