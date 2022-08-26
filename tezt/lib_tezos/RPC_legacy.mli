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

(** Legacy node RPCs. *)

(** THIS MODULE IS DEPRECATED: ITS FUNCTIONS SHOULD BE PORTED TO THE NEW RPC
    ENGINE (IN [RPC.ml], USING MODULE [RPC_core]). *)

(** In all RPCs, default [chain] is "main" and default [block] is
   "head~2" to pick the finalized branch for Tenderbake. *)

(** {2 Protocol RPCs} *)

type ctxt_type = Bytes | Json

module Contracts : sig
  (** Common protocol RPSs for contracts (i.e. under [/contracts]). *)

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

module Seed : sig
  val get_seed :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    string Lwt.t

  val get_seed_status :
    ?endpoint:Client.endpoint ->
    ?hooks:Process.hooks ->
    ?chain:string ->
    ?block:string ->
    Client.t ->
    JSON.t Lwt.t
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
