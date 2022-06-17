(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Octez RPCs. *)

(** [RPC_core] contains functions to declare and call RPCs.
    It does not contain any RPC definition: those are in this module. *)
include module type of RPC_core

(** [RPC_legacy] contains RPCs implemented using a deprecated approach
    that does not allow to use cohttp. If you need to call functions
    from [RPC_legacy], it is recommended to port them to this module
    in order to be able to use cohttp, which is more efficient than using
    the client. *)
include module type of RPC_legacy

(** {2 Naming Conventions} *)

(** Functions in this module are named after the RPC they implement.

    - The name starts with the HTTP verb, in lowercase, followed by an underscore.
      E.g. [get_] for GET, [patch_] for PATCH, [delete_] for DELETE, etc.

    - Then the name contains all constant parts of the endpoint path,
      separated by underscores.
      E.g. [chain_levels_caboose] for [GET /chains/[chain]/levels/caboose].
      The dynamic part [[chain]] is dropped (it becomes an argument of the function).

    - When a word is plural, it becomes singular if the RPC selects one element.
      For instance, [GET /network/connections] becomes [get_network_connections]
      because it returns all elements of the list, but [GET /network/connections/<peer_id>]
      becomes [get_network_connection] because it returns only one connection.
      This allows to differentiate the two RPCs.
      Another example is [GET /chains/[chain]/blocks/[block]/metadata] which becomes
      [get_chain_block_metadata] since it selects one block.
      Another example is [GET /chains/[chain]/levels/checkpoint] which becomes
      [get_chain_level_checkpoint], which illustrates that the selector (here [checkpoint])
      does not need to be dynamic for this rule to apply.

    - Submodules are not used. Do not group all [/network] RPCs in a [Network]
      submodule for instance. *)

(** {2 RPC Definitions} *)

(** RPC: [GET /config] *)
val get_config : JSON.t t

(** RPC: [GET /network/connections]

    Returns the list of [(address, port)] pairs. *)
val get_network_connections : (string * int) list t

(** RPC: [GET /network/connections/<peer_id>]

    Returns the address and port of the given peer ID if connected.
    This RPC returns 404 Not Found if the peer ID is not connected. *)
val get_network_connection : string -> (string * int) t

(** RPC: [GET /network/self] *)
val get_network_self : string t

(** RPC: [GET /network/greylist/ips] *)
val get_network_greylist_ips : JSON.t t

(** RPC: [GET /network/greylist/clear] *)
val get_network_greylist_clear : JSON.t t

(** RPC: [GET /chains/<chain>/blocks]

    [chain] defaults to ["main"]. *)
val get_chain_blocks : ?chain:string -> unit -> JSON.t t

(** RPC: [GET /chains/<chain>/invalid_blocks]

    [chain] defaults to ["main"]. *)
val get_chain_invalid_blocks : ?chain:string -> unit -> JSON.t t

(** RPC: [GET /chains/<chain>/blocks/<block>/context/nonces/<block_level>]

    [chain] defaults to ["main"]. *)
val get_chain_block_context_nonce :
  ?chain:string -> ?block:string -> int -> JSON.t t

(** RPC: [GET /chains/<chain>/blocks/<block>/header/raw]

    [chain] defaults to ["main"].
    [block] defaults to ["head"].
*)
val get_chain_block_header_raw :
  ?chain:string -> ?block:string -> unit -> JSON.t t

(** RPC: [GET /chains/<chain>/blocks/<block>/live_blocks]

    [chain] defaults to ["main"].
    [block] defaults to ["head"]. *)
val get_chain_block_live_blocks :
  ?chain:string -> ?block:string -> unit -> JSON.t t

(** RPC: [GET /chains/<chain>/blocks/<block>/operation_hashes]

    - [chain] defaults to ["main"].
    - [block] defaults to ["head"].
*)
val get_chain_block_operation_hashes :
  ?chain:string -> ?block:string -> unit -> string list list t

(** RPC: [GET /chains/<chain>/blocks/<block>/operation_hashes/<validation_pass>]

    - [chain] defaults to ["main"].
    - [block] defaults to ["head"].
*)
val get_chain_block_operation_hashes_of_validation_pass :
  ?chain:string -> ?block:string -> int -> string list t

(** RPC: [GET /chains/<chain>/blocks/<block>/operation_hashes/<validation_pass>/<operation_offset>]

    - [chain] defaults to ["main"].
    - [block] defaults to ["head"].
*)
val get_chain_block_operation_hash :
  ?chain:string ->
  ?block:string ->
  validation_pass:int ->
  operation_offset:int ->
  unit ->
  string t

(** RPC: [GET /chains/<chain>/blocks/<block>/helpers/complete/<prefix>]

    [chain] defaults to ["main"].
    [block] defaults to ["head"].
*)
val get_chain_block_helper_complete :
  ?chain:string -> ?block:string -> string -> JSON.t t

(** RPC: [GET /network/peers] *)
val get_network_peers : (string * JSON.t) list t

(** RPC: [GET /network/peers/<peer_id>] *)
val get_network_peer : string -> JSON.t t

(** RPC: [GET /network/peer/<peer_id>/ban] *)
val get_network_peer_ban : string -> JSON.t t

(** RPC: [GET /network/peers/<peer_id>/banned] *)
val get_network_peer_banned : string -> JSON.t t

(** RPC: [GET /network/peers/<peer_id>/unban] *)
val get_network_peer_unban : string -> JSON.t t

(** RPC: [GET /network/peers/<peer_id>/untrust] *)
val get_network_peer_untrust : string -> JSON.t t

(** RPC: [GET /network/peers/<peer_id>/trust] *)
val get_network_peer_trust : string -> JSON.t t

(** RPC: [GET /network/points] *)
val get_network_points : (string * JSON.t) list t

(** RPC: [GET /network/points/<point_id>] *)
val get_network_point : string -> JSON.t t

(** RPC: [GET /network/points/<point_id>/ban] *)
val get_network_point_ban : string -> JSON.t t

(** RPC: [GET /network/points/<point_id>/banned] *)
val get_network_point_banned : string -> JSON.t t

(** RPC: [GET /network/points/<point_id>/unban] *)
val get_network_point_unban : string -> JSON.t t

(** RPC: [GET /network/points/<point_id>/untrust] *)
val get_network_point_untrust : string -> JSON.t t

(** RPC: [GET /network/points/<point_id>/trust] *)
val get_network_point_trust : string -> JSON.t t

(** RPC: [GET /network/stat] *)
val get_network_stat : JSON.t t

(** RPC: [GET /network/version] *)
val get_network_version : JSON.t t

(** RPC: [GET /network/versions] *)
val get_network_versions : JSON.t t

(** RPC: [POST /private/injection/operations]

    Returns the hashes of the operations that were injected. *)
val post_private_injection_operations :
  ?force:bool ->
  ?async:bool ->
  ops:Hex.t list ->
  unit ->
  [`OpHash of string] list t

(** RPC: [POST /injection/operation] *)
val post_injection_operation : ?async:bool -> JSON.u -> JSON.t t

(** RPC: [POST /private/injection/operation] *)
val post_private_injection_operation : ?async:bool -> JSON.u -> JSON.t t

(** RPC: [POST /chains/[chain]/blocks/[block]/helpers/scripts/run_operation]

    [chain] defaults to ["main"].
    [block] defaults to ["head"]. *)
val post_run_operation :
  ?chain:string -> ?block:string -> ?async:bool -> JSON.u -> JSON.t t

(** RPC: [GET /chains/[chain]/chain_id]

    Returns the chain ID. *)
val get_chain_chain_id : ?chain:string -> unit -> string t

(** RPC: [GET /chains/[chain]/blocks/[block]]

    [chain] defaults to ["main"].
    [block] defaults to ["head"]. *)
val get_chain_block : ?chain:string -> ?block:string -> unit -> JSON.t t

type block_metadata = {
  protocol : string;
  next_protocol : string;
  proposer : string;
  max_operations_ttl : int;
  dal_slot_availability : bool Array.t option;
      (** This field is [None] if and only if the [DAL] feature flag is disabled. *)
}

(** RPC: [GET /chains/[chain]/blocks/[block]/metadata]

    [chain] defaults to ["main"].
    [block] defaults to ["head"]. *)
val get_chain_block_metadata :
  ?chain:string -> ?block:string -> unit -> block_metadata t

(** RPC: [GET /chains/[chain]/blocks/[block]/hash]

    Returns the hash. *)
val get_chain_block_hash : ?chain:string -> ?block:string -> unit -> string t

(** RPC: [GET /chains/[chain]/blocks/[block]/header]

    [chain] defaults to ["main"].
    [block] defaults to ["head"]. *)
val get_chain_block_header : ?chain:string -> ?block:string -> unit -> JSON.t t

(** RPC: [PATCH /chains/[chain]] to set ["bootstrapped"]

    Example: to force the chain to be considered bootstrapped,
    use [patch_chain_bootstrapped true]. *)
val patch_chain_bootstrapped : ?chain:string -> bool -> unit t

(** RPC: [GET /chains/[chain]/is_bootstrapped]

    Returns the value of [sync_state], e.g. ["synced"]. *)
val get_chain_is_bootstrapped : ?chain:string -> unit -> string t

(** A level and its hash *)
type block_descriptor = {block_hash : string; level : int}

(** RPC: [GET /chains/[chain]/levels/checkpoint]

    [chain] defaults to ["main"]. *)
val get_chain_level_checkpoint : ?chain:string -> unit -> block_descriptor t

(** RPC: [GET /chains/[chain]/levels/savepoint]

    [chain] defaults to ["main"]. *)
val get_chain_level_savepoint : ?chain:string -> unit -> block_descriptor t

(** RPC: [GET /chains/[chain]/levels/caboose]

    [chain] defaults to ["main"]. *)
val get_chain_level_caboose : ?chain:string -> unit -> block_descriptor t

(** RPC: [GET /workers/block_validator] *)
val get_worker_block_validator : JSON.t t

(** RPC: [GET /workers/chain_validators] *)
val get_workers_chain_validators : JSON.t t

(** RPC: [GET /workers/chain_validators/<chain>]

      [chain] defaults to ["main"]. *)
val get_worker_chain_validator : ?chain:string -> unit -> JSON.t t

(** RPC: [GET /workers/chain_validators/<chain>/ddb]

      [chain] defaults to ["main"]. *)
val get_worker_chain_validator_ddb : ?chain:string -> unit -> JSON.t t

(** RPC: [GET /workers/chain_validators/<chain>/peers_validators]

      [chain] defaults to ["main"]. *)
val get_worker_chain_validator_peers_validators :
  ?chain:string -> unit -> JSON.t t

(** RPC: [GET /workers/prevalidators] *)
val get_workers_prevalidators : JSON.t t

(** RPC: [GET /workers/prevalidators/[chain]]

      [chain] defaults to ["main"]. *)
val get_worker_prevalidator : ?chain:string -> unit -> JSON.t t

(** RPC: [GET /errors] *)
val get_errors : JSON.t t

(** RPC: [GET /protocols *)
val get_protocols : string list t

(** RPC: [GET /protocols/<protocol_hash>] *)
val get_protocol : string -> JSON.t t

(** RPC: [GET /fetch_protocol/<protocol_hash>] *)
val get_fetch_protocol : string -> JSON.t t

(** RPC: [GET /stats/gc] *)
val get_stats_gc : JSON.t t

(** RPC: [GET /stats/memory] *)
val get_stats_memory : JSON.t t
