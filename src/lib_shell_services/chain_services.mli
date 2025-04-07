(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

type chain = [`Main | `Test | `Hash of Chain_id.t]

val parse_chain : string -> (chain, string) result

val to_string : chain -> string

val chain_arg : chain Tezos_rpc.Arg.t

type invalid_block = {hash : Block_hash.t; level : Int32.t; errors : error list}

type prefix = unit * chain

type protocol_info = {
  protocol : Protocol_hash.t;
  proto_level : int;
      (* Level of protocol in the sequence of protocol activations. *)
  activation_block : Block_hash.t * int32;
      (* The activation block for a protocol is the migration block, i.e. the
         last level of the previous protocol. *)
}

(* Information regarding the last advertised head of a given peer. *)
type active_peers_info = {
  peer_id : P2p_peer.Id.t;
  block_hash : Block_hash.t;
  block_level : Int32.t;
}

type delegators_contribution = {
  own_delegated : int64;
  delegators_contributions : (string * int64) list;
  former_delegators_unstake_requests : int64;
  overstaked : int64;
  total_delegated_including_overdelegated : int64;
  total_delegated_after_limits : int64;
  overdelegated : int64;
}

module Delegators_contribution_errors : sig
  type error +=
    | Cycle_too_far_in_future
    | Cycle_too_far_in_past
    | Protocol_not_supported of {protocol_hash : Protocol_hash.t}
end

val path : (unit, prefix) Tezos_rpc.Path.path

open Tezos_rpc.Context

val chain_id : #simple -> ?chain:chain -> unit -> Chain_id.t tzresult Lwt.t

module Mempool = Block_services.Empty.Mempool

module Levels : sig
  val checkpoint :
    #simple -> ?chain:chain -> unit -> (Block_hash.t * int32) tzresult Lwt.t

  val savepoint :
    #simple -> ?chain:chain -> unit -> (Block_hash.t * int32) tzresult Lwt.t

  val caboose :
    #simple -> ?chain:chain -> unit -> (Block_hash.t * int32) tzresult Lwt.t
end

module Blocks : sig
  val list :
    #simple ->
    ?chain:chain ->
    ?heads:Block_hash.t list ->
    ?length:int ->
    ?min_date:Time.Protocol.t ->
    unit ->
    Block_hash.t list list tzresult Lwt.t

  include module type of Block_services.Empty

  type protocols = {
    current_protocol : Protocol_hash.t;
    next_protocol : Protocol_hash.t;
  }

  val protocols :
    #Tezos_rpc.Context.simple ->
    ?chain:chain ->
    ?block:Block_services.block ->
    unit ->
    protocols tzresult Lwt.t
end

module Invalid_blocks : sig
  val list :
    #simple -> ?chain:chain -> unit -> invalid_block list tzresult Lwt.t

  val get :
    #simple -> ?chain:chain -> Block_hash.t -> invalid_block tzresult Lwt.t

  val delete : #simple -> ?chain:chain -> Block_hash.t -> unit tzresult Lwt.t
end

module Protocols : sig
  val list :
    #simple -> ?chain:chain -> unit -> protocol_info list tzresult Lwt.t

  val get :
    #simple -> ?chain:chain -> Protocol_hash.t -> protocol_info tzresult Lwt.t
end

module S : sig
  val chain_id :
    ([`GET], prefix, prefix, unit, unit, Chain_id.t) Tezos_rpc.Service.t

  val is_bootstrapped :
    ( [`GET],
      prefix,
      prefix,
      unit,
      unit,
      bool * Chain_validator_worker_state.synchronisation_status )
    Tezos_rpc.Service.t

  val force_bootstrapped :
    ([`PATCH], prefix, prefix, unit, bool, unit) Tezos_rpc.Service.t

  val active_peers_heads :
    ( [`GET],
      prefix,
      prefix,
      unit,
      unit,
      active_peers_info list )
    Tezos_rpc.Service.t

  val delegators_contribution :
    ( [`GET],
      prefix,
      (prefix * int32) * Signature.public_key_hash,
      unit,
      unit,
      delegators_contribution )
    Tezos_rpc.Service.t

  module Levels : sig
    val checkpoint :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        Block_hash.t * int32 )
      Tezos_rpc.Service.t

    val savepoint :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        Block_hash.t * int32 )
      Tezos_rpc.Service.t

    val caboose :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        Block_hash.t * int32 )
      Tezos_rpc.Service.t
  end

  module Blocks : sig
    val path : (prefix, prefix) Tezos_rpc.Path.t

    val list :
      ( [`GET],
        prefix,
        prefix,
        < heads : Block_hash.t list
        ; length : int option
        ; min_date : Time.Protocol.t option >,
        unit,
        Block_hash.t list list )
      Tezos_rpc.Service.t
  end

  module Invalid_blocks : sig
    val list :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        invalid_block list )
      Tezos_rpc.Service.t

    val get :
      ( [`GET],
        prefix,
        prefix * Block_hash.t,
        unit,
        unit,
        invalid_block )
      Tezos_rpc.Service.t

    val delete :
      ( [`DELETE],
        prefix,
        prefix * Block_hash.t,
        unit,
        unit,
        unit )
      Tezos_rpc.Service.t
  end

  module Protocols : sig
    val list :
      ( [`GET],
        prefix,
        prefix,
        unit,
        unit,
        protocol_info list )
      Tezos_rpc.Service.t

    val get :
      ( [`GET],
        prefix,
        prefix * Protocol_hash.t,
        unit,
        unit,
        protocol_info )
      Tezos_rpc.Service.t
  end
end
