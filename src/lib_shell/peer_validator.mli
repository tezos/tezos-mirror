(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

type t

val peer_id : t -> P2p_peer.Id.t

val create :
  ?notify_new_block:(Block_validator.new_block -> unit) ->
  ?notify_termination:(unit -> unit) ->
  Shell_limits.peer_validator_limits ->
  Block_validator.t ->
  Distributed_db.chain_db ->
  P2p_peer.Id.t ->
  t Lwt.t

val shutdown : t -> unit Lwt.t

val notify_branch : t -> Block_locator.t -> unit

val notify_head : t -> Block_hash.t -> Block_header.t -> unit

val running_workers : unit -> ((Chain_id.t * P2p_peer.Id.t) * t) list

val status : t -> Worker_types.worker_status

val information : t -> Worker_types.worker_information

val get_last_advertised_head : t -> Block_hash.t * Block_header.t

val current_request :
  t ->
  (Time.System.t * Time.System.t * Peer_validator_worker_state.Request.view)
  option

val pipeline_length : t -> Peer_validator_worker_state.pipeline_length

module Internal_for_tests : sig
  val validate_new_head :
    t -> Block_hash.t -> Block_header.t -> (unit, error trace) result Lwt.t
end
