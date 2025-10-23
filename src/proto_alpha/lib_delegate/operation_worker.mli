(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** The operation worker is responsible for listening to the node's mempool
    and collecting incoming operations in an accessible operation pool. Upon
    request, it can monitor the (pre)quorum status for a given payload and
    report its completion through the provided quorum stream.

    Only one payload quorum or prequorum can be monitored at a time; new
    monitoring requests override the previous one.
*)

open Protocol
open Alpha_context

(** {1 Datatypes}*)

type t

type candidate = {
  hash : Block_hash.t;
  level_watched : Int32.t;
  round_watched : Round.t;
  payload_hash_watched : Block_payload_hash.t;
  branch_watched : Block_hash.t option;
}

val candidate_encoding : candidate Data_encoding.t

type event =
  | Prequorum_reached of candidate * Kind.preattestation operation list
  | Quorum_reached of candidate * Kind.attestation operation list

(** {1 Constructors}*)

(** [run ?monitor_node_operations ~round_durations cctxt] spawns an operation
    worker.

    @param monitor_node_operations monitor operations on the node (defaults:
   [true]).  Set [monitor_node_operations] to [false] to only consider
   externally provided (non-node) operations.  *)
val run :
  ?monitor_node_operations:bool ->
  round_durations:Protocol.Alpha_context.Round.round_durations ->
  #Protocol_client_context.full ->
  t Lwt.t

(** {1 Utilities} *)

val retrieve_pending_operations :
  #Protocol_client_context.full -> t -> unit tzresult Lwt.t

(** {1 Accessors}*)

val get_current_operations : t -> Operation_pool.pool

val get_quorum_event_stream : t -> event Lwt_stream.t

(** {1 Observers} *)

(** [monitor_preattestation_quorum state threshold get_slot_voting_power candidate]
    Register [candidate] as the currently monitored payload, overriding any other
    prequorum or quorum payload. Completion of the prequorum is signaled through the
    quorum event stream.
*)
val monitor_preattestation_quorum :
  t ->
  consensus_threshold:int64 ->
  consensus_committee:int64 ->
  get_slot_voting_power:(slot:Slot.t -> int64 option) ->
  candidate ->
  unit Lwt.t

(** [monitor_attestation_quorum state threshold get_slot_voting_power candidate]
    Register [candidate] as the currently monitored payload, overriding any other
    prequorum or quorum payload. Completion of the quorum is signaled through the
    quorum event stream.
*)
val monitor_attestation_quorum :
  t ->
  consensus_threshold:int64 ->
  consensus_committee:int64 ->
  get_slot_voting_power:(slot:Slot.t -> int64 option) ->
  candidate ->
  unit Lwt.t

(** [cancel_monitoring state] removes current monitored payload.
    Does nothing if no payload is being monitored.
*)
val cancel_monitoring : t -> unit

(** [shutdown_worker state] closes the monitor_operations stream and
    removes current monitored payload.
*)
val shutdown_worker : t -> (unit, exn list) result Lwt.t
