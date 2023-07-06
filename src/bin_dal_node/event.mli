(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type 'a t

val emit : 'a t -> 'a -> unit Lwt.t

val emit__dont_wait__use_with_care : 'a t -> 'a -> unit

val starting_node : unit t

val shutdown_node : int t

val store_is_ready : unit t

val rpc_server_is_ready : P2p_point.Id.t t

val node_is_ready : unit t

val data_dir_not_found : string t

val fetched_slot : (int * int) t

val layer1_node_new_head : (Block_hash.t * int32) t

val layer1_node_tracking_started : unit t

val protocol_plugin_resolved : Protocol_hash.t t

val no_protocol_plugin : unit t

val unexpected_protocol_plugin : unit t

val daemon_error : Error_monad.tztrace t

(** An event emitted at startup when the configuration of the node is read from
    disk. *)
val configuration_loaded : unit t

(** Storing a slot content event. The given parameter is the slot's commitment
    hash. *)
val stored_slot_content : Cryptobox.Commitment.t t

(** Storing a slot's shards event. The given parameters are the slot's
    commitment hash and the number of its shards. *)
val stored_slot_shards : (Cryptobox.Commitment.t * int) t

(** Decoding a value failed. See {!Types.kind} for kind of considered
    values. *)
val decoding_data_failed : Types.kind t

(** Loading shard data from disk failed. *)
val loading_shard_data_failed : string t

(** Validating a message received via Gossipsub/P2P failed. *)
val message_validation_error : (Gossipsub.message_id * string) t

(** Emitted when the metrics server starts *)
val starting_metrics_server : (string * int) t
