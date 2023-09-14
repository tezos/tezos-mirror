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

val node_is_ready : unit t

val data_dir_not_found : string t

val failed_to_persist_profiles :
  (Services.Types.profiles * Error_monad.tztrace) t

val fetched_slot : (int * int) t

val layer1_node_new_head : (Block_hash.t * int32) t

val layer1_node_final_block : int32 t

val layer1_node_tracking_started : unit t

val layer1_node_tracking_started_for_plugin : unit t

val protocol_plugin_resolved : Protocol_hash.t t

val no_protocol_plugin : unit t

val unexpected_protocol_plugin : unit t

val daemon_error : Error_monad.tztrace t

val configuration_loaded : unit t

val stored_slot_content : Cryptobox.Commitment.t t

val stored_slot_shard : (Cryptobox.Commitment.t * int) t

val decoding_data_failed : Types.kind t

val loading_shard_data_failed : string t

val message_validation_error : (Gossipsub.message_id * string) t

val p2p_server_is_ready : P2p_point.Id.t t

val rpc_server_is_ready : P2p_point.Id.t t

val metrics_server_is_ready : (string * int) t
