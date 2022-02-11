(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** This event is emitted whenever a node is started. *)
val preamble_warning : unit -> unit Lwt.t

(** This event is emitted whenever the configuration file has been written. *)
val configuration_was_written :
  into:string -> config:Configuration.t -> unit tzresult Lwt.t

(** This event is emitted whenever a node is starting. *)
val starting_node : unit -> unit tzresult Lwt.t

(** This event is emitted whenever a node is ready. *)
val node_is_ready : rpc_addr:string -> rpc_port:int -> unit tzresult Lwt.t

(** This event is emitted whenever a node is shutting down. *)
val node_is_shutting_down : exit_status:int -> unit Lwt.t

(** This event is emitted whenever a node is not reachable. *)
val cannot_connect : delay:float -> unit Lwt.t

(** This event is emitted whenever the connection to the node is lost. *)
val connection_lost : unit -> unit Lwt.t

(** This event is emitted whenever a new hash is processed by the node. *)
val new_block : Block_hash.t -> unit tzresult Lwt.t

(** This event is emitted whenever a node is processing a new block. *)
val processing_block :
  block_hash:Block_hash.t ->
  predecessor_hash:Block_hash.t ->
  unit tzresult Lwt.t

(** This event is emitted whenever a new block is processed. *)
val block_processed : Block_hash.t -> unit tzresult Lwt.t

(** This event is emitted whenever a block is already processed. *)
val block_already_seen : Block_hash.t -> unit tzresult Lwt.t

(** This event is emitted whenever a node is processing and block predecessor. *)
val processing_block_predecessor : Block_hash.t -> unit tzresult Lwt.t

(** This event is emitted whenever messages that have to be stored are computed. *)
val messages_application : int -> unit tzresult Lwt.t

(** This event is emitted whenever an inbox has been stored. *)
val inbox_stored :
  block_hash:Block_hash.t ->
  messages:Protocol.Alpha_context.Tx_rollup_message.t list ->
  cumulated_size:int ->
  unit tzresult Lwt.t

(** This event is emitted whenever the storage is loaded. *)
val irmin_store_loaded : string -> unit tzresult Lwt.t

(** This event is emitted whenever a new head is stored. *)
val new_tezos_head : Block_hash.t -> unit tzresult Lwt.t
