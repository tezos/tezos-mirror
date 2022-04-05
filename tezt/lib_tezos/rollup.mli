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

module Tx_rollup : sig
  type range = Empty of int | Interval of int * int

  type state = {
    finalized_commitments : range;
    unfinalized_commitments : range;
    uncommitted_inboxes : range;
    tezos_head_level : int option;
    commitment_newest_hash : string option;
    burn_per_byte : int;
    inbox_ema : int;
  }

  type inbox = {inbox_length : int; cumulated_size : int; merkle_root : string}

  type messages = {
    count : int;
    root : string;
    last_message_result_hash : string;
  }

  type commitment = {
    level : int;
    messages : messages;
    predecessor : string option;
    inbox_merkle_root : string;
  }

  type submitted_commitment = {
    commitment : commitment;
    commitment_hash : string;
    committer : string;
    submitted_at : int;
    finalized_at : int option;
  }

  type message = [`Batch of Hex.t]

  val make_batch : string -> message

  val get_state :
    ?hooks:Process.hooks -> rollup:string -> Client.t -> state Process.runnable

  val get_inbox :
    ?hooks:Process.hooks ->
    rollup:string ->
    level:int ->
    Client.t ->
    inbox option Process.runnable

  val get_commitment :
    ?hooks:Process.hooks ->
    ?block:string ->
    rollup:string ->
    level:int ->
    Client.t ->
    submitted_commitment option Process.runnable

  val get_pending_bonded_commitments :
    ?hooks:Process.hooks ->
    ?block:string ->
    rollup:string ->
    pkh:string ->
    Client.t ->
    JSON.t Process.runnable

  val message_hash :
    ?hooks:Process.hooks ->
    message:message ->
    Client.t ->
    [> `Hash of string] Process.runnable

  val inbox_merkle_tree_hash :
    ?hooks:Process.hooks ->
    message_hashes:[`Hash of string] list ->
    Client.t ->
    [> `Hash of string] Process.runnable

  val inbox_merkle_tree_path :
    ?hooks:Process.hooks ->
    message_hashes:[`Hash of string] list ->
    position:int ->
    Client.t ->
    JSON.t Process.runnable

  val commitment_merkle_tree_hash :
    ?hooks:Process.hooks ->
    message_result_hashes:[`Hash of string] list ->
    Client.t ->
    [> `Hash of string] Process.runnable

  val commitment_merkle_tree_path :
    ?hooks:Process.hooks ->
    message_result_hashes:[`Hash of string] list ->
    position:int ->
    Client.t ->
    JSON.t Process.runnable

  val compute_inbox_from_messages :
    ?hooks:Process.hooks -> message list -> Client.t -> inbox Lwt.t

  module Check : sig
    val state : state Check.typ

    val inbox : inbox Check.typ

    val commitment : submitted_commitment Check.typ
  end

  module Parameters : sig
    type t = {finality_period : int; withdraw_period : int}

    val default : t

    val parameter_file : ?parameters:t -> Protocol.t -> string Lwt.t
  end
end
