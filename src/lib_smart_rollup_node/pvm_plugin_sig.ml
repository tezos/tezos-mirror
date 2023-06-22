(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** Evaluation state for the PVM.  *)
type 'fuel eval_state = {
  state : Context.tree;  (** The actual PVM state. *)
  state_hash : State_hash.t;  (** Hash of [state]. *)
  tick : Z.t;  (** Tick of [state]. *)
  inbox_level : int32;  (** Inbox level in which messages are evaluated. *)
  message_counter_offset : int;
      (** Offset for message index, which corresponds to the number of
            messages of the inbox already evaluated.  *)
  remaining_fuel : 'fuel;
      (** Fuel remaining for the evaluation of the inbox. *)
  remaining_messages : string list;
      (** Messages of the inbox that remain to be evaluated.  *)
}

type 'fuel eval_result = {
  state : 'fuel eval_state;
  num_ticks : Z.t;
  num_messages : int;
}

module type FUELED_PVM = sig
  type fuel

  (** Evaluation result for the PVM which contains the evaluation state and
      additional information.  *)

  (** [eval_block_inbox ~fuel node_ctxt (inbox, messages) state] evaluates the
      [messages] for the [inbox] in the given [state] of the PVM and returns the
      evaluation result containing the new state, the number of messages, the
      inbox level and the remaining fuel. *)
  val eval_block_inbox :
    fuel:fuel ->
    _ Node_context.t ->
    Inbox.t * string list ->
    Context.tree ->
    fuel eval_result Node_context.delayed_write tzresult Lwt.t

  (** [eval_messages ?reveal_map ~fuel node_ctxt ~message_counter_offset state
      inbox_level messages] evaluates the [messages] for inbox level
      [inbox_level] in the given [state] of the PVM and returns the evaluation
      results containing the new state, the remaining fuel, and the number of
      ticks for the evaluation of these messages. If [messages] is empty, the
      PVM progresses until the next input request (within the allocated
      [fuel]). [message_counter_offset] is used when we evaluate partial
      inboxes, such as during simulation. When [reveal_map] is provided, it is
      used as an additional source of data for revelation ticks. *)
  val eval_messages :
    ?reveal_map:string Utils.Reveal_hash_map.t ->
    _ Node_context.t ->
    fuel eval_state ->
    fuel eval_result Node_context.delayed_write tzresult Lwt.t
end

module type S = sig
  val get_tick : Kind.t -> Context.tree -> Z.t Lwt.t

  val state_hash : Kind.t -> Context.tree -> State_hash.t Lwt.t

  val initial_state : Kind.t -> Context.tree Lwt.t

  val parse_boot_sector : Kind.t -> string -> string option

  val install_boot_sector :
    Kind.t -> Context.tree -> string -> Context.tree Lwt.t

  val get_status : _ Node_context.t -> Context.tree -> string Lwt.t

  val get_current_level : Kind.t -> Context.tree -> int32 option Lwt.t

  val start_of_level_serialized : string

  val end_of_level_serialized : string

  val protocol_migration_serialized : string option

  val info_per_level_serialized :
    predecessor:Block_hash.t -> predecessor_timestamp:Time.Protocol.t -> string

  module Fueled : sig
    module Free : FUELED_PVM with type fuel := Fuel.Free.t

    module Accounted : FUELED_PVM with type fuel := Fuel.Accounted.t
  end
end
