(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

(** [generate_proof node_ctxt (game) start_state] generates a serialized proof
    for the current [game] for the execution step starting with
    [start_state]. *)
val generate_proof :
  Node_context.rw -> Game.t -> Context.tree -> string tzresult Lwt.t

(** [state_of_tick node_ctxt ?start_state ~tick level] returns [Some
    (state, hash)] for a given [tick] if this [tick] happened before
    [level]. Otherwise, returns [None]. If provided, the evaluation is resumed
    from [start_state]. *)
val state_of_tick :
  _ Node_context.t ->
  ?start_state:Fuel.Accounted.t Pvm_plugin_sig.eval_state ->
  tick:Z.t ->
  int32 ->
  Fuel.Accounted.t Pvm_plugin_sig.eval_state option tzresult Lwt.t

(** [make_dissection node_ctxt ~start_state ~start_chunk ~our_stop_chunk
    ~default_number_of_sections ~last_level] computes a dissection from between
    [start_chunk] and [our_stop_chunk] at level [last_level]. This dissection
    has [default_number_of_sections] if there are enough ticks. *)
val make_dissection :
  _ Node_context.t ->
  start_state:Fuel.Accounted.t Pvm_plugin_sig.eval_state option ->
  start_chunk:Game.dissection_chunk ->
  our_stop_chunk:Game.dissection_chunk ->
  default_number_of_sections:int ->
  last_level:int32 ->
  Game.dissection_chunk trace tzresult Lwt.t

(** [timeout_reached node_ctxt ~self ~opponent] returns [true] if the
    timeout is reached against opponent in head of the L1 chain. *)
val timeout_reached :
  _ Node_context.t ->
  self:Signature.public_key_hash ->
  opponent:Signature.public_key_hash ->
  bool tzresult Lwt.t

(** [get_conflicts cctxt rollup signer] returns the conflicts for commitments
    staked on by [signer]. *)
val get_conflicts :
  Client_context.full ->
  Address.t ->
  Signature.public_key_hash ->
  Game.conflict list tzresult Lwt.t

(** [get_ongoing_games cctxt rollup signer] returns the games that [signer] is
    currently playing. *)
val get_ongoing_games :
  Client_context.full ->
  Address.t ->
  Signature.public_key_hash ->
  (Game.t * Signature.public_key_hash * Signature.public_key_hash) list tzresult
  Lwt.t
