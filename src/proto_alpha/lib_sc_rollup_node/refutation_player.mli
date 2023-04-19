(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Protocol
open Alpha_context

(** Worker module for a signle refutation game player.
    The node's refutation coordinator will spawn a new refutation player
    for each refutation game.
*)
module Worker : Worker.T

(** Type for a refutation game player.  *)
type worker = Worker.infinite Worker.queue Worker.t

module type S = sig
  (** [init_and_play node_ctxt ~self ~conflict ~game ~level]
      initializes  a new refutation game player for signer [self].
      After initizialization, the worker will play the next move
      depending on the [game] state.
      If no [game] is passed, the worker will play the opening
      move for [conflict].
  *)
  val init_and_play :
    Node_context.rw ->
    self:public_key_hash ->
    conflict:Sc_rollup.Refutation_storage.conflict ->
    game:Sc_rollup.Game.t option ->
    level:int32 ->
    unit tzresult Lwt.t

  (** [play worker game ~level] makes the [worker] play the next move depending
      on the [game] state for their conflict.
  *)
  val play : worker -> Sc_rollup.Game.t -> level:int32 -> unit Lwt.t

  (** Shutdown a refutaiton game player. *)
  val shutdown : worker -> unit Lwt.t

  (** [current_games ()] lists the opponents' this node is playing
      refutation games against, alongside the worker that takes care
      of each game. *)
  val current_games : unit -> (public_key_hash * worker) list
end

module Make (Interpreter : Pvm.S) : S
