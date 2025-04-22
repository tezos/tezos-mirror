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

(** Worker module for a single refutation game player.  The node's refutation
    coordinator will spawn a new refutation player for each refutation game.
*)

(** [init_and_play node_ctxt ~self ~conflict ~game ~level] initializes a new
    refutation game player for signer [self].  After initizialization, the
    worker will play the next move depending on the [game] state.  If no [game]
    is passed, the worker will play the opening move for [conflict].  *)
val init_and_play :
  _ Node_context.rw_context ->
  self:Signature.public_key_hash ->
  conflict:Game.conflict ->
  game:Game.t option ->
  level:int32 ->
  unit tzresult Lwt.t

(** [play opponent game ~level] makes the worker for the game against [opponent]
    play the next move depending on the [game] state for their conflict.  *)
val play : Signature.public_key_hash -> Game.t -> level:int32 -> unit Lwt.t

(** Shutdown a refutation game player against a given opponent. *)
val shutdown : Signature.public_key_hash -> unit Lwt.t

(** [current_games ()] lists the opponents' this node is playing refutation
    games against. *)
val current_games : unit -> Signature.public_key_hash list
