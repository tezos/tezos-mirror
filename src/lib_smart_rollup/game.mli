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

module V1 : sig
  type dissection_chunk = {state_hash : State_hash.t option; tick : Z.t}

  type step = Dissection of dissection_chunk list | Proof of string

  type refutation =
    | Start of {
        player_commitment_hash : Commitment.Hash.t;
        opponent_commitment_hash : Commitment.Hash.t;
      }
    | Move of {choice : Z.t; step : step}

  type timeout = {
    alice_timeout : int;
    bob_timeout : int;
    last_turn_level : int32;
  }

  type index = {
    alice : Signature.Public_key_hash.t;
    bob : Signature.Public_key_hash.t;
  }

  val dissection_chunk_encoding : dissection_chunk Data_encoding.t

  val dissection_encoding : dissection_chunk list Data_encoding.t

  val step_encoding : step Data_encoding.t

  val refutation_encoding : refutation Data_encoding.t

  val index_encoding : index Data_encoding.t

  val make_index :
    Signature.Public_key_hash.t -> Signature.Public_key_hash.t -> index

  type player = Alice | Bob

  type game_state =
    | Dissecting of {
        dissection : dissection_chunk list;
        default_number_of_sections : int;
      }
    | Final_move of {
        agreed_start_chunk : dissection_chunk;
        refuted_stop_chunk : dissection_chunk;
      }

  type t = {
    turn : player;
    inbox_snapshot : Inbox.V1.history_proof;
    dal_snapshot : Dal.Slot_history.t;
    start_level : int32;
    inbox_level : int32;
    game_state : game_state;
  }

  type conflict = {
    other : Signature.Public_key_hash.t;
    their_commitment : Commitment.t;
    our_commitment : Commitment.t;
    parent_commitment : Commitment.Hash.t;
  }

  val game_state_equal : game_state -> game_state -> bool

  val player_encoding : player Data_encoding.t

  val game_state_encoding : game_state Data_encoding.t

  val encoding : t Data_encoding.t

  val conflict_encoding : conflict Data_encoding.t
end

include Versioned_data.S with type t = V1.t

include
  module type of V1
    with type dissection_chunk = V1.dissection_chunk
     and type step = V1.step
     and type refutation = V1.refutation
     and type index = V1.index
     and type player = V1.player
     and type game_state = V1.game_state
     and type conflict = V1.conflict
     and type t = V1.t
