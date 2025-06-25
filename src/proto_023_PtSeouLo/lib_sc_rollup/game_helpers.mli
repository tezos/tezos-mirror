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

open Protocol.Alpha_context.Sc_rollup

(** [default_new_dissection ~default_number_of_sections ~start_chunk
    ~our_stop_chunk] computes a list of intermediary ticks that can
    later be turned into new dissection from between [start_chunk] and
    [our_stop_chunk] with [make_dissection]. The algorithm satisfies
    the default predicate on dissection exported by the protocol. *)
val default_new_dissection :
  default_number_of_sections:int ->
  start_chunk:Game.dissection_chunk ->
  our_stop_chunk:Game.dissection_chunk ->
  Tick.t list

(** [make_dissection ~state_hash_from_tick ~start_chunk
    ~our_stop_chunk intermediary_ticks] computes a new dissection from
    a list of intermediary ticks between [start_chunk] and
    [our_stop_chunk].

    This function assumes [intermediary_ticks] encodes a valid
    dissection from [start_chunk] to [our_stop_chunk], and recomputes
    the state hash associated to each ticks. *)
val make_dissection :
  state_of_tick:(?start_state:'a -> Tick.t -> ('a option, 'trace) result Lwt.t) ->
  state_hash_of_eval_state:('a -> State_hash.t) ->
  ?start_state:'a ->
  start_chunk:Dissection_chunk.t ->
  our_stop_chunk:Dissection_chunk.t ->
  Tick.t list ->
  (Dissection_chunk.t list, 'trace) result Lwt.t

module Wasm : sig
  (** [new_dissection ~default_number_of_sections ~start_chunk
      ~our_stop_chunk] computes a dissection that satisfies the
      dissection predicate of the WASM PVM, that is all the ticks in
      the dissection are aligned with the size of a snapshot.

      If [start_chunk] is not a multiple of the size of a snapshot or
      if the distance between [start_chunk] and [stop_chunk] is not
      greater than the size of a snapshot, then
      {!default_new_dissection} is called, because it means the WASM
      PVM is stuck.  *)
  val new_dissection :
    default_number_of_sections:int ->
    start_chunk:Game.dissection_chunk ->
    our_stop_chunk:Game.dissection_chunk ->
    Tick.t list
end
