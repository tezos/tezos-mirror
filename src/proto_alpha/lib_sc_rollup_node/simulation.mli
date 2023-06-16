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

open Protocol
open Protocol.Alpha_context
module Fueled_pvm = Fueled_pvm.Free

type level_position = Start | Middle | End

type info_per_level = {
  predecessor_timestamp : Timestamp.time;
  predecessor : Block_hash.t;
}

(** Type of the state for a simulation. *)
type t = {
  ctxt : Context.ro;
  inbox_level : Raw_level.t;
  state : Context.tree;
  reveal_map : string Sc_rollup_reveal_hash.Map.t option;
  nb_messages_inbox : int;
  level_position : level_position;
  info_per_level : info_per_level;
}

(** [start_simulation node_ctxt reveal_source block] starts a new simulation {e
    on top} of [block], i.e. for an hypothetical new inbox (level).  *)
val start_simulation :
  Node_context.ro ->
  reveal_map:string Sc_rollup_reveal_hash.Map.t option ->
  Layer1.head ->
  t tzresult Lwt.t

(** [simulate_messages node_ctxt sim messages] runs a simulation of new
    [messages] in the given simulation (state) [sim] and returns a new
    simulation state, the remaining fuel (when [?fuel] is provided) and the
    number of ticks that happened. *)
val simulate_messages :
  Node_context.ro -> t -> string list -> (t * Z.t) tzresult Lwt.t

(** [end_simulation node_ctxt sim] adds and [End_of_level] message and marks the
    simulation as ended. *)
val end_simulation : Node_context.ro -> t -> (t * Z.t) tzresult Lwt.t
