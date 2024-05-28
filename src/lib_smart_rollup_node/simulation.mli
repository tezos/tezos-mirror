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

type level_position = Start | Middle | End

type info_per_level = {
  predecessor_timestamp : Time.Protocol.t;
  predecessor : Block_hash.t;
}

(** Type of the state for a simulation. *)
type t = {
  node_ctxt : Node_context.ro;
  ctxt : Context.ro;
  inbox_level : int32;
  state : Context.pvmstate;
  reveal_map : string Utils.Reveal_hash_map.t option;
  nb_messages_inbox : int;
  level_position : level_position;
  info_per_level : info_per_level;
  plugin : (module Protocol_plugin_sig.S);
}

(** [start_simulation node_ctxt ~reveal_map ?log_kernel_debug_file block] starts
    a new simulation {e on top} of [block], i.e. for an hypothetical new inbox
    (level). If [log_kernel_debug_file] is provided, kernel logs will be written
    to [node_ctxt.data_dir/simulation_kernel_logs/log_kernel_debug_file]. *)
val start_simulation :
  Node_context.ro ->
  reveal_map:string Utils.Reveal_hash_map.t option ->
  ?log_kernel_debug_file:string ->
  Layer1.head ->
  t tzresult Lwt.t

(** [simulate_messages sim messages] runs a simulation of new [messages] in the
    given simulation (state) [sim] and returns a new simulation state, the
    remaining fuel (when [?fuel] is provided) and the number of ticks that
    happened. *)
val simulate_messages : t -> string list -> (t * Z.t) tzresult Lwt.t

(** [end_simulation sim] adds and [End_of_level] message and marks the
    simulation as ended. *)
val end_simulation : t -> (t * Z.t) tzresult Lwt.t
