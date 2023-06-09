(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

open Protocol.Alpha_context

(** [process_head node_ctxt ~predecessor head (inbox, messages)] interprets the
    [messages] associated with a [head] (where [predecessor] is the predecessor
    of [head] in the L1 chain). This requires the [inbox] to be updated
    beforehand. It returns [(ctxt, num_messages, num_ticks, tick)] where [ctxt]
    is the updated layer 2 context (with the new PVM state), [num_messages] is
    the number of [messages], [num_ticks] is the number of ticks taken by the
    PVM for the evaluation and [tick] is the tick reached by the PVM after the
    evaluation. *)
val process_head :
  Node_context.rw ->
  'a Context.t ->
  predecessor:Layer1.header ->
  Layer1.header ->
  Octez_smart_rollup.Inbox.t * string list ->
  ('a Context.t * int * int64 * Z.t) tzresult Lwt.t

(** [state_of_tick node_ctxt ?start_state tick level] returns [Some (state,
    hash)] for a given [tick] if this [tick] happened before [level]. Otherwise,
    returns [None]. If provided, the evaluation is resumed from
    [start_state]. *)
val state_of_tick :
  _ Node_context.t ->
  ?start_state:Fueled_pvm.Accounted.eval_state ->
  Sc_rollup.Tick.t ->
  Raw_level.t ->
  Fueled_pvm.Accounted.eval_state option tzresult Lwt.t

(** [state_of_head node_ctxt ctxt head] returns the state corresponding to the
    block [head], or the state at rollup genesis if the block is before the
    rollup origination. *)
val state_of_head :
  'a Node_context.t ->
  'a Context.t ->
  Layer1.head ->
  ('a Context.t * Context.tree) tzresult Lwt.t
