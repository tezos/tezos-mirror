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

module type S = sig
  module PVM : Pvm.S

  module Accounted_pvm :
    Fueled_pvm.S with module PVM = PVM and type fuel = Fuel.Accounted.t

  module Free_pvm :
    Fueled_pvm.S with module PVM = PVM and type fuel = Fuel.Free.t

  (** [process_head node_ctxt ~predecessor head (inbox, messages)] interprets
      the [messages] associated with a [head] (where [predecessor] is the
      predecessor of [head] in the L1 chain). This requires the [inbox] to be
      updated beforehand. It returns [(ctxt, num_messages, num_ticks, tick)]
      where [ctxt] is the updated layer 2 context (with the new PVM state),
      [num_messages] is the number of [messages], [num_ticks] is the number of
      ticks taken by the PVM for the evaluation and [tick] is the tick reached
      by the PVM after the evaluation. *)
  val process_head :
    Node_context.rw ->
    'a Context.t ->
    predecessor:Layer1.head ->
    Layer1.head ->
    Sc_rollup.Inbox.t * Sc_rollup.Inbox_message.t list ->
    ('a Context.t * int * int64 * Sc_rollup.Tick.t) tzresult Lwt.t

  (** [state_of_tick node_ctxt tick level] returns [Some (state, hash)]
      for a given [tick] if this [tick] happened before
      [level]. Otherwise, returns [None].*)
  val state_of_tick :
    _ Node_context.t ->
    Sc_rollup.Tick.t ->
    Raw_level.t ->
    (PVM.state * PVM.hash) option tzresult Lwt.t

  (** [state_of_head node_ctxt ctxt head] returns the state corresponding to the
      block [head], or the state at rollup genesis if the block is before the
      rollup origination. *)
  val state_of_head :
    'a Node_context.t ->
    'a Context.t ->
    Layer1.head ->
    ('a Context.t * PVM.state) tzresult Lwt.t
end

(** Functor to construct an interpreter for a given PVM. *)
module Make (PVM : Pvm.S) : S with module PVM = PVM
