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

(** [process_head plugin node_ctxt ctxt ~predecessor head (inbox, messages)]
    interprets the [messages] associated with a [head] (where [predecessor] is
    the predecessor of [head] in the L1 chain). This requires the [inbox] to be
    updated beforehand. It returns [(ctxt, num_messages, num_ticks, tick)] where
    [ctxt] is the updated layer 2 context (with the new PVM state),
    [num_messages] is the number of [messages], [num_ticks] is the number of
    ticks taken by the PVM for the evaluation and [tick] is the tick reached by
    the PVM after the evaluation.
    NOTE: [ctxt] is modified in place by [process_head]. It is the
    responsibility of the caller to make a copy and revert if needed in case of
    error.
*)
val process_head :
  (module Protocol_plugin_sig.PARTIAL) ->
  _ Node_context.t ->
  'a Context.t ->
  predecessor:Layer1.head ->
  Layer1.head ->
  Octez_smart_rollup.Inbox.t * string list ->
  (int * int64 * Z.t) tzresult Lwt.t

type patched = Patched

type unpatched = Unpatched

type both = Both

type _ genesis_state =
  | Patched : Context.pvmstate -> patched genesis_state
  | Unpatched : Context.pvmstate -> unpatched genesis_state
  | Both : {
      patched : Context.pvmstate;
      original : Context.pvmstate;
    }
      -> both genesis_state

type _ genesis_state_mode =
  | Patched : patched genesis_state_mode
  | Unpatched : unpatched genesis_state_mode
  | Both : both genesis_state_mode

(** [genesis_state plugin ?genesis_block node_ctxt] returns a pair [s1, s2]
    where [s1] is the PVM state at the genesis block and [s2] is the genesis
    state without any patches applied. [s2] is meant to be used to compute the
    genesis commitment. If there are no unsafe patches for the rollup [s2] is
    the same as [s1]. *)
val genesis_state :
  'm genesis_state_mode ->
  (module Protocol_plugin_sig.PARTIAL) ->
  ?genesis_block:Block_hash.t ->
  _ Node_context.t ->
  Context.pvmstate ->
  'm genesis_state tzresult Lwt.t

(** [state_of_tick plugin node_ctxt cache ?start_state ~tick level] returns [Some
    state] for a given [tick] if this [tick] happened before [level] and where
    [state] is the PVM evaluation state before [tick] happened. Otherwise,
    returns [None]. If provided, the evaluation is resumed from
    [start_state]. *)
val state_of_tick :
  (module Protocol_plugin_sig.PARTIAL) ->
  _ Node_context.t ->
  Pvm_plugin_sig.state_cache ->
  ?start_state:Fuel.Accounted.t Pvm_plugin_sig.eval_state ->
  tick:Z.t ->
  int32 ->
  Fuel.Accounted.t Pvm_plugin_sig.eval_state option tzresult Lwt.t

(** [state_of_head plugin node_ctxt ctxt head] returns the state corresponding
    to the block [head], or the state at rollup genesis if the block is before
    the rollup origination. *)
val state_of_head :
  (module Protocol_plugin_sig.PARTIAL) ->
  < context : 'a ; store : _ > Node_context.t ->
  'a Context.t ->
  Layer1.head ->
  Context.pvmstate tzresult Lwt.t
