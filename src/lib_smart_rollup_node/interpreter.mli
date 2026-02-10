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
  < index : _ ; state : Access_mode.rw > Context.t ->
  predecessor:Layer1.head ->
  Layer1.head ->
  Octez_smart_rollup.Inbox.t * string list ->
  (int * int64 * Z.t) tzresult Lwt.t

(** {2 Genesis state}

    The genesis state can be retrieved in different variants depending on
    whether unsafe patches should be applied. The permission parameter
    tracks the access mode of the underlying PVM state. *)

(** Tag for a genesis state with patches applied. *)
type 'a patched = Patched constraint 'a = [< `Read | `Write > `Read]

(** Tag for a genesis state without patches (the original boot sector). *)
type 'a unpatched = Unpatched constraint 'a = [< `Read | `Write > `Read]

(** Tag for requesting both the patched and unpatched genesis states. *)
type ('a, 'b) both =
  | Both
  constraint 'a = [< `Read | `Write > `Read]
  constraint 'b = [< `Read | `Write > `Read]

(** A genesis PVM state, parameterized by the requested variant. *)
type _ genesis_state =
  | Patched : 'perm Context.pvmstate -> 'perm patched genesis_state
      (** The PVM state after applying unsafe patches. *)
  | Unpatched : 'perm Context.pvmstate -> 'perm unpatched genesis_state
      (** The original PVM state from the boot sector. *)
  | Both : {
      patched : 'perm_patched Context.pvmstate;
      original : 'perm_orig Context.pvmstate;
    }
      -> ('perm_orig, 'perm_patched) both genesis_state
      (** Both variants at once, each with independently tracked permissions. *)

(** Selects which genesis state variant to compute and with what access
    permissions. *)
type _ genesis_state_mode =
  | Patched : 'a Access_mode.t -> 'a patched genesis_state_mode
      (** Request only the patched state. *)
  | Unpatched : 'a Access_mode.t -> 'a unpatched genesis_state_mode
      (** Request only the unpatched state. *)
  | Both :
      'a Access_mode.t * 'b Access_mode.t
      -> ('a, 'b) both genesis_state_mode
      (** Request both states with potentially different access permissions. *)

(** [genesis_state mode plugin ?genesis_block ?empty node_ctxt] the PVM state
    at the genesis block with and/or without patches applied depending on
    [mode]. *)
val genesis_state :
  'm genesis_state_mode ->
  (module Protocol_plugin_sig.PARTIAL) ->
  ?genesis_block:Block_hash.t ->
  ?empty:Access_mode.rw Context.pvmstate ->
  _ Node_context.t ->
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
  ?start_state:
    ( Fuel.Accounted.t,
      Access_mode.rw Context.pvmstate )
    Pvm_plugin_sig.eval_state ->
  tick:Z.t ->
  int32 ->
  (Fuel.Accounted.t, Access_mode.rw Context.pvmstate) Pvm_plugin_sig.eval_state
  option
  tzresult
  Lwt.t

(** {2 State retrieval} *)

(** [state_of_head plugin node_ctxt ctxt head] returns the state corresponding
    to the block [head], or the state at rollup genesis if the block is before
    the rollup origination. *)
val state_of_head :
  (module Protocol_plugin_sig.PARTIAL) ->
  < context : 'a ; store : _ > Node_context.t ->
  < index : 'a ; state : 'b > Context.t ->
  Layer1.head ->
  'b Context.pvmstate tzresult Lwt.t
