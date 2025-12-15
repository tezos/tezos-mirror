(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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
open Alpha_context

(** Desired module type of a PVM from the L2 node's perspective *)
module type S = sig
  type repo

  type tree

  module Ctxt_wrapper :
    Context.Wrapper.S with type repo = repo and type tree = tree

  include
    Sc_rollup.PVM.S
      with type hash = Sc_rollup.State_hash.t
       and type context = ([`Read | `Write], repo) Context_sigs.raw_index
       and type state = tree

  (** Kind of the PVM. *)
  val kind : Sc_rollup.Kind.t

  (** [get_tick state] gets the total tick counter for the given PVM state. *)
  val get_tick : state -> Sc_rollup.Tick.t Lwt.t

  (** PVM status *)
  type status

  (** [get_status ~is_reveal_enabled state] gives you the current execution status for the PVM. *)
  val get_status :
    is_reveal_enabled:Sc_rollup.is_reveal_enabled -> state -> status Lwt.t

  (** [string_of_status status] returns a string representation of [status]. *)
  val string_of_status : status -> string

  (** [get_outbox outbox_level state] returns a list of outputs
     available in the outbox of [state] at a given [outbox_level]. *)
  val get_outbox : Raw_level.t -> state -> Sc_rollup.output list Lwt.t

  (** [eval_many ~max_steps s0] returns a state [s1] resulting from the
      execution of up to [~max_steps] steps of the rollup at state [s0]. *)
  val eval_many :
    ?check_invalid_kernel:bool ->
    reveal_builtins:Tezos_scoru_wasm.Builtins.reveals ->
    write_debug:Tezos_scoru_wasm.Builtins.write_debug ->
    is_reveal_enabled:Sc_rollup.is_reveal_enabled ->
    ?stop_at_snapshot:bool ->
    max_steps:int64 ->
    state ->
    (state * int64) Lwt.t

  val new_dissection :
    default_number_of_sections:int ->
    start_chunk:Sc_rollup.Dissection_chunk.t ->
    our_stop_chunk:Sc_rollup.Dissection_chunk.t ->
    Sc_rollup.Tick.t list

  (** State storage for this PVM. *)
  module State : sig
    type value = state

    (** [empty ()] is the empty state.  *)
    val empty : unit -> state

    (** [find context] returns the PVM state stored in the [context], if any. *)
    val find : ('a, repo, tree) Context_sigs.t -> state option Lwt.t

    (** [lookup state path] returns the data stored for the path [path] in the
        PVM state [state].  *)
    val lookup : state -> string list -> bytes option Lwt.t

    (** [set context state] saves the PVM state [state] in the context and
        returns the updated context. Note: [set] does not perform any write on
        disk, this information must be committed using {!val:Context.commit}. *)
    val set :
      ('a, repo, tree) Context_sigs.t ->
      state ->
      ('a, repo, tree) Context_sigs.t Lwt.t
  end

  (** Inspect durable state using a more specialised way of reading the
      PVM state.
      For example in WASM, it decodes the durable storage in the state
      before reading values.
  *)
  module Inspect_durable_state : sig
    (** [lookup state path] returns the data stored for the path [path] in the
        PVM state [state].  *)
    val lookup : state -> string list -> bytes option Lwt.t
  end

  (** Expose unsafe state patching functions for manual intervention.
      At the moment this feature is only used to increase the maximum number of
      ticks of the WASM PVM in a non refutable setting.  *)
  module Unsafe_patches : sig
    type t

    (** [of_patch p] returns the PVM patch if it has a corresponding one. *)
    val of_patch : Pvm_patches.unsafe_patch -> t tzresult

    (** [apply state patch] applies the unsafe patch [patch] on the state. *)
    val apply : state -> t -> state Lwt.t
  end
end
