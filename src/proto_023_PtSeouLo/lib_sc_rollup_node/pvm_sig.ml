(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2024 TriliTech <contact@trili.tech>                    *)
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

(** Mutable API for the PVM.
    - PVM functions which update the state in-place instead of returning a new
    state.

    This API helps the RISC-V PVM avoid unnecessary state copying. *)
module type MUTABLE_STATE_S = sig
  type t

  type repo

  type hash

  type status

  (** [empty ()] is the empty state.  *)
  val empty : unit -> t

  (** [find context] returns the PVM state stored in the [context], if any. *)
  val find : ('a, repo, t) Context_sigs.t -> t option Lwt.t

  (** [lookup state path] returns the data stored for the path [path] in the
      PVM state [state].  *)
  val lookup : t -> string list -> bytes option Lwt.t

  (** [set context state] saves the PVM state [state] in the context and
      returns the updated context. Note: [set] does not perform any write on
      disk, this information must be committed using {!val:Context.commit}. *)
  val set : ('a, repo, t) Context_sigs.t -> t -> unit Lwt.t

  val get_tick : t -> Sc_rollup.Tick.t Lwt.t

  val get_current_level : t -> int32 option Lwt.t

  val get_outbox : int32 -> t -> Sc_rollup.output list Lwt.t

  val get_status :
    is_reveal_enabled:Sc_rollup.is_reveal_enabled -> t -> status Lwt.t

  val state_hash : t -> hash Lwt.t

  val set_initial_state : empty:t -> unit Lwt.t

  val install_boot_sector : t -> string -> unit Lwt.t

  val is_input_state :
    is_reveal_enabled:Sc_rollup.is_reveal_enabled ->
    t ->
    Sc_rollup.input_request Environment.Lwt.t

  val set_input : Sc_rollup.input -> t -> unit Lwt.t

  val eval_many :
    ?check_invalid_kernel:bool ->
    ?fallback_to_slow_vm:bool ->
    reveal_builtins:Tezos_scoru_wasm.Builtins.reveals ->
    write_debug:Tezos_scoru_wasm.Builtins.write_debug ->
    is_reveal_enabled:Sc_rollup.is_reveal_enabled ->
    ?stop_at_snapshot:bool ->
    max_steps:int64 ->
    t ->
    int64 Lwt.t

  module Inspect_durable_state : sig
    val lookup : t -> string list -> bytes option Lwt.t
  end

  module Internal_for_tests : sig
    val insert_failure : t -> unit Lwt.t
  end
end

(** Desired module type of a PVM from the L2 node's perspective *)
module type S = sig
  type repo

  type tree

  module Ctxt_wrapper :
    Context.Wrapper.S with type repo = repo and type state = tree

  include
    Sc_rollup.PVM.S
      with type hash = Sc_rollup.State_hash.t
       and type context = ([`Read | `Write], repo) Context_sigs.raw_index
       and type state = tree

  (** Kind of the PVM. *)
  val kind : Sc_rollup.Kind.t

  (** PVM status *)
  type status

  (** [string_of_status status] returns a string representation of [status]. *)
  val string_of_status : status -> string

  val new_dissection :
    default_number_of_sections:int ->
    start_chunk:Sc_rollup.Dissection_chunk.t ->
    our_stop_chunk:Sc_rollup.Dissection_chunk.t ->
    Sc_rollup.Tick.t list

  (** Mutable state API which allows updating the PVM state in-place. *)
  module Mutable_state :
    MUTABLE_STATE_S
      with type t = Ctxt_wrapper.mut_state
       and type repo = repo
       and type hash = hash
       and type status = status

  (** Expose unsafe state patching functions for manual intervention.
      At the moment this feature is only used to increase the maximum number of
      ticks of the WASM PVM in a non refutable setting.  *)
  module Unsafe_patches : sig
    type t

    (** [of_patch p] returns the PVM patch if it has a corresponding one. *)
    val of_patch : Pvm_patches.unsafe_patch -> t tzresult

    (** [apply state patch] applies the unsafe patch [patch] on the state. *)
    val apply : state -> t -> state Lwt.t

    val apply_mutable : Mutable_state.t -> t -> unit Lwt.t
  end
end
