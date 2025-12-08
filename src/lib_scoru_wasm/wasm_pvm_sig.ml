(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
open Wasm_pvm_state

module type Unsafe = sig
  type state

  (** Retrieve the maximum number of ticks for the PVM from the state. *)
  val get_max_nb_ticks : state -> Z.t Lwt.t

  (** Change the maximum number of ticks (per snapshot) of the WASM PVM. This is
      to be used only for tests or to increase the tick limit in a non-refutable
      setting. *)
  val set_max_nb_ticks : Z.t -> state -> state Lwt.t

  (** Set a value to a given key in the durable storage of [state].
      If there is an existing value, it is overwritten. *)
  val durable_set : key:string -> value:string -> state -> state Lwt.t

  (** [set_pvm_version ~version state] sets the PVM version of [state] to
      [version], applying any necessary migrations. This is used by the rollup
      node's unsafe patch mechanism to activate specific versions (including
      experimental ones) at origination. *)
  val set_pvm_version : version:Wasm_pvm_state.version -> state -> state Lwt.t
end

module type Internal_for_tests = sig
  open Internal_state

  type state

  val insert_failure : state -> state Lwt.t

  val get_tick_state : state -> tick_state Lwt.t

  val get_module_instance_exn :
    state -> Tezos_webassembly_interpreter.Instance.module_inst Lwt.t

  val is_stuck : state -> Wasm_pvm_errors.t option Lwt.t

  val set_maximum_reboots_per_input : Z.t -> state -> state Lwt.t

  val decr_reboot_counter : state -> state Lwt.t

  val reset_reboot_counter : state -> state Lwt.t

  val get_input_buffer :
    state -> Tezos_webassembly_interpreter.Input_buffer.t Lwt.t

  val get_output_buffer :
    state -> Tezos_webassembly_interpreter.Output_buffer.t Lwt.t

  val compute_step_many_until :
    wasm_entrypoint:string ->
    ?max_steps:int64 ->
    ?hooks:Hooks.t ->
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    (pvm_state -> bool Lwt.t) ->
    state ->
    (state * int64) Lwt.t

  include Unsafe with type state := state
end

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type Machine = sig
  include Wasm_vm_sig.Generic

  (** [initial_state empty_tree] computes the initial state whose hash
      is hard-coded in the protocol. *)
  val initial_state : version -> state -> state Lwt.t

  (** [install_boot_sector ~ticks_per_snapshot ~output_validity_period payload
      state] installs the [payload] passed as an argument in [state] so that it is
      interpreted as the kernel to be used by the PVM. *)
  val install_boot_sector :
    ticks_per_snapshot:Z.t ->
    outbox_validity_period:int32 ->
    outbox_message_limit:Z.t ->
    string ->
    state ->
    state Lwt.t

  (** [get_output output state] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. If the output is missing, this
      function may raise an exception. *)
  val get_output : output_info -> state -> string option Lwt.t

  val get_wasm_version : state -> Wasm_pvm_state.version Lwt.t

  module Unsafe : Unsafe with type state := state

  module Internal_for_tests : Internal_for_tests with type state := state
end

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type S = sig
  include Machine

  type context

  (** [empty_state ctxt] computes an empty state, it must then be
      called with `initial_state` to be validated by the protocol. *)
  val empty_state : unit -> state

  val state_hash : state -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t Lwt.t

  type proof

  val proof_encoding : proof Data_encoding.t

  val proof_start_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val proof_stop_state : proof -> Tezos_crypto.Hashed.Smart_rollup_state_hash.t

  val cast_read_only : proof -> proof

  val verify_proof :
    proof -> (state -> (state * 'a) Lwt.t) -> (state * 'a) option Lwt.t

  val produce_proof :
    context ->
    state ->
    (state -> (state * 'a) Lwt.t) ->
    (proof * 'a) option Lwt.t
end

(* Encodings *)

let input_info_encoding =
  let open Data_encoding in
  let open Wasm_pvm_state in
  conv
    (fun {inbox_level; message_counter} -> (inbox_level, message_counter))
    (fun (inbox_level, message_counter) -> {inbox_level; message_counter})
    (obj2
       (req "inbox_level" Tezos_base.Bounded.Non_negative_int32.encoding)
       (req "message_counter" n))
