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

module type Internal_for_tests = sig
  open Internal_state

  type tree

  val get_tick_state : tree -> tick_state Lwt.t

  val get_module_instance_exn :
    tree -> Tezos_webassembly_interpreter.Instance.module_inst Lwt.t

  val is_stuck : tree -> Wasm_pvm_errors.t option Lwt.t

  val set_max_nb_ticks : Z.t -> tree -> tree Lwt.t

  val set_maximum_reboots_per_input : Z.t -> tree -> tree Lwt.t

  val decr_reboot_counter : tree -> tree Lwt.t

  val reset_reboot_counter : tree -> tree Lwt.t

  val get_input_buffer :
    tree -> Tezos_webassembly_interpreter.Input_buffer.t Lwt.t

  val get_output_buffer :
    tree -> Tezos_webassembly_interpreter.Output_buffer.t Lwt.t

  val compute_step_many_until :
    wasm_entrypoint:string ->
    ?max_steps:int64 ->
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    (pvm_state -> bool Lwt.t) ->
    tree ->
    (tree * int64) Lwt.t

  include Wasm_vm_sig.Internal_for_tests with type state := tree
end

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type S = sig
  type tree

  include Wasm_vm_sig.Generic with type state := tree

  (** [initial_state empty_tree] computes the initial tree whose hash
      is hard-coded in the protocol. *)
  val initial_state : version -> tree -> tree Lwt.t

  (** [install_boot_sector ~ticks_per_snapshot ~output_validity_period payload
      tree] installs the [payload] passed as an argument in [tree] so that it is
      interpreted as the kernel to be used by the PVM. *)
  val install_boot_sector :
    ticks_per_snapshot:Z.t ->
    outbox_validity_period:int32 ->
    outbox_message_limit:Z.t ->
    string ->
    tree ->
    tree Lwt.t

  (** [get_output output state] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. If the output is missing, this
      function may raise an exception. *)
  val get_output : output_info -> tree -> string option Lwt.t

  module Internal_for_tests : Internal_for_tests with type tree := tree
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
