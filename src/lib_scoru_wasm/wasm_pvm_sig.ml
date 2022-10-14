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

(** This module type expose internals necessary for benchmarking.

    /!\ Not intended for unit tests: the functions could be used to redefine the
    main execution loop, at the risk of departing from what is defined in the
    PVM definition. [Internal_for_benchmark.compute_step_many_until] can use
    custom stopping condition and therefore should not be used in unit test:
    the test could hide regression if the condition change in the code, but not
    in the test. *)
module type Internal_for_benchmark = sig
  open Internal_state

  type tree

  val decode : tree -> pvm_state Lwt.t

  val encode : pvm_state -> tree -> tree Lwt.t

  (** [compute_step_many_until_pvm_state max_step should_continue pvm_state]
      advance forwards the VM in the same manners as [compute_step_many]
      as long as [should_continue] returns true.

      Returns the new state and number of the executed ticks.

      IS applied on [pvm_state] rather than a tree.

      /!\ as it allows to redefine the stop condition, this function should
      not be used in unit test: the test could hide regression if the
      condition change in the code, but not in the test.
  *)
  val compute_step_many_until_pvm_state :
    ?max_steps:int64 ->
    (pvm_state -> bool Lwt.t) ->
    pvm_state ->
    (pvm_state * int64) Lwt.t

  (** [compute_step_many_until max_step should_continue tree]
      advance forwards the VM in the same manners as [compute_step_many]
      as long as [should_continue] returns true.

      Returns the new tree and number of the executed ticks.

      /!\ as it allows to redefine the stop condition, this function should
      not be used in unit test: the test could hide regression if the
      condition change in the code, but not in the test.
  *)
  val compute_step_many_until :
    ?max_steps:int64 ->
    (pvm_state -> bool Lwt.t) ->
    tree ->
    (tree * int64) Lwt.t

  val eval_has_finished : tick_state -> bool
end

module type Internal_for_tests = sig
  open Internal_state

  type tree

  val get_tick_state : tree -> tick_state Lwt.t

  val get_module_instance_exn :
    tree -> Tezos_webassembly_interpreter.Instance.module_inst Lwt.t

  val is_stuck : tree -> Wasm_pvm_errors.t option Lwt.t

  val set_max_nb_ticks : Z.t -> tree -> tree Lwt.t

  val set_maximum_reboots_per_input : Z.t -> tree -> tree Lwt.t

  val reset_reboot_counter : tree -> tree Lwt.t

  val get_input_buffer :
    tree -> Tezos_webassembly_interpreter.Input_buffer.t Lwt.t

  val get_output_buffer :
    tree -> Tezos_webassembly_interpreter.Output_buffer.t Lwt.t
end

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type S = sig
  type tree

  include Wasm_vm_sig.Generic with type state := tree

  (** [install_boot_sector payload tree] installs the [payload] passed
      as an argument in [tree] so that it is interpreted as the kernel
      to be used by the PVM. *)
  val install_boot_sector : string -> tree -> tree Lwt.t

  (** [get_output output state] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. If the output is missing, this
      function may raise an exception. *)
  val get_output : output_info -> tree -> string option Lwt.t

  module Internal_for_benchmark : Internal_for_benchmark with type tree := tree

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
