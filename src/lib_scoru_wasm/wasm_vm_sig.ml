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

open Wasm_pvm_state
open Internal_state

module type Internal_for_tests = sig
  type state

  val compute_step_many_with_hooks :
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    ?after_fast_exec:(unit -> unit) ->
    ?stop_at_snapshot:bool ->
    max_steps:int64 ->
    state ->
    (state * int64) Lwt.t
end

module type Generic = sig
  type state

  (** [compute_step_many ~max_steps pvm_state] forwards the VM by at most
      [max_step] compute tick, yielding if it reaches the maximum number of
      ticks for a toplevel kernel call. If the VM is expecting input, it gets
      stuck. If the VM is already stuck, this function may raise an exception.
      It is more efficient than [compute_step] if it has to be called for more
      than one tick, but its resulting pvm_state will be stricly equivalent.
      Returns a tuple containing the number of executed ticks and the new
      pvm_state. *)
  val compute_step_many :
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    ?stop_at_snapshot:bool ->
    max_steps:int64 ->
    state ->
    (state * int64) Lwt.t

  (** [compute_step pvm_state] forwards the VM by one compute tick. If the VM is
      expecting input, it gets stuck. If the VM is already stuck, this function
      may raise an exception. It is strictly equivalent to
      [compute_step_many ~max_step=1 pvm_state]. *)
  val compute_step : state -> state Lwt.t

  (** [compute_step_with_debug ~debug_flag pvm_state] is exactly [compute_step]
      but it has the ability to enable the debugging host functions. *)
  val compute_step_with_debug :
    write_debug:Builtins.write_debug -> state -> state Lwt.t

  (** [set_input_step input_info message pvm_state] forwards the VM by one input
      tick. If the VM is not expecting input, it gets stuck. If the VM is
      already stuck, this function may raise an exception. Note at this point
      the function raises an exception if the VM is not expecting input. *)
  val set_input_step : input_info -> string -> state -> state Lwt.t

  (** [reveal_step reveal_data pvm_state] loads the [reveal_data] in the
      memory of module of the currently executed function.

      If the VM does not expect any reveal data, this function raises
      an exception. *)
  val reveal_step : bytes -> state -> state Lwt.t

  (** [get_info pvm_state] provides a typed view of the current machine state.
      Should not raise. *)
  val get_info : state -> info Lwt.t

  (** [get_wasm_version pvm_state] returns the current version at
      which the WASM PVM operates. *)
  val get_wasm_version : state -> version Lwt.t

  module Internal_for_tests : Internal_for_tests with type state := state
end

module type S = sig
  include Generic with type state := pvm_state

  (** [compute_step_many_until max_steps reveal_builtins write_debug should_continue pvm_state]
      advances forwards the VM in the same manners as [compute_step_many]
      as long as [should_continue] returns true.

     Returns the new state and number of the executed ticks.

      IS applied on [pvm_state] rather than a tree.

      /!\ as it allows to redefine the stop condition, this function should
      not be used in unit test: the test could hide regression if the
      condition change in the code, but not in the test.
  *)
  val compute_step_many_until :
    ?max_steps:int64 ->
    ?reveal_builtins:Builtins.reveals ->
    ?write_debug:Builtins.write_debug ->
    (pvm_state -> bool Lwt.t) ->
    pvm_state ->
    (pvm_state * int64) Lwt.t

  (** [get_output output buffer] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. Raises an exception if the output is
      not present. *)
  val get_output :
    output_info -> Tezos_webassembly_interpreter.Output_buffer.t -> string Lwt.t
end
