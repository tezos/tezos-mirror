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

(** Represents the location of an input message. *)
type input_info = {
  inbox_level : Tezos_base.Bounded.Non_negative_int32.t;
      (** The inbox level at which the message exists.*)
  message_counter : Z.t;  (** The index of the message in the inbox. *)
}

(** Represents the location of an output message. *)
type output_info = {
  outbox_level : Tezos_base.Bounded.Non_negative_int32.t;
      (** The outbox level at which the message exists.*)
  message_index : Z.t;  (** The index of the message in the outbox. *)
}

type input_hash = Tezos_webassembly_interpreter.Reveal.input_hash

let input_hash_to_string =
  Tezos_webassembly_interpreter.Reveal.input_hash_to_string

type reveal = Tezos_webassembly_interpreter.Reveal.reveal =
  | Reveal_raw_data of Tezos_webassembly_interpreter.Reveal.input_hash

(** Represents the state of input requests. *)
type input_request =
  | No_input_required  (** The VM does not expect any input. *)
  | Input_required  (** The VM needs input in order to progress. *)
  | Reveal_required of reveal

(** Represents the state of the VM. *)
type info = {
  current_tick : Z.t;
      (** The number of ticks processed by the VM, zero for the initial state.
          [current_tick] must be incremented for each call to [step] *)
  last_input_read : input_info option;
      (** The last message to be read by the VM, if any. *)
  input_request : input_request;  (** The current VM input request. *)
}

module type Internal_for_tests = sig
  type tree

  type tick_state

  val get_tick_state : tree -> tick_state Lwt.t

  val get_module_instance_exn :
    tree -> Tezos_webassembly_interpreter.Instance.module_inst Lwt.t

  val is_stuck : tree -> Wasm_pvm_errors.t option Lwt.t

  val set_max_nb_ticks : Z.t -> tree -> tree Lwt.t
end

(** This module type defines a WASM VM API used for smart-contract rollups. *)
module type S = sig
  type tree

  type tick_state

  (** [compute_step_many ~max_steps tree] forwards the VM by at most [max_step]
      compute tick, yielding if it reaches the maximum number of ticks for a
      toplevel kernel call. If the VM is expecting input, it gets stuck. If the
      VM is already stuck, this function may raise an exception. It is more
      efficient than [compute_step] if it has to be called for more than one
      tick, but its resulting tree will be stricly equivalent. *)
  val compute_step_many : max_steps:int64 -> tree -> tree Lwt.t

  (** [compute_step tree] forwards the VM by one compute tick. If the VM is expecting
      input, it gets stuck. If the VM is already stuck, this function may raise
      an exception. It is strictly equivalent to [compute_step_many ~max_step=1
      tree]. *)
  val compute_step : tree -> tree Lwt.t

  (** [set_input_step input_info message tree] forwards the VM by one input
      tick. If the VM is not expecting input, it gets stuck. If the VM is
      already stuck, this function may raise an exception. Note at this point
      the function raises an exception if the VM is not expecting input. *)
  val set_input_step : input_info -> string -> tree -> tree Lwt.t

  (** [reveal_step reveal_data tree] loads the [reveal_data] in the
      memory of module of the currently executed function.

      If the VM does not expect any reveal data, this function raises
      an exception. *)
  val reveal_step : bytes -> tree -> tree Lwt.t

  (** [get_output output state] returns the payload associated with the given
      output. The result is meant to be deserialized using
      [Sc_rollup_PVM_sem.output_encoding]. If the output is missing, this
      function may raise an exception. *)
  val get_output : output_info -> tree -> string Lwt.t

  (** [get_info] provides a typed view of the current machine state. Should not
      raise. *)
  val get_info : tree -> info Lwt.t

  module Internal_for_tests :
    Internal_for_tests with type tree := tree and type tick_state := tick_state
end

(* Encodings *)

let input_info_encoding =
  let open Data_encoding in
  conv
    (fun {inbox_level; message_counter} -> (inbox_level, message_counter))
    (fun (inbox_level, message_counter) -> {inbox_level; message_counter})
    (obj2
       (req "inbox_level" Tezos_base.Bounded.Non_negative_int32.encoding)
       (req "message_counter" n))
