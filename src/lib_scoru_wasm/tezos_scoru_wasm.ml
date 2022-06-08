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

(*

  This library acts as a dependency to the protocol environment. Everything that
  must be exposed to the protocol via the environment shall be added here.

*)

open Sigs

type input = {
  inbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
  message_counter : Z.t;
}

type output = {
  outbox_level : Tezos_base.Bounded.Int32.NonNegative.t;
  message_index : Z.t;
}

type input_request = No_input_required | Input_required

type info = {
  current_tick : Z.t;
      (** The number of ticks processed by the VM, zero for the initial state.

      [current_tick] must be incremented for each call to [step] *)
  last_input_read : input option;
      (** The last message to be read by the VM, if any. *)
  input_request : input_request;  (** The current VM input request. *)
}

module Make (T : TreeS) : sig
  (** [compute_step] forwards the VM by one compute tick.

      If the VM is expecting input, it gets stuck.

      If the VM is already stuck, this function may raise an exception. *)
  val compute_step : T.tree -> T.tree Lwt.t

  (** [set_input_step] forwards the VM by one input tick.

      If the VM is not expecting input, it gets stuck.

      If the VM is already stuck, this function may raise an exception. *)
  val set_input_step : input -> string -> T.tree -> T.tree Lwt.t

  (** [get_output output state] returns the payload associated with the given output.

      The result is meant to be deserialized using [Sc_rollup_PVM_sem.output_encoding].

      If the output is missing, this function may raise an exception.
      *)
  val get_output : output -> T.tree -> string Lwt.t

  (** [get_info] provides a typed view of the current machine state.

      Should not raise. *)
  val get_info : T.tree -> info Lwt.t
end = struct
  module Tree = struct
    include T
  end

  module Decodings = Decodings.Make (T)

  let compute_step = Lwt.return

  let set_input_step _ _ = Lwt.return
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/3092

     Implement handling of input logic.
  *)

  let get_output _ _ = Lwt.return ""

  let get_info _ =
    Lwt.return
      {
        current_tick = Z.of_int 0;
        last_input_read = None;
        input_request = No_input_required;
      }

  let _module_instance_of_tree modules =
    Decodings.Tree.Decoding.run (Decodings.module_instance_decoding modules)

  let _module_instances_of_tree =
    Decodings.Tree.Decoding.run Decodings.module_instances_decoding
end
