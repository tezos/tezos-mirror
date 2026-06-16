(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** PVM instance used in benchmark*)
module Wasm :
  Tezos_scoru_wasm.Wasm_pvm_sig.Machine
    with type state = Tezos_scoru_wasm_helpers.Encodings_util.Tree.tree

module Wasm_fast_vm : Tezos_scoru_wasm.Wasm_vm_sig.S

open Tezos_scoru_wasm
open Wasm_pvm_state

val encode_pvm_state :
  Internal_state.pvm_state -> Wasm.state -> Wasm.state Lwt.t

val decode_pvm_state : Wasm.state -> Internal_state.pvm_state Lwt.t

val get_tick_from_tree : Wasm.state -> Z.t Lwt.t

val get_tick_from_pvm_state : Internal_state.pvm_state -> Z.t Lwt.t

(** Pretty Printing utilities*)
module PP : sig
  val pp_error_state : Wasm_pvm_errors.t -> string

  val tick_label : Internal_state.tick_state -> string
end

val builtins : Tezos_scoru_wasm.Builtins.reveals
