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

open Tezos_scoru_wasm
open Tezos_scoru_wasm_helpers
module Wasm = Wasm_utils.Wasm
module Wasm_fast_vm = Tezos_scoru_wasm_fast.Vm
module State = Wasm_utils.State

let encode_pvm_state state tree = State.Encoding_runner.encode state tree

let decode_pvm_state tree = State.Encoding_runner.decode tree

let get_tick_from_tree tree =
  let open Lwt_syntax in
  let* info = Wasm.get_info tree in
  return info.current_tick

let get_tick_from_pvm_state
    (pvm_state : Wasm_pvm_state.Internal_state.pvm_state) =
  Lwt.return pvm_state.current_tick

module PP = struct
  let pp_error_state e = Wasm_utils.print_error_state e

  let tick_label = Format.asprintf "%a" Wasm_utils.pp_state
end

let builtins = Wasm_utils.reveal_builtins
