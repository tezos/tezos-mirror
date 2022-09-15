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

type tick_state =
  | Decode of Tezos_webassembly_interpreter.Decode.decode_kont
  | Link of {
      ast_module : Tezos_webassembly_interpreter.Ast.module_;
      externs :
        Tezos_webassembly_interpreter.Instance.extern
        Tezos_webassembly_interpreter.Instance.Vector.t;
      imports_offset : int32;
    }
  | Init of {
      self : Tezos_webassembly_interpreter.Instance.module_key;
      ast_module : Tezos_webassembly_interpreter.Ast.module_;
      init_kont : Tezos_webassembly_interpreter.Eval.init_kont;
    }
  | Eval of Tezos_webassembly_interpreter.Eval.config
  | Stuck of Wasm_pvm_errors.t

(** Builds a WASM VM given a concrete implementation of {!Tree.S}. *)

module Make (T : Tree_encoding.TREE) :
  Gather_floppies.S with type tree = T.tree and type tick_state = tick_state
