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

(** Maximum number of reboots per inputs. *)
val maximum_reboots_per_input : Z.t

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
      module_reg : Tezos_webassembly_interpreter.Instance.module_reg;
    }
  | Eval of Tezos_webassembly_interpreter.Eval.config
  | Stuck of Wasm_pvm_errors.t
  | Snapshot

type pvm_state = {
  last_input_info : Wasm_pvm_sig.input_info option;
      (** Info about last read input. *)
  current_tick : Z.t;  (** Current tick of the PVM. *)
  reboot_counter : Z.t;  (** Number of reboots for the current input. *)
  durable : Durable.t;  (** The durable storage of the PVM. *)
  buffers : Tezos_webassembly_interpreter.Eval.buffers;
      (** Input and outut buffers used by the PVM host functions. *)
  tick_state : tick_state;  (** The current tick state. *)
  last_top_level_call : Z.t;
      (** Last tick corresponding to a top-level call. *)
  max_nb_ticks : Z.t;  (** Number of ticks between top level call. *)
  maximum_reboots_per_input : Z.t;  (** Number of reboots between two inputs. *)
}

module Make (T : Tezos_tree_encoding.TREE) :
  Gather_floppies.S
    with type tree = T.tree
     and type tick_state = tick_state
     and type pvm_state = pvm_state
