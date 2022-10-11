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

(** Utilities used to run the PVM *)

open Pvm_instance
open Tezos_scoru_wasm.Wasm_pvm_state

(** the different phases in a top level call *)
type phase = Decoding | Initialising | Linking | Evaluating | Padding

(** [run_loop f a] folds [f] on all phases of an exection *)
val run_loop : ('a -> phase -> 'a Lwt.t) -> 'a -> 'a Lwt.t

val show_phase : phase -> string

(** execute the PVM until a the end of a top level call
      e.g. until a snapshotable state is reached *)
val finish_top_level_call_on_state :
  Internal_state.pvm_state -> (Internal_state.pvm_state * int64) Lwt.t

val execute_on_state :
  phase -> Internal_state.pvm_state -> (Internal_state.pvm_state * int64) Lwt.t

(** [run path k] execute [k] on the content of the file at [path] *)
val run : Lwt_io.file_name -> (string -> unit Lwt.t) -> unit Lwt.t

val set_input_step : string -> int -> Wasm.tree -> Wasm.tree Lwt.t

(** [read_message "my_file.out"] returns the content of the file,
      searched in the input repository for messages*)
val read_message : string -> string

(** [initial_boot_sector_from_kernel
        "src/lib_scoru_wasm/bench/inputs/my_kernel.wasm"]
       initialize a state from a kernel (byte format) *)
val initial_boot_sector_from_kernel :
  ?max_tick:int64 -> string -> Wasm.tree Lwt.t
