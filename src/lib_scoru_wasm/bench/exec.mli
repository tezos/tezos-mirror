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

(** [do_while reboot f a] apply [f] on a state [a] regardless of it's type.
    Will reboot at the end of the last phase according to [reboot]. *)
val do_while : ('a -> bool Lwt.t) -> ('a -> 'a Lwt.t) -> 'a -> 'a Lwt.t

(** [run_loop ~reboot f a] folds [f] on all phases of an exection on a state [a]
    regardless of it's type. Will reboot at the end of the last phase according
    to the [reboot] predicate.  *)
val run_loop :
  ?reboot:('a -> bool Lwt.t) option ->
  ('a -> phase -> 'a Lwt.t) ->
  'a ->
  'a Lwt.t

(** [show_phase phase] returns the name of a given phase,
    for debugging and logging.*)
val show_phase : phase -> string

(** execute the PVM until a the end of a top level call
      e.g. until a snapshotable state is reached *)
val finish_top_level_call_on_state :
  Internal_state.pvm_state -> (Internal_state.pvm_state * int64) Lwt.t

(** [execute_on_state ~reveal_builtins phase state] Execute a given [phase] of
    the execution loop on the state. *)
val execute_on_state :
  reveal_builtins:Tezos_scoru_wasm.Builtins.reveals ->
  phase ->
  Internal_state.pvm_state ->
  (Internal_state.pvm_state * int64) Lwt.t

(** Execute one top level call using fast execution. *)
val execute_fast :
  reveal_builtins:Tezos_scoru_wasm.Builtins.reveals ->
  Internal_state.pvm_state ->
  (Internal_state.pvm_state * int64) Lwt.t

(** [run path k] execute [k] on the content of the file at [path] *)
val run : Lwt_io.file_name -> (string -> 'a Lwt.t) -> 'a Lwt.t

(** [initial_boot_sector_from_kernel
        "src/lib_scoru_wasm/bench/inputs/my_kernel.wasm"]
       initialize a state from a kernel (byte format) *)
val initial_boot_sector_from_kernel :
  ?max_tick:int64 -> string -> Wasm.tree Lwt.t

(** Inputs can be given :
    as a filename containing the input,
    or directly as a string.*)
type input = File of string | Str of string

(** Types of messages to send to inbox. Can be either already encoded, or not.
    If it's not encoded then it must be designated as Deposit or Other
    to allow encoding *)
type message = Transfer of input | Other of input | Encoded of input

(** [load_messages messages level state] sends messages to the inbox,
    at a given level. Will fail if the VM is not accepting input,
    and advance until next Snapshot.*)
val load_messages : message list -> int32 -> Wasm.tree -> Wasm.tree Lwt.t
