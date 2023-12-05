(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

type version

val v3 : version

type input = {inbox_level : Bounded.Non_negative_int32.t; message_counter : Z.t}

type output = {outbox_level : Bounded.Non_negative_int32.t; message_index : Z.t}

type reveal = Reveal_raw of string

type input_request =
  | No_input_required
  | Input_required
  | Reveal_required of reveal

type info = {
  current_tick : Z.t;
  last_input_read : input option;
  input_request : input_request;
}

module Make
    (Tree : Context.TREE with type key = string list and type value = bytes) : sig
  val initial_state : version -> Tree.tree -> Tree.tree Lwt.t

  val install_boot_sector :
    ticks_per_snapshot:Z.t ->
    outbox_validity_period:int32 ->
    outbox_message_limit:Z.t ->
    string ->
    Tree.tree ->
    Tree.tree Lwt.t

  val compute_step : Tree.tree -> Tree.tree Lwt.t

  val set_input_step : input -> string -> Tree.tree -> Tree.tree Lwt.t

  val reveal_step : bytes -> Tree.tree -> Tree.tree Lwt.t

  val get_output : output -> Tree.tree -> string option Lwt.t

  val get_info : Tree.tree -> info Lwt.t
end
