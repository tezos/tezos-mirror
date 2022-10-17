(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** This module defines functions that emit the events used when running a PVM
    transition (see {!Interpreter}). *)

module Make (PVM : Pvm.S) : sig
  (** [transition_pvm inbox_level hash n] emits the event that a PVM
   transition is leading to the state of the given [hash] by
   processing [n] messages. *)
  val transitioned_pvm :
    Protocol.Alpha_context.Raw_level.t -> PVM.state -> Z.t -> unit Lwt.t

  (** [intended_failure level message_index message_tick internal] emits
   the event that an intended failure has been injected at some given
   [level], during the processing of a given [message_index] and at
   tick [message_tick] during this message processing. [internal] is
   [true] if the failure is injected in a PVM internal
   step. [internal] is [false] if the failure is injected in the input
   to the PVM. *)
  val intended_failure :
    level:int ->
    message_index:int ->
    message_tick:int64 ->
    internal:bool ->
    unit Lwt.t
end
