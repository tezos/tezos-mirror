(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Merkelizing inbox for smart contract rollups. *)

(** The type of the in-memory state of the inbox for a smart contract rollup. *)
type t

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

(** [number_of_available_messages inbox] returns the number of
   messages that can be consumed in [inbox]. *)
val number_of_available_messages : t -> Z.t

(** The empty inbox. *)
val empty : t

(** [add_messages msg_list level inbox] adds [msg_list] to [inbox] at
    level [level] (preserving their order). *)
val add_messages : string list -> Raw_level_repr.t -> t -> t

(** [consume_n_messages n inbox] returns an inbox where [n] messages have
    been consumed, or [None] if there are strictly less than [n] messages
    available in [inbox].

    @raise Invalid_argument if [n <= 0]
 *)
val consume_n_messages : int -> t -> t option
