(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Counters are used as anti-replay protection mechanism in
    manager operations: each manager account stores a counter and
    each manager operation declares a value for the counter. When
    a manager operation is applied, the value of the counter of
    its manager is checked and incremented. *)

include Compare.S

(** Initial value for a counter (zero). *)
val init : t

(** Successor of a counter. *)
val succ : t -> t

(** Pretty-printer for counters. *)
val pp : Format.formatter -> t -> unit

(** Encoding for a counter to be used in {!Storage}. *)
val encoding_for_storage : t Data_encoding.t

(** Encoding for a counter to be used in {!Operation_repr}. *)
val encoding_for_operation : t Data_encoding.t

(** Encoding for a counter to be used in RPCs. *)
val encoding_for_RPCs : t Data_encoding.t

(** Encoding for a counter to be used in errors. *)
val encoding_for_errors : t Data_encoding.t

(** To be used in client injection only. *)
module Internal_for_injection : sig
  (** Converts a string to a counter.
      Returns [None] if the string does not represent a valid counter. *)
  val of_string : string -> t option
end

module Internal_for_tests : sig
  val of_int : int -> t

  val to_int : t -> int

  val add : t -> int -> t
end
