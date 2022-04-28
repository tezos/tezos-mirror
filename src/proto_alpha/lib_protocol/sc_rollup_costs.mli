(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module contains constants and utility functions for gas metering
    functions used when handling SC rollups operations in context. *)

module Constants : sig
  val cost_add_message_base : Gas_limit_repr.cost

  val cost_add_message_per_byte : Gas_limit_repr.cost

  val cost_add_inbox_per_level : Gas_limit_repr.cost

  val cost_update_num_and_size_of_messages : Gas_limit_repr.cost
end

(** [cost_add_messages ~num_messages ~total_messages_length level] 
    returns the cost of adding [num_messages] with total messages size
    [total_messages_size] to a sc-rollup inbox at level [level]. This 
    function is used internally in the [Sc_rollup_storage] module. *)
val cost_add_messages :
  num_messages:int -> total_messages_size:int -> int32 -> Gas_limit_repr.cost
