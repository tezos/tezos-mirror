(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Trili Tech, <contact@trili.tech>                       *)
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
    functions used for extracting and handling tickets for the global ticket
    balance table. *)

module Constants : sig
  val cost_collect_tickets_step : Alpha_context.Gas.cost

  val cost_compare_ticket_hash : Alpha_context.Gas.cost

  val cost_compare_key_contract : Alpha_context.Gas.cost
end

(** [consume_gas_steps ctxt ~num_steps] consumes gas corresponding to
    a given [num_steps] and [step_cost]. It's useful for paying for gas
    upfront where the number of steps can be determined.

    This function is generic and should probably be moved. See issue
    https://gitlab.com/tezos/tezos/-/issues/1950.

  *)
val consume_gas_steps :
  Alpha_context.t ->
  step_cost:Alpha_context.Gas.cost ->
  num_steps:int ->
  Alpha_context.t tzresult

(** [has_tickets_of_ty_cost ty] returns the cost of producing a [has_tickets],
    used internally in the [Ticket_scanner] module. *)
val has_tickets_of_ty_cost :
  ('a, _) Script_typed_ir.ty -> Saturation_repr.may_saturate Saturation_repr.t

(** [negate_cost z] returns the cost of negating the given value [z]. *)
val negate_cost : Z.t -> Alpha_context.Gas.cost

(** [add_int_cost n1 n2] returns the cost of adding the values [n1] and [n2]. *)
val add_int_cost :
  Script_int.n Script_int.num ->
  Script_int.n Script_int.num ->
  Alpha_context.Gas.cost

(** [add_z_cost z1 z2] returns the cost of adding the values [z1] and [z2]. *)
val add_z_cost : Z.t -> Z.t -> Alpha_context.Gas.cost
