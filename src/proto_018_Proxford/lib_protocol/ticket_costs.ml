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

open Alpha_context
module S = Saturation_repr

module Constants = struct
  (* model tickets/COLLECT_TICKETS_STEP *)
  let cost_collect_tickets_step = S.safe_int 80

  (* model tickets/TYPE_HAS_TICKETS *)
  let cost_has_tickets_of_ty type_size =
    S.add (S.safe_int 10) (S.mul (S.safe_int 6) type_size)

  (* model tickets/COMPARE_TICKET_HASH *)
  let cost_compare_ticket_hash = S.safe_int 10

  (* model tickets/COMPARE_CONTRACT *)
  let cost_compare_key_contract = S.safe_int 10
end

let consume_gas_steps ctxt ~step_cost ~num_steps =
  let ( * ) = S.mul in
  if Compare.Int.(num_steps <= 0) then Ok ctxt
  else
    let gas =
      Gas.atomic_step_cost (step_cost * Saturation_repr.safe_int num_steps)
    in
    Gas.consume ctxt gas

let has_tickets_of_ty_cost ty =
  Constants.cost_has_tickets_of_ty
    Script_typed_ir.(ty_size ty |> Type_size.to_int)

(** Reusing the gas model from [Michelson_v1_gas.Cost_of.neg]
    Approximating 0.066076 x term *)
let negate_cost z =
  let size = (7 + Z.numbits z) / 8 in
  Gas.(S.safe_int 25 +@ S.shift_right (S.safe_int size) 4)

(** Reusing the gas model from [Michelson_v1_gas.Cost_of.add] *)
let add_int_cost = Michelson_v1_gas.Cost_of.Interpreter.add_int

(** Reusing the gas model from [Michelson_v1_gas.Cost_of.add] *)
let add_z_cost z1 z2 =
  add_int_cost (Script_int.of_zint z1) (Script_int.of_zint z2)
