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

module Constants = struct
  module S = Saturation_repr

  (* TODO: Fill in real benchmarked values *)
  let cost_contains_tickets_step = S.safe_int 28

  (* TODO: Fill in real benchmarked values *)
  let cost_collect_tickets_step = S.safe_int 360

  (* TODO: Fill in real benchmarked values *)
  let cost_has_tickets_of_ty type_size = S.mul (S.safe_int 20) type_size
end

let consume_gas_steps ctxt ~step_cost ~num_steps =
  let ( * ) = Saturation_repr.mul in
  if Compare.Int.(num_steps <= 0) then Ok ctxt
  else
    let gas =
      Gas.atomic_step_cost (step_cost * Saturation_repr.safe_int num_steps)
    in
    Gas.consume ctxt gas

let has_tickets_of_ty_cost ty =
  Constants.cost_has_tickets_of_ty
    Script_typed_ir.(ty_size ty |> Type_size.to_int)
