(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(*

   Gas update and check for gas exhaustion
   =======================================

   Costs of various computations are subtracted from an amount of gas made
   available for the script execution.

   Updating the gas counter is a critical aspect to operation validation
   because it is done at many places.

   For this reason, the gas counter must be read and updated as quickly as
   possible. Hence, the gas counter should be stored in a machine register. To
   motivate the OCaml compiler to make that choice, we represent the gas counter
   as a local parameter of the execution [step] function.

*)

type local_gas_counter = int

(*

   The gas counter stored in the context is desynchronized with the
   [local_gas_counter] used locally. When we have to call a gas-consuming
   function working on context with no local gas counter, we must update the
   context so that it carries an up-to-date gas counter. Similarly, when we
   return from such a function, the [local_gas_counter] must be updated as well.

   To statically track these points where the context's gas counter
   must be updated, we introduce a type for outdated contexts. The
   [step] function carries an [outdated_context]. When an external
   function needs a [context], the typechecker points out the need for
   a conversion: this forces us to either call [update_context], or
   better, when this is possible, the function
   [use_gas_counter_in_ctxt].

*)
type outdated_context = OutDatedContext of context [@@unboxed]

let update_context local_gas_counter = function
  | OutDatedContext ctxt ->
      Gas.update_remaining_operation_gas
        ctxt
        (Saturation_repr.safe_int local_gas_counter)
  [@@ocaml.inline always]

let update_local_gas_counter ctxt =
  (Gas.remaining_operation_gas ctxt :> int)
  [@@ocaml.inline always]

let outdated ctxt = OutDatedContext ctxt [@@ocaml.inline always]

let context_from_outdated_context (OutDatedContext ctxt) =
  ctxt
  [@@ocaml.inline always]

let use_gas_counter_in_ctxt ctxt local_gas_counter f =
  let ctxt = update_context local_gas_counter ctxt in
  f ctxt >>=? fun (y, ctxt) ->
  return (y, outdated ctxt, update_local_gas_counter ctxt)
  [@@ocaml.inline always]

let update_and_check gas_counter (cost : Gas.cost) =
  let gas_counter = gas_counter - (cost :> int) in
  if Compare.Int.(gas_counter < 0) then None else Some gas_counter
  [@@ocaml.inline always]

let consume local_gas_counter cost =
  match update_and_check local_gas_counter cost with
  | None -> error Gas.Operation_quota_exceeded
  | Some local_gas_counter -> Ok local_gas_counter
  [@@ocaml.inline always]
