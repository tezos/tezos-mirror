(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

(** This module deals with the size of Michelson types. Definitions of compound
    types allow for arbitrarily complex type expressions, however at some point
    these may become too expensive to practically compute. To protect the
    blockchain against DoS attacks a runtime limit is set for the size of type
    expressions. This limit is defined in the constants (see:
    constants_repr.ml). *)

open Alpha_context
open Script_typed_ir

(**
   [deduce_type_size ~remaining ty] returns [remaining] minus the size of type [ty]
   or any negative value if that result would be negative.
   It is guaranteed to not grow the stack by more than [remaining] non-tail calls.
*)
val deduce_type_size : remaining:int -> 't Script_typed_ir.ty -> int

(**
   [check_comparable_type_size ~legacy ~loc ty] checks that the size of type [ty]
   is not larger than the constant [maximum_type_size].
   If the check fails, an error [Type_too_large] is returned.
   If [legacy] is [true], there is no check at all and [ok_unit] is returned directly.

   It is guaranteed to not grow the stack by more than [maximum_type_size] non-tail calls.
*)
val check_comparable_type_size :
  legacy:bool ->
  loc:Script.location ->
  't Script_typed_ir.comparable_ty ->
  unit tzresult

(** Assert that expression's type size does not exceed the limit.

    [check_type_size ~legacy ~loc ty] returns [Ok ()] if the size of
    [ty] is not greater than the limit or an error otherwise. The limit is
    taken from constants except for legacy mode, where there is no limit. *)
val check_type_size :
  legacy:bool -> loc:int -> 'a ty -> (unit, error trace) result

(** Assert that the type size of neither of some top stack items exceeds the
    limit.

    [check_type_size_of_stack_head ~loc ~maximum_type_size stack ~up_to] checks
    [up_to] top items on the [stack] and verifies that type type size of neither
    of them exceeds [maximum_type_size]. If successful, returns [Ok ()],
    otherwise an error is returned. *)
val check_type_size_of_stack_head :
  loc:Script.location ->
  maximum_type_size:int ->
  ('a, 's) stack_ty ->
  up_to:int ->
  unit tzresult
