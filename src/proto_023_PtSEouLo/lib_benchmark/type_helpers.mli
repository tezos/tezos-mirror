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

(** Type conversion helpers *)

open Protocol

(** Exception raised in case an error occurs in this module. *)
exception Type_helpers_error of string

(** [michelson_type_list_to_ex_stack_ty] converts a list of types in
    Micheline form to a stack type in IR form.

    @raise Type_helpers_error if parsing the Michelson type fails.
 *)
val michelson_type_list_to_ex_stack_ty :
  Alpha_context.Script.expr list ->
  Alpha_context.t ->
  Script_ir_translator.ex_stack_ty

(** [michelson_type_to_ex_ty ty ctxt] parses the type [ty].

    @raise Type_helpers_error if an error arises during parsing. *)
val michelson_type_to_ex_ty :
  Alpha_context.Script.expr -> Alpha_context.t -> Script_typed_ir.ex_ty

(** [stack_type_to_michelson_type_list] converts a Mikhailsky stack type
    to a stack represented as a list of Micheline expressions, each
    element denoting a type on the stack type.

    @raise Type_helpers_error if the stack type contains variables. *)
val stack_type_to_michelson_type_list : Type.Stack.t -> Script_repr.expr list

(** [base_type_to_ex_ty] converts a Mikhailsky type to a Michelson one. *)
val base_type_to_ex_ty : Type.Base.t -> Alpha_context.t -> Script_typed_ir.ex_ty
