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
open Script_typed_ir

(** This module defines the typechecking context used during the translation
    from Michelson untyped nodes to typed nodes ([Script_ir_translator]).
    The context keeps track of the origin of the code (top-level from a contract,
    in a view, etc.), plus some information to allow or forbid instructions
    given the context (no `SELF` in a lambda for example). *)

(** Lambdas are a bit special when considering stateful instructions such as
    [TRANSFER_TOKENS].
    For instance, a view containing a [TRANSFER_TOKENS] is not OK, because
    calling the view would transfer tokens from the view's owner.
    However, a view returning a lambda containing a [TRANSFER_TOKENS] could be
    considered OK, as the decision whether to execute it or not falls on
    the view's caller, whose tokens would be transfered.
    This type is used to keep track of whether we are inside a lambda: it is
    [true] when inside a lambda, and [false] otherwise. *)
type in_lambda = bool

(** The calling context when parsing Michelson code: either a top-level contract
    code, the code of a view, or code in data (when pushing a block of
    instructions for example). *)
type callsite =
  | Toplevel : {
      storage_type : ('sto, _) ty;
      param_type : ('param, _) ty;
      entrypoints : 'param Script_typed_ir.entrypoints;
    }
      -> callsite
  | View : callsite
  | Data : callsite

type t = {callsite : callsite; in_lambda : in_lambda}

val init : callsite -> t

val toplevel :
  storage_type:('sto, _) ty ->
  param_type:('param, _) ty ->
  entrypoints:'param Script_typed_ir.entrypoints ->
  t

val view : t

(** This value can be used outside the translation module as a simple context
    when testing code, for example. *)
val data : t

val add_lambda : t -> t

val is_in_lambda : t -> bool

val check_not_in_view :
  Script.location -> legacy:bool -> t -> Script.prim -> unit tzresult
