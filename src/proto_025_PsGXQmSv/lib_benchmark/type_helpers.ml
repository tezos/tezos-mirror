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

exception Type_helpers_error of string

let helpers_error msg = raise (Type_helpers_error msg)

(* Convert a Micheline-encoded type to its internal GADT format. *)
let michelson_type_to_ex_ty (typ : Alpha_context.Script.expr)
    (ctxt : Alpha_context.t) =
  Script_ir_translator.parse_ty
    ctxt
    ~legacy:false
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:false
    ~allow_ticket:false
    (Micheline.root typ)
  |> Environment.wrap_tzresult
  |> function
  | Ok (ex_ty, _ctxt) -> ex_ty
  | Error errs ->
      let msg =
        Format.asprintf
          "Michelson_generation.michelson_type_to_ex_ty (%a)"
          Error_monad.pp_print_trace
          errs
      in
      helpers_error msg

(* Convert a list of Micheline-encoded Michelson types to the
   internal GADT format. *)
let rec michelson_type_list_to_ex_stack_ty
    (stack_ty : Alpha_context.Script.expr list) ctxt =
  let open Script_ir_translator in
  let open Script_typed_ir in
  match stack_ty with
  | [] -> Ex_stack_ty Bot_t
  | hd :: tl -> (
      let ex_ty = michelson_type_to_ex_ty hd ctxt in
      match ex_ty with
      | Ex_ty ty -> (
          let ex_stack_ty = michelson_type_list_to_ex_stack_ty tl ctxt in
          match ex_stack_ty with
          | Ex_stack_ty tl -> Ex_stack_ty (Item_t (ty, tl))))

let base_type_to_michelson_type (typ : Type.Base.t) =
  let typ = Mikhailsky.map_var (fun _ -> Mikhailsky.unit_ty) typ in
  Mikhailsky.to_michelson typ

(* Convert a Mikhailsky stack to a list of Micheline-encoded types *)
let rec stack_type_to_michelson_type_list (typ : Type.Stack.t) =
  let node = typ.node in
  match node with
  | Type.Stack.Stack_var_t _ ->
      helpers_error "stack_type_to_michelson_type_list: bug found"
  | Type.Stack.Empty_t -> []
  | Type.Stack.Item_t (ty, tl) ->
      base_type_to_michelson_type ty :: stack_type_to_michelson_type_list tl

let base_type_to_ex_ty ty =
  michelson_type_to_ex_ty (base_type_to_michelson_type ty)
