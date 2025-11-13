(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_typed_ir

(** Types declaration for native contracts. *)

type z_n = Script_int.z Script_int.num

module Helpers = struct
  open Micheline

  (* Node describing a type in its typed and untyped version, that serves as
     combinator to define native contract types. *)
  type 'a ty_node = {untyped : Script.node; typed : 'a ty_ex_c}

  let prim ?(loc = dummy_location) ?(annot = []) ?named prim args =
    let annot = match named with Some name -> name :: annot | None -> annot in
    Prim (loc, prim, args, annot)

  let int_ty ?loc () =
    {untyped = prim ?loc Script.T_int []; typed = Ty_ex_c int_t}

  let pair_ty (type a b) ?(loc = dummy_location)
      ({untyped = unty1; typed = Ty_ex_c ty1; _} : a ty_node)
      ({untyped = unty2; typed = Ty_ex_c ty2; _} : b ty_node) :
      (a * b) ty_node tzresult =
    let open Result_syntax in
    let+ pair_t = Script_typed_ir.pair_t loc ty1 ty2 in
    {untyped = prim ~loc Script.T_pair [unty1; unty2]; typed = pair_t}
end

module Accumulator_types = struct
  open Helpers

  type arg = z_n

  type storage = z_n * z_n

  let arg_type : arg ty_node = int_ty ()

  let storage_type : storage ty_node tzresult = pair_ty (int_ty ()) (int_ty ())
end

type ('arg, 'storage) kind =
  | Accumulator_kind : (Accumulator_types.arg, Accumulator_types.storage) kind

type ex_kind_and_types =
  | Ex_kind_and_types :
      (('arg, 'storage) kind * ('arg, _, 'storage, _) types)
      -> ex_kind_and_types

let get_typed_kind_and_types =
  let open Result_syntax in
  function
  | Script_native_repr.Accumulator ->
      let {typed = Ty_ex_c arg_type; untyped} = Accumulator_types.arg_type in
      let+ {typed = Ty_ex_c storage_type; _} = Accumulator_types.storage_type in
      (* The entrypoints will be introduced in a later MR (!19584). *)
      let entrypoints =
        {
          root = {at_node = None; nested = Entrypoints_None};
          original_type_expr = untyped;
        }
      in
      Ex_kind_and_types (Accumulator_kind, {arg_type; storage_type; entrypoints})

module Internal_for_tests = struct
  let eq_native_kind (type arg arg' storage storage')
      (kind : (arg, storage) kind) (kind' : (arg', storage') kind) =
    match (kind, kind') with Accumulator_kind, Accumulator_kind -> true

  type 'a ty_node = 'a Helpers.ty_node = {
    untyped : Script.node;
    typed : 'a ty_ex_c;
  }

  type ('arg, 'storage) tys = 'arg Helpers.ty_node * 'storage Helpers.ty_node

  type ex_ty_node = Ex : ('arg, 'storage) tys -> ex_ty_node

  let types_of_kind =
    let open Result_syntax in
    function
    | Script_native_repr.Accumulator ->
        let arg_type = Accumulator_types.arg_type in
        let* storage_type = Accumulator_types.storage_type in
        return (Ex (arg_type, storage_type))
end
