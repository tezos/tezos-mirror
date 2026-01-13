(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Alpha_context
open Script_typed_ir

(** Types declaration for native contracts. *)

module Helpers = struct
  open Micheline

  (* Node describing a type in its typed and untyped version, that serves as
     combinator to define native contract types. *)
  type 'a ty_node = {untyped : Script.node; typed : 'a ty_ex_c}

  (* We actually don't need the location at all *)
  let loc = dummy_location

  let prim ?(annot = []) ?named prim args =
    let annot = match named with Some name -> name :: annot | None -> annot in
    Prim (loc, prim, args, annot)

  let unit_ty () = {untyped = prim Script.T_unit []; typed = Ty_ex_c unit_t}

  (* Some combinators are unused for now but will be used later, and they serve as an
     example to implement the rest. They are not exported as they are specific
     to building native contracts types. *)
  [@@@ocaml.warning "-32"]

  let int_ty () = {untyped = prim Script.T_int []; typed = Ty_ex_c int_t}

  let nat_ty () = {untyped = prim Script.T_nat []; typed = Ty_ex_c nat_t}

  let address_ty () =
    {untyped = prim Script.T_address []; typed = Ty_ex_c address_t}

  let address_big_map_ty (type value)
      ({untyped = unty_value; typed = Ty_ex_c ty_value; _} : value ty_node) :
      (address, value) big_map ty_node tzresult =
    let open Result_syntax in
    let* big_map_t : ((address, value) big_map, _) ty =
      big_map_t loc address_t ty_value
    in
    let untyped_big_map =
      prim Script.T_big_map [(address_ty ()).untyped; unty_value]
    in
    return {untyped = untyped_big_map; typed = Ty_ex_c big_map_t}

  let pair_ty (type a b) ({untyped = unty1; typed = Ty_ex_c ty1; _} : a ty_node)
      ({untyped = unty2; typed = Ty_ex_c ty2; _} : b ty_node) :
      (a * b) ty_node tzresult =
    let open Result_syntax in
    let+ pair_t = Script_typed_ir.pair_t loc ty1 ty2 in
    {untyped = prim Script.T_pair [unty1; unty2]; typed = pair_t}

  let or_ty (type l r) ({untyped = untyl; typed = Ty_ex_c tyl; _} : l ty_node)
      ({untyped = untyr; typed = Ty_ex_c tyr; _} : r ty_node) :
      (l, r) or_ ty_node tzresult =
    let open Result_syntax in
    let+ typed = Script_typed_ir.or_t loc tyl tyr in
    {untyped = prim Script.T_or [untyl; untyr]; typed}

  (** Entrypoints combinator *)

  (* The combinators will build the `or-tree` with the correct entrypoints representation. *)

  (** Generates the leaf of the entrypoint tree, i.e. an entrypoint. *)
  let make_entrypoint_leaf (type t) name ({untyped; typed} : t ty_node) =
    let open Result_syntax in
    let untyped =
      match untyped with
      | Prim (loc, prim, args, annot) ->
          Prim (loc, prim, args, ("%" ^ name) :: annot)
      | untyped -> untyped
    in
    let* name = Entrypoint.of_string_strict ~loc name in
    let at_node = Some {name; original_type_expr = untyped} in
    return ({typed; untyped}, {at_node; nested = Entrypoints_None})

  (** Generates a `or` node out of two entrypoints. *)
  let make_entrypoint_node (type left right)
      ((left_ty, left_etp) : left ty_node * left entrypoints_node)
      ((right_ty, right_etp) : right ty_node * right entrypoints_node) =
    let open Result_syntax in
    let* node_ty = or_ty left_ty right_ty in
    let entrypoints = Entrypoints_Or {left = left_etp; right = right_etp} in
    return (node_ty, {at_node = None; nested = entrypoints})

  (** Generate the entrypoints representation for contract that don't have
      entrypoint. *)
  let finalize_no_entrypoint (type t) (ty : t ty_node) =
    ( ty,
      {
        root = {at_node = None; nested = Entrypoints_None};
        original_type_expr = ty.untyped;
      } )

  (** Once the entrypoints tree has been built, simply generate the
      `entrypoints` type out of it. *)
  let finalize_entrypoint (type t)
      ((ty, entrypoint) : t ty_node * t entrypoints_node) =
    (ty, {root = entrypoint; original_type_expr = ty.untyped})
end

type ('arg, 'output) view_type = {
  input_ty : 'arg ty_ex_c;
  output_ty : 'output ty_ex_c;
}

type ('arg, 'storage, 'output) view = {
  name : Script_string.t;
  ty : ('arg, 'output) view_type;
  implementation :
    context * step_constants ->
    'arg ->
    'storage ->
    ('output * context) tzresult Lwt.t;
}

type 'storage ex_view =
  | Ex_view : ('arg, 'storage, 'output) view -> 'storage ex_view

type 'storage view_map = (Script_string.t, 'storage ex_view) map

module CLST_types = struct
  open Helpers

  type nat = Script_int.n Script_int.num

  type deposit = unit

  type withdraw = nat

  type arg = (deposit, withdraw) or_

  type ledger = (address, nat) big_map

  type storage = ledger

  let deposit_type : (deposit ty_node * deposit entrypoints_node) tzresult =
    make_entrypoint_leaf "deposit" (unit_ty ())

  let withdraw_type : (withdraw ty_node * withdraw entrypoints_node) tzresult =
    make_entrypoint_leaf "withdraw" (nat_ty ())

  let arg_type : (arg ty_node * arg entrypoints) tzresult =
    let open Result_syntax in
    let* deposit_type in
    let* withdraw_type in
    let* arg_type = make_entrypoint_node deposit_type withdraw_type in
    return (finalize_entrypoint arg_type)

  let storage_type : storage ty_node tzresult = address_big_map_ty (nat_ty ())
end

type ('arg, 'storage) kind =
  | CLST_kind : (CLST_types.arg, CLST_types.storage) kind

type ex_kind_and_types =
  | Ex_kind_and_types :
      (('arg, 'storage) kind * ('arg, _, 'storage, _) types)
      -> ex_kind_and_types

let get_typed_kind_and_types =
  let open Result_syntax in
  function
  | Script_native_repr.CLST ->
      let* {typed = Ty_ex_c arg_type; untyped = _}, entrypoints =
        CLST_types.arg_type
      in
      let* {typed = Ty_ex_c storage_type; untyped = _} =
        CLST_types.storage_type
      in
      return
        (Ex_kind_and_types (CLST_kind, {arg_type; storage_type; entrypoints}))

module Internal_for_tests = struct
  let eq_native_kind (type arg arg' storage storage')
      (kind : (arg, storage) kind) (kind' : (arg', storage') kind) =
    match (kind, kind') with CLST_kind, CLST_kind -> true

  type 'a ty_node = 'a Helpers.ty_node = {
    untyped : Script.node;
    typed : 'a ty_ex_c;
  }

  type ('arg, 'storage) tys =
    'arg Helpers.ty_node * 'arg entrypoints * 'storage Helpers.ty_node

  type ex_ty_node = Ex : ('arg, 'storage) tys -> ex_ty_node

  let types_of_kind =
    let open Result_syntax in
    function
    | Script_native_repr.CLST ->
        let* arg_type, arg_entrypoints = CLST_types.arg_type in
        let* storage_type = CLST_types.storage_type in
        return (Ex (arg_type, arg_entrypoints, storage_type))
end
