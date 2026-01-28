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

  (* Michelson comb-pairs representation *)
  type ('a, 'b, 'c) tup3 = 'a * ('b * 'c)

  type ('a, 'b, 'c, 'd) tup4 = 'a * ('b, 'c, 'd) tup3

  (* Node describing a type in its typed and untyped version, that serves as
     combinator to define native contract types. *)
  type 'a ty_node = {untyped : Script.node; typed : 'a ty_ex_c}

  (* We actually don't need the location at all *)
  let loc = dummy_location

  let prim ?(annot = []) ?named prim args =
    let annot = match named with Some name -> name :: annot | None -> annot in
    Prim (loc, prim, args, annot)

  let unit_ty = {untyped = prim Script.T_unit []; typed = Ty_ex_c unit_t}

  (* Some combinators are unused for now but will be used later, and they serve as an
     example to implement the rest. They are not exported as they are specific
     to building native contracts types. *)
  [@@@ocaml.warning "-32"]

  let int_ty = {untyped = prim Script.T_int []; typed = Ty_ex_c int_t}

  let nat_ty = {untyped = prim Script.T_nat []; typed = Ty_ex_c nat_t}

  let bool_ty = {untyped = prim Script.T_bool []; typed = Ty_ex_c bool_t}

  let address_ty =
    {untyped = prim Script.T_address []; typed = Ty_ex_c address_t}

  let address_map_ty (type value)
      ({untyped = unty_value; typed = Ty_ex_c ty_value; _} : value ty_node) :
      (address, value) map ty_node tzresult =
    let open Result_syntax in
    let* map_t : ((address, value) map, _) ty = map_t loc address_t ty_value in
    let untyped_map = prim Script.T_map [address_ty.untyped; unty_value] in
    return {untyped = untyped_map; typed = Ty_ex_c map_t}

  let address_big_map_ty (type value)
      ({untyped = unty_value; typed = Ty_ex_c ty_value; _} : value ty_node) :
      (address, value) big_map ty_node tzresult =
    let open Result_syntax in
    let* big_map_t : ((address, value) big_map, _) ty =
      big_map_t loc address_t ty_value
    in
    let untyped_big_map =
      prim Script.T_big_map [address_ty.untyped; unty_value]
    in
    return {untyped = untyped_big_map; typed = Ty_ex_c big_map_t}

  let pair_ty (type a b) ({untyped = unty1; typed = Ty_ex_c ty1; _} : a ty_node)
      ({untyped = unty2; typed = Ty_ex_c ty2; _} : b ty_node) :
      (a * b) ty_node tzresult =
    let open Result_syntax in
    let+ pair_t = Script_typed_ir.pair_t loc ty1 ty2 in
    {untyped = prim Script.T_pair [unty1; unty2]; typed = pair_t}

  let tup3_ty (type a b c) ty1 ty2 ty3 : (a, b, c) tup3 ty_node tzresult =
    let open Result_syntax in
    let* r = pair_ty ty2 ty3 in
    pair_ty ty1 r

  let tup4_ty (type a b c d) ty1 ty2 ty3 ty4 :
      (a, b, c, d) tup4 ty_node tzresult =
    let open Result_syntax in
    let* r = tup3_ty ty2 ty3 ty4 in
    pair_ty ty1 r

  let or_ty (type l r) ({untyped = untyl; typed = Ty_ex_c tyl; _} : l ty_node)
      ({untyped = untyr; typed = Ty_ex_c tyr; _} : r ty_node) :
      (l, r) or_ ty_node tzresult =
    let open Result_syntax in
    let+ typed = Script_typed_ir.or_t loc tyl tyr in
    {untyped = prim Script.T_or [untyl; untyr]; typed}

  let list_ty ({untyped; typed = Ty_ex_c ty_elt} : 'a ty_node) :
      'a Script_list.t ty_node tzresult =
    let open Result_syntax in
    let* ty_list = Script_typed_ir.list_t loc ty_elt in
    return {untyped = prim Script.T_list [untyped]; typed = Ty_ex_c ty_list}

  let option_ty (type t) ({untyped; typed = Ty_ex_c ty; _} : t ty_node) :
      t option ty_node tzresult =
    let open Result_syntax in
    let+ typed = Script_typed_ir.option_t loc ty in
    {untyped = prim Script.T_option [untyped]; typed = Ty_ex_c typed}

  (** Entrypoints combinator *)

  (* The combinators will build the `or-tree` with the correct entrypoints representation. *)

  (** Annotates a node with a name. *)
  let add_name name node =
    let untyped =
      match node.untyped with
      | Prim (loc, prim, args, annot) ->
          Prim (loc, prim, args, ("%" ^ name) :: annot)
      | (Int _ | String _ | Bytes _ | Seq _) as x -> x
    in
    {node with untyped}

  (** Generates the leaf of the entrypoint tree, i.e. an entrypoint. *)
  let make_entrypoint_leaf (type t) name (node : t ty_node) =
    let open Result_syntax in
    let {typed; untyped} = add_name name node in
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

type 'a ty_node = 'a Helpers.ty_node = {
  untyped : Script.node;
  typed : 'a ty_ex_c;
}

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

  type ('a, 'b, 'c) tup3 = ('a, 'b, 'c) Helpers.tup3

  type ('a, 'b, 'c, 'd) tup4 = ('a, 'b, 'c, 'd) Helpers.tup4

  type 'a s_list = 'a Script_list.t

  type nat = Script_int.n Script_int.num

  type int = Script_int.z Script_int.num

  type deposit = unit

  type redeem = nat

  type clst_entrypoints = (deposit, redeem) or_

  type transfer =
    ( address (* from_ *),
      (address (* to_ *), nat (* token_id *), nat (* amount *)) tup3
      Script_list.t )
    (* txs *)
    pair
    Script_list.t

  type allowance_delta = (nat (* increase *), nat (* decrease *)) or_

  type approval =
    ( address (* owner *),
      address (* spender *),
      nat (* token_id *),
      allowance_delta (* action *) )
    tup4

  type approve = approval s_list

  type fa21_entrypoints = (transfer, approve) or_

  type arg = (clst_entrypoints, fa21_entrypoints) or_

  type ledger = (address, nat) big_map

  type total_supply = nat

  type operators = (address, nat option) map

  type operators_table = (address, operators) big_map

  type storage = (ledger, total_supply, operators_table) tup3

  type entrypoint =
    | Deposit of deposit
    | Redeem of redeem
    | Transfer of transfer
    | Approve of approve

  let entrypoint_from_arg : arg -> entrypoint = function
    | L (L p) -> Deposit p
    | L (R p) -> Redeem p
    | R (L p) -> Transfer p
    | R (R p) -> Approve p

  let entrypoint_to_arg : entrypoint -> arg = function
    | Deposit p -> L (L p)
    | Redeem p -> L (R p)
    | Transfer p -> R (L p)
    | Approve p -> R (R p)

  let deposit_type : (deposit ty_node * deposit entrypoints_node) tzresult =
    make_entrypoint_leaf "deposit" unit_ty

  let redeem_type : (redeem ty_node * redeem entrypoints_node) tzresult =
    make_entrypoint_leaf "redeem" nat_ty

  let transfer_type : (transfer ty_node * transfer entrypoints_node) tzresult =
    let open Result_syntax in
    let* tx =
      tup3_ty
        (add_name "to_" address_ty)
        (add_name "token_id" nat_ty)
        (add_name "amount" nat_ty)
    in
    let* txs = list_ty tx in
    let* elt = pair_ty (add_name "from_" address_ty) (add_name "txs" txs) in
    let* transfer = list_ty elt in
    make_entrypoint_leaf "transfer" transfer

  let approval_type : approval ty_node tzresult =
    let open Result_syntax in
    let* token_id_approval =
      or_ty (add_name "increase" nat_ty) (add_name "decrease" nat_ty)
    in
    tup4_ty
      (add_name "owner" address_ty)
      (add_name "spender" address_ty)
      (add_name "token_id" nat_ty)
      (add_name "action" token_id_approval)

  let approve_type : (approve ty_node * approve entrypoints_node) tzresult =
    let open Result_syntax in
    let* approval_type in
    let* approve_type = list_ty approval_type in
    make_entrypoint_leaf "approve" approve_type

  let arg_type : (arg ty_node * arg entrypoints) tzresult =
    let open Result_syntax in
    let* deposit_type in
    let* redeem_type in
    let* transfer_type in
    let* approve_type in
    let* clst_entrypoints_type =
      make_entrypoint_node deposit_type redeem_type
    in
    let* fa21_entrypoints_type =
      make_entrypoint_node transfer_type approve_type
    in
    let* arg_type =
      make_entrypoint_node clst_entrypoints_type fa21_entrypoints_type
    in
    return (finalize_entrypoint arg_type)

  let operators_table_ty : operators_table ty_node tzresult =
    let open Result_syntax in
    let* allowance_ty = option_ty nat_ty in
    let* operators_ty = address_map_ty allowance_ty in
    address_big_map_ty operators_ty

  let storage_type : storage ty_node tzresult =
    let open Result_syntax in
    let* ledger_ty = address_big_map_ty nat_ty in
    let* operators_table_ty in
    tup3_ty ledger_ty nat_ty operators_table_ty

  type balance_view = (address * nat, nat) view_type

  type total_supply_view = (unit, nat) view_type

  type is_token_view = (nat, bool) view_type

  type get_allowance_view =
    ( (address (* owner *), address (* spender *), nat (* token_id *)) tup3,
      nat (* allowance *) )
    view_type

  let balance_view_ty =
    let open Result_syntax in
    let* {typed = input_ty; _} = pair_ty address_ty nat_ty in
    return {input_ty; output_ty = nat_ty.typed}

  let total_supply_view_ty =
    {input_ty = unit_ty.typed; output_ty = nat_ty.typed}

  let is_token_view_ty = {input_ty = nat_ty.typed; output_ty = bool_ty.typed}

  let get_allowance_view_ty =
    let open Result_syntax in
    let* {typed = input_ty; _} = tup3_ty address_ty address_ty nat_ty in
    return {input_ty; output_ty = nat_ty.typed}

  let transfer_event_type =
    let open Result_syntax in
    let* x =
      tup4_ty
        (add_name "from_" address_ty)
        (add_name "to_" address_ty)
        (add_name "token_id" nat_ty)
        (add_name "amount" nat_ty)
    in
    return @@ add_name "transfer_event" x

  let balance_update_event_type =
    let open Result_syntax in
    let* x =
      tup4_ty
        (add_name "owner" address_ty)
        (add_name "token_id" nat_ty)
        (add_name "new_balance" nat_ty)
        (add_name "diff" int_ty)
    in
    return @@ add_name "balance_update" x

  let total_supply_update_event_type =
    let open Result_syntax in
    let* x =
      tup3_ty
        (add_name "token_id" nat_ty)
        (add_name "new_total_supply" nat_ty)
        (add_name "diff" int_ty)
    in
    return @@ add_name "total_supply_update" x
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
