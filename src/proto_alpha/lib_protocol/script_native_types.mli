(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Native contracts types declaration and combinators. *)

open Alpha_context
open Script_typed_ir

type ('arg, 'output) view_type = {
  input_ty : 'arg ty_ex_c;
  output_ty : 'output ty_ex_c;
}

type 'ty ty_node = {untyped : Script.node; typed : 'ty Script_typed_ir.ty_ex_c}

(** Types declaration of CLST contracts (entrypoints and storage). *)
module CLST_types : sig
  type nat = Script_int.n Script_int.num

  type int = Script_int.z Script_int.num

  (** Michelson representation for tuples. *)
  type ('a, 'b, 'c) tup3 = 'a * ('b * 'c)

  type ('a, 'b, 'c, 'd) tup4 = 'a * ('b * ('c * 'd))

  type ('a, 'b, 'c, 'd, 'e) tup5 = 'a * ('b * ('c * ('d * 'e)))

  type 'a s_list = 'a Script_list.t

  type deposit = unit

  type redeem = nat

  type finalize = unit

  type staker_entrypoints = ((deposit, redeem) or_, finalize) or_

  type delegate_parameters =
    nat (* edge_of_clst_staking *) * nat (* ratio_of_clst_staking *)

  type register_delegate = delegate_parameters option

  type update_delegate_parameters = delegate_parameters

  type delegate_entrypoints =
    (register_delegate, update_delegate_parameters) or_

  type clst_entrypoints = (staker_entrypoints, delegate_entrypoints) or_

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

  type operator =
    (address (* owner *), address (* operator *), nat (* token_id *)) tup3

  type operator_delta =
    (operator (* add_operator *), operator (* remove_operator *)) or_

  type update_operators = operator_delta s_list

  type balance_request = address (* owner *) * nat (* token_id *)

  type balance_of =
    balance_request s_list
    * (balance_request * nat (* balance*)) s_list typed_contract

  type ticket_with_token_id = (nat (* token_id *) * bytes option) ticket

  type export_ticket =
    ( (address * ticket_with_token_id s_list) s_list typed_contract option
    (* destination *),
      ( address (* to_ *),
        (address (* from_ *), nat (* token_id *), nat (* amount *)) tup3 s_list
      (* tickets_to_export *) )
      pair
      s_list
    (* txs *) )
    pair
    s_list

  type import_ticket =
    (address (* to_ *), ticket_with_token_id s_list (* tickets *)) pair s_list

  type import_ticket_from_implicit = ticket_with_token_id

  type lambda_export =
    ( (address (* from_ *), nat (* token_id *), nat (* amount *)) tup3 s_list
    (* tickets_to_export *),
      (ticket_with_token_id s_list, operation s_list) lambda
    (* action *) )
    pair

  type allowance_entrypoints = (approve, update_operators) or_

  type tickets_entrypoints =
    ( (export_ticket, import_ticket) or_,
      (lambda_export, import_ticket_from_implicit) or_ )
    or_

  type fa21_entrypoints =
    ( (transfer, balance_of) or_,
      (allowance_entrypoints, tickets_entrypoints) or_ )
    or_

  type arg = (clst_entrypoints, fa21_entrypoints) or_

  type ledger = (address, nat) big_map

  type total_supply = nat

  type operators = (address, nat option) map

  (** The operators table associates each token owner with a list of
      accounts that have permission to transfer tokens on their
      behalf. This permission can either be a finite allowance (`Some
      allowance`) or an infinite allowance (`None`).

      A finite allowance decreases with each transfer or export of
      tokens into tickets. Operators cannot increase the allowance. *)
  type operators_table = (address, operators) big_map

  type token_info = (Script_string.t, bytes) map

  type token_metadata = (nat, nat (* token_id *) * token_info) big_map

  type storage = (ledger, total_supply, operators_table, token_metadata) tup4

  type balance_view = (address * nat, nat) view_type

  type total_supply_view = (nat, nat) view_type

  type is_token_view = (nat, bool) view_type

  type get_allowance_view =
    ( (address (* owner *), address (* spender *), nat (* token_id *)) tup3,
      nat (* allowance *) )
    view_type

  type is_operator_view =
    ( (address (* owner *), address (* operator *), nat (* token_id *)) tup3,
      bool (* is_operator *) )
    view_type

  type get_token_metadata_view = (nat (* token_id *), token_info) view_type

  type entrypoint =
    | Deposit of deposit
    | Redeem of redeem
    | Finalize of finalize
    | Register_delegate of register_delegate
    | Update_delegate_parameters of update_delegate_parameters
    | Transfer of transfer
    | Balance_of of balance_of
    | Approve of approve
    | Update_operators of update_operators
    | Export_ticket of export_ticket
    | Import_ticket of import_ticket
    | Lambda_export of lambda_export
    | Import_ticket_from_implicit of import_ticket_from_implicit

  val entrypoint_from_arg : arg -> entrypoint

  val entrypoint_to_arg : entrypoint -> arg

  val clst_contents_ticket_ty : (nat * bytes option) comparable_ty tzresult

  val clst_ticket_ty : (ticket_with_token_id, Dependent_bool.no) ty tzresult

  val balance_view_ty : balance_view tzresult

  val total_supply_view_ty : total_supply_view

  val is_token_view_ty : is_token_view

  val get_allowance_view_ty : get_allowance_view tzresult

  val is_operator_view_ty : is_operator_view tzresult

  val get_token_metadata_view_ty : get_token_metadata_view tzresult

  val transfer_event_type :
    ( address (* from_ *),
      address (* to_ *),
      nat (* token_id *),
      nat (* amount *) )
    tup4
    ty_node
    tzresult

  val balance_update_event_type :
    ( address (* owner *),
      nat (* token_id *),
      nat (* new_balance *),
      int (* diff *) )
    tup4
    ty_node
    tzresult

  val total_supply_update_event_type :
    (nat (* token_id *), nat (* new_total_supply *), int (* diff *)) tup3
    ty_node
    tzresult

  val allowance_update_event_type :
    ( address (* owner *),
      address (* spender *),
      nat (* token_id *),
      nat (* new_allowance *),
      int (* diff *) )
    tup5
    ty_node
    tzresult

  val operator_update_event_type :
    ( address (* owner *),
      address (* operator *),
      nat (* token_id *),
      bool (* is_operator *) )
    tup4
    ty_node
    tzresult

  val token_metadata_update_event_type :
    (nat (* token_id *), token_info option (* new_metadata *)) pair ty_node
    tzresult
end

(** Typed equivalent of `Script_native_repr.kind` *)
type ('arg, 'storage) kind =
  | CLST_kind : (CLST_types.arg, CLST_types.storage) kind

type ex_kind_and_types =
  | Ex_kind_and_types :
      (('arg, 'storage) kind * ('arg, _, 'storage, _) Script_typed_ir.types)
      -> ex_kind_and_types

(** [get_typed_kind_and_types k] returns the typed representation and reified
    types for the given native contract kind. *)
val get_typed_kind_and_types :
  Script_native_repr.t -> ex_kind_and_types tzresult

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

module Internal_for_tests : sig
  val eq_native_kind : ('arg, 'storage) kind -> ('arg', 'storage') kind -> bool

  type ('arg, 'storage) tys = 'arg ty_node * 'arg entrypoints * 'storage ty_node

  type ex_ty_node = Ex : ('arg, 'storage) tys -> ex_ty_node

  val types_of_kind : Script_native_repr.t -> ex_ty_node tzresult
end
