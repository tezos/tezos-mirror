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

  type deposit = unit

  type redeem = nat

  type transfer =
    ( address (* from_ *),
      (address (* to_ *), nat (* token_id *), nat (* amount *)) tup3
      Script_list.t )
    (* txs *)
    pair
    Script_list.t

  type arg = (deposit, (redeem, transfer) or_) or_

  type ledger = (address, nat) big_map

  type total_supply = nat

  type storage = ledger * total_supply

  type balance_view = (address * nat, nat) view_type

  type total_supply_view = (unit, nat) view_type

  type is_token_view = (nat, bool) view_type

  type entrypoint =
    | Deposit of deposit
    | Redeem of redeem
    | Transfer of transfer

  val entrypoint_from_arg : arg -> entrypoint

  val entrypoint_to_arg : entrypoint -> arg

  val balance_view_ty : balance_view tzresult

  val total_supply_view_ty : total_supply_view

  val is_token_view_ty : is_token_view

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
