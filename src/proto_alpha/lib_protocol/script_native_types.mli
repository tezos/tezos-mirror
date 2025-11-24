(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Native contracts types declaration and combinators. *)

open Alpha_context

(** Types declaration of CLST contracts (entrypoints and storage). *)
module CLST_types : sig
  type arg = unit

  type storage = unit
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

module Internal_for_tests : sig
  val eq_native_kind : ('arg, 'storage) kind -> ('arg', 'storage') kind -> bool

  type 'ty ty_node = {
    untyped : Script.node;
    typed : 'ty Script_typed_ir.ty_ex_c;
  }

  type ('arg, 'storage) tys = 'arg ty_node * 'storage ty_node

  type ex_ty_node = Ex : ('arg, 'storage) tys -> ex_ty_node

  val types_of_kind : Script_native_repr.t -> ex_ty_node tzresult
end
