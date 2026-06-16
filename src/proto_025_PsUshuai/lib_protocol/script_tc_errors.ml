(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Script

(* ---- Error definitions ---------------------------------------------------*)

type kind = Int_kind | String_kind | Bytes_kind | Prim_kind | Seq_kind

type unparsed_stack_ty = Script.expr list

type type_map = (Script.location * (unparsed_stack_ty * unparsed_stack_ty)) list

(* Structure errors *)
type error += Invalid_arity of Script.location * prim * int * int

type error += Invalid_seq_arity of Script.location * int * int

type error +=
  | Invalid_namespace of
      Script.location
      * prim
      * Michelson_v1_primitives.namespace
      * Michelson_v1_primitives.namespace

type error += Invalid_primitive of Script.location * prim list * prim

type error += Invalid_kind of Script.location * kind list * kind

type error += Invalid_never_expr of Script.location

type error += Missing_field of prim

type error += Duplicate_field of Script.location * prim

type error += Unexpected_lazy_storage of Script.location

type error += Unexpected_operation of Script.location

type error += Unexpected_contract of Script.location

type error += Unpackable_type of Script.location

type error += No_such_entrypoint of Entrypoint.t

type error += Duplicate_entrypoint of Entrypoint.t

type error += Unreachable_entrypoint of prim list

(* Transaction rollup errors *)

type error += Tx_rollup_bad_deposit_parameter of Script.location * Script.expr

type error += Tx_rollup_invalid_ticket_amount of Z.t

type error += Forbidden_zero_ticket_quantity

(* Smart-contract rollup errors *)

type error += Sc_rollup_disabled of Script.location

(* Zero Knowledge rollup errors *)

type error += Zk_rollup_disabled of Script.location

type error += Tz5_account_disabled of Script.location

type error += Zk_rollup_bad_deposit_parameter of Script.location * Script.expr

(* Instruction typing errors *)
type error += Fail_not_in_tail_position of Script.location

type error +=
  | Undefined_binop :
      Script.location * prim * Script.expr * Script.expr
      -> error

type error += Undefined_unop : Script.location * prim * Script.expr -> error

type error +=
  | Bad_return : Script.location * unparsed_stack_ty * Script.expr -> error

type error +=
  | Bad_stack : Script.location * prim * int * unparsed_stack_ty -> error

type error +=
  | Unmatched_branches :
      Script.location * unparsed_stack_ty * unparsed_stack_ty
      -> error

(* View errors *)
type error += View_name_too_long of string

type error += Bad_view_name of Script.location

type error +=
  | Ill_typed_view of {
      loc : Script.location;
      actual : unparsed_stack_ty;
      expected : unparsed_stack_ty;
    }

type error += Duplicated_view_name of Script.location

type context_desc = Lambda | View

type error +=
  | Forbidden_instr_in_context of Script.location * context_desc * prim

type error += Bad_stack_length

type error += Bad_stack_item of int

type error += Unexpected_annotation of Script.location

type error += Ungrouped_annotations of Script.location

type error += Invalid_map_body : Script.location * unparsed_stack_ty -> error

type error += Invalid_map_block_fail of Script.location

type error +=
  | Invalid_iter_body :
      Script.location * unparsed_stack_ty * unparsed_stack_ty
      -> error

type error += Type_too_large : Script.location * int -> error

type error += Pair_bad_argument of Script.location

type error += Unpair_bad_argument of Script.location

type error += Dup_n_bad_argument of Script.location

type error += Dup_n_bad_stack of Script.location

(* Value typing errors *)
type error +=
  | Invalid_constant : Script.location * Script.expr * Script.expr -> error

type error +=
  | Invalid_syntactic_constant : Script.location * Script.expr * string -> error

type error += Invalid_contract of Script.location * Contract.t

type error += Invalid_big_map of Script.location * Big_map.Id.t

type error += Comparable_type_expected : Script.location * Script.expr -> error

type error += Inconsistent_type_sizes : int * int -> error

type error +=
  | Inconsistent_types : Script.location * Script.expr * Script.expr -> error

type error +=
  | Unexpected_implicit_account_parameters_type :
      Script.location * Script.expr
      -> error

type error +=
  | Inconsistent_memo_sizes : Sapling.Memo_size.t * Sapling.Memo_size.t -> error

type error += Unordered_map_keys of Script.location * Script.expr

type error += Unordered_set_values of Script.location * Script.expr

type error += Duplicate_map_keys of Script.location * Script.expr

type error += Duplicate_set_values of Script.location * Script.expr

(* Toplevel errors *)
type error +=
  | Ill_typed_data : string option * Script.expr * Script.expr -> error

type error += Ill_formed_type of string option * Script.expr * Script.location

type error += Ill_typed_contract : Script.expr * type_map -> error

(* Deprecation errors *)
type error += Deprecated_instruction of prim

(* Stackoverflow errors *)
type error += Typechecking_too_many_recursive_calls

type error += Unparsing_too_many_recursive_calls

(* Ticket errors *)
type error += Unexpected_ticket of Script.location

type error += Unexpected_forged_value of Script.location

type error += Non_dupable_type of Script.location * Script.expr

type error += Unexpected_ticket_owner of Destination.t

(* Merge type errors *)

type inconsistent_types_fast_error =
  | Inconsistent_types_fast
      (** This value is only used when the details of the error don't matter because
the error will be ignored later. For example, when types are compared during
the interpretation of the [CONTRACT] instruction any error will lead to
returning [None] but the content of the error will be ignored. *)

type (_, _) error_details =
  | Informative : 'error_context -> ('error_context, error trace) error_details
  | Fast : (_, inconsistent_types_fast_error) error_details
