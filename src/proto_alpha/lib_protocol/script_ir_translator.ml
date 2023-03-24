(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
(* Copyright (c) 2022 DaiLambda, Inc. <contact@dailambda,jp>                 *)
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
open Micheline
open Script
open Script_tc_errors
open Script_ir_annot
open Script_typed_ir
open Script_ir_unparser
module Typecheck_costs = Michelson_v1_gas.Cost_of.Typechecking
module Unparse_costs = Michelson_v1_gas.Cost_of.Unparsing
module Tc_context = Script_tc_context

type elab_conf = Script_ir_translator_config.elab_config

type ex_stack_ty = Ex_stack_ty : ('a, 's) stack_ty -> ex_stack_ty

(* Equality witnesses *)
type ('ta, 'tb) eq = Eq : ('same, 'same) eq

(*

   The following type represents an instruction parameterized by its
   continuation. During the elaboration of the typed term, a sequence
   of instructions in Micheline is read from left to right: hence, the
   elaboration needs to wait for the next instruction to be elaborated
   to be able to construct the current instruction.

*)
type ('a, 's, 'b, 'u) cinstr = {
  apply : 'r 'f. ('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
}
[@@ocaml.unboxed]

(*

   While a [Script_typed_ir.descr] contains a fully defined
   instruction, [descr] contains a [cinstr], that is an instruction
   parameterized by the next instruction, as explained in the previous
   comment.

*)
type ('a, 's, 'b, 'u) descr = {
  loc : Script.location;
  bef : ('a, 's) stack_ty;
  aft : ('b, 'u) stack_ty;
  instr : ('a, 's, 'b, 'u) cinstr;
}

let close_descr {loc; bef; aft; instr} =
  let kinstr = instr.apply (IHalt loc) in
  {kloc = loc; kbef = bef; kaft = aft; kinstr}

let compose_descr :
    type a s b u c v.
    Script.location ->
    (a, s, b, u) descr ->
    (b, u, c, v) descr ->
    (a, s, c, v) descr =
 fun loc d1 d2 ->
  {
    loc;
    bef = d1.bef;
    aft = d2.aft;
    instr = {apply = (fun k -> d1.instr.apply (d2.instr.apply k))};
  }

type tc_context = Tc_context.t

(* ---- Error helpers -------------------------------------------------------*)

let location = function
  | Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) ->
      loc

let kind_equal a b =
  match (a, b) with
  | Int_kind, Int_kind
  | String_kind, String_kind
  | Bytes_kind, Bytes_kind
  | Prim_kind, Prim_kind
  | Seq_kind, Seq_kind ->
      true
  | _ -> false

let kind = function
  | Int _ -> Int_kind
  | String _ -> String_kind
  | Bytes _ -> Bytes_kind
  | Prim _ -> Prim_kind
  | Seq _ -> Seq_kind

let unexpected expr exp_kinds exp_ns exp_prims =
  match expr with
  | Int (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Int_kind)
  | String (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, String_kind)
  | Bytes (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Bytes_kind)
  | Seq (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Seq_kind)
  | Prim (loc, name, _, _) -> (
      let open Michelson_v1_primitives in
      match (namespace name, exp_ns) with
      | Type_namespace, Type_namespace
      | Instr_namespace, Instr_namespace
      | Constant_namespace, Constant_namespace ->
          Invalid_primitive (loc, exp_prims, name)
      | ns, _ -> Invalid_namespace (loc, name, exp_ns, ns))

let check_kind kinds expr =
  let kind = kind expr in
  if List.exists (kind_equal kind) kinds then Result.return_unit
  else
    let loc = location expr in
    error (Invalid_kind (loc, kinds, kind))

let check_comparable :
    type a ac.
    Script.location -> (a, ac) ty -> (ac, Dependent_bool.yes) eq tzresult =
 fun loc ty ->
  match is_comparable ty with
  | Yes -> ok Eq
  | No ->
      let t = Script_ir_unparser.serialize_ty_for_error ty in
      error (Comparable_type_expected (loc, t))

let pack_node unparsed ctxt =
  let bytes =
    Data_encoding.(
      Binary.to_bytes_exn (tup2 (Fixed.string Plain 1) expr_encoding))
      ("\x05", unparsed)
  in
  (bytes, ctxt)

let pack_comparable_data ctxt ty data =
  unparse_comparable_data ctxt Optimized_legacy ty data
  >|=? fun (unparsed, ctxt) -> pack_node unparsed ctxt

let hash_bytes ctxt bytes =
  Gas.consume ctxt (Michelson_v1_gas.Cost_of.Interpreter.blake2b bytes)
  >|? fun ctxt -> (Script_expr_hash.(hash_bytes [bytes]), ctxt)

let hash_comparable_data ctxt ty data =
  pack_comparable_data ctxt ty data >>=? fun (bytes, ctxt) ->
  Lwt.return @@ hash_bytes ctxt bytes

(* ---- Tickets ------------------------------------------------------------ *)

(*
   All comparable types are dupable, this function exists only to not forget
   checking this property when adding new types.
*)
let check_dupable_comparable_ty : type a. a comparable_ty -> unit = function
  | Unit_t | Never_t | Int_t | Nat_t | Signature_t | String_t | Bytes_t
  | Mutez_t | Bool_t | Key_hash_t | Key_t | Timestamp_t | Chain_id_t | Address_t
  | Tx_rollup_l2_address_t | Pair_t _ | Or_t _ | Option_t _ ->
      ()

let check_dupable_ty ctxt loc ty =
  let rec aux : type a ac. location -> (a, ac) ty -> (unit, error) Gas_monad.t =
   fun loc ty ->
    let open Gas_monad.Syntax in
    let* () = Gas_monad.consume_gas Typecheck_costs.check_dupable_cycle in
    match ty with
    | Unit_t -> return_unit
    | Int_t -> return_unit
    | Nat_t -> return_unit
    | Signature_t -> return_unit
    | String_t -> return_unit
    | Bytes_t -> return_unit
    | Mutez_t -> return_unit
    | Key_hash_t -> return_unit
    | Key_t -> return_unit
    | Timestamp_t -> return_unit
    | Address_t -> return_unit
    | Tx_rollup_l2_address_t -> return_unit
    | Bool_t -> return_unit
    | Contract_t _ -> return_unit
    | Operation_t -> return_unit
    | Chain_id_t -> return_unit
    | Never_t -> return_unit
    | Bls12_381_g1_t -> return_unit
    | Bls12_381_g2_t -> return_unit
    | Bls12_381_fr_t -> return_unit
    | Sapling_state_t _ -> return_unit
    | Sapling_transaction_t _ -> return_unit
    | Sapling_transaction_deprecated_t _ -> return_unit
    | Chest_t -> return_unit
    | Chest_key_t -> return_unit
    | Ticket_t _ -> fail @@ Unexpected_ticket loc
    | Pair_t (ty_a, ty_b, _, _) ->
        let* () = aux loc ty_a in
        aux loc ty_b
    | Or_t (ty_a, ty_b, _, _) ->
        let* () = aux loc ty_a in
        aux loc ty_b
    | Lambda_t (_, _, _) ->
        (*
        Lambda are dupable as long as:
          - they don't contain non-dupable values, e.g. in `PUSH`
            (mostly non-dupable values should probably be considered forged)
          - they are not the result of a partial application on a non-dupable
            value. `APPLY` rejects non-packable types (because of `PUSH`).
            Hence non-dupable should imply non-packable.
      *)
        return_unit
    | Option_t (ty, _, _) -> aux loc ty
    | List_t (ty, _) -> aux loc ty
    | Set_t (key_ty, _) ->
        let () = check_dupable_comparable_ty key_ty in
        return_unit
    | Map_t (key_ty, val_ty, _) ->
        let () = check_dupable_comparable_ty key_ty in
        aux loc val_ty
    | Big_map_t (key_ty, val_ty, _) ->
        let () = check_dupable_comparable_ty key_ty in
        aux loc val_ty
  in
  let gas = aux loc ty in
  Gas_monad.run ctxt gas >>? fun (res, ctxt) ->
  match res with Ok () -> ok ctxt | Error e -> error e

let type_metadata_eq :
    type error_trace.
    error_details:(_, error_trace) error_details ->
    'a ty_metadata ->
    'b ty_metadata ->
    (unit, error_trace) result =
 fun ~error_details {size = size_a} {size = size_b} ->
  Type_size.check_eq ~error_details size_a size_b

let default_ty_eq_error loc ty1 ty2 =
  let ty1 = serialize_ty_for_error ty1 in
  let ty2 = serialize_ty_for_error ty2 in
  Inconsistent_types (loc, ty1, ty2)

let memo_size_eq :
    type error_trace.
    error_details:(_, error_trace) error_details ->
    Sapling.Memo_size.t ->
    Sapling.Memo_size.t ->
    (unit, error_trace) result =
 fun ~error_details ms1 ms2 ->
  if Sapling.Memo_size.equal ms1 ms2 then Result.return_unit
  else
    Error
      (match error_details with
      | Fast -> Inconsistent_types_fast
      | Informative _ -> trace_of_error @@ Inconsistent_memo_sizes (ms1, ms2))

(* Check that two types are equal.

   The result is an equality witness between the types of the two inputs within
   the gas monad (for gas consumption).
*)
let ty_eq :
    type a ac b bc error_trace.
    error_details:(Script.location, error_trace) error_details ->
    (a, ac) ty ->
    (b, bc) ty ->
    (((a, ac) ty, (b, bc) ty) eq, error_trace) Gas_monad.t =
 fun ~error_details ty1 ty2 ->
  let type_metadata_eq meta1 meta2 =
    Gas_monad.of_result (type_metadata_eq ~error_details meta1 meta2)
    |> Gas_monad.record_trace_eval ~error_details (fun loc ->
           default_ty_eq_error loc ty1 ty2)
  in
  let memo_size_eq ms1 ms2 =
    Gas_monad.of_result (memo_size_eq ~error_details ms1 ms2)
  in
  let rec help :
      type ta tac tb tbc.
      (ta, tac) ty ->
      (tb, tbc) ty ->
      (((ta, tac) ty, (tb, tbc) ty) eq, error_trace) Gas_monad.t =
   fun ty1 ty2 ->
    help0 ty1 ty2
    |> Gas_monad.record_trace_eval ~error_details (fun loc ->
           default_ty_eq_error loc ty1 ty2)
  and help0 :
      type ta tac tb tbc.
      (ta, tac) ty ->
      (tb, tbc) ty ->
      (((ta, tac) ty, (tb, tbc) ty) eq, error_trace) Gas_monad.t =
   fun ty1 ty2 ->
    let open Gas_monad.Syntax in
    let* () = Gas_monad.consume_gas Typecheck_costs.merge_cycle in
    let not_equal () =
      Gas_monad.of_result
      @@ Error
           (match error_details with
           | Fast -> (Inconsistent_types_fast : error_trace)
           | Informative loc ->
               trace_of_error @@ default_ty_eq_error loc ty1 ty2)
    in
    match (ty1, ty2) with
    | Unit_t, Unit_t -> return (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Unit_t, _ -> not_equal ()
    | Int_t, Int_t -> return Eq
    | Int_t, _ -> not_equal ()
    | Nat_t, Nat_t -> return Eq
    | Nat_t, _ -> not_equal ()
    | Key_t, Key_t -> return Eq
    | Key_t, _ -> not_equal ()
    | Key_hash_t, Key_hash_t -> return Eq
    | Key_hash_t, _ -> not_equal ()
    | String_t, String_t -> return Eq
    | String_t, _ -> not_equal ()
    | Bytes_t, Bytes_t -> return Eq
    | Bytes_t, _ -> not_equal ()
    | Signature_t, Signature_t -> return Eq
    | Signature_t, _ -> not_equal ()
    | Mutez_t, Mutez_t -> return Eq
    | Mutez_t, _ -> not_equal ()
    | Timestamp_t, Timestamp_t -> return Eq
    | Timestamp_t, _ -> not_equal ()
    | Address_t, Address_t -> return Eq
    | Address_t, _ -> not_equal ()
    | Tx_rollup_l2_address_t, Tx_rollup_l2_address_t -> return Eq
    | Tx_rollup_l2_address_t, _ -> not_equal ()
    | Bool_t, Bool_t -> return Eq
    | Bool_t, _ -> not_equal ()
    | Chain_id_t, Chain_id_t -> return Eq
    | Chain_id_t, _ -> not_equal ()
    | Never_t, Never_t -> return Eq
    | Never_t, _ -> not_equal ()
    | Operation_t, Operation_t -> return Eq
    | Operation_t, _ -> not_equal ()
    | Bls12_381_g1_t, Bls12_381_g1_t -> return Eq
    | Bls12_381_g1_t, _ -> not_equal ()
    | Bls12_381_g2_t, Bls12_381_g2_t -> return Eq
    | Bls12_381_g2_t, _ -> not_equal ()
    | Bls12_381_fr_t, Bls12_381_fr_t -> return Eq
    | Bls12_381_fr_t, _ -> not_equal ()
    | Map_t (tal, tar, meta1), Map_t (tbl, tbr, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let* Eq = help tar tbr in
        let+ Eq = help tal tbl in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Map_t _, _ -> not_equal ()
    | Big_map_t (tal, tar, meta1), Big_map_t (tbl, tbr, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let* Eq = help tar tbr in
        let+ Eq = help tal tbl in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Big_map_t _, _ -> not_equal ()
    | Set_t (ea, meta1), Set_t (eb, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let+ Eq = help ea eb in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Set_t _, _ -> not_equal ()
    | Ticket_t (ea, meta1), Ticket_t (eb, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let+ Eq = help ea eb in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Ticket_t _, _ -> not_equal ()
    | Pair_t (tal, tar, meta1, cmp1), Pair_t (tbl, tbr, meta2, cmp2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let* Eq = help tal tbl in
        let+ Eq = help tar tbr in
        let Eq = Dependent_bool.merge_dand cmp1 cmp2 in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Pair_t _, _ -> not_equal ()
    | Or_t (tal, tar, meta1, cmp1), Or_t (tbl, tbr, meta2, cmp2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let* Eq = help tal tbl in
        let+ Eq = help tar tbr in
        let Eq = Dependent_bool.merge_dand cmp1 cmp2 in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Or_t _, _ -> not_equal ()
    | Lambda_t (tal, tar, meta1), Lambda_t (tbl, tbr, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let* Eq = help tal tbl in
        let+ Eq = help tar tbr in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Lambda_t _, _ -> not_equal ()
    | Contract_t (tal, meta1), Contract_t (tbl, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let+ Eq = help tal tbl in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Contract_t _, _ -> not_equal ()
    | Option_t (tva, meta1, _), Option_t (tvb, meta2, _) ->
        let* () = type_metadata_eq meta1 meta2 in
        let+ Eq = help tva tvb in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | Option_t _, _ -> not_equal ()
    | List_t (tva, meta1), List_t (tvb, meta2) ->
        let* () = type_metadata_eq meta1 meta2 in
        let+ Eq = help tva tvb in
        (Eq : ((ta, tac) ty, (tb, tbc) ty) eq)
    | List_t _, _ -> not_equal ()
    | Sapling_state_t ms1, Sapling_state_t ms2 ->
        let+ () = memo_size_eq ms1 ms2 in
        Eq
    | Sapling_state_t _, _ -> not_equal ()
    | Sapling_transaction_t ms1, Sapling_transaction_t ms2 ->
        let+ () = memo_size_eq ms1 ms2 in
        Eq
    | Sapling_transaction_t _, _ -> not_equal ()
    | Sapling_transaction_deprecated_t ms1, Sapling_transaction_deprecated_t ms2
      ->
        let+ () = memo_size_eq ms1 ms2 in
        Eq
    | Sapling_transaction_deprecated_t _, _ -> not_equal ()
    | Chest_t, Chest_t -> return Eq
    | Chest_t, _ -> not_equal ()
    | Chest_key_t, Chest_key_t -> return Eq
    | Chest_key_t, _ -> not_equal ()
  in
  help ty1 ty2

(* Same as ty_eq but for stacks.
   A single error monad is used here because there is no need to
   recover from stack merging errors. *)
let rec stack_eq :
    type ta tb ts tu.
    Script.location ->
    context ->
    int ->
    (ta, ts) stack_ty ->
    (tb, tu) stack_ty ->
    (((ta, ts) stack_ty, (tb, tu) stack_ty) eq * context) tzresult =
 fun loc ctxt lvl stack1 stack2 ->
  match (stack1, stack2) with
  | Bot_t, Bot_t -> ok (Eq, ctxt)
  | Item_t (ty1, rest1), Item_t (ty2, rest2) ->
      Gas_monad.run ctxt @@ ty_eq ~error_details:(Informative loc) ty1 ty2
      |> record_trace (Bad_stack_item lvl)
      >>? fun (eq, ctxt) ->
      eq >>? fun Eq ->
      stack_eq loc ctxt (lvl + 1) rest1 rest2 >|? fun (Eq, ctxt) ->
      ((Eq : ((ta, ts) stack_ty, (tb, tu) stack_ty) eq), ctxt)
  | _, _ -> error Bad_stack_length

(* ---- Type checker results -------------------------------------------------*)

type ('a, 's) judgement =
  | Typed : ('a, 's, 'b, 'u) descr -> ('a, 's) judgement
  | Failed : {
      descr : 'b 'u. ('b, 'u) stack_ty -> ('a, 's, 'b, 'u) descr;
    }
      -> ('a, 's) judgement

(* ---- Type checker (Untyped expressions -> Typed IR) ----------------------*)

type ('a, 's, 'b, 'u, 'c, 'v) branch = {
  branch :
    'r 'f.
    ('a, 's, 'r, 'f) descr -> ('b, 'u, 'r, 'f) descr -> ('c, 'v, 'r, 'f) descr;
}
[@@unboxed]

let merge_branches :
    type a s b u c v.
    context ->
    Script.location ->
    (a, s) judgement ->
    (b, u) judgement ->
    (a, s, b, u, c, v) branch ->
    ((c, v) judgement * context) tzresult =
 fun ctxt loc btr bfr {branch} ->
  match (btr, bfr) with
  | Typed ({aft = aftbt; _} as dbt), Typed ({aft = aftbf; _} as dbf) ->
      let unmatched_branches () =
        let aftbt = serialize_stack_for_error ctxt aftbt in
        let aftbf = serialize_stack_for_error ctxt aftbf in
        Unmatched_branches (loc, aftbt, aftbf)
      in
      record_trace_eval
        unmatched_branches
        ( stack_eq loc ctxt 1 aftbt aftbf >|? fun (Eq, ctxt) ->
          (Typed (branch dbt dbf), ctxt) )
  | Failed {descr = descrt}, Failed {descr = descrf} ->
      let descr ret = branch (descrt ret) (descrf ret) in
      ok (Failed {descr}, ctxt)
  | Typed dbt, Failed {descr = descrf} ->
      ok (Typed (branch dbt (descrf dbt.aft)), ctxt)
  | Failed {descr = descrt}, Typed dbf ->
      ok (Typed (branch (descrt dbf.aft) dbf), ctxt)

let parse_memo_size (n : (location, _) Micheline.node) :
    Sapling.Memo_size.t tzresult =
  match n with
  | Int (_, z) -> (
      match Sapling.Memo_size.parse_z z with
      | Ok _ as ok_memo_size -> ok_memo_size
      | Error msg ->
          error
          @@ Invalid_syntactic_constant (location n, strip_locations n, msg))
  | _ -> error @@ Invalid_kind (location n, [Int_kind], kind n)

type ex_comparable_ty =
  | Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty

type ex_parameter_ty_and_entrypoints_node =
  | Ex_parameter_ty_and_entrypoints_node : {
      arg_type : ('a, _) ty;
      entrypoints : 'a entrypoints_node;
    }
      -> ex_parameter_ty_and_entrypoints_node

(** [parse_ty] can be used to parse regular types as well as parameter types
    together with their entrypoints.

    In the first case, use [~ret:Don't_parse_entrypoints], [parse_ty] will
    return an [ex_ty].

    In the second case, use [~ret:Parse_entrypoints], [parse_ty] will return
    an [ex_parameter_ty_and_entrypoints_node].
*)
type ('ret, 'name) parse_ty_ret =
  | Don't_parse_entrypoints : (ex_ty, unit) parse_ty_ret
  | Parse_entrypoints
      : (ex_parameter_ty_and_entrypoints_node, Entrypoint.t option) parse_ty_ret

let rec parse_ty :
    type ret name.
    context ->
    stack_depth:int ->
    legacy:bool ->
    allow_lazy_storage:bool ->
    allow_operation:bool ->
    allow_contract:bool ->
    allow_ticket:bool ->
    ret:(ret, name) parse_ty_ret ->
    Script.node ->
    (ret * context) tzresult =
 fun ctxt
     ~stack_depth
     ~legacy
     ~allow_lazy_storage
     ~allow_operation
     ~allow_contract
     ~allow_ticket
     ~ret
     node ->
  Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
  if Compare.Int.(stack_depth > 10000) then
    error Typechecking_too_many_recursive_calls
  else
    (match ret with
    | Don't_parse_entrypoints -> ok (node, (() : name))
    | Parse_entrypoints -> extract_entrypoint_annot node)
    >>? fun (node, name) ->
    let return ctxt ty : ret * context =
      match ret with
      | Don't_parse_entrypoints -> (Ex_ty ty, ctxt)
      | Parse_entrypoints ->
          let at_node =
            Option.map (fun name -> {name; original_type_expr = node}) name
          in
          ( Ex_parameter_ty_and_entrypoints_node
              {
                arg_type = ty;
                entrypoints = {at_node; nested = Entrypoints_None};
              },
            ctxt )
    in
    match node with
    | Prim (loc, T_unit, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt unit_t
    | Prim (loc, T_int, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt int_t
    | Prim (loc, T_nat, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt nat_t
    | Prim (loc, T_string, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt string_t
    | Prim (loc, T_bytes, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt bytes_t
    | Prim (loc, T_mutez, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt mutez_t
    | Prim (loc, T_bool, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt bool_t
    | Prim (loc, T_key, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt key_t
    | Prim (loc, T_key_hash, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt key_hash_t
    | Prim (loc, T_chest_key, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt chest_key_t
    | Prim (loc, T_chest, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt chest_t
    | Prim (loc, T_timestamp, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt timestamp_t
    | Prim (loc, T_address, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt address_t
    | Prim (loc, T_tx_rollup_l2_address, [], annot) ->
        if Constants.tx_rollup_enable ctxt || legacy then
          check_type_annot loc annot >|? fun () ->
          return ctxt tx_rollup_l2_address_t
        else error @@ Tx_rollup_addresses_disabled loc
    | Prim (loc, T_signature, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt signature_t
    | Prim (loc, T_operation, [], annot) ->
        if allow_operation then
          check_type_annot loc annot >|? fun () -> return ctxt operation_t
        else error (Unexpected_operation loc)
    | Prim (loc, T_chain_id, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt chain_id_t
    | Prim (loc, T_never, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt never_t
    | Prim (loc, T_bls12_381_g1, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt bls12_381_g1_t
    | Prim (loc, T_bls12_381_g2, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt bls12_381_g2_t
    | Prim (loc, T_bls12_381_fr, [], annot) ->
        check_type_annot loc annot >|? fun () -> return ctxt bls12_381_fr_t
    | Prim (loc, T_contract, [utl], annot) ->
        if allow_contract then
          check_type_annot loc annot >>? fun () ->
          parse_passable_ty
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~legacy
            utl
            ~ret:Don't_parse_entrypoints
          >>? fun (Ex_ty tl, ctxt) ->
          contract_t loc tl >|? fun ty -> return ctxt ty
        else error (Unexpected_contract loc)
    | Prim (loc, T_pair, utl :: utr, annot) ->
        check_type_annot loc annot >>? fun () ->
        remove_field_annot utl >>? fun utl ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret:Don't_parse_entrypoints
          utl
        >>? fun (Ex_ty tl, ctxt) ->
        (match utr with
        | [utr] -> remove_field_annot utr
        | utr ->
            (* Unfold [pair t1 ... tn] as [pair t1 (... (pair tn-1 tn))] *)
            ok (Prim (loc, T_pair, utr, [])))
        >>? fun utr ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret:Don't_parse_entrypoints
          utr
        >>? fun (Ex_ty tr, ctxt) ->
        pair_t loc tl tr >|? fun (Ty_ex_c ty) -> return ctxt ty
    | Prim (loc, T_or, [utl; utr], annot) -> (
        check_type_annot loc annot >>? fun () ->
        (match ret with
        | Don't_parse_entrypoints ->
            remove_field_annot utl >>? fun utl ->
            remove_field_annot utr >|? fun utr -> (utl, utr)
        | Parse_entrypoints -> ok (utl, utr))
        >>? fun (utl, utr) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret
          utl
        >>? fun (parsed_l, ctxt) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret
          utr
        >>? fun (parsed_r, ctxt) ->
        match ret with
        | Don't_parse_entrypoints ->
            let (Ex_ty tl) = parsed_l in
            let (Ex_ty tr) = parsed_r in
            or_t loc tl tr >|? fun (Ty_ex_c ty) -> ((Ex_ty ty : ret), ctxt)
        | Parse_entrypoints ->
            let (Ex_parameter_ty_and_entrypoints_node
                  {arg_type = tl; entrypoints = left}) =
              parsed_l
            in
            let (Ex_parameter_ty_and_entrypoints_node
                  {arg_type = tr; entrypoints = right}) =
              parsed_r
            in
            or_t loc tl tr >|? fun (Ty_ex_c arg_type) ->
            let entrypoints =
              let at_node =
                Option.map (fun name -> {name; original_type_expr = node}) name
              in
              {at_node; nested = Entrypoints_Or {left; right}}
            in
            (Ex_parameter_ty_and_entrypoints_node {arg_type; entrypoints}, ctxt)
        )
    | Prim (loc, T_lambda, [uta; utr], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy uta
        >>? fun (Ex_ty ta, ctxt) ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy utr
        >>? fun (Ex_ty tr, ctxt) ->
        lambda_t loc ta tr >|? fun ty -> return ctxt ty
    | Prim (loc, T_option, [ut], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret:Don't_parse_entrypoints
          ut
        >>? fun (Ex_ty t, ctxt) ->
        option_t loc t >|? fun ty -> return ctxt ty
    | Prim (loc, T_list, [ut], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret:Don't_parse_entrypoints
          ut
        >>? fun (Ex_ty t, ctxt) ->
        list_t loc t >|? fun ty -> return ctxt ty
    | Prim (loc, T_ticket, [ut], annot) ->
        if allow_ticket then
          check_type_annot loc annot >>? fun () ->
          parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
          >>? fun (Ex_comparable_ty t, ctxt) ->
          ticket_t loc t >|? fun ty -> return ctxt ty
        else error (Unexpected_ticket loc)
    | Prim (loc, T_set, [ut], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
        >>? fun (Ex_comparable_ty t, ctxt) ->
        set_t loc t >|? fun ty -> return ctxt ty
    | Prim (loc, T_map, [uta; utr], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt uta
        >>? fun (Ex_comparable_ty ta, ctxt) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ~ret:Don't_parse_entrypoints
          utr
        >>? fun (Ex_ty tr, ctxt) ->
        map_t loc ta tr >|? fun ty -> return ctxt ty
    | Prim (loc, T_sapling_transaction, [memo_size], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_memo_size memo_size >|? fun memo_size ->
        return ctxt (sapling_transaction_t ~memo_size)
    | Prim (loc, T_sapling_transaction_deprecated, [memo_size], annot) ->
        if legacy then
          check_type_annot loc annot >>? fun () ->
          parse_memo_size memo_size >|? fun memo_size ->
          return ctxt (sapling_transaction_deprecated_t ~memo_size)
        else error (Deprecated_instruction T_sapling_transaction_deprecated)
    (*
    /!\ When adding new lazy storage kinds, be careful to use
    [when allow_lazy_storage] /!\
    Lazy storage should not be packable to avoid stealing a lazy storage
    from another contract with `PUSH t id` or `UNPACK`.
  *)
    | Prim (loc, T_big_map, args, annot) when allow_lazy_storage ->
        parse_big_map_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          loc
          args
          annot
        >|? fun (Ex_ty ty, ctxt) -> return ctxt ty
    | Prim (loc, T_sapling_state, [memo_size], annot) when allow_lazy_storage ->
        check_type_annot loc annot >>? fun () ->
        parse_memo_size memo_size >|? fun memo_size ->
        return ctxt (sapling_state_t ~memo_size)
    | Prim (loc, (T_big_map | T_sapling_state), _, _) ->
        error (Unexpected_lazy_storage loc)
    | Prim
        ( loc,
          (( T_unit | T_signature | T_int | T_nat | T_string | T_bytes | T_mutez
           | T_bool | T_key | T_key_hash | T_timestamp | T_address
           | T_tx_rollup_l2_address | T_chain_id | T_operation | T_never ) as
          prim),
          l,
          _ ) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim
        ( loc,
          ((T_set | T_list | T_option | T_contract | T_ticket) as prim),
          l,
          _ ) ->
        error (Invalid_arity (loc, prim, 1, List.length l))
    | Prim (loc, ((T_pair | T_or | T_map | T_lambda) as prim), l, _) ->
        error (Invalid_arity (loc, prim, 2, List.length l))
    | expr ->
        error
        @@ unexpected
             expr
             []
             Type_namespace
             [
               T_bls12_381_fr;
               T_bls12_381_g1;
               T_bls12_381_g2;
               T_bool;
               T_bytes;
               T_chain_id;
               T_contract;
               T_int;
               T_key;
               T_key_hash;
               T_lambda;
               T_list;
               T_map;
               T_mutez;
               T_nat;
               T_never;
               T_operation;
               T_option;
               T_or;
               T_pair;
               T_set;
               T_signature;
               T_string;
               T_ticket;
               T_timestamp;
               T_tx_rollup_l2_address;
               T_unit;
             ]

and parse_comparable_ty :
    context ->
    stack_depth:int ->
    Script.node ->
    (ex_comparable_ty * context) tzresult =
 fun ctxt ~stack_depth node ->
  parse_ty
    ~ret:Don't_parse_entrypoints
    ctxt
    ~stack_depth:(stack_depth + 1)
    ~legacy:false
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:false
    ~allow_ticket:false
    node
  >>? fun (Ex_ty t, ctxt) ->
  match is_comparable t with
  | Yes -> ok (Ex_comparable_ty t, ctxt)
  | No ->
      error
        (Comparable_type_expected (location node, Micheline.strip_locations node))

and parse_passable_ty :
    type ret name.
    context ->
    stack_depth:int ->
    legacy:bool ->
    ret:(ret, name) parse_ty_ret ->
    Script.node ->
    (ret * context) tzresult =
 fun ctxt ~stack_depth ~legacy ->
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:true
    ~allow_operation:false
    ~allow_contract:true
    ~allow_ticket:true

and parse_any_ty :
    context ->
    stack_depth:int ->
    legacy:bool ->
    Script.node ->
    (ex_ty * context) tzresult =
 fun ctxt ~stack_depth ~legacy ->
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:true
    ~allow_operation:true
    ~allow_contract:true
    ~allow_ticket:true
    ~ret:Don't_parse_entrypoints

and parse_big_map_ty ctxt ~stack_depth ~legacy big_map_loc args map_annot =
  Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
  match args with
  | [key_ty; value_ty] ->
      check_type_annot big_map_loc map_annot >>? fun () ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt key_ty
      >>? fun (Ex_comparable_ty key_ty, ctxt) ->
      parse_big_map_value_ty
        ctxt
        ~stack_depth:(stack_depth + 1)
        ~legacy
        value_ty
      >>? fun (Ex_ty value_ty, ctxt) ->
      big_map_t big_map_loc key_ty value_ty >|? fun big_map_ty ->
      (Ex_ty big_map_ty, ctxt)
  | args -> error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)

and parse_big_map_value_ty ctxt ~stack_depth ~legacy value_ty =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:legacy
    ~allow_ticket:true
    ~ret:Don't_parse_entrypoints
    value_ty

let parse_packable_ty ctxt ~stack_depth ~legacy node =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:legacy
      (* type contract is forbidden in UNPACK because of
         https://gitlab.com/tezos/tezos/-/issues/301 *)
    ~allow_ticket:false
    ~ret:Don't_parse_entrypoints
    node

let parse_view_input_ty ctxt ~stack_depth ~legacy node =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:true
    ~allow_ticket:false
    ~ret:Don't_parse_entrypoints
    node

let parse_view_output_ty ctxt ~stack_depth ~legacy node =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:true
    ~allow_ticket:false
    ~ret:Don't_parse_entrypoints
    node

let parse_normal_storage_ty ctxt ~stack_depth ~legacy node =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:true
    ~allow_operation:false
    ~allow_contract:legacy
    ~allow_ticket:true
    ~ret:Don't_parse_entrypoints
    node

let parse_storage_ty :
    context ->
    stack_depth:int ->
    legacy:bool ->
    Script.node ->
    (ex_ty * context) tzresult =
 fun ctxt ~stack_depth ~legacy node ->
  match node with
  | Prim
      ( loc,
        T_pair,
        [Prim (big_map_loc, T_big_map, args, map_annot); remaining_storage],
        storage_annot )
    when legacy -> (
      match storage_annot with
      | [] ->
          (parse_normal_storage_ty [@tailcall]) ctxt ~stack_depth ~legacy node
      | [single]
        when Compare.Int.(String.length single > 0)
             && Compare.Char.(single.[0] = '%') ->
          (parse_normal_storage_ty [@tailcall]) ctxt ~stack_depth ~legacy node
      | _ ->
          (* legacy semantics of big maps used the wrong annotation parser *)
          Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
          parse_big_map_ty
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~legacy
            big_map_loc
            args
            map_annot
          >>? fun (Ex_ty big_map_ty, ctxt) ->
          parse_normal_storage_ty
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~legacy
            remaining_storage
          >>? fun (Ex_ty remaining_storage, ctxt) ->
          check_composed_type_annot loc storage_annot >>? fun () ->
          pair_t loc big_map_ty remaining_storage >|? fun (Ty_ex_c ty) ->
          (Ex_ty ty, ctxt))
  | _ -> (parse_normal_storage_ty [@tailcall]) ctxt ~stack_depth ~legacy node

(* check_packable: determine if a `ty` is packable into Michelson *)
let check_packable ~legacy loc root =
  let rec check : type t tc. (t, tc) ty -> unit tzresult = function
    (* /!\ When adding new lazy storage kinds, be sure to return an error. /!\
       Lazy storage should not be packable. *)
    | Big_map_t _ -> error (Unexpected_lazy_storage loc)
    | Sapling_state_t _ -> error (Unexpected_lazy_storage loc)
    | Operation_t -> error (Unexpected_operation loc)
    | Unit_t -> Result.return_unit
    | Int_t -> Result.return_unit
    | Nat_t -> Result.return_unit
    | Signature_t -> Result.return_unit
    | String_t -> Result.return_unit
    | Bytes_t -> Result.return_unit
    | Mutez_t -> Result.return_unit
    | Key_hash_t -> Result.return_unit
    | Key_t -> Result.return_unit
    | Timestamp_t -> Result.return_unit
    | Address_t -> Result.return_unit
    | Tx_rollup_l2_address_t -> Result.return_unit
    | Bool_t -> Result.return_unit
    | Chain_id_t -> Result.return_unit
    | Never_t -> Result.return_unit
    | Set_t (_, _) -> Result.return_unit
    | Ticket_t _ -> error (Unexpected_ticket loc)
    | Lambda_t (_, _, _) -> Result.return_unit
    | Bls12_381_g1_t -> Result.return_unit
    | Bls12_381_g2_t -> Result.return_unit
    | Bls12_381_fr_t -> Result.return_unit
    | Pair_t (l_ty, r_ty, _, _) -> check l_ty >>? fun () -> check r_ty
    | Or_t (l_ty, r_ty, _, _) -> check l_ty >>? fun () -> check r_ty
    | Option_t (v_ty, _, _) -> check v_ty
    | List_t (elt_ty, _) -> check elt_ty
    | Map_t (_, elt_ty, _) -> check elt_ty
    | Contract_t (_, _) when legacy -> Result.return_unit
    | Contract_t (_, _) -> error (Unexpected_contract loc)
    | Sapling_transaction_t _ -> ok ()
    | Sapling_transaction_deprecated_t _ -> ok ()
    | Chest_key_t -> Result.return_unit
    | Chest_t -> Result.return_unit
  in
  check root

type toplevel = {
  code_field : Script.node;
  arg_type : Script.node;
  storage_type : Script.node;
  views : view_map;
}

type ('arg, 'storage) code =
  | Code : {
      code :
        (('arg, 'storage) pair, (operation Script_list.t, 'storage) pair) lambda;
      arg_type : ('arg, _) ty;
      storage_type : ('storage, _) ty;
      views : view_map;
      entrypoints : 'arg entrypoints;
      code_size : Cache_memory_helpers.sint;
    }
      -> ('arg, 'storage) code

type ex_script = Ex_script : ('a, 'c) Script_typed_ir.script -> ex_script

type ex_code = Ex_code : ('a, 'c) code -> ex_code

type 'storage typed_view =
  | Typed_view : {
      input_ty : ('input, _) ty;
      output_ty : ('output, _) ty;
      kinstr : ('input * 'storage, end_of_stack, 'output, end_of_stack) kinstr;
      original_code_expr : Script.node;
    }
      -> 'storage typed_view

type 'storage typed_view_map = (Script_string.t, 'storage typed_view) map

type (_, _) dig_proof_argument =
  | Dig_proof_argument :
      ('x, 'a * 's, 'a, 's, 'b, 't, 'c, 'u) stack_prefix_preservation_witness
      * ('x, _) ty
      * ('c, 'u) stack_ty
      -> ('b, 't) dig_proof_argument

type (_, _, _) dug_proof_argument =
  | Dug_proof_argument :
      (('a, 's, 'x, 'a * 's, 'b, 't, 'c, 'u) stack_prefix_preservation_witness
      * ('c, 'u) stack_ty)
      -> ('b, 't, 'x) dug_proof_argument

type (_, _) dipn_proof_argument =
  | Dipn_proof_argument :
      ('fa, 'fs, 'fb, 'fu, 'a, 's, 'b, 'u) stack_prefix_preservation_witness
      * context
      * ('fa, 'fs, 'fb, 'fu) descr
      * ('b, 'u) stack_ty
      -> ('a, 's) dipn_proof_argument

type (_, _) dropn_proof_argument =
  | Dropn_proof_argument :
      ('fa, 'fs, 'fa, 'fs, 'a, 's, 'a, 's) stack_prefix_preservation_witness
      * ('fa, 'fs) stack_ty
      -> ('a, 's) dropn_proof_argument

type (_, _, _) comb_proof_argument =
  | Comb_proof_argument :
      ('a, 'b, 's, 'c, 'd, 't) comb_gadt_witness * ('c, 'd * 't) stack_ty
      -> ('a, 'b, 's) comb_proof_argument

type (_, _, _) uncomb_proof_argument =
  | Uncomb_proof_argument :
      ('a, 'b, 's, 'c, 'd, 't) uncomb_gadt_witness * ('c, 'd * 't) stack_ty
      -> ('a, 'b, 's) uncomb_proof_argument

type 'before comb_get_proof_argument =
  | Comb_get_proof_argument :
      ('before, 'after) comb_get_gadt_witness * ('after, _) ty
      -> 'before comb_get_proof_argument

type ('rest, 'before) comb_set_proof_argument =
  | Comb_set_proof_argument :
      ('rest, 'before, 'after) comb_set_gadt_witness * ('after, _) ty
      -> ('rest, 'before) comb_set_proof_argument

type (_, _, _) dup_n_proof_argument =
  | Dup_n_proof_argument :
      ('a, 'b, 's, 't) dup_n_gadt_witness * ('t, _) ty
      -> ('a, 'b, 's) dup_n_proof_argument

let rec make_dug_proof_argument :
    type a s x xc.
    location ->
    int ->
    (x, xc) ty ->
    (a, s) stack_ty ->
    (a, s, x) dug_proof_argument option =
 fun loc n x stk ->
  match (n, stk) with
  | 0, rest -> Some (Dug_proof_argument (KRest, Item_t (x, rest)))
  | n, Item_t (v, rest) ->
      make_dug_proof_argument loc (n - 1) x rest
      |> Option.map @@ fun (Dug_proof_argument (n', aft')) ->
         Dug_proof_argument (KPrefix (loc, v, n'), Item_t (v, aft'))
  | _, _ -> None

let rec make_comb_get_proof_argument :
    type b bc. int -> (b, bc) ty -> b comb_get_proof_argument option =
 fun n ty ->
  match (n, ty) with
  | 0, value_ty -> Some (Comb_get_proof_argument (Comb_get_zero, value_ty))
  | 1, Pair_t (hd_ty, _, _annot, _) ->
      Some (Comb_get_proof_argument (Comb_get_one, hd_ty))
  | n, Pair_t (_, tl_ty, _annot, _) ->
      make_comb_get_proof_argument (n - 2) tl_ty
      |> Option.map
         @@ fun (Comb_get_proof_argument (comb_get_left_witness, ty')) ->
         Comb_get_proof_argument (Comb_get_plus_two comb_get_left_witness, ty')
  | _ -> None

let rec make_comb_set_proof_argument :
    type value valuec before beforec a s.
    context ->
    (a, s) stack_ty ->
    location ->
    int ->
    (value, valuec) ty ->
    (before, beforec) ty ->
    (value, before) comb_set_proof_argument tzresult =
 fun ctxt stack_ty loc n value_ty ty ->
  match (n, ty) with
  | 0, _ -> ok @@ Comb_set_proof_argument (Comb_set_zero, value_ty)
  | 1, Pair_t (_hd_ty, tl_ty, _, _) ->
      pair_t loc value_ty tl_ty >|? fun (Ty_ex_c after_ty) ->
      Comb_set_proof_argument (Comb_set_one, after_ty)
  | n, Pair_t (hd_ty, tl_ty, _, _) ->
      make_comb_set_proof_argument ctxt stack_ty loc (n - 2) value_ty tl_ty
      >>? fun (Comb_set_proof_argument (comb_set_left_witness, tl_ty')) ->
      pair_t loc hd_ty tl_ty' >|? fun (Ty_ex_c after_ty) ->
      Comb_set_proof_argument (Comb_set_plus_two comb_set_left_witness, after_ty)
  | _ ->
      let whole_stack = serialize_stack_for_error ctxt stack_ty in
      error (Bad_stack (loc, I_UPDATE, 2, whole_stack))

type 'a ex_ty_cstr =
  | Ex_ty_cstr : {
      ty : ('b, _) Script_typed_ir.ty;
      construct : 'b -> 'a;
      original_type_expr : Script.node;
    }
      -> 'a ex_ty_cstr

let find_entrypoint (type full fullc error_context error_trace)
    ~(error_details : (error_context, error_trace) error_details)
    (full : (full, fullc) ty) (entrypoints : full entrypoints) entrypoint :
    (full ex_ty_cstr, error_trace) Gas_monad.t =
  let open Gas_monad.Syntax in
  let rec find_entrypoint :
      type t tc.
      (t, tc) ty ->
      t entrypoints_node ->
      Entrypoint.t ->
      (t ex_ty_cstr, unit) Gas_monad.t =
   fun ty entrypoints entrypoint ->
    let* () = Gas_monad.consume_gas Typecheck_costs.find_entrypoint_cycle in
    match (ty, entrypoints) with
    | _, {at_node = Some {name; original_type_expr}; _}
      when Entrypoint.(name = entrypoint) ->
        return (Ex_ty_cstr {ty; construct = (fun e -> e); original_type_expr})
    | Or_t (tl, tr, _, _), {nested = Entrypoints_Or {left; right}; _} -> (
        Gas_monad.bind_recover (find_entrypoint tl left entrypoint) @@ function
        | Ok (Ex_ty_cstr {ty; construct; original_type_expr}) ->
            return
              (Ex_ty_cstr
                 {
                   ty;
                   construct = (fun e -> L (construct e));
                   original_type_expr;
                 })
        | Error () ->
            let+ (Ex_ty_cstr {ty; construct; original_type_expr}) =
              find_entrypoint tr right entrypoint
            in
            Ex_ty_cstr
              {ty; construct = (fun e -> R (construct e)); original_type_expr})
    | _, {nested = Entrypoints_None; _} -> Gas_monad.of_result (Error ())
  in
  let {root; original_type_expr} = entrypoints in
  Gas_monad.bind_recover (find_entrypoint full root entrypoint) @@ function
  | Ok f_t -> return f_t
  | Error () ->
      if Entrypoint.is_default entrypoint then
        return
          (Ex_ty_cstr {ty = full; construct = (fun e -> e); original_type_expr})
      else
        Gas_monad.of_result
        @@ Error
             (match error_details with
             | Fast -> (Inconsistent_types_fast : error_trace)
             | Informative _ -> trace_of_error @@ No_such_entrypoint entrypoint)

let find_entrypoint_for_type (type full fullc exp expc error_trace)
    ~error_details ~(full : (full, fullc) ty) ~(expected : (exp, expc) ty)
    entrypoints entrypoint :
    (Entrypoint.t * (exp, expc) ty, error_trace) Gas_monad.t =
  let open Gas_monad.Syntax in
  let* res = find_entrypoint ~error_details full entrypoints entrypoint in
  match res with
  | Ex_ty_cstr {ty; _} -> (
      match entrypoints.root.at_node with
      | Some {name; original_type_expr = _}
        when Entrypoint.is_root name && Entrypoint.is_default entrypoint ->
          Gas_monad.bind_recover
            (ty_eq ~error_details:Fast ty expected)
            (function
              | Ok Eq -> return (Entrypoint.default, (ty : (exp, expc) ty))
              | Error Inconsistent_types_fast ->
                  let+ Eq = ty_eq ~error_details full expected in
                  (Entrypoint.root, (full : (exp, expc) ty)))
      | _ ->
          let+ Eq = ty_eq ~error_details ty expected in
          (entrypoint, (ty : (exp, expc) ty)))

let well_formed_entrypoints (type full fullc) (full : (full, fullc) ty)
    entrypoints =
  let merge path (type t tc) (ty : (t, tc) ty)
      (entrypoints : t entrypoints_node) reachable
      ((first_unreachable, all) as acc) =
    match entrypoints.at_node with
    | None ->
        ok
          ( (if reachable then acc
            else
              match ty with
              | Or_t _ -> acc
              | _ -> (
                  match first_unreachable with
                  | None -> (Some (List.rev path), all)
                  | Some _ -> acc)),
            reachable )
    | Some {name; original_type_expr = _} ->
        if Entrypoint.Set.mem name all then error (Duplicate_entrypoint name)
        else ok ((first_unreachable, Entrypoint.Set.add name all), true)
  in
  let rec check :
      type t tc.
      (t, tc) ty ->
      t entrypoints_node ->
      prim list ->
      bool ->
      prim list option * Entrypoint.Set.t ->
      (prim list option * Entrypoint.Set.t) tzresult =
   fun t entrypoints path reachable acc ->
    match (t, entrypoints) with
    | Or_t (tl, tr, _, _), {nested = Entrypoints_Or {left; right}; _} ->
        merge (D_Left :: path) tl left reachable acc
        >>? fun (acc, l_reachable) ->
        merge (D_Right :: path) tr right reachable acc
        >>? fun (acc, r_reachable) ->
        check tl left (D_Left :: path) l_reachable acc >>? fun acc ->
        check tr right (D_Right :: path) r_reachable acc
    | _ -> ok acc
  in
  let init, reachable =
    match entrypoints.at_node with
    | None -> (Entrypoint.Set.empty, false)
    | Some {name; original_type_expr = _} ->
        (Entrypoint.Set.singleton name, true)
  in
  check full entrypoints [] reachable (None, init)
  >>? fun (first_unreachable, all) ->
  if not (Entrypoint.Set.mem Entrypoint.default all) then Result.return_unit
  else
    match first_unreachable with
    | None -> Result.return_unit
    | Some path -> error (Unreachable_entrypoint path)

type ex_parameter_ty_and_entrypoints =
  | Ex_parameter_ty_and_entrypoints : {
      arg_type : ('a, _) ty;
      entrypoints : 'a entrypoints;
    }
      -> ex_parameter_ty_and_entrypoints

let parse_parameter_ty_and_entrypoints :
    context ->
    stack_depth:int ->
    legacy:bool ->
    Script.node ->
    (ex_parameter_ty_and_entrypoints * context) tzresult =
 fun ctxt ~stack_depth ~legacy node ->
  parse_passable_ty
    ctxt
    ~stack_depth:(stack_depth + 1)
    ~legacy
    node
    ~ret:Parse_entrypoints
  >>? fun (Ex_parameter_ty_and_entrypoints_node {arg_type; entrypoints}, ctxt)
    ->
  (if legacy then Result.return_unit
  else well_formed_entrypoints arg_type entrypoints)
  >|? fun () ->
  let entrypoints = {root = entrypoints; original_type_expr = node} in
  (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, ctxt)

let parse_passable_ty = parse_passable_ty ~ret:Don't_parse_entrypoints

let parse_uint ~nb_bits =
  assert (Compare.Int.(nb_bits >= 0 && nb_bits <= 30)) ;
  let max_int = (1 lsl nb_bits) - 1 in
  let max_z = Z.of_int max_int in
  function
  | Micheline.Int (_, n) when Compare.Z.(Z.zero <= n) && Compare.Z.(n <= max_z)
    ->
      ok (Z.to_int n)
  | node ->
      error
      @@ Invalid_syntactic_constant
           ( location node,
             strip_locations node,
             "a positive " ^ string_of_int nb_bits
             ^ "-bit integer (between 0 and " ^ string_of_int max_int ^ ")" )

let parse_uint10 = parse_uint ~nb_bits:10

let parse_uint11 = parse_uint ~nb_bits:11

(* The type returned by this function is used to:
   - serialize and deserialize tickets when they are stored or transferred,
   - type the READ_TICKET instruction. *)
let opened_ticket_type loc ty = comparable_pair_3_t loc address_t ty nat_t

(* -- parse data of primitive types -- *)

let parse_unit ctxt ~legacy = function
  | Prim (loc, D_Unit, [], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.unit >|? fun ctxt -> ((), ctxt)
  | Prim (loc, D_Unit, l, _) ->
      error @@ Invalid_arity (loc, D_Unit, 0, List.length l)
  | expr -> error @@ unexpected expr [] Constant_namespace [D_Unit]

let parse_bool ctxt ~legacy = function
  | Prim (loc, D_True, [], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.bool >|? fun ctxt -> (true, ctxt)
  | Prim (loc, D_False, [], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.bool >|? fun ctxt -> (false, ctxt)
  | Prim (loc, ((D_True | D_False) as c), l, _) ->
      error @@ Invalid_arity (loc, c, 0, List.length l)
  | expr -> error @@ unexpected expr [] Constant_namespace [D_True; D_False]

let parse_string ctxt : Script.node -> (Script_string.t * context) tzresult =
  function
  | String (loc, v) as expr ->
      Gas.consume ctxt (Typecheck_costs.check_printable v) >>? fun ctxt ->
      record_trace
        (Invalid_syntactic_constant
           (loc, strip_locations expr, "a printable ascii string"))
        (Script_string.of_string v >|? fun s -> (s, ctxt))
  | expr -> error @@ Invalid_kind (location expr, [String_kind], kind expr)

let parse_bytes ctxt = function
  | Bytes (_, v) -> ok (v, ctxt)
  | expr -> error @@ Invalid_kind (location expr, [Bytes_kind], kind expr)

let parse_int ctxt = function
  | Int (_, v) -> ok (Script_int.of_zint v, ctxt)
  | expr -> error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_nat ctxt :
    Script.node -> (Script_int.n Script_int.num * context) tzresult = function
  | Int (loc, v) as expr -> (
      let v = Script_int.of_zint v in
      match Script_int.is_nat v with
      | Some nat -> ok (nat, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a non-negative integer"))
  | expr -> error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_mutez ctxt : Script.node -> (Tez.t * context) tzresult = function
  | Int (loc, v) as expr -> (
      match
        let open Option in
        bind (catch (fun () -> Z.to_int64 v)) Tez.of_mutez
      with
      | Some tez -> Ok (tez, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid mutez amount"))
  | expr -> error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_timestamp ctxt :
    Script.node -> (Script_timestamp.t * context) tzresult = function
  | Int (_, v) (* As unparsed with [Optimized] or out of bounds [Readable]. *)
    ->
      ok (Script_timestamp.of_zint v, ctxt)
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt (Typecheck_costs.timestamp_readable s) >>? fun ctxt ->
      match Script_timestamp.of_string s with
      | Some v -> ok (v, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid timestamp"))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Int_kind], kind expr)

let parse_key ctxt : Script.node -> (public_key * context) tzresult = function
  | Bytes (loc, bytes) as expr -> (
      (* As unparsed with [Optimized]. *)
      Gas.consume ctxt Typecheck_costs.public_key_optimized
      >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt Signature.Public_key.encoding bytes
      with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid public key"))
  | String (loc, s) as expr -> (
      (* As unparsed with [Readable]. *)
      Gas.consume ctxt Typecheck_costs.public_key_readable
      >>? fun ctxt ->
      match Signature.Public_key.of_b58check_opt s with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid public key"))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_key_hash ctxt : Script.node -> (public_key_hash * context) tzresult =
  function
  | Bytes (loc, bytes) as expr -> (
      (* As unparsed with [Optimized]. *)
      Gas.consume ctxt Typecheck_costs.key_hash_optimized
      >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt
          Signature.Public_key_hash.encoding
          bytes
      with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid key hash"))
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.key_hash_readable >>? fun ctxt ->
      match Signature.Public_key_hash.of_b58check_opt s with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid key hash"))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_signature ctxt : Script.node -> (signature * context) tzresult =
  function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.signature_optimized >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt Script_signature.encoding bytes
      with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid signature"))
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.signature_readable >>? fun ctxt ->
      match Script_signature.of_b58check_opt s with
      | Some s -> ok (s, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid signature"))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_chain_id ctxt : Script.node -> (Script_chain_id.t * context) tzresult
    = function
  | Bytes (loc, bytes) as expr -> (
      Gas.consume ctxt Typecheck_costs.chain_id_optimized >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt Script_chain_id.encoding bytes
      with
      | Some k -> ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid chain id"))
  | String (loc, s) as expr -> (
      Gas.consume ctxt Typecheck_costs.chain_id_readable >>? fun ctxt ->
      match Script_chain_id.of_b58check_opt s with
      | Some s -> ok (s, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid chain id"))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_address ctxt : Script.node -> (address * context) tzresult =
  let destination_allowed loc {destination; entrypoint} ctxt =
    match destination with
    | Destination.Sc_rollup _ when not (Constants.sc_rollup_enable ctxt) ->
        error @@ Sc_rollup_disabled loc
    | Destination.Zk_rollup _ when not (Constants.zk_rollup_enable ctxt) ->
        error @@ Zk_rollup_disabled loc
    | _ -> Ok ({destination; entrypoint}, ctxt)
  in
  function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.contract_optimized >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt
          Data_encoding.(tup2 Destination.encoding Entrypoint.value_encoding)
          bytes
      with
      | Some (destination, entrypoint) ->
          destination_allowed loc {destination; entrypoint} ctxt
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid address"))
  | String (loc, s) (* As unparsed with [Readable]. *) ->
      Gas.consume ctxt Typecheck_costs.contract_readable >>? fun ctxt ->
      (match String.index_opt s '%' with
      | None -> ok (s, Entrypoint.default)
      | Some pos ->
          let len = String.length s - pos - 1 in
          let name = String.sub s (pos + 1) len in
          Entrypoint.of_string_strict ~loc name >|? fun entrypoint ->
          (String.sub s 0 pos, entrypoint))
      >>? fun (addr, entrypoint) ->
      Destination.of_b58check addr >>? fun destination ->
      destination_allowed loc {destination; entrypoint} ctxt
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_tx_rollup_l2_address ctxt :
    Script.node -> (tx_rollup_l2_address * context) tzresult = function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.tx_rollup_l2_address >>? fun ctxt ->
      match Tx_rollup_l2_address.of_bytes_opt bytes with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               ( loc,
                 strip_locations expr,
                 "a valid transaction rollup L2 address" ))
  | String (loc, str) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.tx_rollup_l2_address >>? fun ctxt ->
      match Tx_rollup_l2_address.of_b58check_opt str with
      | Some txa -> ok (Tx_rollup_l2_address.Indexable.value txa, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               ( loc,
                 strip_locations expr,
                 "a valid transaction rollup L2 address" ))
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_never expr : (never * context) tzresult =
  error @@ Invalid_never_expr (location expr)

(* -- parse data of complex types -- *)

let parse_pair (type r) parse_l parse_r ctxt ~legacy
    (r_comb_witness : (r, unit -> _) comb_witness) expr =
  let parse_comb loc l rs =
    parse_l ctxt l >>=? fun (l, ctxt) ->
    (match (rs, r_comb_witness) with
    | [r], _ -> ok r
    | [], _ -> error @@ Invalid_arity (loc, D_Pair, 2, 1)
    | _ :: _, Comb_Pair _ ->
        (* Unfold [Pair x1 ... xn] as [Pair x1 (Pair x2 ... xn-1 xn))]
           for type [pair ta (pair tb1 tb2)] and n >= 3 only *)
        ok (Prim (loc, D_Pair, rs, []))
    | _ -> error @@ Invalid_arity (loc, D_Pair, 2, 1 + List.length rs))
    >>?= fun r ->
    parse_r ctxt r >|=? fun (r, ctxt) -> ((l, r), ctxt)
  in
  match expr with
  | Prim (loc, D_Pair, l :: rs, annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () -> parse_comb loc l rs
  | Prim (loc, D_Pair, l, _) ->
      tzfail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
  (* Unfold [{x1; ...; xn}] as [Pair x1 x2 ... xn-1 xn] for n >= 2 *)
  | Seq (loc, l :: (_ :: _ as rs)) -> parse_comb loc l rs
  | Seq (loc, l) -> tzfail @@ Invalid_seq_arity (loc, 2, List.length l)
  | expr -> tzfail @@ unexpected expr [] Constant_namespace [D_Pair]

let parse_or parse_l parse_r ctxt ~legacy = function
  | Prim (loc, D_Left, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_l ctxt v >|=? fun (v, ctxt) -> (L v, ctxt)
  | Prim (loc, D_Left, l, _) ->
      tzfail @@ Invalid_arity (loc, D_Left, 1, List.length l)
  | Prim (loc, D_Right, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_r ctxt v >|=? fun (v, ctxt) -> (R v, ctxt)
  | Prim (loc, D_Right, l, _) ->
      tzfail @@ Invalid_arity (loc, D_Right, 1, List.length l)
  | expr -> tzfail @@ unexpected expr [] Constant_namespace [D_Left; D_Right]

let parse_option parse_v ctxt ~legacy = function
  | Prim (loc, D_Some, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_v ctxt v >|=? fun (v, ctxt) -> (Some v, ctxt)
  | Prim (loc, D_Some, l, _) ->
      tzfail @@ Invalid_arity (loc, D_Some, 1, List.length l)
  | Prim (loc, D_None, [], annot) ->
      Lwt.return
        ( (if legacy then Result.return_unit
          else error_unexpected_annot loc annot)
        >|? fun () -> (None, ctxt) )
  | Prim (loc, D_None, l, _) ->
      tzfail @@ Invalid_arity (loc, D_None, 0, List.length l)
  | expr -> tzfail @@ unexpected expr [] Constant_namespace [D_Some; D_None]

let comb_witness1 : type t tc. (t, tc) ty -> (t, unit -> unit) comb_witness =
  function
  | Pair_t _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

let parse_view_name ctxt : Script.node -> (Script_string.t * context) tzresult =
  function
  | String (loc, v) as expr ->
      (* The limitation of length of string is same as entrypoint *)
      if Compare.Int.(String.length v > 31) then error (View_name_too_long v)
      else
        let rec check_char i =
          if Compare.Int.(i < 0) then ok v
          else if Script_ir_annot.is_allowed_char v.[i] then check_char (i - 1)
          else error (Bad_view_name loc)
        in
        Gas.consume ctxt (Typecheck_costs.check_printable v) >>? fun ctxt ->
        record_trace
          (Invalid_syntactic_constant
             ( loc,
               strip_locations expr,
               "string [a-zA-Z0-9_.%@] and the maximum string length of 31 \
                characters" ))
          ( check_char (String.length v - 1) >>? fun v ->
            Script_string.of_string v >|? fun s -> (s, ctxt) )
  | expr -> error @@ Invalid_kind (location expr, [String_kind], kind expr)

let parse_toplevel :
    context -> legacy:bool -> Script.expr -> (toplevel * context) tzresult =
 fun ctxt ~legacy:_ toplevel ->
  record_trace (Ill_typed_contract (toplevel, []))
  @@
  match root toplevel with
  | Int (loc, _) -> error (Invalid_kind (loc, [Seq_kind], Int_kind))
  | String (loc, _) -> error (Invalid_kind (loc, [Seq_kind], String_kind))
  | Bytes (loc, _) -> error (Invalid_kind (loc, [Seq_kind], Bytes_kind))
  | Prim (loc, _, _, _) -> error (Invalid_kind (loc, [Seq_kind], Prim_kind))
  | Seq (_, fields) -> (
      let rec find_fields ctxt p s c views fields =
        match fields with
        | [] -> ok (ctxt, (p, s, c, views))
        | Int (loc, _) :: _ -> error (Invalid_kind (loc, [Prim_kind], Int_kind))
        | String (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], String_kind))
        | Bytes (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], Bytes_kind))
        | Seq (loc, _) :: _ -> error (Invalid_kind (loc, [Prim_kind], Seq_kind))
        | Prim (loc, K_parameter, [arg], annot) :: rest -> (
            match p with
            | None -> find_fields ctxt (Some (arg, loc, annot)) s c views rest
            | Some _ -> error (Duplicate_field (loc, K_parameter)))
        | Prim (loc, K_storage, [arg], annot) :: rest -> (
            match s with
            | None -> find_fields ctxt p (Some (arg, loc, annot)) c views rest
            | Some _ -> error (Duplicate_field (loc, K_storage)))
        | Prim (loc, K_code, [arg], annot) :: rest -> (
            match c with
            | None -> find_fields ctxt p s (Some (arg, loc, annot)) views rest
            | Some _ -> error (Duplicate_field (loc, K_code)))
        | Prim (loc, ((K_parameter | K_storage | K_code) as name), args, _) :: _
          ->
            error (Invalid_arity (loc, name, 1, List.length args))
        | Prim (loc, K_view, [name; input_ty; output_ty; view_code], _) :: rest
          ->
            parse_view_name ctxt name >>? fun (str, ctxt) ->
            Gas.consume
              ctxt
              (Michelson_v1_gas.Cost_of.Interpreter.view_update str views)
            >>? fun ctxt ->
            if Script_map.mem str views then error (Duplicated_view_name loc)
            else
              let views' =
                Script_map.update
                  str
                  (Some {input_ty; output_ty; view_code})
                  views
              in
              find_fields ctxt p s c views' rest
        | Prim (loc, K_view, args, _) :: _ ->
            error (Invalid_arity (loc, K_view, 4, List.length args))
        | Prim (loc, name, _, _) :: _ ->
            let allowed = [K_parameter; K_storage; K_code; K_view] in
            error (Invalid_primitive (loc, allowed, name))
      in
      find_fields ctxt None None None (Script_map.empty string_t) fields
      >>? fun (ctxt, toplevel) ->
      match toplevel with
      | None, _, _, _ -> error (Missing_field K_parameter)
      | Some _, None, _, _ -> error (Missing_field K_storage)
      | Some _, Some _, None, _ -> error (Missing_field K_code)
      | ( Some (p, ploc, pannot),
          Some (s, sloc, sannot),
          Some (c, cloc, cannot),
          views ) ->
          Script_ir_annot.error_unexpected_annot ploc pannot >>? fun () ->
          Script_ir_annot.error_unexpected_annot cloc cannot >>? fun () ->
          Script_ir_annot.error_unexpected_annot sloc sannot >|? fun () ->
          ({code_field = c; arg_type = p; views; storage_type = s}, ctxt))

(* Normalize lambdas during parsing *)

let normalized_lam ~unparse_code_rec ~stack_depth ctxt kdescr code_field =
  unparse_code_rec ctxt ~stack_depth:(stack_depth + 1) Optimized code_field
  >|=? fun (code_field, ctxt) -> (Lam (kdescr, code_field), ctxt)

let normalized_lam_rec ~unparse_code_rec ~stack_depth ctxt kdescr code_field =
  unparse_code_rec ctxt ~stack_depth:(stack_depth + 1) Optimized code_field
  >|=? fun (code_field, ctxt) -> (LamRec (kdescr, code_field), ctxt)

(* -- parse data of any type -- *)

(*
  Some values, such as operations, tickets, or big map ids, are used only
  internally and are not allowed to be forged by users.
  In [parse_data], [allow_forged] should be [false] for:
  - PUSH
  - UNPACK
  - user-provided script parameters
  - storage on origination
  And [true] for:
  - internal calls parameters
  - storage after origination
*)

let rec parse_data :
    type a ac.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    stack_depth:int ->
    context ->
    allow_forged:bool ->
    (a, ac) ty ->
    Script.node ->
    (a * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~elab_conf ~stack_depth ctxt ~allow_forged ty script_data ->
  Gas.consume ctxt Typecheck_costs.parse_data_cycle >>?= fun ctxt ->
  let non_terminal_recursion ctxt ty script_data =
    if Compare.Int.(stack_depth > 10_000) then
      tzfail Typechecking_too_many_recursive_calls
    else
      parse_data
        ~unparse_code_rec
        ~elab_conf
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~allow_forged
        ty
        script_data
  in
  let parse_data_error () =
    let ty = serialize_ty_for_error ty in
    Invalid_constant (location script_data, strip_locations script_data, ty)
  in
  let fail_parse_data () = tzfail (parse_data_error ()) in
  let traced_no_lwt body = record_trace_eval parse_data_error body in
  let traced body = trace_eval parse_data_error body in
  let traced_fail err = Lwt.return @@ traced_no_lwt (error err) in
  let parse_items ctxt expr key_type value_type items item_wrapper =
    List.fold_left_es
      (fun (last_value, map, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if elab_conf.legacy then Result.return_unit
            else error_unexpected_annot loc annot)
            >>?= fun () ->
            non_terminal_recursion ctxt key_type k >>=? fun (k, ctxt) ->
            non_terminal_recursion ctxt value_type v >>=? fun (v, ctxt) ->
            Lwt.return
              ( (match last_value with
                | Some value ->
                    Gas.consume
                      ctxt
                      (Michelson_v1_gas.Cost_of.Interpreter.compare
                         key_type
                         value
                         k)
                    >>? fun ctxt ->
                    let c =
                      Script_comparable.compare_comparable key_type value k
                    in
                    if Compare.Int.(0 <= c) then
                      if Compare.Int.(0 = c) then
                        error (Duplicate_map_keys (loc, strip_locations expr))
                      else
                        error (Unordered_map_keys (loc, strip_locations expr))
                    else ok ctxt
                | None -> ok ctxt)
              >>? fun ctxt ->
                Gas.consume
                  ctxt
                  (Michelson_v1_gas.Cost_of.Interpreter.map_update k map)
                >|? fun ctxt ->
                (Some k, Script_map.update k (Some (item_wrapper v)) map, ctxt)
              )
        | Prim (loc, D_Elt, l, _) ->
            tzfail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            tzfail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ -> fail_parse_data ())
      (None, Script_map.empty key_type, ctxt)
      items
    |> traced
    >|=? fun (_, items, ctxt) -> (items, ctxt)
  in
  let parse_big_map_items (type t) ctxt expr (key_type : t comparable_ty)
      value_type items item_wrapper =
    List.fold_left_es
      (fun (last_key, {map; size}, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if elab_conf.legacy then Result.return_unit
            else error_unexpected_annot loc annot)
            >>?= fun () ->
            non_terminal_recursion ctxt key_type k >>=? fun (k, ctxt) ->
            hash_comparable_data ctxt key_type k >>=? fun (key_hash, ctxt) ->
            non_terminal_recursion ctxt value_type v >>=? fun (v, ctxt) ->
            Lwt.return
              ( (match last_key with
                | Some last_key ->
                    Gas.consume
                      ctxt
                      (Michelson_v1_gas.Cost_of.Interpreter.compare
                         key_type
                         last_key
                         k)
                    >>? fun ctxt ->
                    let c =
                      Script_comparable.compare_comparable key_type last_key k
                    in
                    if Compare.Int.(0 <= c) then
                      if Compare.Int.(0 = c) then
                        error (Duplicate_map_keys (loc, strip_locations expr))
                      else
                        error (Unordered_map_keys (loc, strip_locations expr))
                    else ok ctxt
                | None -> ok ctxt)
              >>? fun ctxt ->
                Gas.consume
                  ctxt
                  (Michelson_v1_gas.Cost_of.Interpreter.big_map_update
                     {map; size})
                >>? fun ctxt ->
                if Big_map_overlay.mem key_hash map then
                  error (Duplicate_map_keys (loc, strip_locations expr))
                else
                  ok
                    ( Some k,
                      {
                        map =
                          Big_map_overlay.add key_hash (k, item_wrapper v) map;
                        size = size + 1;
                      },
                      ctxt ) )
        | Prim (loc, D_Elt, l, _) ->
            tzfail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            tzfail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ -> fail_parse_data ())
      (None, {map = Big_map_overlay.empty; size = 0}, ctxt)
      items
    |> traced
    >|=? fun (_, map, ctxt) -> (map, ctxt)
  in
  let legacy = elab_conf.legacy in
  match (ty, script_data) with
  | Unit_t, expr ->
      Lwt.return @@ traced_no_lwt
      @@ (parse_unit ctxt ~legacy expr : (a * context) tzresult)
  | Bool_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_bool ctxt ~legacy expr
  | String_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_string ctxt expr
  | Bytes_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_bytes ctxt expr
  | Int_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_int ctxt expr
  | Nat_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_nat ctxt expr
  | Mutez_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_mutez ctxt expr
  | Timestamp_t, expr ->
      Lwt.return @@ traced_no_lwt @@ parse_timestamp ctxt expr
  | Key_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_key ctxt expr
  | Key_hash_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_key_hash ctxt expr
  | Signature_t, expr ->
      Lwt.return @@ traced_no_lwt @@ parse_signature ctxt expr
  | Operation_t, _ ->
      (* operations cannot appear in parameters or storage,
          the protocol should never parse the bytes of an operation *)
      assert false
  | Chain_id_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_chain_id ctxt expr
  | Address_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_address ctxt expr
  | Tx_rollup_l2_address_t, expr ->
      if Constants.tx_rollup_enable ctxt || legacy then
        Lwt.return @@ traced_no_lwt @@ parse_tx_rollup_l2_address ctxt expr
      else tzfail (Deprecated_instruction T_tx_rollup_l2_address)
  | Contract_t (arg_ty, _), expr ->
      traced
        ( parse_address ctxt expr >>?= fun (address, ctxt) ->
          let loc = location expr in
          parse_contract_data
            ~stack_depth:(stack_depth + 1)
            ctxt
            loc
            arg_ty
            address.destination
            ~entrypoint:address.entrypoint
          >|=? fun (ctxt, typed_contract) -> (typed_contract, ctxt) )
  (* Pairs *)
  | Pair_t (tl, tr, _, _), expr ->
      let r_witness = comb_witness1 tr in
      let parse_l ctxt v = non_terminal_recursion ctxt tl v in
      let parse_r ctxt v = non_terminal_recursion ctxt tr v in
      traced @@ parse_pair parse_l parse_r ctxt ~legacy r_witness expr
  (* Ors *)
  | Or_t (tl, tr, _, _), expr ->
      let parse_l ctxt v = non_terminal_recursion ctxt tl v in
      let parse_r ctxt v = non_terminal_recursion ctxt tr v in
      traced @@ parse_or parse_l parse_r ctxt ~legacy expr
  (* Lambdas *)
  | Lambda_t (ta, tr, _ty_name), (Seq (_loc, _) as script_instr) ->
      traced
      @@ parse_kdescr
           ~unparse_code_rec
           Tc_context.data
           ~elab_conf
           ~stack_depth:(stack_depth + 1)
           ctxt
           ta
           tr
           script_instr
      >>=? fun (kdescr, ctxt) ->
      (normalized_lam [@ocaml.tailcall])
        ~unparse_code_rec
        ctxt
        ~stack_depth
        kdescr
        script_instr
  | ( Lambda_t (ta, tr, _ty_name),
      Prim (loc, D_Lambda_rec, [(Seq (_loc, _) as script_instr)], []) ) ->
      traced
      @@ ( lambda_t loc ta tr >>?= fun lambda_rec_ty ->
           parse_lam_rec
             ~unparse_code_rec
             Tc_context.(add_lambda data)
             ~elab_conf
             ~stack_depth:(stack_depth + 1)
             ctxt
             ta
             tr
             lambda_rec_ty
             script_instr )
  | Lambda_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Options *)
  | Option_t (t, _, _), expr ->
      let parse_v ctxt v = non_terminal_recursion ctxt t v in
      traced @@ parse_option parse_v ctxt ~legacy expr
  (* Lists *)
  | List_t (t, _ty_name), Seq (_loc, items) ->
      traced
      @@ List.fold_left_es
           (fun (rest, ctxt) v ->
             non_terminal_recursion ctxt t v >|=? fun (v, ctxt) ->
             (Script_list.cons v rest, ctxt))
           (Script_list.empty, ctxt)
           (List.rev items)
  | List_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Tickets *)
  | Ticket_t (t, _ty_name), expr ->
      if allow_forged then
        opened_ticket_type (location expr) t >>?= fun ty ->
        non_terminal_recursion ctxt ty expr
        >>=? fun (({destination; entrypoint = _}, (contents, amount)), ctxt) ->
        match Ticket_amount.of_n amount with
        | Some amount -> (
            match destination with
            | Contract ticketer -> return ({ticketer; contents; amount}, ctxt)
            | Sc_rollup _ | Zk_rollup _ ->
                tzfail (Unexpected_ticket_owner destination))
        | None -> traced_fail Forbidden_zero_ticket_quantity
      else traced_fail (Unexpected_forged_value (location expr))
  (* Sets *)
  | Set_t (t, _ty_name), (Seq (loc, vs) as expr) ->
      traced
      @@ List.fold_left_es
           (fun (last_value, set, ctxt) v ->
             non_terminal_recursion ctxt t v >>=? fun (v, ctxt) ->
             Lwt.return
               ( (match last_value with
                 | Some value ->
                     Gas.consume
                       ctxt
                       (Michelson_v1_gas.Cost_of.Interpreter.compare t value v)
                     >>? fun ctxt ->
                     let c = Script_comparable.compare_comparable t value v in
                     if Compare.Int.(0 <= c) then
                       if Compare.Int.(0 = c) then
                         error
                           (Duplicate_set_values (loc, strip_locations expr))
                       else
                         error
                           (Unordered_set_values (loc, strip_locations expr))
                     else ok ctxt
                 | None -> ok ctxt)
               >>? fun ctxt ->
                 Gas.consume
                   ctxt
                   (Michelson_v1_gas.Cost_of.Interpreter.set_update v set)
                 >|? fun ctxt -> (Some v, Script_set.update v true set, ctxt) ))
           (None, Script_set.empty t, ctxt)
           vs
      >|=? fun (_, set, ctxt) -> (set, ctxt)
  | Set_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Maps *)
  | Map_t (tk, tv, _ty_name), (Seq (_, vs) as expr) ->
      parse_items ctxt expr tk tv vs (fun x -> x)
  | Map_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  | Big_map_t (tk, tv, _ty_name), expr ->
      (match expr with
      | Int (loc, id) ->
          return (Some (id, loc), {map = Big_map_overlay.empty; size = 0}, ctxt)
      | Seq (_, vs) ->
          parse_big_map_items ctxt expr tk tv vs (fun x -> Some x)
          >|=? fun (diff, ctxt) -> (None, diff, ctxt)
      | Prim (loc, D_Pair, [Int (loc_id, id); Seq (_, vs)], annot) ->
          error_unexpected_annot loc annot >>?= fun () ->
          option_t loc tv >>?= fun tv_opt ->
          parse_big_map_items ctxt expr tk tv_opt vs (fun x -> x)
          >|=? fun (diff, ctxt) -> (Some (id, loc_id), diff, ctxt)
      | Prim (_, D_Pair, [Int _; expr], _) ->
          traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
      | Prim (_, D_Pair, [expr; _], _) ->
          traced_fail (Invalid_kind (location expr, [Int_kind], kind expr))
      | Prim (loc, D_Pair, l, _) ->
          traced_fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
      | _ ->
          traced_fail
            (unexpected expr [Seq_kind; Int_kind] Constant_namespace [D_Pair]))
      >>=? fun (id_opt, diff, ctxt) ->
      (match id_opt with
      | None -> return @@ (None, ctxt)
      | Some (id, loc) ->
          if allow_forged then
            let id = Big_map.Id.parse_z id in
            Big_map.exists ctxt id >>=? function
            | _, None -> traced_fail (Invalid_big_map (loc, id))
            | ctxt, Some (btk, btv) ->
                Lwt.return
                  ( parse_comparable_ty
                      ~stack_depth:(stack_depth + 1)
                      ctxt
                      (Micheline.root btk)
                  >>? fun (Ex_comparable_ty btk, ctxt) ->
                    parse_big_map_value_ty
                      ctxt
                      ~stack_depth:(stack_depth + 1)
                      ~legacy
                      (Micheline.root btv)
                    >>? fun (Ex_ty btv, ctxt) ->
                    (Gas_monad.run ctxt
                    @@
                    let open Gas_monad.Syntax in
                    let error_details = Informative loc in
                    let* Eq = ty_eq ~error_details tk btk in
                    ty_eq ~error_details tv btv)
                    >>? fun (eq, ctxt) ->
                    eq >|? fun Eq -> (Some id, ctxt) )
          else traced_fail (Unexpected_forged_value loc))
      >|=? fun (id, ctxt) ->
      (Big_map {id; diff; key_type = tk; value_type = tv}, ctxt)
  | Never_t, expr -> Lwt.return @@ traced_no_lwt @@ parse_never expr
  (* Bls12_381 types *)
  | Bls12_381_g1_t, Bytes (_, bs) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g1 >>?= fun ctxt ->
      match Script_bls.G1.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | Bls12_381_g1_t, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | Bls12_381_g2_t, Bytes (_, bs) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g2 >>?= fun ctxt ->
      match Script_bls.G2.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | Bls12_381_g2_t, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | Bls12_381_fr_t, Bytes (_, bs) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_fr >>?= fun ctxt ->
      match Script_bls.Fr.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | Bls12_381_fr_t, Int (_, v) ->
      Gas.consume ctxt Typecheck_costs.bls12_381_fr >>?= fun ctxt ->
      return (Script_bls.Fr.of_z v, ctxt)
  | Bls12_381_fr_t, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  (*
        /!\ When adding new lazy storage kinds, you may want to guard the parsing
        of identifiers with [allow_forged].
    *)
  (* Sapling *)
  | Sapling_transaction_t memo_size, Bytes (_, bytes) -> (
      match
        Data_encoding.Binary.of_bytes_opt Sapling.transaction_encoding bytes
      with
      | Some transaction -> (
          match Sapling.transaction_get_memo_size transaction with
          | None -> return (transaction, ctxt)
          | Some transac_memo_size ->
              Lwt.return
                ( memo_size_eq
                    ~error_details:(Informative ())
                    memo_size
                    transac_memo_size
                >|? fun () -> (transaction, ctxt) ))
      | None -> fail_parse_data ())
  | Sapling_transaction_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | Sapling_transaction_deprecated_t memo_size, Bytes (_, bytes) -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Sapling.Legacy.transaction_encoding
          bytes
      with
      | Some transaction -> (
          match Sapling.Legacy.transaction_get_memo_size transaction with
          | None -> return (transaction, ctxt)
          | Some transac_memo_size ->
              Lwt.return
                ( memo_size_eq
                    ~error_details:(Informative ())
                    memo_size
                    transac_memo_size
                >|? fun () -> (transaction, ctxt) ))
      | None -> fail_parse_data ())
  | Sapling_transaction_deprecated_t _, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | Sapling_state_t memo_size, Int (loc, id) ->
      if allow_forged then
        let id = Sapling.Id.parse_z id in
        Sapling.state_from_id ctxt id >>=? fun (state, ctxt) ->
        Lwt.return
          ( traced_no_lwt
          @@ memo_size_eq
               ~error_details:(Informative ())
               memo_size
               state.Sapling.memo_size
          >|? fun () -> (state, ctxt) )
      else traced_fail (Unexpected_forged_value loc)
  | Sapling_state_t memo_size, Seq (_, []) ->
      return (Sapling.empty_state ~memo_size (), ctxt)
  | Sapling_state_t _, expr ->
      (* Do not allow to input diffs as they are untrusted and may not be the
          result of a verify_update. *)
      traced_fail
        (Invalid_kind (location expr, [Int_kind; Seq_kind], kind expr))
  (* Time lock*)
  | Chest_key_t, Bytes (_, bytes) -> (
      Gas.consume ctxt Typecheck_costs.chest_key >>?= fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt
          Script_timelock.chest_key_encoding
          bytes
      with
      | Some chest_key -> return (chest_key, ctxt)
      | None -> fail_parse_data ())
  | Chest_key_t, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | Chest_t, Bytes (_, bytes) -> (
      Gas.consume ctxt (Typecheck_costs.chest ~bytes:(Bytes.length bytes))
      >>?= fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt Script_timelock.chest_encoding bytes
      with
      | Some chest -> return (chest, ctxt)
      | None -> fail_parse_data ())
  | Chest_t, expr ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))

and parse_view :
    type storage storagec.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    context ->
    (storage, storagec) ty ->
    view ->
    (storage typed_view * context) tzresult Lwt.t =
 fun ~unparse_code_rec
     ~elab_conf
     ctxt
     storage_type
     {input_ty; output_ty; view_code} ->
  let legacy = elab_conf.legacy in
  let input_ty_loc = location input_ty in
  record_trace_eval
    (fun () ->
      Ill_formed_type
        (Some "arg of view", strip_locations input_ty, input_ty_loc))
    (parse_view_input_ty ctxt ~stack_depth:0 ~legacy input_ty)
  >>?= fun (Ex_ty input_ty, ctxt) ->
  let output_ty_loc = location output_ty in
  record_trace_eval
    (fun () ->
      Ill_formed_type
        (Some "return of view", strip_locations output_ty, output_ty_loc))
    (parse_view_output_ty ctxt ~stack_depth:0 ~legacy output_ty)
  >>?= fun (Ex_ty output_ty, ctxt) ->
  pair_t input_ty_loc input_ty storage_type >>?= fun (Ty_ex_c pair_ty) ->
  parse_instr
    ~unparse_code_rec
    ~elab_conf
    ~stack_depth:0
    Tc_context.view
    ctxt
    view_code
    (Item_t (pair_ty, Bot_t))
  >>=? fun (judgement, ctxt) ->
  Lwt.return
  @@
  match judgement with
  | Failed {descr} ->
      let {kinstr; _} = close_descr (descr (Item_t (output_ty, Bot_t))) in
      ok
        ( Typed_view
            {input_ty; output_ty; kinstr; original_code_expr = view_code},
          ctxt )
  | Typed ({loc; aft; _} as descr) -> (
      let ill_type_view stack_ty loc =
        let actual = serialize_stack_for_error ctxt stack_ty in
        let expected_stack = Item_t (output_ty, Bot_t) in
        let expected = serialize_stack_for_error ctxt expected_stack in
        Ill_typed_view {loc; actual; expected}
      in
      match aft with
      | Item_t (ty, Bot_t) ->
          let error_details = Informative loc in
          Gas_monad.run ctxt
          @@ Gas_monad.record_trace_eval ~error_details (fun loc ->
                 ill_type_view aft loc)
          @@ ty_eq ~error_details ty output_ty
          >>? fun (eq, ctxt) ->
          eq >|? fun Eq ->
          let {kinstr; _} = close_descr descr in
          ( Typed_view
              {input_ty; output_ty; kinstr; original_code_expr = view_code},
            ctxt )
      | _ -> error (ill_type_view aft loc))

and parse_views :
    type storage storagec.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    context ->
    (storage, storagec) ty ->
    view_map ->
    (storage typed_view_map * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~elab_conf ctxt storage_type views ->
  let aux ctxt name cur_view =
    Gas.consume
      ctxt
      (Michelson_v1_gas.Cost_of.Interpreter.view_update name views)
    >>?= fun ctxt ->
    parse_view ~unparse_code_rec ~elab_conf ctxt storage_type cur_view
  in
  Script_map.map_es_in_context aux ctxt views

and parse_kdescr :
    type arg argc ret retc.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    stack_depth:int ->
    tc_context ->
    context ->
    (arg, argc) ty ->
    (ret, retc) ty ->
    Script.node ->
    ((arg, end_of_stack, ret, end_of_stack) kdescr * context) tzresult Lwt.t =
 fun ~unparse_code_rec
     ~elab_conf
     ~stack_depth
     tc_context
     ctxt
     arg
     ret
     script_instr ->
  parse_instr
    ~unparse_code_rec
    ~elab_conf
    tc_context
    ctxt
    ~stack_depth:(stack_depth + 1)
    script_instr
    (Item_t (arg, Bot_t))
  >>=? function
  | Typed ({loc; aft = Item_t (ty, Bot_t) as stack_ty; _} as descr), ctxt ->
      Lwt.return
        (let error_details = Informative loc in
         Gas_monad.run ctxt
         @@ Gas_monad.record_trace_eval ~error_details (fun loc ->
                let ret = serialize_ty_for_error ret in
                let stack_ty = serialize_stack_for_error ctxt stack_ty in
                Bad_return (loc, stack_ty, ret))
         @@ ty_eq ~error_details ty ret
         >>? fun (eq, ctxt) ->
         eq >|? fun Eq ->
         ( (close_descr descr : (arg, end_of_stack, ret, end_of_stack) kdescr),
           ctxt ))
  | Typed {loc; aft = stack_ty; _}, ctxt ->
      let ret = serialize_ty_for_error ret in
      let stack_ty = serialize_stack_for_error ctxt stack_ty in
      tzfail @@ Bad_return (loc, stack_ty, ret)
  | Failed {descr}, ctxt ->
      return
        ( (close_descr (descr (Item_t (ret, Bot_t)))
            : (arg, end_of_stack, ret, end_of_stack) kdescr),
          ctxt )

and parse_lam_rec :
    type arg argc ret retc.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    stack_depth:int ->
    tc_context ->
    context ->
    (arg, argc) ty ->
    (ret, retc) ty ->
    ((arg, ret) lambda, _) ty ->
    Script.node ->
    ((arg, ret) lambda * context) tzresult Lwt.t =
 fun ~unparse_code_rec
     ~elab_conf
     ~stack_depth
     tc_context
     ctxt
     arg
     ret
     lambda_rec_ty
     script_instr ->
  parse_instr
    ~unparse_code_rec
    ~elab_conf
    tc_context
    ctxt
    ~stack_depth:(stack_depth + 1)
    script_instr
    (Item_t (arg, Item_t (lambda_rec_ty, Bot_t)))
  >>=? function
  | Typed ({loc; aft = Item_t (ty, Bot_t) as stack_ty; _} as descr), ctxt ->
      Lwt.return
        (let error_details = Informative loc in
         Gas_monad.run ctxt
         @@ Gas_monad.record_trace_eval ~error_details (fun loc ->
                let ret = serialize_ty_for_error ret in
                let stack_ty = serialize_stack_for_error ctxt stack_ty in
                Bad_return (loc, stack_ty, ret))
         @@ ty_eq ~error_details ty ret
         >>? fun (eq, ctxt) ->
         eq >|? fun Eq ->
         ( (close_descr descr
             : (arg, (arg, ret) lambda * end_of_stack, ret, end_of_stack) kdescr),
           ctxt ))
      >>=? fun (closed_descr, ctxt) ->
      (normalized_lam_rec [@ocaml.tailcall])
        ~unparse_code_rec
        ~stack_depth
        ctxt
        closed_descr
        script_instr
  | Typed {loc; aft = stack_ty; _}, ctxt ->
      let ret = serialize_ty_for_error ret in
      let stack_ty = serialize_stack_for_error ctxt stack_ty in
      tzfail @@ Bad_return (loc, stack_ty, ret)
  | Failed {descr}, ctxt ->
      (normalized_lam_rec [@ocaml.tailcall])
        ~unparse_code_rec
        ~stack_depth
        ctxt
        (close_descr (descr (Item_t (ret, Bot_t))))
        script_instr

and parse_instr :
    type a s.
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    stack_depth:int ->
    tc_context ->
    context ->
    Script.node ->
    (a, s) stack_ty ->
    ((a, s) judgement * context) tzresult Lwt.t =
 fun ~unparse_code_rec
     ~elab_conf
     ~stack_depth
     tc_context
     ctxt
     script_instr
     stack_ty ->
  let for_logging_only x =
    if elab_conf.keep_extra_types_for_interpreter_logging then Some x else None
  in
  let check_item_ty (type a ac b bc) ctxt (exp : (a, ac) ty) (got : (b, bc) ty)
      loc name n m : ((a, b) eq * context) tzresult =
    record_trace_eval (fun () ->
        let stack_ty = serialize_stack_for_error ctxt stack_ty in
        Bad_stack (loc, name, m, stack_ty))
    @@ record_trace
         (Bad_stack_item n)
         ( Gas_monad.run ctxt @@ ty_eq ~error_details:(Informative loc) exp got
         >>? fun (eq, ctxt) ->
           eq >|? fun Eq -> ((Eq : (a, b) eq), ctxt) )
  in
  let log_stack loc stack_ty aft =
    match (elab_conf.type_logger, script_instr) with
    | None, _ | Some _, (Int _ | String _ | Bytes _) -> ()
    | Some log, (Prim _ | Seq _) ->
        (* Unparsing for logging is not carbonated as this
              is used only by the client and not the protocol *)
        let stack_ty_before = unparse_stack_uncarbonated stack_ty in
        let stack_ty_after = unparse_stack_uncarbonated aft in
        log loc ~stack_ty_before ~stack_ty_after
  in
  let typed_no_lwt ctxt loc instr aft =
    log_stack loc stack_ty aft ;
    let j = Typed {loc; instr; bef = stack_ty; aft} in
    Ok (j, ctxt)
  in
  let typed ctxt loc instr aft =
    Lwt.return @@ typed_no_lwt ctxt loc instr aft
  in
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>?= fun ctxt ->
  let non_terminal_recursion tc_context ctxt script_instr stack_ty =
    if Compare.Int.(stack_depth > 10000) then
      tzfail Typechecking_too_many_recursive_calls
    else
      parse_instr
        ~unparse_code_rec
        ~elab_conf
        tc_context
        ctxt
        ~stack_depth:(stack_depth + 1)
        script_instr
        stack_ty
  in
  let bad_stack_error ctxt loc prim relevant_stack_portion =
    let whole_stack = serialize_stack_for_error ctxt stack_ty in
    error (Bad_stack (loc, prim, relevant_stack_portion, whole_stack))
  in
  let legacy = elab_conf.legacy in
  match (script_instr, stack_ty) with
  (* stack ops *)
  | Prim (loc, I_DROP, [], annot), Item_t (_, rest) ->
      (error_unexpected_annot loc annot >>?= fun () ->
       typed ctxt loc {apply = (fun k -> IDrop (loc, k))} rest
        : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_DROP, [n], result_annot), whole_stack ->
      parse_uint10 n >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n) >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dropn_proof_argument tzresult =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | true, rest -> ok @@ Dropn_proof_argument (KRest, rest)
        | false, Item_t (a, rest) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dropn_proof_argument (n', stack_after_drops)) ->
            Dropn_proof_argument (KPrefix (loc, a, n'), stack_after_drops)
        | _, _ ->
            let whole_stack = serialize_stack_for_error ctxt whole_stack in
            error (Bad_stack (loc, I_DROP, whole_n, whole_stack))
      in
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument whole_n whole_stack
      >>?= fun (Dropn_proof_argument (n', stack_after_drops)) ->
      let kdropn k = IDropn (loc, whole_n, n', k) in
      typed ctxt loc {apply = kdropn} stack_after_drops
  | Prim (loc, I_DROP, (_ :: _ :: _ as l), _), _ ->
      (* Technically, the arities 0 and 1 are allowed but the error only mentions 1.
            However, DROP is equivalent to DROP 1 so hinting at an arity of 1 makes sense. *)
      tzfail (Invalid_arity (loc, I_DROP, 1, List.length l))
  | Prim (loc, I_DUP, [], annot), (Item_t (v, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      record_trace_eval
        (fun () ->
          let t = serialize_ty_for_error v in
          Non_dupable_type (loc, t))
        (check_dupable_ty ctxt loc v)
      >>?= fun ctxt ->
      let dup = {apply = (fun k -> IDup (loc, k))} in
      typed ctxt loc dup (Item_t (v, stack))
  | Prim (loc, I_DUP, [n], v_annot), (Item_t _ as stack_ty) ->
      check_var_annot loc v_annot >>?= fun () ->
      let rec make_proof_argument :
          type a b s.
          int -> (a, b * s) stack_ty -> (a, b, s) dup_n_proof_argument tzresult
          =
       fun n (stack_ty : (a, b * s) stack_ty) ->
        match (n, stack_ty) with
        | 1, Item_t (hd_ty, _) -> ok @@ Dup_n_proof_argument (Dup_n_zero, hd_ty)
        | n, Item_t (_, (Item_t (_, _) as tl_ty)) ->
            make_proof_argument (n - 1) tl_ty
            >|? fun (Dup_n_proof_argument (dup_n_witness, b_ty)) ->
            Dup_n_proof_argument (Dup_n_succ dup_n_witness, b_ty)
        | _ -> bad_stack_error ctxt loc I_DUP 1
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 0) (Dup_n_bad_argument loc)
      >>?= fun () ->
      record_trace (Dup_n_bad_stack loc) (make_proof_argument n stack_ty)
      >>?= fun (Dup_n_proof_argument (witness, after_ty)) ->
      record_trace_eval
        (fun () ->
          let t = serialize_ty_for_error after_ty in
          Non_dupable_type (loc, t))
        (check_dupable_ty ctxt loc after_ty)
      >>?= fun ctxt ->
      let dupn = {apply = (fun k -> IDup_n (loc, n, witness, k))} in
      typed ctxt loc dupn (Item_t (after_ty, stack_ty))
  | Prim (loc, I_DIG, [n], result_annot), stack ->
      let rec make_proof_argument :
          type a s. int -> (a, s) stack_ty -> (a, s) dig_proof_argument tzresult
          =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | true, Item_t (v, rest) -> ok @@ Dig_proof_argument (KRest, v, rest)
        | false, Item_t (v, rest) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dig_proof_argument (n', x, aft')) ->
            Dig_proof_argument (KPrefix (loc, v, n'), x, Item_t (v, aft'))
        | _, _ ->
            let whole_stack = serialize_stack_for_error ctxt stack in
            error (Bad_stack (loc, I_DIG, 3, whole_stack))
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument n stack >>?= fun (Dig_proof_argument (n', x, aft)) ->
      let dig = {apply = (fun k -> IDig (loc, n, n', k))} in
      typed ctxt loc dig (Item_t (x, aft))
  | Prim (loc, I_DIG, (([] | _ :: _ :: _) as l), _), _ ->
      tzfail (Invalid_arity (loc, I_DIG, 1, List.length l))
  | Prim (loc, I_DUG, [n], result_annot), Item_t (x, whole_stack) -> (
      parse_uint10 n >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n) >>?= fun ctxt ->
      error_unexpected_annot loc result_annot >>?= fun () ->
      match make_dug_proof_argument loc whole_n x whole_stack with
      | None ->
          let whole_stack = serialize_stack_for_error ctxt whole_stack in
          tzfail (Bad_stack (loc, I_DUG, whole_n, whole_stack))
      | Some (Dug_proof_argument (n', aft)) ->
          let dug = {apply = (fun k -> IDug (loc, whole_n, n', k))} in
          typed ctxt loc dug aft)
  | Prim (loc, I_DUG, [_], result_annot), stack ->
      Lwt.return
        ( error_unexpected_annot loc result_annot >>? fun () ->
          let stack = serialize_stack_for_error ctxt stack in
          error (Bad_stack (loc, I_DUG, 1, stack)) )
  | Prim (loc, I_DUG, (([] | _ :: _ :: _) as l), _), _ ->
      tzfail (Invalid_arity (loc, I_DUG, 1, List.length l))
  | Prim (loc, I_SWAP, [], annot), Item_t (v, Item_t (w, rest)) ->
      error_unexpected_annot loc annot >>?= fun () ->
      let swap = {apply = (fun k -> ISwap (loc, k))} in
      let stack_ty = Item_t (w, Item_t (v, rest)) in
      typed ctxt loc swap stack_ty
  | Prim (loc, I_PUSH, [t; d], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      parse_data
        ~unparse_code_rec
        ~elab_conf
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~allow_forged:false
        t
        d
      >>=? fun (v, ctxt) ->
      let push = {apply = (fun k -> IPush (loc, t, v, k))} in
      typed ctxt loc push (Item_t (t, stack))
  | Prim (loc, I_UNIT, [], annot), stack ->
      check_var_type_annot loc annot >>?= fun () ->
      let push = {apply = (fun k -> IPush (loc, unit_t, (), k))} in
      typed ctxt loc push (Item_t (unit_t, stack))
  (* options *)
  | Prim (loc, I_SOME, [], annot), Item_t (t, rest) ->
      check_var_type_annot loc annot >>?= fun () ->
      let cons_some = {apply = (fun k -> ICons_some (loc, k))} in
      option_t loc t >>?= fun ty -> typed ctxt loc cons_some (Item_t (ty, rest))
  | Prim (loc, I_NONE, [t], annot), stack ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let cons_none = {apply = (fun k -> ICons_none (loc, t, k))} in
      option_t loc t >>?= fun ty ->
      let stack_ty = Item_t (ty, stack) in
      typed ctxt loc cons_none stack_ty
  | Prim (loc, I_MAP, [body], annot), Item_t (Option_t (t, _, _), rest) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body (Item_t (t, rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ({loc; aft = Item_t (ret, aft_rest); _} as kibody) ->
          let invalid_map_body () =
            let aft = serialize_stack_for_error ctxt kibody.aft in
            Invalid_map_body (loc, aft)
          in
          record_trace_eval
            invalid_map_body
            ( stack_eq loc ctxt 1 aft_rest rest >>? fun (Eq, ctxt) ->
              option_t loc ret >>? fun opt_ty ->
              let final_stack = Item_t (opt_ty, rest) in
              let body = kibody.instr.apply (IHalt loc) in
              let apply k = IOpt_map {loc; body; k} in
              typed_no_lwt ctxt loc {apply} final_stack )
      | Typed {aft = Bot_t; _} ->
          let aft = serialize_stack_for_error ctxt Bot_t in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | ( Prim (loc, I_IF_NONE, [bt; bf], annot),
      (Item_t (Option_t (t, _, _), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt bt rest >>=? fun (btr, ctxt) ->
      let stack_ty = Item_t (t, rest) in
      non_terminal_recursion tc_context ctxt bf stack_ty >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let ifnone =
          {
            apply =
              (fun k ->
                let hloc = kinstr_location k in
                let branch_if_none = ibt.instr.apply (IHalt hloc)
                and branch_if_some = ibf.instr.apply (IHalt hloc) in
                IIf_none {loc; branch_if_none; branch_if_some; k});
          }
        in
        {loc; instr = ifnone; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ctxt loc btr bfr {branch}
  (* pairs *)
  | Prim (loc, I_PAIR, [], annot), Item_t (a, Item_t (b, rest)) ->
      check_constr_annot loc annot >>?= fun () ->
      pair_t loc a b >>?= fun (Ty_ex_c ty) ->
      let stack_ty = Item_t (ty, rest) in
      let cons_pair = {apply = (fun k -> ICons_pair (loc, k))} in
      typed ctxt loc cons_pair stack_ty
  | Prim (loc, I_PAIR, [n], annot), (Item_t _ as stack_ty) ->
      check_var_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type a b s.
          int -> (a, b * s) stack_ty -> (a, b, s) comb_proof_argument tzresult =
       fun n stack_ty ->
        match (n, stack_ty) with
        | 1, Item_t _ -> ok (Comb_proof_argument (Comb_one, stack_ty))
        | n, Item_t (a_ty, (Item_t _ as tl_ty)) ->
            make_proof_argument (n - 1) tl_ty
            >>? fun (Comb_proof_argument (comb_witness, Item_t (b_ty, tl_ty')))
              ->
            pair_t loc a_ty b_ty >|? fun (Ty_ex_c pair_t) ->
            Comb_proof_argument (Comb_succ comb_witness, Item_t (pair_t, tl_ty'))
        | _ -> bad_stack_error ctxt loc I_PAIR 1
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Pair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Comb_proof_argument (witness, after_ty)) ->
      let comb = {apply = (fun k -> IComb (loc, n, witness, k))} in
      typed ctxt loc comb after_ty
  | Prim (loc, I_UNPAIR, [n], annot), (Item_t _ as stack_ty) ->
      error_unexpected_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type a b s.
          int -> (a, b * s) stack_ty -> (a, b, s) uncomb_proof_argument tzresult
          =
       fun n stack_ty ->
        match (n, stack_ty) with
        | 1, (Item_t _ as stack) ->
            ok @@ Uncomb_proof_argument (Uncomb_one, stack)
        | n, Item_t (Pair_t (a_ty, b_ty, _, _), tl_ty) ->
            make_proof_argument (n - 1) (Item_t (b_ty, tl_ty))
            >|? fun (Uncomb_proof_argument (uncomb_witness, after_ty)) ->
            Uncomb_proof_argument
              (Uncomb_succ uncomb_witness, Item_t (a_ty, after_ty))
        | _ -> bad_stack_error ctxt loc I_UNPAIR 1
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Unpair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Uncomb_proof_argument (witness, after_ty)) ->
      let uncomb = {apply = (fun k -> IUncomb (loc, n, witness, k))} in
      typed ctxt loc uncomb after_ty
  | Prim (loc, I_GET, [n], annot), Item_t (comb_ty, rest_ty) -> (
      check_var_annot loc annot >>?= fun () ->
      parse_uint11 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      match make_comb_get_proof_argument n comb_ty with
      | None ->
          let whole_stack = serialize_stack_for_error ctxt stack_ty in
          tzfail (Bad_stack (loc, I_GET, 1, whole_stack))
      | Some (Comb_get_proof_argument (witness, ty')) ->
          let after_stack_ty = Item_t (ty', rest_ty) in
          let comb_get = {apply = (fun k -> IComb_get (loc, n, witness, k))} in
          typed ctxt loc comb_get after_stack_ty)
  | ( Prim (loc, I_UPDATE, [n], annot),
      Item_t (value_ty, Item_t (comb_ty, rest_ty)) ) ->
      check_var_annot loc annot >>?= fun () ->
      parse_uint11 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      make_comb_set_proof_argument ctxt stack_ty loc n value_ty comb_ty
      >>?= fun (Comb_set_proof_argument (witness, after_ty)) ->
      let after_stack_ty = Item_t (after_ty, rest_ty) in
      let comb_set = {apply = (fun k -> IComb_set (loc, n, witness, k))} in
      typed ctxt loc comb_set after_stack_ty
  | Prim (loc, I_UNPAIR, [], annot), Item_t (Pair_t (a, b, _, _), rest) ->
      check_unpair_annot loc annot >>?= fun () ->
      let unpair = {apply = (fun k -> IUnpair (loc, k))} in
      typed ctxt loc unpair (Item_t (a, Item_t (b, rest)))
  | Prim (loc, I_CAR, [], annot), Item_t (Pair_t (a, _, _, _), rest) ->
      check_destr_annot loc annot >>?= fun () ->
      let car = {apply = (fun k -> ICar (loc, k))} in
      typed ctxt loc car (Item_t (a, rest))
  | Prim (loc, I_CDR, [], annot), Item_t (Pair_t (_, b, _, _), rest) ->
      check_destr_annot loc annot >>?= fun () ->
      let cdr = {apply = (fun k -> ICdr (loc, k))} in
      typed ctxt loc cdr (Item_t (b, rest))
  (* ors *)
  | Prim (loc, I_LEFT, [tr], annot), Item_t (tl, rest) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tr
      >>?= fun (Ex_ty tr, ctxt) ->
      check_constr_annot loc annot >>?= fun () ->
      let cons_left = {apply = (fun k -> ICons_left (loc, tr, k))} in
      or_t loc tl tr >>?= fun (Ty_ex_c ty) ->
      let stack_ty = Item_t (ty, rest) in
      typed ctxt loc cons_left stack_ty
  | Prim (loc, I_RIGHT, [tl], annot), Item_t (tr, rest) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tl
      >>?= fun (Ex_ty tl, ctxt) ->
      check_constr_annot loc annot >>?= fun () ->
      let cons_right = {apply = (fun k -> ICons_right (loc, tl, k))} in
      or_t loc tl tr >>?= fun (Ty_ex_c ty) ->
      let stack_ty = Item_t (ty, rest) in
      typed ctxt loc cons_right stack_ty
  | ( Prim (loc, I_IF_LEFT, [bt; bf], annot),
      (Item_t (Or_t (tl, tr, _, _), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt bt (Item_t (tl, rest))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion tc_context ctxt bf (Item_t (tr, rest))
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let instr =
          {
            apply =
              (fun k ->
                let hloc = kinstr_location k in
                let branch_if_left = ibt.instr.apply (IHalt hloc)
                and branch_if_right = ibf.instr.apply (IHalt hloc) in
                IIf_left {loc; branch_if_left; branch_if_right; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ctxt loc btr bfr {branch}
  (* lists *)
  | Prim (loc, I_NIL, [t], annot), stack ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let nil = {apply = (fun k -> INil (loc, t, k))} in
      list_t loc t >>?= fun ty -> typed ctxt loc nil (Item_t (ty, stack))
  | ( Prim (loc, I_CONS, [], annot),
      Item_t (tv, (Item_t (List_t (t, _), _) as stack)) ) ->
      check_item_ty ctxt tv t loc I_CONS 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let cons_list = {apply = (fun k -> ICons_list (loc, k))} in
      (typed ctxt loc cons_list stack
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_IF_CONS, [bt; bf], annot),
      (Item_t (List_t (t, _), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt bt (Item_t (t, bef))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion tc_context ctxt bf rest >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let instr =
          {
            apply =
              (fun k ->
                let hloc = kinstr_location k in
                let branch_if_cons = ibt.instr.apply (IHalt hloc)
                and branch_if_nil = ibf.instr.apply (IHalt hloc) in
                IIf_cons {loc; branch_if_nil; branch_if_cons; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ctxt loc btr bfr {branch}
  | Prim (loc, I_SIZE, [], annot), Item_t (List_t _, rest) ->
      check_var_type_annot loc annot >>?= fun () ->
      let list_size = {apply = (fun k -> IList_size (loc, k))} in
      typed ctxt loc list_size (Item_t (nat_t, rest))
  | Prim (loc, I_MAP, [body], annot), Item_t (List_t (elt, _), starting_rest)
    -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body (Item_t (elt, starting_rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ({aft = Item_t (ret, rest) as aft; _} as kibody) ->
          let invalid_map_body () =
            let aft = serialize_stack_for_error ctxt aft in
            Invalid_map_body (loc, aft)
          in
          record_trace_eval
            invalid_map_body
            ( stack_eq loc ctxt 1 rest starting_rest >>? fun (Eq, ctxt) ->
              let hloc = loc in
              let ibody = kibody.instr.apply (IHalt hloc) in
              list_t loc ret >>? fun ty ->
              let list_map =
                {
                  apply =
                    (fun k -> IList_map (loc, ibody, for_logging_only ty, k));
                }
              in
              let stack = Item_t (ty, rest) in
              typed_no_lwt ctxt loc list_map stack )
      | Typed {aft; _} ->
          let aft = serialize_stack_for_error ctxt aft in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | Prim (loc, I_ITER, [body], annot), Item_t (List_t (elt, _), rest) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body (Item_t (elt, rest))
      >>=? fun (judgement, ctxt) ->
      let mk_list_iter ibody =
        {
          apply =
            (fun k ->
              let hinfo = loc in
              let ibody = ibody.instr.apply (IHalt hinfo) in
              IList_iter (loc, for_logging_only elt, ibody, k));
        }
      in
      Lwt.return
      @@
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            let rest = serialize_stack_for_error ctxt rest in
            Invalid_iter_body (loc, rest, aft)
          in
          record_trace_eval
            invalid_iter_body
            ( stack_eq loc ctxt 1 aft rest
            >>? fun (Eq, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (mk_list_iter ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (mk_list_iter (descr rest)) rest
      )
  (* sets *)
  | Prim (loc, I_EMPTY_SET, [t], annot), rest ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt t
      >>?= fun (Ex_comparable_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEmpty_set (loc, t, k))} in
      set_t loc t >>?= fun ty -> typed ctxt loc instr (Item_t (ty, rest))
  | Prim (loc, I_ITER, [body], annot), Item_t (Set_t (elt, _), rest) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body (Item_t (elt, rest))
      >>=? fun (judgement, ctxt) ->
      let mk_iset_iter ibody =
        {
          apply =
            (fun k ->
              let hinfo = loc in
              let ibody = ibody.instr.apply (IHalt hinfo) in
              ISet_iter (loc, for_logging_only elt, ibody, k));
        }
      in
      Lwt.return
      @@
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            let rest = serialize_stack_for_error ctxt rest in
            Invalid_iter_body (loc, rest, aft)
          in
          record_trace_eval
            invalid_iter_body
            ( stack_eq loc ctxt 1 aft rest
            >>? fun (Eq, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (mk_iset_iter ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (mk_iset_iter (descr rest)) rest
      )
  | Prim (loc, I_MEM, [], annot), Item_t (v, Item_t (Set_t (elt, _), rest)) ->
      check_var_type_annot loc annot >>?= fun () ->
      check_item_ty ctxt elt v loc I_MEM 1 2 >>?= fun (Eq, ctxt) ->
      let instr = {apply = (fun k -> ISet_mem (loc, k))} in
      (typed ctxt loc instr (Item_t (bool_t, rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t (v, Item_t (Bool_t, (Item_t (Set_t (elt, _), _) as stack))) ) ->
      check_item_ty ctxt elt v loc I_UPDATE 1 3 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISet_update (loc, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_SIZE, [], annot), Item_t (Set_t _, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISet_size (loc, k))} in
      typed ctxt loc instr (Item_t (nat_t, rest))
  (* maps *)
  | Prim (loc, I_EMPTY_MAP, [tk; tv], annot), stack ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun k -> IEmpty_map (loc, tk, for_logging_only tv, k))}
      in
      map_t loc tk tv >>?= fun ty -> typed ctxt loc instr (Item_t (ty, stack))
  | Prim (loc, I_MAP, [body], annot), Item_t (Map_t (kt, elt, _), starting_rest)
    -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      pair_t loc kt elt >>?= fun (Ty_ex_c ty) ->
      non_terminal_recursion tc_context ctxt body (Item_t (ty, starting_rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ({aft = Item_t (ret, rest) as aft; _} as ibody) ->
          let invalid_map_body () =
            let aft = serialize_stack_for_error ctxt aft in
            Invalid_map_body (loc, aft)
          in
          record_trace_eval
            invalid_map_body
            ( stack_eq loc ctxt 1 rest starting_rest >>? fun (Eq, ctxt) ->
              map_t loc kt ret >>? fun ty ->
              let instr =
                {
                  apply =
                    (fun k ->
                      let hinfo = loc in
                      let ibody = ibody.instr.apply (IHalt hinfo) in
                      IMap_map (loc, for_logging_only ty, ibody, k));
                }
              in
              let stack = Item_t (ty, rest) in
              typed_no_lwt ctxt loc instr stack )
      | Typed {aft; _} ->
          let aft = serialize_stack_for_error ctxt aft in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | Prim (loc, I_ITER, [body], annot), Item_t (Map_t (key, element_ty, _), rest)
    -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      pair_t loc key element_ty >>?= fun (Ty_ex_c ty) ->
      non_terminal_recursion tc_context ctxt body (Item_t (ty, rest))
      >>=? fun (judgement, ctxt) ->
      let make_instr ibody =
        {
          apply =
            (fun k ->
              let hinfo = loc in
              let ibody = ibody.instr.apply (IHalt hinfo) in
              IMap_iter (loc, for_logging_only ty, ibody, k));
        }
      in
      Lwt.return
      @@
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            let rest = serialize_stack_for_error ctxt rest in
            Invalid_iter_body (loc, rest, aft)
          in
          record_trace_eval
            invalid_iter_body
            ( stack_eq loc ctxt 1 aft rest
            >>? fun (Eq, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (make_instr ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (make_instr (descr rest)) rest)
  | Prim (loc, I_MEM, [], annot), Item_t (vk, Item_t (Map_t (k, _, _), rest)) ->
      check_item_ty ctxt vk k loc I_MEM 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMap_mem (loc, k))} in
      (typed ctxt loc instr (Item_t (bool_t, rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_GET, [], annot), Item_t (vk, Item_t (Map_t (k, elt, _), rest))
    ->
      check_item_ty ctxt vk k loc I_GET 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMap_get (loc, k))} in
      option_t loc elt
      >>?= fun ty : ((a, s) judgement * context) tzresult Lwt.t ->
      typed ctxt loc instr (Item_t (ty, rest))
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t (Option_t (vv, _, _), (Item_t (Map_t (k, v, _), _) as stack))
        ) ) ->
      check_item_ty ctxt vk k loc I_UPDATE 1 3 >>?= fun (Eq, ctxt) ->
      check_item_ty ctxt vv v loc I_UPDATE 2 3 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMap_update (loc, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          (Item_t (Option_t (vv, _, _), Item_t (Map_t (k, v, _), _)) as stack)
        ) ) ->
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3 >>?= fun (Eq, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMap_get_and_update (loc, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_SIZE, [], annot), Item_t (Map_t (_, _, _), rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMap_size (loc, k))} in
      typed ctxt loc instr (Item_t (nat_t, rest))
  (* big_map *)
  | Prim (loc, I_EMPTY_BIG_MAP, [tk; tv], annot), stack ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_big_map_value_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEmpty_big_map (loc, tk, tv, k))} in
      big_map_t loc tk tv >>?= fun ty ->
      let stack = Item_t (ty, stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MEM, [], annot),
      Item_t (set_key, Item_t (Big_map_t (k, _, _), rest)) ) ->
      check_item_ty ctxt set_key k loc I_MEM 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBig_map_mem (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Big_map_t (k, elt, _), rest)) ) ->
      check_item_ty ctxt vk k loc I_GET 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBig_map_get (loc, k))} in
      option_t loc elt >>?= fun ty ->
      let stack = Item_t (ty, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( set_key,
          Item_t
            ( Option_t (set_value, _, _),
              (Item_t (Big_map_t (map_key, map_value, _), _) as stack) ) ) ) ->
      check_item_ty ctxt set_key map_key loc I_UPDATE 1 3 >>?= fun (Eq, ctxt) ->
      check_item_ty ctxt set_value map_value loc I_UPDATE 2 3
      >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBig_map_update (loc, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          (Item_t (Option_t (vv, _, _), Item_t (Big_map_t (k, v, _), _)) as
          stack) ) ) ->
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3 >>?= fun (Eq, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBig_map_get_and_update (loc, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  (* Sapling *)
  | Prim (loc, I_SAPLING_EMPTY_STATE, [memo_size], annot), rest ->
      parse_memo_size memo_size >>?= fun memo_size ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun k -> ISapling_empty_state (loc, memo_size, k))}
      in
      let stack = Item_t (sapling_state_t ~memo_size, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SAPLING_VERIFY_UPDATE, [], _),
      Item_t
        ( Sapling_transaction_deprecated_t transaction_memo_size,
          Item_t ((Sapling_state_t state_memo_size as state_ty), rest) ) ) ->
      if legacy then
        memo_size_eq
          ~error_details:(Informative ())
          state_memo_size
          transaction_memo_size
        >>?= fun () ->
        let instr =
          {apply = (fun k -> ISapling_verify_update_deprecated (loc, k))}
        in
        pair_t loc int_t state_ty >>?= fun (Ty_ex_c pair_ty) ->
        option_t loc pair_ty >>?= fun ty ->
        let stack = Item_t (ty, rest) in
        typed ctxt loc instr stack
      else tzfail (Deprecated_instruction T_sapling_transaction_deprecated)
  | ( Prim (loc, I_SAPLING_VERIFY_UPDATE, [], _),
      Item_t
        ( Sapling_transaction_t transaction_memo_size,
          Item_t ((Sapling_state_t state_memo_size as state_ty), rest) ) ) ->
      memo_size_eq
        ~error_details:(Informative ())
        state_memo_size
        transaction_memo_size
      >>?= fun () ->
      let instr = {apply = (fun k -> ISapling_verify_update (loc, k))} in
      pair_t loc int_t state_ty >>?= fun (Ty_ex_c pair_ty) ->
      pair_t loc bytes_t pair_ty >>?= fun (Ty_ex_c pair_ty) ->
      option_t loc pair_ty >>?= fun ty ->
      let stack = Item_t (ty, rest) in
      typed ctxt loc instr stack
  (* control *)
  | Seq (loc, []), stack ->
      let instr = {apply = (fun k -> k)} in
      typed ctxt loc instr stack
  | Seq (_, [single]), stack ->
      non_terminal_recursion tc_context ctxt single stack
  | Seq (loc, hd :: tl), stack -> (
      non_terminal_recursion tc_context ctxt hd stack
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Failed _ -> tzfail (Fail_not_in_tail_position (Micheline.location hd))
      | Typed ({aft = middle; _} as ihd) ->
          non_terminal_recursion
            tc_context
            ctxt
            (Seq (Micheline.dummy_location, tl))
            middle
          >|=? fun (judgement, ctxt) ->
          let judgement =
            match judgement with
            | Failed {descr} ->
                let descr ret = compose_descr loc ihd (descr ret) in
                Failed {descr}
            | Typed itl -> Typed (compose_descr loc ihd itl)
          in
          (judgement, ctxt))
  | Prim (loc, I_IF, [bt; bf], annot), (Item_t (Bool_t, rest) as bef) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt bt rest >>=? fun (btr, ctxt) ->
      non_terminal_recursion tc_context ctxt bf rest >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let instr =
          {
            apply =
              (fun k ->
                let hloc = kinstr_location k in
                let branch_if_true = ibt.instr.apply (IHalt hloc)
                and branch_if_false = ibf.instr.apply (IHalt hloc) in
                IIf {loc; branch_if_true; branch_if_false; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ctxt loc btr bfr {branch}
  | Prim (loc, I_LOOP, [body], annot), (Item_t (Bool_t, rest) as stack) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body rest
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ibody ->
          let unmatched_branches () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            let stack = serialize_stack_for_error ctxt stack in
            Unmatched_branches (loc, aft, stack)
          in
          record_trace_eval
            unmatched_branches
            ( stack_eq loc ctxt 1 ibody.aft stack >>? fun (Eq, ctxt) ->
              let instr =
                {
                  apply =
                    (fun k ->
                      let loc = kinstr_location k in
                      let ibody = ibody.instr.apply (IHalt loc) in
                      ILoop (loc, ibody, k));
                }
              in
              typed_no_lwt ctxt loc instr rest )
      | Failed {descr} ->
          let instr =
            {
              apply =
                (fun k ->
                  let loc = kinstr_location k in
                  let ibody = descr stack in
                  let ibody = ibody.instr.apply (IHalt loc) in
                  ILoop (loc, ibody, k));
            }
          in
          typed_no_lwt ctxt loc instr rest)
  | ( Prim (loc, I_LOOP_LEFT, [body], annot),
      (Item_t (Or_t (tl, tr, _, _), rest) as stack) ) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      non_terminal_recursion tc_context ctxt body (Item_t (tl, rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ibody ->
          let unmatched_branches () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            let stack = serialize_stack_for_error ctxt stack in
            Unmatched_branches (loc, aft, stack)
          in
          record_trace_eval
            unmatched_branches
            ( stack_eq loc ctxt 1 ibody.aft stack >>? fun (Eq, ctxt) ->
              let instr =
                {
                  apply =
                    (fun k ->
                      let loc = kinstr_location k in
                      let ibody = ibody.instr.apply (IHalt loc) in
                      ILoop_left (loc, ibody, k));
                }
              in
              let stack = Item_t (tr, rest) in
              typed_no_lwt ctxt loc instr stack )
      | Failed {descr} ->
          let instr =
            {
              apply =
                (fun k ->
                  let loc = kinstr_location k in
                  let ibody = descr stack in
                  let ibody = ibody.instr.apply (IHalt loc) in
                  ILoop_left (loc, ibody, k));
            }
          in
          let stack = Item_t (tr, rest) in
          typed_no_lwt ctxt loc instr stack)
  | Prim (loc, I_LAMBDA, [arg; ret; code], annot), stack ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy arg
      >>?= fun (Ex_ty arg, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ret
      >>?= fun (Ex_ty ret, ctxt) ->
      check_kind [Seq_kind] code >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      parse_kdescr
        ~unparse_code_rec
        (Tc_context.add_lambda tc_context)
        ~elab_conf
        ~stack_depth:(stack_depth + 1)
        ctxt
        arg
        ret
        code
      >>=? fun (kdescr, ctxt) ->
      (* No need to normalize the unparsed component to Optimized mode here
         because the script is already normalized in Optimized mode. *)
      let instr = {apply = (fun k -> ILambda (loc, Lam (kdescr, code), k))} in
      lambda_t loc arg ret >>?= fun ty ->
      let stack = Item_t (ty, stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_LAMBDA_REC, [arg_ty_expr; ret_ty_expr; lambda_expr], annot),
      stack ) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy arg_ty_expr
      >>?= fun (Ex_ty arg, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ret_ty_expr
      >>?= fun (Ex_ty ret, ctxt) ->
      check_kind [Seq_kind] lambda_expr >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      lambda_t loc arg ret >>?= fun lambda_rec_ty ->
      parse_lam_rec
        ~unparse_code_rec:(fun ctxt ~stack_depth:_ _unparsing_mode node ->
          return (node, ctxt))
        (* No need to normalize the unparsed component to Optimized mode here
           because the script is already normalized in Optimized mode. *)
        Tc_context.(add_lambda tc_context)
        ~elab_conf
        ~stack_depth:(stack_depth + 1)
        ctxt
        arg
        ret
        lambda_rec_ty
        lambda_expr
      >>=? fun (code, ctxt) ->
      let instr = {apply = (fun k -> ILambda (loc, code, k))} in
      let stack = Item_t (lambda_rec_ty, stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_EXEC, [], annot),
      Item_t (arg, Item_t (Lambda_t (param, ret, _), rest)) ) ->
      check_item_ty ctxt arg param loc I_EXEC 1 2 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let stack = Item_t (ret, rest) in
      let instr = {apply = (fun k -> IExec (loc, for_logging_only stack, k))} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_APPLY, [], annot),
      Item_t
        ( capture,
          Item_t (Lambda_t (Pair_t (capture_ty, arg_ty, _, _), ret, _), rest) )
    ) ->
      check_packable ~legacy:false loc capture_ty >>?= fun () ->
      check_item_ty ctxt capture capture_ty loc I_APPLY 1 2
      >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IApply (loc, capture_ty, k))} in
      lambda_t loc arg_ty ret
      (* This cannot tzfail because the type [lambda 'arg 'ret] is always smaller than
         the input type [lambda (pair 'arg 'capture) 'ret]. In an ideal world, there
         would be a smart deconstructor to ensure this statically. *)
      >>?=
      fun res_ty ->
      let stack = Item_t (res_ty, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_DIP, [code], annot), Item_t (v, rest) -> (
      error_unexpected_annot loc annot >>?= fun () ->
      check_kind [Seq_kind] code >>?= fun () ->
      non_terminal_recursion tc_context ctxt code rest
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed descr ->
          let instr =
            {
              apply =
                (fun k ->
                  let b = descr.instr.apply (IHalt descr.loc) in
                  IDip (loc, b, for_logging_only v, k));
            }
          in
          let stack = Item_t (v, descr.aft) in
          typed ctxt loc instr stack
      | Failed _ -> tzfail (Fail_not_in_tail_position loc))
  | Prim (loc, I_DIP, [n; code], result_annot), stack ->
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dipn_proof_argument tzresult Lwt.t =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | true, rest -> (
            non_terminal_recursion tc_context ctxt code rest
            >>=? fun (judgement, ctxt) ->
            Lwt.return
            @@
            match judgement with
            | Typed descr ->
                ok
                  (Dipn_proof_argument (KRest, ctxt, descr, descr.aft)
                    : (a, s) dipn_proof_argument)
            | Failed _ -> error (Fail_not_in_tail_position loc))
        | false, Item_t (v, rest) ->
            make_proof_argument (n - 1) rest
            >|=? fun (Dipn_proof_argument (n', ctxt, descr, aft')) ->
            let w = KPrefix (loc, v, n') in
            Dipn_proof_argument (w, ctxt, descr, Item_t (v, aft'))
        | _, _ ->
            Lwt.return
              (let whole_stack = serialize_stack_for_error ctxt stack in
               error (Bad_stack (loc, I_DIP, 1, whole_stack)))
      in
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument n stack
      >>=? fun (Dipn_proof_argument (n', ctxt, descr, aft)) ->
      let b = descr.instr.apply (IHalt descr.loc) in
      let res = {apply = (fun k -> IDipn (loc, n, n', b, k))} in
      typed ctxt loc res aft
  | Prim (loc, I_DIP, (([] | _ :: _ :: _ :: _) as l), _), _ ->
      (* Technically, the arities 1 and 2 are allowed but the error only mentions 2.
            However, DIP {code} is equivalent to DIP 1 {code} so hinting at an arity of 2 makes sense. *)
      tzfail (Invalid_arity (loc, I_DIP, 2, List.length l))
  | Prim (loc, I_FAILWITH, [], annot), Item_t (v, _rest) ->
      Lwt.return
        ( error_unexpected_annot loc annot >>? fun () ->
          (if legacy then Result.return_unit
          else check_packable ~legacy:false loc v)
          >|? fun () ->
          let instr = {apply = (fun _k -> IFailwith (loc, v))} in
          let descr aft = {loc; instr; bef = stack_ty; aft} in
          log_stack loc stack_ty Bot_t ;
          (Failed {descr}, ctxt) )
  | Prim (loc, I_NEVER, [], annot), Item_t (Never_t, _rest) ->
      Lwt.return
        ( error_unexpected_annot loc annot >|? fun () ->
          let instr = {apply = (fun _k -> INever loc)} in
          let descr aft = {loc; instr; bef = stack_ty; aft} in
          log_stack loc stack_ty Bot_t ;
          (Failed {descr}, ctxt) )
  (* timestamp operations *)
  | Prim (loc, I_ADD, [], annot), Item_t (Timestamp_t, Item_t (Int_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_timestamp_to_seconds (loc, k))} in
      typed ctxt loc instr (Item_t (Timestamp_t, rest))
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Int_t, (Item_t (Timestamp_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_seconds_to_timestamp (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SUB, [], annot), Item_t (Timestamp_t, Item_t (Int_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_timestamp_seconds (loc, k))} in
      let stack = Item_t (Timestamp_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t, Item_t (Timestamp_t, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IDiff_timestamps (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  (* string operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (String_t, (Item_t (String_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IConcat_string_pair (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_CONCAT, [], annot), Item_t (List_t (String_t, _), rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IConcat_string (loc, k))} in
      typed ctxt loc instr (Item_t (String_t, rest))
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t, Item_t (Nat_t, Item_t (String_t, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISlice_string (loc, k))} in
      let stack = Item_t (option_string_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_SIZE, [], annot), Item_t (String_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IString_size (loc, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  (* bytes operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (Bytes_t, (Item_t (Bytes_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IConcat_bytes_pair (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_CONCAT, [], annot), Item_t (List_t (Bytes_t, _), rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IConcat_bytes (loc, k))} in
      let stack = Item_t (Bytes_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t, Item_t (Nat_t, Item_t (Bytes_t, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISlice_bytes (loc, k))} in
      let stack = Item_t (option_bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_SIZE, [], annot), Item_t (Bytes_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBytes_size (loc, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_BYTES, [], annot), Item_t (Nat_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBytes_nat (loc, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_NAT, [], annot), Item_t (Bytes_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INat_bytes (loc, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_BYTES, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBytes_int (loc, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_INT, [], annot), Item_t (Bytes_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IInt_bytes (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  (* currency operations *)
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Mutez_t, (Item_t (Mutez_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_tez (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Mutez_t, (Item_t (Mutez_t, _) as stack)) ) ->
      if legacy then
        check_var_annot loc annot >>?= fun () ->
        let instr = {apply = (fun k -> ISub_tez_legacy (loc, k))} in
        typed ctxt loc instr stack
      else tzfail (Deprecated_instruction I_SUB)
  | Prim (loc, I_SUB_MUTEZ, [], annot), Item_t (Mutez_t, Item_t (Mutez_t, rest))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_tez (loc, k))} in
      let stack = Item_t (option_mutez_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Mutez_t, Item_t (Nat_t, rest)) ->
      (* no type name check *)
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_teznat (loc, k))} in
      let stack = Item_t (Mutez_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Nat_t, (Item_t (Mutez_t, _) as stack))
    ->
      (* no type name check *)
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_nattez (loc, k))} in
      typed ctxt loc instr stack
  (* boolean operations *)
  | Prim (loc, I_OR, [], annot), Item_t (Bool_t, (Item_t (Bool_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IOr (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_AND, [], annot), Item_t (Bool_t, (Item_t (Bool_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAnd (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_XOR, [], annot), Item_t (Bool_t, (Item_t (Bool_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IXor (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NOT, [], annot), (Item_t (Bool_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INot (loc, k))} in
      typed ctxt loc instr stack
  (* integer operations *)
  | Prim (loc, I_ABS, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAbs_int (loc, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_ISNAT, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IIs_nat (loc, k))} in
      let stack = Item_t (option_nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_INT, [], annot), Item_t (Nat_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IInt_nat (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_NEG, [], annot), (Item_t (Int_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeg (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NEG, [], annot), Item_t (Nat_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeg (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_ADD, [], annot), Item_t (Int_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_ADD, [], annot), Item_t (Int_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_int (loc, k))} in
      let stack = Item_t (Int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_ADD, [], annot), Item_t (Nat_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_ADD, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SUB, [], annot), Item_t (Int_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SUB, [], annot), Item_t (Int_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_int (loc, k))} in
      let stack = Item_t (Int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_SUB, [], annot), Item_t (Nat_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SUB, [], annot), Item_t (Nat_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISub_int (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Int_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Int_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_int (loc, k))} in
      let stack = Item_t (Int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Nat_t, (Item_t (Int_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Mutez_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_teznat (loc, k))} in
      let stack = Item_t (option_pair_mutez_mutez_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Mutez_t, Item_t (Mutez_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_tez (loc, k))} in
      let stack = Item_t (option_pair_nat_mutez_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Int_t, Item_t (Int_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_int (loc, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Int_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_int (loc, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Nat_t, Item_t (Int_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_nat (loc, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_EDIV, [], annot), Item_t (Nat_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEdiv_nat (loc, k))} in
      let stack = Item_t (option_pair_nat_nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_LSL, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILsl_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_LSL, [], annot), Item_t (Bytes_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILsl_bytes (loc, k))} in
      let stack = Item_t (Bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_LSR, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILsr_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_LSR, [], annot), Item_t (Bytes_t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILsr_bytes (loc, k))} in
      let stack = Item_t (Bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_OR, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IOr_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_OR, [], annot), Item_t (Bytes_t, (Item_t (Bytes_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IOr_bytes (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_AND, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAnd_nat (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_AND, [], annot), Item_t (Int_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAnd_int_nat (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_AND, [], annot),
      Item_t (Bytes_t, (Item_t (Bytes_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAnd_bytes (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_XOR, [], annot), Item_t (Nat_t, (Item_t (Nat_t, _) as stack))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IXor_nat (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_XOR, [], annot),
      Item_t (Bytes_t, (Item_t (Bytes_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IXor_bytes (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NOT, [], annot), (Item_t (Int_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INot_int (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NOT, [], annot), Item_t (Nat_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INot_int (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_NOT, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INot_bytes (loc, k))} in
      typed ctxt loc instr stack
  (* comparison *)
  | Prim (loc, I_COMPARE, [], annot), Item_t (t1, Item_t (t2, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      check_item_ty ctxt t1 t2 loc I_COMPARE 1 2 >>?= fun (Eq, ctxt) ->
      check_comparable loc t1 >>?= fun Eq ->
      let instr = {apply = (fun k -> ICompare (loc, t1, k))} in
      let stack = Item_t (int_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  (* comparators *)
  | Prim (loc, I_EQ, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IEq (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_NEQ, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeq (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_LT, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILt (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_GT, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IGt (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_LE, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILe (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_GE, [], annot), Item_t (Int_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IGe (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  (* annotations *)
  | Prim (loc, I_CAST, [cast_t], annot), (Item_t (t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy cast_t
      >>?= fun (Ex_ty cast_t, ctxt) ->
      Gas_monad.run ctxt @@ ty_eq ~error_details:(Informative loc) cast_t t
      >>?= fun (eq, ctxt) ->
      eq >>?= fun Eq ->
      (* We can reuse [stack] because [a ty = b ty] means [a = b]. *)
      let instr = {apply = (fun k -> k)} in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | Prim (loc, I_RENAME, [], annot), (Item_t _ as stack) ->
      check_var_annot loc annot >>?= fun () ->
      (* can erase annot *)
      let instr = {apply = (fun k -> k)} in
      typed ctxt loc instr stack
  (* packing *)
  | Prim (loc, I_PACK, [], annot), Item_t (t, rest) ->
      check_packable
        ~legacy:true
        (* allow to pack contracts for hash/signature checks *) loc
        t
      >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IPack (loc, t, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_UNPACK, [ty], annot), Item_t (Bytes_t, rest) ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      option_t loc t >>?= fun res_ty ->
      let instr = {apply = (fun k -> IUnpack (loc, t, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  (* protocol *)
  | Prim (loc, I_ADDRESS, [], annot), Item_t (Contract_t _, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAddress (loc, k))} in
      let stack = Item_t (address_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_CONTRACT, [ty], annot), Item_t (Address_t, rest) ->
      parse_passable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      contract_t loc t >>?= fun contract_ty ->
      option_t loc contract_ty >>?= fun res_ty ->
      parse_entrypoint_annot_strict loc annot >>?= fun entrypoint ->
      let instr = {apply = (fun k -> IContract (loc, t, entrypoint, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_VIEW, [name; output_ty], annot),
      Item_t (input_ty, Item_t (Address_t, rest)) ) ->
      let output_ty_loc = location output_ty in
      parse_view_name ctxt name >>?= fun (name, ctxt) ->
      parse_view_output_ty ctxt ~stack_depth:0 ~legacy output_ty
      >>?= fun (Ex_ty output_ty, ctxt) ->
      option_t output_ty_loc output_ty >>?= fun res_ty ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {
          apply =
            (fun k ->
              IView
                ( loc,
                  View_signature {name; input_ty; output_ty},
                  for_logging_only rest,
                  k ));
        }
      in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, (I_TRANSFER_TOKENS as prim), [], annot),
      Item_t (p, Item_t (Mutez_t, Item_t (Contract_t (cp, _), rest))) ) ->
      Tc_context.check_not_in_view loc ~legacy tc_context prim >>?= fun () ->
      check_item_ty ctxt p cp loc prim 1 4 >>?= fun (Eq, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ITransfer_tokens (loc, k))} in
      let stack = Item_t (operation_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, (I_SET_DELEGATE as prim), [], annot),
      Item_t (Option_t (Key_hash_t, _, _), rest) ) ->
      Tc_context.check_not_in_view loc ~legacy tc_context prim >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISet_delegate (loc, k))} in
      let stack = Item_t (operation_t, rest) in
      typed ctxt loc instr stack
  | Prim (_, I_CREATE_ACCOUNT, _, _), _ ->
      tzfail (Deprecated_instruction I_CREATE_ACCOUNT)
  | Prim (loc, I_IMPLICIT_ACCOUNT, [], annot), Item_t (Key_hash_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IImplicit_account (loc, k))} in
      let stack = Item_t (contract_unit_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, (I_CREATE_CONTRACT as prim), [(Seq _ as code)], annot),
      Item_t
        (Option_t (Key_hash_t, _, _), Item_t (Mutez_t, Item_t (ginit, rest))) )
    -> (
      Tc_context.check_not_in_view ~legacy loc tc_context prim >>?= fun () ->
      check_two_var_annot loc annot >>?= fun () ->
      (* We typecheck the script to make sure we will originate only well-typed
         contracts but then we throw away the typed version, except for the
         storage type which is kept for efficiency in the ticket scanner. *)
      let canonical_code = Micheline.strip_locations code in
      parse_toplevel ctxt ~legacy canonical_code
      >>?= fun ({arg_type; storage_type; code_field; views}, ctxt) ->
      record_trace
        (Ill_formed_type (Some "parameter", canonical_code, location arg_type))
        (parse_parameter_ty_and_entrypoints
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           arg_type)
      >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, ctxt)
        ->
      record_trace
        (Ill_formed_type (Some "storage", canonical_code, location storage_type))
        (parse_storage_ty
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           storage_type)
      >>?= fun (Ex_ty storage_type, ctxt) ->
      pair_t loc arg_type storage_type >>?= fun (Ty_ex_c arg_type_full) ->
      pair_t loc list_operation_t storage_type
      >>?= fun (Ty_ex_c ret_type_full) ->
      trace
        (Ill_typed_contract (canonical_code, []))
        (parse_kdescr
           ~unparse_code_rec
           (Tc_context.toplevel ~storage_type ~param_type:arg_type ~entrypoints)
           ctxt
           ~elab_conf
           ~stack_depth:(stack_depth + 1)
           arg_type_full
           ret_type_full
           code_field)
      >>=? function
      | {kbef = Item_t (arg, Bot_t); kaft = Item_t (ret, Bot_t); _}, ctxt ->
          let views_result =
            parse_views ~unparse_code_rec ctxt ~elab_conf storage_type views
          in
          trace (Ill_typed_contract (canonical_code, [])) views_result
          >>=? fun (_typed_views, ctxt) ->
          (let error_details = Informative loc in
           Gas_monad.run ctxt
           @@
           let open Gas_monad.Syntax in
           let* Eq = ty_eq ~error_details arg arg_type_full in
           let* Eq = ty_eq ~error_details ret ret_type_full in
           ty_eq ~error_details storage_type ginit)
          >>?= fun (storage_eq, ctxt) ->
          storage_eq >>?= fun Eq ->
          let instr =
            {
              apply =
                (fun k ->
                  ICreate_contract {loc; storage_type; code = canonical_code; k});
            }
          in
          let stack = Item_t (operation_t, Item_t (address_t, rest)) in
          typed ctxt loc instr stack)
  | Prim (loc, I_NOW, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INow (loc, k))} in
      let stack = Item_t (timestamp_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_MIN_BLOCK_TIME, [], _), stack ->
      typed
        ctxt
        loc
        {apply = (fun k -> IMin_block_time (loc, k))}
        (Item_t (nat_t, stack))
  | Prim (loc, I_AMOUNT, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAmount (loc, k))} in
      let stack = Item_t (mutez_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_CHAIN_ID, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IChainId (loc, k))} in
      let stack = Item_t (chain_id_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_BALANCE, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBalance (loc, k))} in
      let stack = Item_t (mutez_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_LEVEL, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ILevel (loc, k))} in
      let stack = Item_t (nat_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_VOTING_POWER, [], annot), Item_t (Key_hash_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IVoting_power (loc, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_TOTAL_VOTING_POWER, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ITotal_voting_power (loc, k))} in
      let stack = Item_t (nat_t, stack) in
      typed ctxt loc instr stack
  | Prim (_, I_STEPS_TO_QUOTA, _, _), _ ->
      tzfail (Deprecated_instruction I_STEPS_TO_QUOTA)
  | Prim (loc, I_SOURCE, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISource (loc, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, I_SENDER, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISender (loc, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  | Prim (loc, (I_SELF as prim), [], annot), stack ->
      Lwt.return
        ( parse_entrypoint_annot_lax loc annot >>? fun entrypoint ->
          let open Tc_context in
          match tc_context.callsite with
          | _ when is_in_lambda tc_context ->
              error
                (Forbidden_instr_in_context (loc, Script_tc_errors.Lambda, prim))
          (* [Data] is for pushed instructions of lambda type. *)
          | Data ->
              error
                (Forbidden_instr_in_context (loc, Script_tc_errors.Lambda, prim))
          | View ->
              error
                (Forbidden_instr_in_context (loc, Script_tc_errors.View, prim))
          | Toplevel {param_type; entrypoints; storage_type = _} ->
              Gas_monad.run ctxt
              @@ find_entrypoint
                   ~error_details:(Informative ())
                   param_type
                   entrypoints
                   entrypoint
              >>? fun (r, ctxt) ->
              r >>? fun (Ex_ty_cstr {ty = param_type; _}) ->
              contract_t loc param_type >>? fun res_ty ->
              let instr =
                {apply = (fun k -> ISelf (loc, param_type, entrypoint, k))}
              in
              let stack = Item_t (res_ty, stack) in
              typed_no_lwt ctxt loc instr stack )
  | Prim (loc, I_SELF_ADDRESS, [], annot), stack ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISelf_address (loc, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  (* cryptography *)
  | Prim (loc, I_HASH_KEY, [], annot), Item_t (Key_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IHash_key (loc, k))} in
      let stack = Item_t (key_hash_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_CHECK_SIGNATURE, [], annot),
      Item_t (Key_t, Item_t (Signature_t, Item_t (Bytes_t, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ICheck_signature (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_BLAKE2B, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IBlake2b (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SHA256, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISha256 (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SHA512, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISha512 (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_KECCAK, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IKeccak (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_SHA3, [], annot), (Item_t (Bytes_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> ISha3 (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g1_t, (Item_t (Bls12_381_g1_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_bls12_381_g1 (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g2_t, (Item_t (Bls12_381_g2_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_bls12_381_g2 (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_fr_t, (Item_t (Bls12_381_fr_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IAdd_bls12_381_fr (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g1_t, Item_t (Bls12_381_fr_t, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_g1 (loc, k))} in
      let stack = Item_t (Bls12_381_g1_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g2_t, Item_t (Bls12_381_fr_t, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_g2 (loc, k))} in
      let stack = Item_t (Bls12_381_g2_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t, (Item_t (Bls12_381_fr_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_fr (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t, (Item_t (Bls12_381_fr_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_fr_z (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Int_t, (Item_t (Bls12_381_fr_t, _) as stack)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_fr_z (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Bls12_381_fr_t, Item_t (Int_t, rest))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_z_fr (loc, k))} in
      let stack = Item_t (Bls12_381_fr_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_MUL, [], annot), Item_t (Bls12_381_fr_t, Item_t (Nat_t, rest))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IMul_bls12_381_z_fr (loc, k))} in
      let stack = Item_t (Bls12_381_fr_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_INT, [], annot), Item_t (Bls12_381_fr_t, rest) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IInt_bls12_381_fr (loc, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_NEG, [], annot), (Item_t (Bls12_381_g1_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeg_bls12_381_g1 (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NEG, [], annot), (Item_t (Bls12_381_g2_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeg_bls12_381_g2 (loc, k))} in
      typed ctxt loc instr stack
  | Prim (loc, I_NEG, [], annot), (Item_t (Bls12_381_fr_t, _) as stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> INeg_bls12_381_fr (loc, k))} in
      typed ctxt loc instr stack
  | ( Prim (loc, I_PAIRING_CHECK, [], annot),
      Item_t (List_t (Pair_t (Bls12_381_g1_t, Bls12_381_g2_t, _, _), _), rest) )
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun k -> IPairing_check_bls12_381 (loc, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  (* Tickets *)
  | Prim (loc, I_TICKET, [], annot), Item_t (t, Item_t (Nat_t, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      check_comparable loc t >>?= fun Eq ->
      ticket_t loc t >>?= fun res_ty ->
      let instr = {apply = (fun k -> ITicket (loc, for_logging_only t, k))} in
      option_t loc res_ty >>?= fun res_ty ->
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | Prim (loc, I_TICKET_DEPRECATED, [], annot), Item_t (t, Item_t (Nat_t, rest))
    ->
      if legacy then
        check_var_annot loc annot >>?= fun () ->
        check_comparable loc t >>?= fun Eq ->
        ticket_t loc t >>?= fun res_ty ->
        let instr =
          {apply = (fun k -> ITicket_deprecated (loc, for_logging_only t, k))}
        in
        let stack = Item_t (res_ty, rest) in
        typed ctxt loc instr stack
      else tzfail (Deprecated_instruction I_TICKET_DEPRECATED)
  | ( Prim (loc, I_READ_TICKET, [], annot),
      (Item_t (Ticket_t (t, _), _) as full_stack) ) ->
      check_var_annot loc annot >>?= fun () ->
      let () = check_dupable_comparable_ty t in
      opened_ticket_type loc t >>?= fun result ->
      let instr =
        {apply = (fun k -> IRead_ticket (loc, for_logging_only t, k))}
      in
      let stack = Item_t (result, full_stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SPLIT_TICKET, [], annot),
      Item_t
        ( (Ticket_t (t, _) as ticket_t),
          Item_t (Pair_t (Nat_t, Nat_t, _, _), rest) ) ) ->
      check_var_annot loc annot >>?= fun () ->
      let () = check_dupable_comparable_ty t in
      pair_t loc ticket_t ticket_t >>?= fun (Ty_ex_c pair_tickets_ty) ->
      option_t loc pair_tickets_ty >>?= fun res_ty ->
      let instr = {apply = (fun k -> ISplit_ticket (loc, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_JOIN_TICKETS, [], annot),
      Item_t
        ( Pair_t
            ( (Ticket_t (contents_ty_a, _) as ty_a),
              Ticket_t (contents_ty_b, _),
              _,
              _ ),
          rest ) ) ->
      check_var_annot loc annot >>?= fun () ->
      Gas_monad.run ctxt
      @@ ty_eq ~error_details:(Informative loc) contents_ty_a contents_ty_b
      >>?= fun (eq, ctxt) ->
      eq >>?= fun Eq ->
      option_t loc ty_a >>?= fun res_ty ->
      let instr = {apply = (fun k -> IJoin_tickets (loc, contents_ty_a, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  (* Timelocks *)
  | ( Prim (loc, I_OPEN_CHEST, [], _),
      Item_t (Chest_key_t, Item_t (Chest_t, Item_t (Nat_t, rest))) ) ->
      let instr = {apply = (fun k -> IOpen_chest (loc, k))} in
      typed ctxt loc instr (Item_t (or_bytes_bool_t, rest))
  (* Events *)
  | Prim (loc, I_EMIT, [], annot), Item_t (data, rest) ->
      check_packable ~legacy loc data >>?= fun () ->
      parse_entrypoint_annot_strict loc annot >>?= fun tag ->
      unparse_ty ~loc:() ctxt data >>?= fun (unparsed_ty, ctxt) ->
      Gas.consume ctxt (Script.strip_locations_cost unparsed_ty)
      >>?= fun ctxt ->
      let unparsed_ty = Micheline.strip_locations unparsed_ty in
      let instr =
        {apply = (fun k -> IEmit {loc; tag; ty = data; unparsed_ty; k})}
      in
      typed ctxt loc instr (Item_t (Operation_t, rest))
  | Prim (loc, I_EMIT, [ty_node], annot), Item_t (data, rest) ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty_node
      >>?= fun (Ex_ty ty, ctxt) ->
      check_item_ty ctxt ty data loc I_EMIT 1 2 >>?= fun (Eq, ctxt) ->
      parse_entrypoint_annot_strict loc annot >>?= fun tag ->
      Gas.consume ctxt (Script.strip_locations_cost ty_node) >>?= fun ctxt ->
      let unparsed_ty = Micheline.strip_locations ty_node in
      let instr =
        {apply = (fun k -> IEmit {loc; tag; ty = data; unparsed_ty; k})}
      in
      typed ctxt loc instr (Item_t (Operation_t, rest))
  (* Primitive parsing errors *)
  | ( Prim
        ( loc,
          (( I_DUP | I_SWAP | I_SOME | I_UNIT | I_PAIR | I_UNPAIR | I_CAR
           | I_CDR | I_CONS | I_CONCAT | I_SLICE | I_MEM | I_UPDATE | I_GET
           | I_EXEC | I_FAILWITH | I_SIZE | I_ADD | I_SUB | I_SUB_MUTEZ | I_MUL
           | I_EDIV | I_OR | I_AND | I_XOR | I_NOT | I_ABS | I_NEG | I_LSL
           | I_LSR | I_COMPARE | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE
           | I_TRANSFER_TOKENS | I_SET_DELEGATE | I_NOW | I_MIN_BLOCK_TIME
           | I_IMPLICIT_ACCOUNT | I_AMOUNT | I_BALANCE | I_LEVEL
           | I_CHECK_SIGNATURE | I_HASH_KEY | I_SOURCE | I_SENDER | I_BLAKE2B
           | I_SHA256 | I_SHA512 | I_ADDRESS | I_RENAME | I_PACK | I_ISNAT
           | I_INT | I_SELF | I_CHAIN_ID | I_NEVER | I_VOTING_POWER
           | I_TOTAL_VOTING_POWER | I_KECCAK | I_SHA3 | I_PAIRING_CHECK
           | I_TICKET | I_READ_TICKET | I_SPLIT_TICKET | I_JOIN_TICKETS
           | I_OPEN_CHEST ) as name),
          (_ :: _ as l),
          _ ),
      _ ) ->
      tzfail (Invalid_arity (loc, name, 0, List.length l))
  | ( Prim
        ( loc,
          (( I_NONE | I_LEFT | I_RIGHT | I_NIL | I_MAP | I_ITER | I_EMPTY_SET
           | I_LOOP | I_LOOP_LEFT | I_CONTRACT | I_CAST | I_UNPACK
           | I_CREATE_CONTRACT | I_EMIT ) as name),
          (([] | _ :: _ :: _) as l),
          _ ),
      _ ) ->
      tzfail (Invalid_arity (loc, name, 1, List.length l))
  | ( Prim
        ( loc,
          (( I_PUSH | I_VIEW | I_IF_NONE | I_IF_LEFT | I_IF_CONS | I_EMPTY_MAP
           | I_EMPTY_BIG_MAP | I_IF ) as name),
          (([] | [_] | _ :: _ :: _ :: _) as l),
          _ ),
      _ ) ->
      tzfail (Invalid_arity (loc, name, 2, List.length l))
  | ( Prim (loc, I_LAMBDA, (([] | [_] | [_; _] | _ :: _ :: _ :: _ :: _) as l), _),
      _ ) ->
      tzfail (Invalid_arity (loc, I_LAMBDA, 3, List.length l))
  (* Stack errors *)
  | ( Prim
        ( loc,
          (( I_ADD | I_SUB | I_SUB_MUTEZ | I_MUL | I_EDIV | I_AND | I_OR | I_XOR
           | I_LSL | I_LSR | I_CONCAT | I_PAIRING_CHECK ) as name),
          [],
          _ ),
      Item_t (ta, Item_t (tb, _)) ) ->
      let ta = serialize_ty_for_error ta in
      let tb = serialize_ty_for_error tb in
      tzfail (Undefined_binop (loc, name, ta, tb))
  | ( Prim
        ( loc,
          (( I_NEG | I_ABS | I_NOT | I_SIZE | I_EQ | I_NEQ | I_LT | I_GT | I_LE
           | I_GE
           (* CONCAT is both unary and binary; this case can only be triggered
               on a singleton stack *)
           | I_CONCAT ) as name),
          [],
          _ ),
      Item_t (t, _) ) ->
      let t = serialize_ty_for_error t in
      tzfail (Undefined_unop (loc, name, t))
  | Prim (loc, ((I_UPDATE | I_SLICE | I_OPEN_CHEST) as name), [], _), stack ->
      Lwt.return
        (let stack = serialize_stack_for_error ctxt stack in
         error (Bad_stack (loc, name, 3, stack)))
  | Prim (loc, I_CREATE_CONTRACT, _, _), stack ->
      let stack = serialize_stack_for_error ctxt stack in
      tzfail (Bad_stack (loc, I_CREATE_CONTRACT, 7, stack))
  | Prim (loc, I_TRANSFER_TOKENS, [], _), stack ->
      Lwt.return
        (let stack = serialize_stack_for_error ctxt stack in
         error (Bad_stack (loc, I_TRANSFER_TOKENS, 4, stack)))
  | ( Prim
        ( loc,
          (( I_DROP | I_DUP | I_CAR | I_CDR | I_UNPAIR | I_SOME | I_BLAKE2B
           | I_SHA256 | I_SHA512 | I_DIP | I_IF_NONE | I_LEFT | I_RIGHT
           | I_IF_LEFT | I_IF | I_LOOP | I_IF_CONS | I_IMPLICIT_ACCOUNT | I_NEG
           | I_ABS | I_INT | I_NOT | I_HASH_KEY | I_EQ | I_NEQ | I_LT | I_GT
           | I_LE | I_GE | I_SIZE | I_FAILWITH | I_RENAME | I_PACK | I_ISNAT
           | I_ADDRESS | I_SET_DELEGATE | I_CAST | I_MAP | I_ITER | I_LOOP_LEFT
           | I_UNPACK | I_CONTRACT | I_NEVER | I_KECCAK | I_SHA3 | I_READ_TICKET
           | I_JOIN_TICKETS ) as name),
          _,
          _ ),
      stack ) ->
      Lwt.return
        (let stack = serialize_stack_for_error ctxt stack in
         error (Bad_stack (loc, name, 1, stack)))
  | ( Prim
        ( loc,
          (( I_SWAP | I_PAIR | I_CONS | I_GET | I_MEM | I_EXEC
           | I_CHECK_SIGNATURE | I_ADD | I_SUB | I_SUB_MUTEZ | I_MUL | I_EDIV
           | I_AND | I_OR | I_XOR | I_LSL | I_LSR | I_COMPARE | I_PAIRING_CHECK
           | I_TICKET | I_SPLIT_TICKET ) as name),
          _,
          _ ),
      stack ) ->
      Lwt.return
        (let stack = serialize_stack_for_error ctxt stack in
         error (Bad_stack (loc, name, 2, stack)))
  (* Generic parsing errors *)
  | expr, _ ->
      tzfail
      @@ unexpected
           expr
           [Seq_kind]
           Instr_namespace
           [
             I_ABS;
             I_ADD;
             I_AMOUNT;
             I_AND;
             I_BALANCE;
             I_BLAKE2B;
             I_CAR;
             I_CDR;
             I_CHECK_SIGNATURE;
             I_COMPARE;
             I_CONCAT;
             I_CONS;
             I_CREATE_CONTRACT;
             I_DIG;
             I_DIP;
             I_DROP;
             I_DUG;
             I_DUP;
             I_EDIV;
             I_EMPTY_BIG_MAP;
             I_EMPTY_MAP;
             I_EMPTY_SET;
             I_EQ;
             I_EXEC;
             I_FAILWITH;
             I_GE;
             I_GET;
             I_GET_AND_UPDATE;
             I_GT;
             I_HASH_KEY;
             I_IF;
             I_IF_CONS;
             I_IF_LEFT;
             I_IF_NONE;
             I_IMPLICIT_ACCOUNT;
             I_INT;
             I_ITER;
             I_JOIN_TICKETS;
             I_KECCAK;
             I_LAMBDA;
             I_LE;
             I_LEFT;
             I_LEVEL;
             I_LOOP;
             I_LSL;
             I_LSR;
             I_LT;
             I_MAP;
             I_MEM;
             I_MIN_BLOCK_TIME;
             I_MUL;
             I_NEG;
             I_NEQ;
             I_NEVER;
             I_NIL;
             I_NONE;
             I_NOT;
             I_NOW;
             I_OPEN_CHEST;
             I_OR;
             I_PAIR;
             I_PAIRING_CHECK;
             I_PUSH;
             I_READ_TICKET;
             I_RIGHT;
             I_SAPLING_EMPTY_STATE;
             I_SAPLING_VERIFY_UPDATE;
             I_SELF;
             I_SELF_ADDRESS;
             I_SENDER;
             I_SHA256;
             I_SHA3;
             I_SHA512;
             I_SIZE;
             I_SOME;
             I_SOURCE;
             I_SPLIT_TICKET;
             I_SUB;
             I_SUB_MUTEZ;
             I_SWAP;
             I_TICKET;
             I_TOTAL_VOTING_POWER;
             I_TRANSFER_TOKENS;
             I_UNIT;
             I_UNPAIR;
             I_UPDATE;
             I_VIEW;
             I_VOTING_POWER;
             I_XOR;
           ]

and parse_contract_data :
    type arg argc.
    stack_depth:int ->
    context ->
    Script.location ->
    (arg, argc) ty ->
    Destination.t ->
    entrypoint:Entrypoint.t ->
    (context * arg typed_contract) tzresult Lwt.t =
 fun ~stack_depth ctxt loc arg destination ~entrypoint ->
  let error_details = Informative loc in
  parse_contract
    ~stack_depth:(stack_depth + 1)
    ctxt
    ~error_details
    loc
    arg
    destination
    ~entrypoint
  >>=? fun (ctxt, res) -> Lwt.return (res >|? fun res -> (ctxt, res))

(* [parse_contract] is used both to:
   - parse contract data by [parse_data] ([parse_contract_data])
   - to execute the [CONTRACT] instruction ([parse_contract_for_script]).

   The return type resembles the [Gas_monad]:
   - the outer [tzresult] is for gas exhaustion and internal errors
   - the inner [result] is for other legitimate cases of failure.

   The inner [result] is turned into an [option] by [parse_contract_for_script].
   Both [tzresult] are merged by [parse_contract_data].
*)
and parse_contract :
    type arg argc err.
    stack_depth:int ->
    context ->
    error_details:(location, err) error_details ->
    Script.location ->
    (arg, argc) ty ->
    Destination.t ->
    entrypoint:Entrypoint.t ->
    (context * (arg typed_contract, err) result) tzresult Lwt.t =
 fun ~stack_depth ctxt ~error_details loc arg destination ~entrypoint ->
  let error ctxt f_err : context * (_, err) result =
    ( ctxt,
      Error
        (match error_details with
        | Fast -> (Inconsistent_types_fast : err)
        | Informative loc -> trace_of_error @@ f_err loc) )
  in
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>?= fun ctxt ->
  match destination with
  | Contract contract -> (
      match contract with
      | Implicit destination ->
          Lwt.return
          @@
          if Entrypoint.is_default entrypoint then
            (* An implicit account on the "default" entrypoint always exists and has type unit
               or (ticket cty). *)
            let typecheck =
              let open Gas_monad.Syntax in
              let* () = Gas_monad.consume_gas Typecheck_costs.merge_cycle in
              match arg with
              | Unit_t ->
                  return (Typed_implicit destination : arg typed_contract)
              | Ticket_t _ as ticket_ty ->
                  return (Typed_implicit_with_ticket {ticket_ty; destination})
              | _ ->
                  Gas_monad.of_result
                  @@ Error
                       (match error_details with
                       | Fast -> (Inconsistent_types_fast : err)
                       | Informative loc ->
                           trace_of_error @@ default_ty_eq_error loc arg unit_t)
            in
            Gas_monad.run ctxt typecheck >|? fun (v, ctxt) -> (ctxt, v)
          else
            (* An implicit account on any other entrypoint is not a valid contract. *)
            ok @@ error ctxt (fun _loc -> No_such_entrypoint entrypoint)
      | Originated contract_hash ->
          trace
            (Invalid_contract (loc, contract))
            ( Contract.get_script_code ctxt contract_hash
            >>=? fun (ctxt, code) ->
              Lwt.return
                (match code with
                | None ->
                    ok
                      (error ctxt (fun loc -> Invalid_contract (loc, contract)))
                | Some code ->
                    Script.force_decode_in_context
                      ~consume_deserialization_gas:When_needed
                      ctxt
                      code
                    >>? fun (code, ctxt) ->
                    (* can only fail because of gas *)
                    parse_toplevel ctxt ~legacy:true code
                    >>? fun ({arg_type; _}, ctxt) ->
                    parse_parameter_ty_and_entrypoints
                      ctxt
                      ~stack_depth:(stack_depth + 1)
                      ~legacy:true
                      arg_type
                    >>? fun ( Ex_parameter_ty_and_entrypoints
                                {arg_type = targ; entrypoints},
                              ctxt ) ->
                    Gas_monad.run ctxt
                    @@ find_entrypoint_for_type
                         ~error_details
                         ~full:targ
                         ~expected:arg
                         entrypoints
                         entrypoint
                    >|? fun (entrypoint_arg, ctxt) ->
                    ( ctxt,
                      entrypoint_arg >|? fun (entrypoint, arg_ty) ->
                      Typed_originated {arg_ty; contract_hash; entrypoint} )) ))
  | Zk_rollup zk_rollup ->
      Zk_rollup.assert_exist ctxt zk_rollup >|=? fun ctxt ->
      if Entrypoint.(is_deposit entrypoint) then
        match arg with
        | Pair_t (Ticket_t (_, _), Bytes_t, _, _) ->
            ( ctxt,
              ok
              @@ (Typed_zk_rollup {arg_ty = arg; zk_rollup}
                   : arg typed_contract) )
        | _ ->
            error ctxt (fun loc ->
                Zk_rollup_bad_deposit_parameter (loc, serialize_ty_for_error arg))
      else error ctxt (fun _loc -> No_such_entrypoint entrypoint)
  | Sc_rollup sc_rollup ->
      Sc_rollup.parameters_type ctxt sc_rollup
      >>=? fun (parameters_type, ctxt) ->
      Lwt.return
        (match parameters_type with
        | None ->
            ok
              (error ctxt (fun _loc ->
                   Sc_rollup.Errors.Sc_rollup_does_not_exist sc_rollup))
        | Some parameters_type ->
            Script.force_decode_in_context
              ~consume_deserialization_gas:When_needed
              ctxt
              parameters_type
            >>? fun (parameters_type, ctxt) ->
            parse_parameter_ty_and_entrypoints
              ctxt
              ~stack_depth:(stack_depth + 1)
              ~legacy:true
              (root parameters_type)
            >>? fun ( Ex_parameter_ty_and_entrypoints
                        {arg_type = full; entrypoints},
                      ctxt ) ->
            Gas_monad.run ctxt
            @@ find_entrypoint_for_type
                 ~error_details
                 ~full
                 ~expected:arg
                 entrypoints
                 entrypoint
            >|? fun (entrypoint_arg, ctxt) ->
            ( ctxt,
              entrypoint_arg >|? fun (entrypoint, arg_ty) ->
              Typed_sc_rollup {arg_ty; sc_rollup; entrypoint} ))

(* Same as [parse_contract], but does not fail when the contact is missing or
   if the expected type doesn't match the actual one. In that case None is
   returned and some overapproximation of the typechecking gas is consumed.
   This can still fail on gas exhaustion. *)
let parse_contract_for_script :
    type arg argc.
    context ->
    Script.location ->
    (arg, argc) ty ->
    Destination.t ->
    entrypoint:Entrypoint.t ->
    (context * arg typed_contract option) tzresult Lwt.t =
 fun ctxt loc arg destination ~entrypoint ->
  parse_contract
    ~stack_depth:0
    ctxt
    ~error_details:Fast
    loc
    arg
    destination
    ~entrypoint
  >|=? fun (ctxt, res) ->
  ( ctxt,
    match res with Ok res -> Some res | Error Inconsistent_types_fast -> None )

let view_size view =
  let open Script_typed_ir_size in
  node_size view.view_code ++ node_size view.input_ty
  ++ node_size view.output_ty

let code_size ctxt code views =
  let open Script_typed_ir_size in
  let views_size = Script_map.fold (fun _ v s -> view_size v ++ s) views zero in
  (* The size of the storage_type and the arg_type is counted by
     [lambda_size]. *)
  let ir_size = lambda_size code in
  let nodes, code_size = views_size ++ ir_size in
  (* We consume gas after the fact in order to not have to instrument
     [node_size] (for efficiency).
     This is safe, as we already pay gas proportional to [views_size] and
     [ir_size] during their typechecking. *)
  Gas.consume ctxt (Script_typed_ir_size_costs.nodes_cost ~nodes)
  >|? fun ctxt -> (code_size, ctxt)

let parse_code :
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    context ->
    code:lazy_expr ->
    (ex_code * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~elab_conf ctxt ~code ->
  Script.force_decode_in_context
    ~consume_deserialization_gas:When_needed
    ctxt
    code
  >>?= fun (code, ctxt) ->
  let legacy = elab_conf.legacy in
  Global_constants_storage.expand ctxt code >>=? fun (ctxt, code) ->
  parse_toplevel ctxt ~legacy code
  >>?= fun ({arg_type; storage_type; code_field; views}, ctxt) ->
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty_and_entrypoints ctxt ~stack_depth:0 ~legacy arg_type)
  >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, ctxt) ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (Ex_ty storage_type, ctxt) ->
  pair_t storage_type_loc arg_type storage_type
  >>?= fun (Ty_ex_c arg_type_full) ->
  pair_t storage_type_loc list_operation_t storage_type
  >>?= fun (Ty_ex_c ret_type_full) ->
  trace
    (Ill_typed_contract (code, []))
    (parse_kdescr
       ~unparse_code_rec
       Tc_context.(toplevel ~storage_type ~param_type:arg_type ~entrypoints)
       ~elab_conf
       ctxt
       ~stack_depth:0
       arg_type_full
       ret_type_full
       code_field)
  >>=? fun (kdescr, ctxt) ->
  let code = Lam (kdescr, code_field) in
  Lwt.return
    ( code_size ctxt code views >>? fun (code_size, ctxt) ->
      ok
        ( Ex_code
            (Code {code; arg_type; storage_type; views; entrypoints; code_size}),
          ctxt ) )

let parse_storage :
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    context ->
    allow_forged:bool ->
    ('storage, _) ty ->
    storage:lazy_expr ->
    ('storage * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~elab_conf ctxt ~allow_forged storage_type ~storage ->
  Script.force_decode_in_context
    ~consume_deserialization_gas:When_needed
    ctxt
    storage
  >>?= fun (storage, ctxt) ->
  trace_eval
    (fun () ->
      let storage_type = serialize_ty_for_error storage_type in
      Ill_typed_data (None, storage, storage_type))
    (parse_data
       ~unparse_code_rec
       ~elab_conf
       ~stack_depth:0
       ctxt
       ~allow_forged
       storage_type
       (root storage))

let parse_script :
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    elab_conf:elab_conf ->
    context ->
    allow_forged_in_storage:bool ->
    Script.t ->
    (ex_script * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~elab_conf ctxt ~allow_forged_in_storage {code; storage} ->
  parse_code ~unparse_code_rec ~elab_conf ctxt ~code
  >>=? fun ( Ex_code
               (Code
                 {code; arg_type; storage_type; views; entrypoints; code_size}),
             ctxt ) ->
  parse_storage
    ~unparse_code_rec
    ~elab_conf
    ctxt
    ~allow_forged:allow_forged_in_storage
    storage_type
    ~storage
  >|=? fun (storage, ctxt) ->
  ( Ex_script
      (Script
         {code_size; code; arg_type; storage; storage_type; views; entrypoints}),
    ctxt )

type typechecked_code_internal =
  | Typechecked_code_internal : {
      toplevel : toplevel;
      arg_type : ('arg, _) ty;
      storage_type : ('storage, _) ty;
      entrypoints : 'arg entrypoints;
      typed_views : 'storage typed_view_map;
      type_map : type_map;
    }
      -> typechecked_code_internal

let typecheck_code :
    unparse_code_rec:Script_ir_unparser.unparse_code_rec ->
    legacy:bool ->
    show_types:bool ->
    context ->
    Script.expr ->
    (typechecked_code_internal * context) tzresult Lwt.t =
 fun ~unparse_code_rec ~legacy ~show_types ctxt code ->
  (* Constants need to be expanded or [parse_toplevel] may fail. *)
  Global_constants_storage.expand ctxt code >>=? fun (ctxt, code) ->
  parse_toplevel ctxt ~legacy code >>?= fun (toplevel, ctxt) ->
  let {arg_type; storage_type; code_field; views} = toplevel in
  let type_map = ref [] in
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty_and_entrypoints ctxt ~stack_depth:0 ~legacy arg_type)
  >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; entrypoints}, ctxt) ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (ex_storage_type, ctxt) ->
  let (Ex_ty storage_type) = ex_storage_type in
  pair_t storage_type_loc arg_type storage_type
  >>?= fun (Ty_ex_c arg_type_full) ->
  pair_t storage_type_loc list_operation_t storage_type
  >>?= fun (Ty_ex_c ret_type_full) ->
  let type_logger loc ~stack_ty_before ~stack_ty_after =
    type_map := (loc, (stack_ty_before, stack_ty_after)) :: !type_map
  in
  let type_logger = if show_types then Some type_logger else None in
  let elab_conf = Script_ir_translator_config.make ~legacy ?type_logger () in
  let result =
    parse_kdescr
      ~unparse_code_rec
      (Tc_context.toplevel ~storage_type ~param_type:arg_type ~entrypoints)
      ctxt
      ~elab_conf
      ~stack_depth:0
      arg_type_full
      ret_type_full
      code_field
  in
  trace (Ill_typed_contract (code, !type_map)) result
  >>=? fun ((_ : (_, _, _, _) kdescr), ctxt) ->
  let views_result =
    parse_views ~unparse_code_rec ctxt ~elab_conf storage_type views
  in
  trace (Ill_typed_contract (code, !type_map)) views_result
  >|=? fun (typed_views, ctxt) ->
  ( Typechecked_code_internal
      {
        toplevel;
        arg_type;
        storage_type;
        entrypoints;
        typed_views;
        type_map = !type_map;
      },
    ctxt )

(* Uncarbonated because used only in RPCs *)
let list_entrypoints_uncarbonated (type full fullc) (full : (full, fullc) ty)
    (entrypoints : full entrypoints) =
  let merge path (type t tc) (ty : (t, tc) ty)
      (entrypoints : t entrypoints_node) reachable ((unreachables, all) as acc)
      =
    match entrypoints.at_node with
    | None ->
        ( (if reachable then acc
          else
            match ty with
            | Or_t _ -> acc
            | _ -> (List.rev path :: unreachables, all)),
          reachable )
    | Some {name; original_type_expr} ->
        ( (if Entrypoint.Map.mem name all then
           (List.rev path :: unreachables, all)
          else
            ( unreachables,
              Entrypoint.Map.add name (Ex_ty ty, original_type_expr) all )),
          true )
  in
  let rec fold_tree :
      type t tc.
      (t, tc) ty ->
      t entrypoints_node ->
      prim list ->
      bool ->
      prim list list * (ex_ty * Script.node) Entrypoint.Map.t ->
      prim list list * (ex_ty * Script.node) Entrypoint.Map.t =
   fun t entrypoints path reachable acc ->
    match (t, entrypoints) with
    | Or_t (tl, tr, _, _), {nested = Entrypoints_Or {left; right}; _} ->
        let acc, l_reachable = merge (D_Left :: path) tl left reachable acc in
        let acc, r_reachable = merge (D_Right :: path) tr right reachable acc in
        let acc = fold_tree tl left (D_Left :: path) l_reachable acc in
        fold_tree tr right (D_Right :: path) r_reachable acc
    | _ -> acc
  in
  let init, reachable =
    match entrypoints.root.at_node with
    | None -> (Entrypoint.Map.empty, false)
    | Some {name; original_type_expr} ->
        (Entrypoint.Map.singleton name (Ex_ty full, original_type_expr), true)
  in
  fold_tree full entrypoints.root [] reachable ([], init)

include Data_unparser (struct
  let opened_ticket_type = opened_ticket_type

  let parse_packable_ty = parse_packable_ty

  let parse_data = parse_data
end)

let unparse_code_rec : unparse_code_rec =
 fun ctxt ~stack_depth mode node ->
  unparse_code ctxt ~stack_depth mode node >>=? fun (code, ctxt) ->
  return (Micheline.root code, ctxt)

let parse_and_unparse_script_unaccounted ctxt ~legacy ~allow_forged_in_storage
    mode ~normalize_types {code; storage} =
  Script.force_decode_in_context
    ~consume_deserialization_gas:When_needed
    ctxt
    code
  >>?= fun (code, ctxt) ->
  typecheck_code ~unparse_code_rec ~legacy ~show_types:false ctxt code
  >>=? fun ( Typechecked_code_internal
               {
                 toplevel =
                   {
                     code_field;
                     arg_type = original_arg_type_expr;
                     storage_type = original_storage_type_expr;
                     views;
                   };
                 arg_type;
                 storage_type;
                 entrypoints;
                 typed_views;
                 type_map = _;
               },
             ctxt ) ->
  parse_storage
    ~unparse_code_rec
    ~elab_conf:(Script_ir_translator_config.make ~legacy ())
    ctxt
    ~allow_forged:allow_forged_in_storage
    storage_type
    ~storage
  >>=? fun (storage, ctxt) ->
  unparse_code ctxt ~stack_depth:0 mode code_field >>=? fun (code, ctxt) ->
  unparse_data ctxt ~stack_depth:0 mode storage_type storage
  >>=? fun (storage, ctxt) ->
  let loc = Micheline.dummy_location in
  (if normalize_types then
   unparse_parameter_ty ~loc ctxt arg_type ~entrypoints
   >>?= fun (arg_type, ctxt) ->
   unparse_ty ~loc ctxt storage_type >>?= fun (storage_type, ctxt) ->
   Script_map.map_es_in_context
     (fun ctxt
          _name
          (Typed_view {input_ty; output_ty; kinstr = _; original_code_expr}) ->
       Lwt.return
         ( unparse_ty ~loc ctxt input_ty >>? fun (input_ty, ctxt) ->
           unparse_ty ~loc ctxt output_ty >|? fun (output_ty, ctxt) ->
           ({input_ty; output_ty; view_code = original_code_expr}, ctxt) ))
     ctxt
     typed_views
   >|=? fun (views, ctxt) -> (arg_type, storage_type, views, ctxt)
  else return (original_arg_type_expr, original_storage_type_expr, views, ctxt))
  >>=? fun (arg_type, storage_type, views, ctxt) ->
  Script_map.map_es_in_context
    (fun ctxt _name {input_ty; output_ty; view_code} ->
      unparse_code ctxt ~stack_depth:0 mode view_code
      >|=? fun (view_code, ctxt) ->
      let view_code = Micheline.root view_code in
      ({input_ty; output_ty; view_code}, ctxt))
    ctxt
    views
  >>=? fun (views, ctxt) ->
  let open Micheline in
  let unparse_view_unaccounted name {input_ty; output_ty; view_code} views =
    Prim
      ( loc,
        K_view,
        [
          String (loc, Script_string.to_string name);
          input_ty;
          output_ty;
          view_code;
        ],
        [] )
    :: views
  in
  let views = Script_map.fold unparse_view_unaccounted views [] |> List.rev in
  let code =
    Seq
      ( loc,
        [
          Prim (loc, K_parameter, [arg_type], []);
          Prim (loc, K_storage, [storage_type], []);
          Prim (loc, K_code, [Micheline.root code], []);
        ]
        @ views )
  in
  return
    ( {code = lazy_expr (strip_locations code); storage = lazy_expr storage},
      ctxt )

let pack_data_with_mode ctxt ty data ~mode =
  unparse_data ~stack_depth:0 ctxt mode ty data >|=? fun (unparsed, ctxt) ->
  pack_node unparsed ctxt

let hash_data ctxt ty data =
  pack_data_with_mode ctxt ty data ~mode:Optimized_legacy
  >>=? fun (bytes, ctxt) -> Lwt.return @@ hash_bytes ctxt bytes

let pack_data ctxt ty data =
  pack_data_with_mode ctxt ty data ~mode:Optimized_legacy

(* ---------------- Lazy storage---------------------------------------------*)

type lazy_storage_ids = Lazy_storage.IdSet.t

let no_lazy_storage_id = Lazy_storage.IdSet.empty

let diff_of_big_map ctxt mode ~temporary ~ids_to_copy
    (Big_map {id; key_type; value_type; diff}) =
  (match id with
  | Some id ->
      if Lazy_storage.IdSet.mem Big_map id ids_to_copy then
        Big_map.fresh ~temporary ctxt >|=? fun (ctxt, duplicate) ->
        (ctxt, Lazy_storage.Copy {src = id}, duplicate)
      else
        (* The first occurrence encountered of a big_map reuses the
             ID. This way, the payer is only charged for the diff.
             For this to work, this diff has to be put at the end of
             the global diff, otherwise the duplicates will use the
             updated version as a base. This is true because we add
             this diff first in the accumulator of
             `extract_lazy_storage_updates`, and this accumulator is not
             reversed. *)
        return (ctxt, Lazy_storage.Existing, id)
  | None ->
      Big_map.fresh ~temporary ctxt >>=? fun (ctxt, id) ->
      Lwt.return
        (let kt = unparse_comparable_ty_uncarbonated ~loc:() key_type in
         Gas.consume ctxt (Script.strip_locations_cost kt) >>? fun ctxt ->
         unparse_ty ~loc:() ctxt value_type >>? fun (kv, ctxt) ->
         Gas.consume ctxt (Script.strip_locations_cost kv) >|? fun ctxt ->
         let key_type = Micheline.strip_locations kt in
         let value_type = Micheline.strip_locations kv in
         (ctxt, Lazy_storage.(Alloc Big_map.{key_type; value_type}), id)))
  >>=? fun (ctxt, init, id) ->
  let pairs =
    Big_map_overlay.fold
      (fun key_hash (key, value) acc -> (key_hash, key, value) :: acc)
      diff.map
      []
  in
  List.fold_left_es
    (fun (acc, ctxt) (key_hash, key, value) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>?= fun ctxt ->
      unparse_comparable_data ctxt mode key_type key >>=? fun (key, ctxt) ->
      (match value with
      | None -> return (None, ctxt)
      | Some x ->
          unparse_data ~stack_depth:0 ctxt mode value_type x
          >|=? fun (node, ctxt) -> (Some node, ctxt))
      >|=? fun (value, ctxt) ->
      let diff_item = Big_map.{key; key_hash; value} in
      (diff_item :: acc, ctxt))
    ([], ctxt)
    (List.rev pairs)
  >|=? fun (updates, ctxt) -> (Lazy_storage.Update {init; updates}, id, ctxt)

let diff_of_sapling_state ctxt ~temporary ~ids_to_copy
    ({id; diff; memo_size} : Sapling.state) =
  (match id with
  | Some id ->
      if Lazy_storage.IdSet.mem Sapling_state id ids_to_copy then
        Sapling.fresh ~temporary ctxt >|=? fun (ctxt, duplicate) ->
        (ctxt, Lazy_storage.Copy {src = id}, duplicate)
      else return (ctxt, Lazy_storage.Existing, id)
  | None ->
      Sapling.fresh ~temporary ctxt >|=? fun (ctxt, id) ->
      (ctxt, Lazy_storage.Alloc Sapling.{memo_size}, id))
  >|=? fun (ctxt, init, id) ->
  (Lazy_storage.Update {init; updates = diff}, id, ctxt)

(**
    Witness flag for whether a type can be populated by a value containing a
    lazy storage.
    [False_f] must be used only when a value of the type cannot contain a lazy
    storage.

    This flag is built in [has_lazy_storage] and used only in
    [extract_lazy_storage_updates] and [collect_lazy_storage].

    This flag is necessary to avoid these two functions to have a quadratic
    complexity in the size of the type.

    Add new lazy storage kinds here.

    Please keep the usage of this GADT local.
*)

type 'ty has_lazy_storage =
  | Big_map_f : ('a, 'b) big_map has_lazy_storage
  | Sapling_state_f : Sapling.state has_lazy_storage
  | False_f : _ has_lazy_storage
  | Pair_f :
      'a has_lazy_storage * 'b has_lazy_storage
      -> ('a, 'b) pair has_lazy_storage
  | Or_f :
      'a has_lazy_storage * 'b has_lazy_storage
      -> ('a, 'b) or_ has_lazy_storage
  | Option_f : 'a has_lazy_storage -> 'a option has_lazy_storage
  | List_f : 'a has_lazy_storage -> 'a Script_list.t has_lazy_storage
  | Map_f : 'v has_lazy_storage -> (_, 'v) map has_lazy_storage

(**
    This function is called only on storage and parameter types of contracts,
    once per typechecked contract. It has a complexity linear in the size of
    the types, which happen to be literally written types, so the gas for them
    has already been paid.
*)
let rec has_lazy_storage : type t tc. (t, tc) ty -> t has_lazy_storage =
 fun ty ->
  let aux1 cons t =
    match has_lazy_storage t with False_f -> False_f | h -> cons h
  in
  let aux2 cons t1 t2 =
    match (has_lazy_storage t1, has_lazy_storage t2) with
    | False_f, False_f -> False_f
    | h1, h2 -> cons h1 h2
  in
  match ty with
  | Big_map_t (_, _, _) -> Big_map_f
  | Sapling_state_t _ -> Sapling_state_f
  | Unit_t -> False_f
  | Int_t -> False_f
  | Nat_t -> False_f
  | Signature_t -> False_f
  | String_t -> False_f
  | Bytes_t -> False_f
  | Mutez_t -> False_f
  | Key_hash_t -> False_f
  | Key_t -> False_f
  | Timestamp_t -> False_f
  | Address_t -> False_f
  | Tx_rollup_l2_address_t -> False_f
  | Bool_t -> False_f
  | Lambda_t (_, _, _) -> False_f
  | Set_t (_, _) -> False_f
  | Contract_t (_, _) -> False_f
  | Operation_t -> False_f
  | Chain_id_t -> False_f
  | Never_t -> False_f
  | Bls12_381_g1_t -> False_f
  | Bls12_381_g2_t -> False_f
  | Bls12_381_fr_t -> False_f
  | Sapling_transaction_t _ -> False_f
  | Sapling_transaction_deprecated_t _ -> False_f
  | Ticket_t _ -> False_f
  | Chest_key_t -> False_f
  | Chest_t -> False_f
  | Pair_t (l, r, _, _) -> aux2 (fun l r -> Pair_f (l, r)) l r
  | Or_t (l, r, _, _) -> aux2 (fun l r -> Or_f (l, r)) l r
  | Option_t (t, _, _) -> aux1 (fun h -> Option_f h) t
  | List_t (t, _) -> aux1 (fun h -> List_f h) t
  | Map_t (_, t, _) -> aux1 (fun h -> Map_f h) t

(**
  Transforms a value potentially containing lazy storage in an intermediary
  state to a value containing lazy storage only represented by identifiers.

  Returns the updated value, the updated set of ids to copy, and the lazy
  storage diff to show on the receipt and apply on the storage.

*)
let extract_lazy_storage_updates ctxt mode ~temporary ids_to_copy acc ty x =
  let rec aux :
      type a ac.
      context ->
      unparsing_mode ->
      temporary:bool ->
      Lazy_storage.IdSet.t ->
      Lazy_storage.diffs ->
      (a, ac) ty ->
      a ->
      has_lazy_storage:a has_lazy_storage ->
      (context * a * Lazy_storage.IdSet.t * Lazy_storage.diffs) tzresult Lwt.t =
   fun ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage ->
    Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>?= fun ctxt ->
    match (has_lazy_storage, ty, x) with
    | False_f, _, _ -> return (ctxt, x, ids_to_copy, acc)
    | Big_map_f, Big_map_t (_, _, _), map ->
        diff_of_big_map ctxt mode ~temporary ~ids_to_copy map
        >|=? fun (diff, id, ctxt) ->
        let map =
          let (Big_map map) = map in
          Big_map
            {
              map with
              diff = {map = Big_map_overlay.empty; size = 0};
              id = Some id;
            }
        in
        let diff = Lazy_storage.make Big_map id diff in
        let ids_to_copy = Lazy_storage.IdSet.add Big_map id ids_to_copy in
        (ctxt, map, ids_to_copy, diff :: acc)
    | Sapling_state_f, Sapling_state_t _, sapling_state ->
        diff_of_sapling_state ctxt ~temporary ~ids_to_copy sapling_state
        >|=? fun (diff, id, ctxt) ->
        let sapling_state =
          Sapling.empty_state ~id ~memo_size:sapling_state.memo_size ()
        in
        let diff = Lazy_storage.make Sapling_state id diff in
        let ids_to_copy = Lazy_storage.IdSet.add Sapling_state id ids_to_copy in
        (ctxt, sapling_state, ids_to_copy, diff :: acc)
    | Pair_f (hl, hr), Pair_t (tyl, tyr, _, _), (xl, xr) ->
        aux ctxt mode ~temporary ids_to_copy acc tyl xl ~has_lazy_storage:hl
        >>=? fun (ctxt, xl, ids_to_copy, acc) ->
        aux ctxt mode ~temporary ids_to_copy acc tyr xr ~has_lazy_storage:hr
        >|=? fun (ctxt, xr, ids_to_copy, acc) ->
        (ctxt, (xl, xr), ids_to_copy, acc)
    | Or_f (has_lazy_storage, _), Or_t (ty, _, _, _), L x ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, L x, ids_to_copy, acc)
    | Or_f (_, has_lazy_storage), Or_t (_, ty, _, _), R x ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, R x, ids_to_copy, acc)
    | Option_f has_lazy_storage, Option_t (ty, _, _), Some x ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, Some x, ids_to_copy, acc)
    | List_f has_lazy_storage, List_t (ty, _), l ->
        List.fold_left_es
          (fun (ctxt, l, ids_to_copy, acc) x ->
            aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
            >|=? fun (ctxt, x, ids_to_copy, acc) ->
            (ctxt, Script_list.cons x l, ids_to_copy, acc))
          (ctxt, Script_list.empty, ids_to_copy, acc)
          l.elements
        >|=? fun (ctxt, l, ids_to_copy, acc) ->
        let reversed = Script_list.rev l in
        (ctxt, reversed, ids_to_copy, acc)
    | Map_f has_lazy_storage, Map_t (_, ty, _), map ->
        let (module M) = Script_map.get_module map in
        let bindings m = M.OPS.fold (fun k v bs -> (k, v) :: bs) m [] in
        List.fold_left_es
          (fun (ctxt, m, ids_to_copy, acc) (k, x) ->
            aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
            >|=? fun (ctxt, x, ids_to_copy, acc) ->
            (ctxt, M.OPS.add k x m, ids_to_copy, acc))
          (ctxt, M.OPS.empty, ids_to_copy, acc)
          (bindings M.boxed)
        >|=? fun (ctxt, m, ids_to_copy, acc) ->
        let module M = struct
          module OPS = M.OPS

          type key = M.key

          type value = M.value

          let boxed = m

          let size = M.size
        end in
        ( ctxt,
          Script_map.make
            (module M : Boxed_map
              with type key = M.key
               and type value = M.value),
          ids_to_copy,
          acc )
    | _, Option_t (_, _, _), None -> return (ctxt, None, ids_to_copy, acc)
  in
  let has_lazy_storage = has_lazy_storage ty in
  aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage

(** We namespace an error type for [fold_lazy_storage]. The error case is only
    available when the ['error] parameter is equal to unit. *)
module Fold_lazy_storage = struct
  type ('acc, 'error) result =
    | Ok : 'acc -> ('acc, 'error) result
    | Error : ('acc, unit) result
end

(** Prematurely abort if [f] generates an error. Use this function without the
    [unit] type for [error] if you are in a case where errors are impossible.
*)
let rec fold_lazy_storage :
    type a ac error.
    f:('acc, error) Fold_lazy_storage.result Lazy_storage.IdSet.fold_f ->
    init:'acc ->
    context ->
    (a, ac) ty ->
    a ->
    has_lazy_storage:a has_lazy_storage ->
    (('acc, error) Fold_lazy_storage.result * context) tzresult =
 fun ~f ~init ctxt ty x ~has_lazy_storage ->
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
  match (has_lazy_storage, ty, x) with
  | Big_map_f, Big_map_t (_, _, _), Big_map {id = Some id; _} ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
      ok (f.f Big_map id (Fold_lazy_storage.Ok init), ctxt)
  | Sapling_state_f, Sapling_state_t _, {id = Some id; _} ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
      ok (f.f Sapling_state id (Fold_lazy_storage.Ok init), ctxt)
  | False_f, _, _ -> ok (Fold_lazy_storage.Ok init, ctxt)
  | Big_map_f, Big_map_t (_, _, _), Big_map {id = None; _} ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | Sapling_state_f, Sapling_state_t _, {id = None; _} ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | Pair_f (hl, hr), Pair_t (tyl, tyr, _, _), (xl, xr) -> (
      fold_lazy_storage ~f ~init ctxt tyl xl ~has_lazy_storage:hl
      >>? fun (init, ctxt) ->
      match init with
      | Fold_lazy_storage.Ok init ->
          fold_lazy_storage ~f ~init ctxt tyr xr ~has_lazy_storage:hr
      | Fold_lazy_storage.Error -> ok (init, ctxt))
  | Or_f (has_lazy_storage, _), Or_t (ty, _, _, _), L x ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | Or_f (_, has_lazy_storage), Or_t (_, ty, _, _), R x ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | _, Option_t (_, _, _), None -> ok (Fold_lazy_storage.Ok init, ctxt)
  | Option_f has_lazy_storage, Option_t (ty, _, _), Some x ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | List_f has_lazy_storage, List_t (ty, _), l ->
      List.fold_left_e
        (fun ((init, ctxt) : ('acc, error) Fold_lazy_storage.result * context) x ->
          match init with
          | Fold_lazy_storage.Ok init ->
              fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
          | Fold_lazy_storage.Error -> ok (init, ctxt))
        (Fold_lazy_storage.Ok init, ctxt)
        l.elements
  | Map_f has_lazy_storage, Map_t (_, ty, _), m ->
      Script_map.fold
        (fun _
             v
             (acc : (('acc, error) Fold_lazy_storage.result * context) tzresult) ->
          acc >>? fun (init, ctxt) ->
          match init with
          | Fold_lazy_storage.Ok init ->
              fold_lazy_storage ~f ~init ctxt ty v ~has_lazy_storage
          | Fold_lazy_storage.Error -> ok (init, ctxt))
        m
        (ok (Fold_lazy_storage.Ok init, ctxt))

let collect_lazy_storage ctxt ty x =
  let has_lazy_storage = has_lazy_storage ty in
  let f kind id (acc : (_, never) Fold_lazy_storage.result) =
    let acc = match acc with Fold_lazy_storage.Ok acc -> acc in
    Fold_lazy_storage.Ok (Lazy_storage.IdSet.add kind id acc)
  in
  fold_lazy_storage ~f:{f} ~init:no_lazy_storage_id ctxt ty x ~has_lazy_storage
  >>? fun (ids, ctxt) ->
  match ids with Fold_lazy_storage.Ok ids -> ok (ids, ctxt)

let extract_lazy_storage_diff ctxt mode ~temporary ~to_duplicate ~to_update ty v
    =
  (*
    Basically [to_duplicate] are ids from the argument and [to_update] are ids
    from the storage before execution (i.e. it is safe to reuse them since they
    will be owned by the same contract).
  *)
  let to_duplicate = Lazy_storage.IdSet.diff to_duplicate to_update in
  extract_lazy_storage_updates ctxt mode ~temporary to_duplicate [] ty v
  >|=? fun (ctxt, v, alive, diffs) ->
  let diffs =
    if temporary then diffs
    else
      let dead = Lazy_storage.IdSet.diff to_update alive in
      Lazy_storage.IdSet.fold_all
        {f = (fun kind id acc -> Lazy_storage.make kind id Remove :: acc)}
        dead
        diffs
  in
  match diffs with
  | [] -> (v, None, ctxt)
  | diffs -> (v, Some diffs (* do not reverse *), ctxt)

let list_of_big_map_ids ids =
  Lazy_storage.IdSet.fold Big_map (fun id acc -> id :: acc) ids []

let parse_data ~elab_conf ctxt ~allow_forged ty t =
  parse_data ~unparse_code_rec ~elab_conf ~allow_forged ~stack_depth:0 ctxt ty t

let parse_view ~elab_conf ctxt ty view =
  parse_view ~unparse_code_rec ~elab_conf ctxt ty view

let parse_views ~elab_conf ctxt ty views =
  parse_views ~unparse_code_rec ~elab_conf ctxt ty views

let parse_code ~elab_conf ctxt ~code =
  parse_code ~unparse_code_rec ~elab_conf ctxt ~code

let parse_storage ~elab_conf ctxt ~allow_forged ty ~storage =
  parse_storage ~unparse_code_rec ~elab_conf ctxt ~allow_forged ty ~storage

let parse_script ~elab_conf ctxt ~allow_forged_in_storage script =
  parse_script ~unparse_code_rec ~elab_conf ctxt ~allow_forged_in_storage script

let parse_comparable_data ?type_logger ctxt ty t =
  parse_data
    ~elab_conf:Script_ir_translator_config.(make ~legacy:false ?type_logger ())
    ~allow_forged:false
    ctxt
    ty
    t

let parse_instr :
    type a s.
    elab_conf:elab_conf ->
    tc_context ->
    context ->
    Script.node ->
    (a, s) stack_ty ->
    ((a, s) judgement * context) tzresult Lwt.t =
 fun ~elab_conf tc_context ctxt script_instr stack_ty ->
  parse_instr
    ~unparse_code_rec
    ~elab_conf
    ~stack_depth:0
    tc_context
    ctxt
    script_instr
    stack_ty

let unparse_data = unparse_data ~stack_depth:0

let unparse_code ctxt mode code =
  (* Constants need to be expanded or [unparse_code] may fail. *)
  Global_constants_storage.expand ctxt (strip_locations code)
  >>=? fun (ctxt, code) -> unparse_code ~stack_depth:0 ctxt mode (root code)

let parse_contract_data context loc arg_ty contract ~entrypoint =
  parse_contract_data ~stack_depth:0 context loc arg_ty contract ~entrypoint

let parse_toplevel ctxt ~legacy toplevel =
  Global_constants_storage.expand ctxt toplevel >>=? fun (ctxt, toplevel) ->
  Lwt.return @@ parse_toplevel ctxt ~legacy toplevel

let parse_comparable_ty = parse_comparable_ty ~stack_depth:0

let parse_big_map_value_ty = parse_big_map_value_ty ~stack_depth:0

let parse_packable_ty = parse_packable_ty ~stack_depth:0

let parse_passable_ty = parse_passable_ty ~stack_depth:0

let parse_any_ty = parse_any_ty ~stack_depth:0

let parse_ty = parse_ty ~stack_depth:0 ~ret:Don't_parse_entrypoints

let parse_parameter_ty_and_entrypoints =
  parse_parameter_ty_and_entrypoints ~stack_depth:0

let get_single_sapling_state ctxt ty x =
  let has_lazy_storage = has_lazy_storage ty in
  let f (type i a u) (kind : (i, a, u) Lazy_storage.Kind.t) (id : i)
      single_id_opt : (Sapling.Id.t option, unit) Fold_lazy_storage.result =
    match kind with
    | Lazy_storage.Kind.Sapling_state -> (
        match single_id_opt with
        | Fold_lazy_storage.Ok None -> Fold_lazy_storage.Ok (Some id)
        | Fold_lazy_storage.Ok (Some _) ->
            Fold_lazy_storage.Error (* more than one *)
        | Fold_lazy_storage.Error -> single_id_opt)
    | _ -> single_id_opt
  in
  fold_lazy_storage ~f:{f} ~init:None ctxt ty x ~has_lazy_storage
  >>? fun (id, ctxt) ->
  match id with
  | Fold_lazy_storage.Ok (Some id) -> ok (Some id, ctxt)
  | Fold_lazy_storage.Ok None | Fold_lazy_storage.Error -> ok (None, ctxt)

(*

   {!Script_cache} needs a measure of the script size in memory.
   Determining this size is not easy in OCaml because of sharing.

   Indeed, many values present in the script share the same memory
   area. This is especially true for types and stack types: they are
   heavily shared in every typed IR internal representation. As a
   consequence, computing the size of the typed IR without taking
   sharing into account leads to a size which is sometimes two order
   of magnitude bigger than the actual size.

   We could track down this sharing. Unfortunately, sharing is not
   part of OCaml semantics: for this reason, a compiler can optimize
   memory representation by adding more sharing.  If two nodes use
   different optimization flags or compilers, such a precise
   computation of the memory footprint of scripts would lead to two
   distinct sizes. As these sizes occur in the blockchain context,
   this situation would lead to a fork.

   For this reason, we introduce a *size model* for the script size.
   This model provides an overapproximation of the actual size in
   memory. The risk is to be too far from the actual size: the cache
   would then be wrongly marked as full. This situation would make the
   cache less useful but should present no security risk .

*)
let script_size
    (Ex_script
      (Script
        {
          code_size;
          code = _;
          arg_type = _;
          storage;
          storage_type;
          entrypoints = _;
          views = _;
        })) =
  let nodes, storage_size =
    Script_typed_ir_size.value_size storage_type storage
  in
  let cost = Script_typed_ir_size_costs.nodes_cost ~nodes in
  (Saturation_repr.(add code_size storage_size |> to_int), cost)

let typecheck_code ~legacy ~show_types ctxt code =
  typecheck_code ~unparse_code_rec ~legacy ~show_types ctxt code
  >|=? fun (Typechecked_code_internal {type_map; _}, ctxt) -> (type_map, ctxt)
