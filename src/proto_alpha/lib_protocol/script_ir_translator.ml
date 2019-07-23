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
open Micheline
open Script
open Script_typed_ir
open Script_tc_errors
open Script_ir_annot

module Typecheck_costs = Michelson_v1_gas.Cost_of.Typechecking
module Unparse_costs = Michelson_v1_gas.Cost_of.Unparse

type ex_comparable_ty = Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty
type ex_ty = Ex_ty : 'a ty -> ex_ty
type ex_stack_ty = Ex_stack_ty : 'a stack_ty -> ex_stack_ty

type tc_context =
  | Lambda : tc_context
  | Dip : 'a stack_ty * tc_context -> tc_context
  | Toplevel : { storage_type : 'sto ty ; param_type : 'param ty ; root_name : string option ;
                 legacy_create_contract_literal : bool } -> tc_context

type unparsing_mode = Optimized | Readable

type type_logger =
  int -> (Script.expr * Script.annot) list -> (Script.expr * Script.annot)  list -> unit

let add_dip ty annot prev =
  match prev with
  | Lambda | Toplevel _ -> Dip (Item_t (ty, Empty_t, annot), prev)
  | Dip (stack, _) -> Dip (Item_t (ty, stack, annot), prev)

(* ---- Type size accounting ------------------------------------------------*)

let rec comparable_type_size : type t a. (t, a) comparable_struct -> int = fun ty ->
  (* No wildcard to force the update when comparable_ty chages. *)
  match ty with
  | Int_key _ -> 1
  | Nat_key _ -> 1
  | String_key _ -> 1
  | Bytes_key _ -> 1
  | Mutez_key _ -> 1
  | Bool_key _ -> 1
  | Key_hash_key _ -> 1
  | Timestamp_key _ -> 1
  | Address_key _ -> 1
  | Pair_key (_, (t, _), _) -> 1 + comparable_type_size t

let rec type_size : type t. t ty -> int =
  fun ty -> match ty with
    | Unit_t _ -> 1
    | Int_t _ -> 1
    | Nat_t _ -> 1
    | Signature_t _ -> 1
    | Bytes_t _ -> 1
    | String_t _ -> 1
    | Mutez_t _ -> 1
    | Key_hash_t _ -> 1
    | Key_t _ -> 1
    | Timestamp_t _ -> 1
    | Address_t _ -> 1
    | Bool_t _ -> 1
    | Operation_t _ -> 1
    | Pair_t ((l, _, _), (r, _, _), _, _) ->
        1 + type_size l + type_size r
    | Union_t ((l, _), (r, _), _, _) ->
        1 + type_size l + type_size r
    | Lambda_t (arg, ret, _) ->
        1 + type_size arg + type_size ret
    | Option_t (t, _, _) ->
        1 + type_size t
    | List_t (t, _, _) ->
        1 + type_size t
    | Set_t (k, _) ->
        1 + comparable_type_size k
    | Map_t (k, v, _, _) ->
        1 + comparable_type_size k + type_size v
    | Big_map_t (k, v, _) ->
        1 + comparable_type_size k + type_size v
    | Contract_t (arg, _) ->
        1 + type_size arg
    | Chain_id_t _ -> 1

let rec type_size_of_stack_head
  : type st. st stack_ty -> up_to:int -> int
  = fun stack ~up_to ->
    match stack with
    | Empty_t -> 0
    | Item_t (head, tail, _annot) ->
        if Compare.Int.(up_to > 0) then
          Compare.Int.max (type_size head)
            (type_size_of_stack_head tail ~up_to:(up_to - 1))
        else
          0

(* This is the depth of the stack to inspect for sizes overflow. We
   only need to check the produced types that can be larger than the
   arguments. That's why Swap is 0 for instance as no type grows.
   Constant sized types are not checked: it is assumed they are lower
   than the bound (otherwise every program would be rejected). *)
let number_of_generated_growing_types : type b a. (b, a) instr -> int = function
  | Drop -> 0
  | Dup -> 0
  | Swap -> 0
  | Const _ -> 1
  | Cons_pair -> 1
  | Car -> 0
  | Cdr -> 0
  | Cons_some -> 1
  | Cons_none _ -> 1
  | If_none _ -> 0
  | Left -> 0
  | Right -> 0
  | If_left _ -> 0
  | Cons_list -> 1
  | Nil -> 1
  | If_cons _ -> 0
  | List_map _ -> 1
  | List_size -> 0
  | List_iter _ -> 1
  | Empty_set _ -> 1
  | Set_iter _ -> 0
  | Set_mem -> 0
  | Set_update -> 0
  | Set_size -> 0
  | Empty_map _ -> 1
  | Map_map _ -> 1
  | Map_iter _ -> 1
  | Map_mem -> 0
  | Map_get -> 0
  | Map_update -> 0
  | Map_size -> 0
  | Empty_big_map _ -> 1
  | Big_map_get -> 0
  | Big_map_update -> 0
  | Big_map_mem -> 0
  | Concat_string -> 0
  | Concat_string_pair -> 0
  | Slice_string -> 0
  | String_size -> 0
  | Concat_bytes -> 0
  | Concat_bytes_pair -> 0
  | Slice_bytes -> 0
  | Bytes_size -> 0
  | Add_seconds_to_timestamp -> 0
  | Add_timestamp_to_seconds -> 0
  | Sub_timestamp_seconds -> 0
  | Diff_timestamps -> 0
  | Add_tez -> 0
  | Sub_tez -> 0
  | Mul_teznat -> 0
  | Mul_nattez -> 0
  | Ediv_teznat -> 0
  | Ediv_tez -> 0
  | Or -> 0
  | And -> 0
  | Xor -> 0
  | Not -> 0
  | Is_nat -> 0
  | Neg_nat -> 0
  | Neg_int -> 0
  | Abs_int -> 0
  | Int_nat -> 0
  | Add_intint -> 0
  | Add_intnat -> 0
  | Add_natint -> 0
  | Add_natnat -> 0
  | Sub_int -> 0
  | Mul_intint -> 0
  | Mul_intnat -> 0
  | Mul_natint -> 0
  | Mul_natnat -> 0
  | Ediv_intint -> 0
  | Ediv_intnat -> 0
  | Ediv_natint -> 0
  | Ediv_natnat -> 0
  | Lsl_nat -> 0
  | Lsr_nat -> 0
  | Or_nat -> 0
  | And_nat -> 0
  | And_int_nat -> 0
  | Xor_nat -> 0
  | Not_nat -> 0
  | Not_int -> 0
  | Seq _ -> 0
  | If _ -> 0
  | Loop _ -> 0
  | Loop_left _ -> 0
  | Dip _ -> 0
  | Exec -> 0
  | Apply _ -> 0
  | Lambda _ -> 1
  | Failwith _ -> 1
  | Nop -> 0
  | Compare _ -> 1
  | Eq -> 0
  | Neq -> 0
  | Lt -> 0
  | Gt -> 0
  | Le -> 0
  | Ge -> 0
  | Address -> 0
  | Contract _ -> 1
  | Transfer_tokens -> 1
  | Create_account -> 0
  | Implicit_account -> 0
  | Create_contract _ -> 1
  | Create_contract_2 _ -> 1
  | Now -> 0
  | Balance -> 0
  | Check_signature -> 0
  | Hash_key -> 0
  | Blake2b -> 0
  | Sha256 -> 0
  | Sha512 -> 0
  | Steps_to_quota -> 0
  | Source -> 0
  | Sender -> 0
  | Self _ -> 1
  | Amount -> 0
  | Set_delegate -> 0
  | Pack _ -> 0
  | Unpack _ -> 1
  | Dig _ -> 0
  | Dug _ -> 0
  | Dipn _ -> 0
  | Dropn _ -> 0
  | ChainId -> 0

(* ---- Error helpers -------------------------------------------------------*)

let location = function
  | Prim (loc, _, _, _)
  | Int (loc, _)
  | String (loc, _)
  | Bytes (loc, _)
  | Seq (loc, _) -> loc

let kind = function
  | Int _ -> Int_kind
  | String _ -> String_kind
  | Bytes _ -> Bytes_kind
  | Prim _ -> Prim_kind
  | Seq _ -> Seq_kind

let namespace = function
  | K_parameter
  | K_storage
  | K_code -> Keyword_namespace
  | D_False
  | D_Elt
  | D_Left
  | D_None
  | D_Pair
  | D_Right
  | D_Some
  | D_True
  | D_Unit -> Constant_namespace
  | I_PACK
  | I_UNPACK
  | I_BLAKE2B
  | I_SHA256
  | I_SHA512
  | I_ABS
  | I_ADD
  | I_AMOUNT
  | I_AND
  | I_BALANCE
  | I_CAR
  | I_CDR
  | I_CHAIN_ID
  | I_CHECK_SIGNATURE
  | I_COMPARE
  | I_CONCAT
  | I_CONS
  | I_CREATE_ACCOUNT
  | I_CREATE_CONTRACT
  | I_IMPLICIT_ACCOUNT
  | I_DIP
  | I_DROP
  | I_DUP
  | I_EDIV
  | I_EMPTY_BIG_MAP
  | I_EMPTY_MAP
  | I_EMPTY_SET
  | I_EQ
  | I_EXEC
  | I_APPLY
  | I_FAILWITH
  | I_GE
  | I_GET
  | I_GT
  | I_HASH_KEY
  | I_IF
  | I_IF_CONS
  | I_IF_LEFT
  | I_IF_NONE
  | I_INT
  | I_LAMBDA
  | I_LE
  | I_LEFT
  | I_LOOP
  | I_LSL
  | I_LSR
  | I_LT
  | I_MAP
  | I_MEM
  | I_MUL
  | I_NEG
  | I_NEQ
  | I_NIL
  | I_NONE
  | I_NOT
  | I_NOW
  | I_OR
  | I_PAIR
  | I_PUSH
  | I_RIGHT
  | I_SIZE
  | I_SOME
  | I_SOURCE
  | I_SENDER
  | I_SELF
  | I_SLICE
  | I_STEPS_TO_QUOTA
  | I_SUB
  | I_SWAP
  | I_TRANSFER_TOKENS
  | I_SET_DELEGATE
  | I_UNIT
  | I_UPDATE
  | I_XOR
  | I_ITER
  | I_LOOP_LEFT
  | I_ADDRESS
  | I_CONTRACT
  | I_ISNAT
  | I_CAST
  | I_RENAME
  | I_DIG
  | I_DUG -> Instr_namespace
  | T_bool
  | T_contract
  | T_int
  | T_key
  | T_key_hash
  | T_lambda
  | T_list
  | T_map
  | T_big_map
  | T_nat
  | T_option
  | T_or
  | T_pair
  | T_set
  | T_signature
  | T_string
  | T_bytes
  | T_mutez
  | T_timestamp
  | T_unit
  | T_operation
  | T_address
  | T_chain_id -> Type_namespace


let unexpected expr exp_kinds exp_ns exp_prims =
  match expr with
  | Int (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Int_kind)
  | String (loc, _ ) -> Invalid_kind (loc, Prim_kind :: exp_kinds, String_kind)
  | Bytes (loc, _ ) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Bytes_kind)
  | Seq (loc, _) -> Invalid_kind (loc, Prim_kind :: exp_kinds, Seq_kind)
  | Prim (loc, name, _, _) ->
      match namespace name, exp_ns with
      | Type_namespace, Type_namespace
      | Instr_namespace, Instr_namespace
      | Constant_namespace, Constant_namespace ->
          Invalid_primitive (loc, exp_prims, name)
      | ns, _ ->
          Invalid_namespace (loc, name, exp_ns, ns)

let check_kind kinds expr =
  let kind = kind expr in
  if List.mem kind kinds then
    return_unit
  else
    let loc = location expr in
    fail (Invalid_kind (loc, kinds, kind))

(* ---- Sets and Maps -------------------------------------------------------*)

let wrap_compare compare a b =
  let res = compare a b in
  if Compare.Int.(res = 0) then 0
  else if Compare.Int.(res > 0) then 1
  else -1

let rec compare_comparable
  : type a s. (a, s) comparable_struct -> a -> a -> int
  = fun kind -> match kind with
    | String_key _ -> wrap_compare Compare.String.compare
    | Bool_key _ -> wrap_compare Compare.Bool.compare
    | Mutez_key _ -> wrap_compare Tez.compare
    | Key_hash_key _ -> wrap_compare Signature.Public_key_hash.compare
    | Int_key _ -> wrap_compare Script_int.compare
    | Nat_key _ -> wrap_compare Script_int.compare
    | Timestamp_key _ -> wrap_compare Script_timestamp.compare
    | Address_key _ ->
        wrap_compare @@ fun (x, ex) (y, ey) ->
        let lres = Contract.compare x y in
        if Compare.Int.(lres = 0) then
          Compare.String.compare ex ey
        else lres
    | Bytes_key _ -> wrap_compare MBytes.compare
    | Pair_key ((tl, _), (tr, _), _) ->
        fun (lx, rx) (ly, ry) ->
          let lres = compare_comparable tl lx ly in
          if Compare.Int.(lres = 0) then
            compare_comparable tr rx ry
          else lres

let empty_set
  : type a. a comparable_ty -> a set
  = fun ty ->
    let module OPS = Set.Make (struct
        type t = a
        let compare = compare_comparable ty
      end) in
    (module struct
      type elt = a
      let elt_ty = ty
      module OPS = OPS
      let boxed = OPS.empty
      let size = 0
    end)

let set_update
  : type a. a -> bool -> a set -> a set
  = fun v b (module Box) ->
    (module struct
      type elt = a
      let elt_ty = Box.elt_ty
      module OPS = Box.OPS
      let boxed =
        if b
        then Box.OPS.add v Box.boxed
        else Box.OPS.remove v Box.boxed
      let size =
        let mem = Box.OPS.mem v Box.boxed in
        if mem
        then if b then Box.size else Box.size - 1
        else if b then Box.size + 1 else Box.size
    end)

let set_mem
  : type elt. elt -> elt set -> bool
  = fun v (module Box) ->
    Box.OPS.mem v Box.boxed

let set_fold
  : type elt acc. (elt -> acc -> acc) -> elt set -> acc -> acc
  = fun f (module Box) ->
    Box.OPS.fold f Box.boxed

let set_size
  : type elt. elt set -> Script_int.n Script_int.num =
  fun (module Box) ->
    Script_int.(abs (of_int Box.size))

let map_key_ty
  : type a b. (a, b) map -> a comparable_ty
  = fun (module Box) -> Box.key_ty

let empty_map
  : type a b. a comparable_ty -> (a, b) map
  = fun ty ->
    let module OPS = Map.Make (struct
        type t = a
        let compare = compare_comparable ty
      end) in
    (module struct
      type key = a
      type value = b
      let key_ty = ty
      module OPS = OPS
      let boxed = (OPS.empty, 0)
    end)

let map_get
  : type key value. key -> (key, value) map -> value option
  = fun k (module Box) ->
    Box.OPS.find_opt k (fst Box.boxed)

let map_update
  : type a b. a -> b option -> (a, b) map -> (a, b) map
  = fun k v (module Box) ->
    (module struct
      type key = a
      type value = b
      let key_ty = Box.key_ty
      module OPS = Box.OPS
      let boxed =
        let (map, size) = Box.boxed in
        let contains = Box.OPS.mem k map in
        match v with
        | Some v -> (Box.OPS.add k v map, size + if contains then 0 else 1)
        | None -> (Box.OPS.remove k map, size - if contains then 1 else 0)
    end)

let map_set
  : type a b. a -> b -> (a, b) map -> (a, b) map
  = fun k v (module Box) ->
    (module struct
      type key = a
      type value = b
      let key_ty = Box.key_ty
      module OPS = Box.OPS
      let boxed =
        let (map, size) = Box.boxed in
        (Box.OPS.add k v map, if Box.OPS.mem k map then size else size + 1)
    end)

let map_mem
  : type key value. key -> (key, value) map -> bool
  = fun k (module Box) ->
    Box.OPS.mem k (fst Box.boxed)

let map_fold
  : type key value acc. (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc
  = fun f (module Box) ->
    Box.OPS.fold f (fst Box.boxed)

let map_size
  : type key value. (key, value) map -> Script_int.n Script_int.num =
  fun (module Box) ->
    Script_int.(abs (of_int (snd Box.boxed)))

(* ---- Unparsing (Typed IR -> Untyped expressions) of types -----------------*)

let rec ty_of_comparable_ty
  : type a s. (a, s) comparable_struct -> a ty
  = function
    | Int_key tname -> Int_t tname
    | Nat_key tname -> Nat_t tname
    | String_key tname -> String_t tname
    | Bytes_key tname -> Bytes_t tname
    | Mutez_key tname -> Mutez_t tname
    | Bool_key tname -> Bool_t tname
    | Key_hash_key tname -> Key_hash_t tname
    | Timestamp_key tname -> Timestamp_t tname
    | Address_key tname -> Address_t tname
    | Pair_key ((l, al), (r, ar), tname) ->
        Pair_t ((ty_of_comparable_ty l, al, None), (ty_of_comparable_ty r, ar, None), tname, false)

let rec comparable_ty_of_ty
  : type a. a ty -> a comparable_ty option
  = function
    | Int_t tname -> Some (Int_key tname)
    | Nat_t tname -> Some (Nat_key tname)
    | String_t tname -> Some (String_key tname)
    | Bytes_t tname -> Some (Bytes_key tname)
    | Mutez_t tname -> Some (Mutez_key tname)
    | Bool_t tname -> Some (Bool_key tname)
    | Key_hash_t tname -> Some (Key_hash_key tname)
    | Timestamp_t tname -> Some (Timestamp_key tname)
    | Address_t tname -> Some (Address_key tname)
    | Pair_t ((l, al, _), (r, ar, _), pname, _) ->
        begin match comparable_ty_of_ty r with
        | None -> None
        | Some rty ->
            match comparable_ty_of_ty l with
            | None -> None
            | Some (Pair_key _) -> None (* not a comb *)
            | Some (Int_key tname) -> Some (Pair_key ((Int_key tname, al), (rty, ar), pname))
            | Some (Nat_key tname) -> Some (Pair_key ((Nat_key tname, al), (rty, ar), pname))
            | Some (String_key tname) -> Some (Pair_key ((String_key tname, al), (rty, ar), pname))
            | Some (Bytes_key tname) -> Some (Pair_key ((Bytes_key tname, al), (rty, ar), pname))
            | Some (Mutez_key tname) -> Some (Pair_key ((Mutez_key tname, al), (rty, ar), pname))
            | Some (Bool_key tname) -> Some (Pair_key ((Bool_key tname, al), (rty, ar), pname))
            | Some (Key_hash_key tname) ->  Some (Pair_key ((Key_hash_key tname, al), (rty, ar), pname))
            | Some (Timestamp_key tname) -> Some (Pair_key ((Timestamp_key tname, al), (rty, ar), pname))
            | Some (Address_key tname) -> Some (Pair_key ((Address_key tname, al), (rty, ar), pname))
        end
    | _ -> None

let add_field_annot a var = function
  | Prim (loc, prim, args, annots) ->
      Prim (loc, prim, args, annots @ unparse_field_annot a @ unparse_var_annot var )
  | expr -> expr

let rec unparse_comparable_ty
  : type a s. (a, s) comparable_struct -> Script.node
  = function
    | Int_key tname -> Prim (-1, T_int, [], unparse_type_annot tname)
    | Nat_key tname -> Prim (-1, T_nat, [], unparse_type_annot tname)
    | String_key tname -> Prim (-1, T_string, [], unparse_type_annot tname)
    | Bytes_key tname -> Prim (-1, T_bytes, [], unparse_type_annot tname)
    | Mutez_key tname -> Prim (-1, T_mutez, [], unparse_type_annot tname)
    | Bool_key tname -> Prim (-1, T_bool, [], unparse_type_annot tname)
    | Key_hash_key tname -> Prim (-1, T_key_hash, [], unparse_type_annot tname)
    | Timestamp_key tname -> Prim (-1, T_timestamp, [], unparse_type_annot tname)
    | Address_key tname -> Prim (-1, T_address, [], unparse_type_annot tname)
    | Pair_key ((l, al), (r, ar), pname) ->
        let tl = add_field_annot al None (unparse_comparable_ty l) in
        let tr = add_field_annot ar None (unparse_comparable_ty r) in
        Prim (-1, T_pair, [ tl ; tr ], unparse_type_annot pname)

let rec unparse_ty_no_lwt
  : type a. context -> a ty -> (Script.node * context) tzresult
  = fun ctxt ty ->
    Gas.consume ctxt Unparse_costs.cycle >>? fun ctxt ->
    let return ctxt (name, args, annot) =
      let result = Prim (-1, name, args, annot) in
      Gas.consume ctxt (Unparse_costs.prim_cost (List.length args) annot) >>? fun ctxt ->
      ok (result, ctxt) in
    match ty with
    | Unit_t tname -> return ctxt (T_unit, [], unparse_type_annot tname)
    | Int_t tname -> return ctxt (T_int, [], unparse_type_annot tname)
    | Nat_t tname -> return ctxt (T_nat, [], unparse_type_annot tname)
    | String_t tname -> return ctxt (T_string, [], unparse_type_annot tname)
    | Bytes_t tname -> return ctxt (T_bytes, [], unparse_type_annot tname)
    | Mutez_t tname -> return ctxt (T_mutez, [], unparse_type_annot tname)
    | Bool_t tname -> return ctxt (T_bool, [], unparse_type_annot tname)
    | Key_hash_t tname -> return ctxt (T_key_hash, [], unparse_type_annot tname)
    | Key_t tname -> return ctxt (T_key, [], unparse_type_annot tname)
    | Timestamp_t tname -> return ctxt (T_timestamp, [], unparse_type_annot tname)
    | Address_t tname -> return ctxt (T_address, [], unparse_type_annot tname)
    | Signature_t tname -> return ctxt (T_signature, [], unparse_type_annot tname)
    | Operation_t tname -> return ctxt (T_operation, [], unparse_type_annot tname)
    | Chain_id_t tname -> return ctxt (T_chain_id, [], unparse_type_annot tname)
    | Contract_t (ut, tname) ->
        unparse_ty_no_lwt ctxt ut >>? fun (t, ctxt) ->
        return ctxt (T_contract, [ t ], unparse_type_annot tname)
    | Pair_t ((utl, l_field, l_var), (utr, r_field, r_var), tname, _) ->
        let annot = unparse_type_annot tname in
        unparse_ty_no_lwt ctxt utl >>? fun (utl, ctxt) ->
        let tl = add_field_annot l_field l_var utl in
        unparse_ty_no_lwt ctxt utr >>? fun (utr, ctxt) ->
        let tr = add_field_annot r_field r_var utr in
        return ctxt (T_pair, [ tl; tr ], annot)
    | Union_t ((utl, l_field), (utr, r_field), tname, _) ->
        let annot = unparse_type_annot tname in
        unparse_ty_no_lwt ctxt utl >>? fun (utl, ctxt) ->
        let tl = add_field_annot l_field None utl in
        unparse_ty_no_lwt ctxt utr >>? fun (utr, ctxt) ->
        let tr = add_field_annot r_field None utr in
        return ctxt (T_or, [ tl; tr ], annot)
    | Lambda_t (uta, utr, tname) ->
        unparse_ty_no_lwt ctxt uta >>? fun (ta, ctxt) ->
        unparse_ty_no_lwt ctxt utr >>? fun (tr, ctxt) ->
        return ctxt (T_lambda, [ ta; tr ], unparse_type_annot tname)
    | Option_t (ut, tname, _) ->
        let annot = unparse_type_annot tname in
        unparse_ty_no_lwt ctxt ut >>? fun (ut, ctxt) ->
        return ctxt (T_option, [ ut ], annot)
    | List_t (ut, tname, _) ->
        unparse_ty_no_lwt ctxt ut >>? fun (t, ctxt) ->
        return ctxt (T_list, [ t ], unparse_type_annot tname)
    | Set_t (ut, tname) ->
        let t = unparse_comparable_ty ut in
        return ctxt (T_set, [ t ], unparse_type_annot tname)
    | Map_t (uta, utr, tname, _) ->
        let ta = unparse_comparable_ty uta in
        unparse_ty_no_lwt ctxt utr >>? fun (tr, ctxt) ->
        return ctxt (T_map, [ ta; tr ], unparse_type_annot tname)
    | Big_map_t (uta, utr, tname) ->
        let ta = unparse_comparable_ty uta in
        unparse_ty_no_lwt ctxt utr >>? fun (tr, ctxt) ->
        return ctxt (T_big_map, [ ta; tr ], unparse_type_annot tname)

let unparse_ty ctxt ty = Lwt.return (unparse_ty_no_lwt ctxt ty)

let rec strip_var_annots = function
  | Int _ | String _ | Bytes _ as atom -> atom
  | Seq (loc, args) -> Seq (loc, List.map strip_var_annots args)
  | Prim (loc, name, args, annots) ->
      let not_var_annot s = Compare.Char.(String.get s 0 <> '@') in
      let annots = List.filter not_var_annot annots in
      Prim (loc, name, List.map strip_var_annots args, annots)

let serialize_ty_for_error ctxt ty =
  unparse_ty_no_lwt ctxt ty |>
  record_trace Cannot_serialize_error >|? fun (ty, ctxt) ->
  strip_locations (strip_var_annots ty), ctxt

let rec unparse_stack
  : type a. context -> a stack_ty -> ((Script.expr * Script.annot) list * context) tzresult Lwt.t
  = fun ctxt -> function
    | Empty_t -> return ([], ctxt)
    | Item_t (ty, rest, annot) ->
        unparse_ty ctxt ty >>=? fun (uty, ctxt) ->
        unparse_stack ctxt rest >>=? fun (urest, ctxt) ->
        return ((strip_locations uty, unparse_var_annot annot) :: urest, ctxt)

let serialize_stack_for_error ctxt stack_ty =
  trace Cannot_serialize_error (unparse_stack ctxt stack_ty)

let name_of_ty
  : type a. a ty -> type_annot option
  = function
    | Unit_t tname -> tname
    | Int_t tname -> tname
    | Nat_t tname -> tname
    | String_t tname -> tname
    | Bytes_t tname -> tname
    | Mutez_t tname -> tname
    | Bool_t tname -> tname
    | Key_hash_t tname -> tname
    | Key_t tname -> tname
    | Timestamp_t tname -> tname
    | Address_t tname -> tname
    | Signature_t tname -> tname
    | Operation_t tname -> tname
    | Chain_id_t tname -> tname
    | Contract_t (_, tname) -> tname
    | Pair_t (_, _, tname, _) -> tname
    | Union_t (_, _, tname, _) -> tname
    | Lambda_t (_, _, tname) -> tname
    | Option_t (_, tname, _) -> tname
    | List_t (_, tname, _) -> tname
    | Set_t (_, tname) -> tname
    | Map_t (_, _, tname, _) -> tname
    | Big_map_t (_, _, tname) -> tname

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

let comparable_ty_eq
  : type ta tb.
    context ->
    ta comparable_ty -> tb comparable_ty ->
    (ta comparable_ty, tb comparable_ty) eq tzresult
  = fun ctxt ta tb -> match ta, tb with
    | Int_key _, Int_key _ -> Ok Eq
    | Nat_key _, Nat_key _ -> Ok Eq
    | String_key _, String_key _ -> Ok Eq
    | Bytes_key _, Bytes_key _ -> Ok Eq
    | Mutez_key _, Mutez_key _ -> Ok Eq
    | Bool_key _, Bool_key _ -> Ok Eq
    | Key_hash_key _, Key_hash_key _ -> Ok Eq
    | Timestamp_key _, Timestamp_key _ -> Ok Eq
    | Address_key _, Address_key _ -> Ok Eq
    | _, _ ->
        serialize_ty_for_error ctxt (ty_of_comparable_ty ta) >>? fun (ta, ctxt) ->
        serialize_ty_for_error ctxt (ty_of_comparable_ty tb) >>? fun (tb, _ctxt) ->
        error (Inconsistent_types (ta, tb))

let record_inconsistent ctxt ta tb =
  record_trace_eval (fun () ->
      serialize_ty_for_error ctxt ta >>? fun (ta, ctxt) ->
      serialize_ty_for_error ctxt tb >|? fun (tb, _ctxt) ->
      Inconsistent_types (ta, tb))

let record_inconsistent_type_annotations ctxt loc ta tb =
  record_trace_eval (fun () ->
      serialize_ty_for_error ctxt ta >>? fun (ta, ctxt) ->
      serialize_ty_for_error ctxt tb >|? fun (tb, _ctxt) ->
      Inconsistent_type_annotations (loc, ta, tb))

let rec ty_eq
  : type ta tb. context -> ta ty -> tb ty -> ((ta ty, tb ty) eq * context) tzresult
  = fun ctxt ta tb ->
    let ok (eq : (ta ty, tb ty) eq) ctxt nb_args :
      ((ta ty, tb ty) eq * context) tzresult =
      Gas.consume ctxt (Typecheck_costs.type_ (2 * nb_args)) >>? fun ctxt ->
      Ok (eq, ctxt) in
    Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
    match ta, tb with
    | Unit_t _, Unit_t _ -> ok Eq ctxt 0
    | Int_t _, Int_t _ -> ok Eq ctxt 0
    | Nat_t _, Nat_t _ -> ok Eq ctxt 0
    | Key_t _, Key_t _ -> ok Eq ctxt 0
    | Key_hash_t _, Key_hash_t _ -> ok Eq ctxt 0
    | String_t _, String_t _ -> ok Eq ctxt 0
    | Bytes_t _, Bytes_t _ -> ok Eq ctxt 0
    | Signature_t _, Signature_t _ -> ok Eq ctxt 0
    | Mutez_t _, Mutez_t _ -> ok Eq ctxt 0
    | Timestamp_t _, Timestamp_t _ -> ok Eq ctxt 0
    | Chain_id_t _, Chain_id_t _ -> ok Eq ctxt 0
    | Address_t _, Address_t _ -> ok Eq ctxt 0
    | Bool_t _, Bool_t _ -> ok Eq ctxt 0
    | Operation_t _, Operation_t _ -> ok Eq ctxt 0
    | Map_t (tal, tar, _, _), Map_t (tbl, tbr, _, _) ->
        (comparable_ty_eq ctxt tal tbl >>? fun Eq ->
         ty_eq ctxt tar tbr >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 2)) |>
        record_inconsistent ctxt ta tb
    | Big_map_t (tal, tar, _), Big_map_t (tbl, tbr, _) ->
        (comparable_ty_eq ctxt tal tbl >>? fun Eq ->
         ty_eq ctxt tar tbr >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 2)) |>
        record_inconsistent ctxt ta tb
    | Set_t (ea, _), Set_t (eb, _) ->
        (comparable_ty_eq ctxt ea eb >>? fun Eq ->
         (ok Eq ctxt 1)) |>
        record_inconsistent ctxt ta tb
    | Pair_t ((tal, _, _), (tar, _, _), _, _),
      Pair_t ((tbl, _, _), (tbr, _, _), _, _) ->
        (ty_eq ctxt tal tbl >>? fun (Eq, ctxt) ->
         ty_eq ctxt tar tbr >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 2)) |>
        record_inconsistent ctxt ta tb
    | Union_t ((tal, _), (tar, _), _, _),
      Union_t ((tbl, _), (tbr, _), _, _) ->
        (ty_eq ctxt tal tbl >>? fun (Eq, ctxt) ->
         ty_eq ctxt tar tbr >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 2)) |>
        record_inconsistent ctxt ta tb
    | Lambda_t (tal, tar, _), Lambda_t (tbl, tbr, _) ->
        (ty_eq ctxt tal tbl >>? fun (Eq, ctxt) ->
         ty_eq ctxt tar tbr >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 2)) |>
        record_inconsistent ctxt ta tb
    | Contract_t (tal, _), Contract_t (tbl, _) ->
        (ty_eq ctxt tal tbl >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 1)) |>
        record_inconsistent ctxt ta tb
    | Option_t (tva, _, _), Option_t (tvb, _, _) ->
        (ty_eq ctxt tva tvb >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 1)) |>
        record_inconsistent ctxt ta tb
    | List_t (tva, _, _), List_t (tvb, _, _) ->
        (ty_eq ctxt tva tvb >>? fun (Eq, ctxt) ->
         (ok Eq ctxt 1)) |>
        record_inconsistent ctxt ta tb
    | _, _ ->
        serialize_ty_for_error ctxt ta >>? fun (ta, ctxt) ->
        serialize_ty_for_error ctxt tb >>? fun (tb, _ctxt) ->
        error (Inconsistent_types (ta, tb))

let rec stack_ty_eq
  : type ta tb. context -> int -> ta stack_ty -> tb stack_ty ->
    ((ta stack_ty, tb stack_ty) eq * context) tzresult
  = fun ctxt lvl ta tb ->
    match ta, tb with
    | Item_t (tva, ra, _), Item_t (tvb, rb, _) ->
        ty_eq ctxt tva tvb |>
        record_trace (Bad_stack_item lvl) >>? fun (Eq, ctxt) ->
        stack_ty_eq ctxt (lvl + 1) ra rb >>? fun (Eq, ctxt) ->
        (Ok (Eq, ctxt) : ((ta stack_ty, tb stack_ty) eq * context) tzresult)
    | Empty_t, Empty_t -> Ok (Eq, ctxt)
    | _, _ -> error Bad_stack_length

let merge_comparable_types
  : type ta. legacy: bool -> ta comparable_ty -> ta comparable_ty -> ta comparable_ty tzresult
  = fun ~legacy ta tb ->
    match ta, tb with
    | Int_key annot_a, Int_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Int_key annot
    | Nat_key annot_a, Nat_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Nat_key annot
    | String_key annot_a, String_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        String_key annot
    | Bytes_key annot_a, Bytes_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Bytes_key annot
    | Mutez_key annot_a, Mutez_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Mutez_key annot
    | Bool_key annot_a, Bool_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Bool_key annot
    | Key_hash_key annot_a, Key_hash_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Key_hash_key annot
    | Timestamp_key annot_a, Timestamp_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Timestamp_key annot
    | Address_key annot_a, Address_key annot_b ->
        merge_type_annot ~legacy annot_a annot_b >|? fun annot ->
        Address_key annot
    | _, _ -> assert false (* FIXME: fix injectivity of some types *)

let merge_types :
  type b. legacy: bool -> context -> Script.location -> b ty -> b ty -> (b ty * context) tzresult = fun ~legacy ->
  let rec help : type a. context -> a ty -> a ty -> (a ty * context) tzresult
    = fun ctxt ty1 ty2 ->
      match ty1, ty2 with
      | Unit_t tn1, Unit_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Unit_t tname, ctxt
      | Int_t tn1, Int_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Int_t tname, ctxt
      | Nat_t tn1, Nat_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Nat_t tname, ctxt
      | Key_t tn1, Key_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Key_t tname, ctxt
      | Key_hash_t tn1, Key_hash_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Key_hash_t tname, ctxt
      | String_t tn1, String_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          String_t tname, ctxt
      | Bytes_t tn1, Bytes_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Bytes_t tname, ctxt
      | Signature_t tn1, Signature_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Signature_t tname, ctxt
      | Mutez_t tn1, Mutez_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Mutez_t tname, ctxt
      | Timestamp_t tn1, Timestamp_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Timestamp_t tname, ctxt
      | Address_t tn1, Address_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Address_t tname, ctxt
      | Bool_t tn1, Bool_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Bool_t tname, ctxt
      | Chain_id_t tn1, Chain_id_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Chain_id_t tname, ctxt
      | Operation_t tn1, Operation_t tn2 ->
          merge_type_annot ~legacy tn1 tn2 >|? fun tname ->
          Operation_t tname, ctxt
      | Map_t (tal, tar, tn1, has_big_map), Map_t (tbl, tbr, tn2, _) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tar tbr >>? fun (value, ctxt) ->
          ty_eq ctxt tar value >>? fun (Eq, ctxt) ->
          merge_comparable_types ~legacy tal tbl >|? fun tk ->
          Map_t (tk, value, tname, has_big_map), ctxt
      | Big_map_t (tal, tar, tn1), Big_map_t (tbl, tbr, tn2) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tar tbr >>? fun (value, ctxt) ->
          ty_eq ctxt tar value >>? fun (Eq, ctxt) ->
          merge_comparable_types ~legacy tal tbl >|? fun tk ->
          Big_map_t (tk, value, tname), ctxt
      | Set_t (ea, tn1), Set_t (eb, tn2) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          merge_comparable_types ~legacy ea eb >|? fun e ->
          Set_t (e, tname), ctxt
      | Pair_t ((tal, l_field1, l_var1), (tar, r_field1, r_var1), tn1, has_big_map),
        Pair_t ((tbl, l_field2, l_var2), (tbr, r_field2, r_var2), tn2, _) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          merge_field_annot ~legacy l_field1 l_field2 >>? fun l_field ->
          merge_field_annot ~legacy r_field1 r_field2 >>? fun r_field ->
          let l_var = merge_var_annot l_var1 l_var2 in
          let r_var = merge_var_annot r_var1 r_var2 in
          help ctxt tal tbl >>? fun (left_ty, ctxt) ->
          help ctxt tar tbr >|? fun (right_ty, ctxt) ->
          Pair_t ((left_ty, l_field, l_var), (right_ty, r_field, r_var), tname, has_big_map),
          ctxt
      | Union_t ((tal, tal_annot), (tar, tar_annot), tn1, has_big_map),
        Union_t ((tbl, tbl_annot), (tbr, tbr_annot), tn2, _) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          merge_field_annot ~legacy tal_annot tbl_annot >>? fun left_annot ->
          merge_field_annot ~legacy tar_annot tbr_annot >>? fun right_annot ->
          help ctxt tal tbl >>? fun (left_ty, ctxt) ->
          help ctxt tar tbr >|? fun (right_ty, ctxt) ->
          Union_t ((left_ty, left_annot), (right_ty, right_annot), tname, has_big_map),
          ctxt
      | Lambda_t (tal, tar, tn1), Lambda_t (tbl, tbr, tn2) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tal tbl >>? fun (left_ty, ctxt) ->
          help ctxt tar tbr >|? fun (right_ty, ctxt) ->
          Lambda_t (left_ty, right_ty, tname), ctxt
      | Contract_t (tal, tn1), Contract_t (tbl, tn2) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tal tbl >|? fun (arg_ty, ctxt) ->
          Contract_t (arg_ty, tname), ctxt
      | Option_t (tva, tn1, has_big_map),
        Option_t (tvb, tn2, _) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tva tvb >|? fun (ty, ctxt) ->
          Option_t (ty, tname, has_big_map), ctxt
      | List_t (tva, tn1, has_big_map), List_t (tvb, tn2, _) ->
          merge_type_annot ~legacy tn1 tn2 >>? fun tname ->
          help ctxt tva tvb >|? fun (ty, ctxt) ->
          List_t (ty, tname, has_big_map), ctxt
      | _, _ -> assert false
  in (fun ctxt loc ty1 ty2 ->
      record_inconsistent_type_annotations ctxt loc ty1 ty2
        (help ctxt ty1 ty2))

let merge_stacks
  : type ta. legacy: bool -> Script.location -> context -> ta stack_ty -> ta stack_ty ->
  (ta stack_ty * context) tzresult
  = fun ~legacy loc ->
    let rec help : type a. context -> a stack_ty -> a stack_ty ->
      (a stack_ty * context) tzresult
      = fun ctxt stack1 stack2 ->
        match stack1, stack2 with
        | Empty_t, Empty_t -> ok (Empty_t, ctxt)
        | Item_t (ty1, rest1, annot1),
          Item_t (ty2, rest2, annot2) ->
            let annot = merge_var_annot annot1 annot2 in
            merge_types ~legacy ctxt loc ty1 ty2 >>? fun (ty, ctxt) ->
            help ctxt rest1 rest2 >|? fun (rest, ctxt) ->
            Item_t (ty, rest, annot), ctxt
    in help

let has_big_map
  : type t. t ty -> bool
  = function
    | Unit_t _ -> false
    | Int_t _ -> false
    | Nat_t _ -> false
    | Signature_t _ -> false
    | String_t _ -> false
    | Bytes_t _ -> false
    | Mutez_t _ -> false
    | Key_hash_t _ -> false
    | Key_t _ -> false
    | Timestamp_t _ -> false
    | Address_t _ -> false
    | Bool_t _ -> false
    | Lambda_t (_, _, _) -> false
    | Set_t (_, _) -> false
    | Big_map_t (_, _, _) -> false
    | Contract_t (_, _) -> false
    | Operation_t _ -> false
    | Chain_id_t _ -> false
    | Pair_t (_, _, _, has_big_map) -> has_big_map
    | Union_t (_, _, _, has_big_map) -> has_big_map
    | Option_t (_, _, has_big_map) -> has_big_map
    | List_t (_, _, has_big_map) -> has_big_map
    | Map_t (_, _, _, has_big_map) -> has_big_map

(* ---- Type checker results -------------------------------------------------*)

type 'bef judgement =
  | Typed : ('bef, 'aft) descr -> 'bef judgement
  | Failed : { descr : 'aft. 'aft stack_ty -> ('bef, 'aft) descr } -> 'bef judgement

(* ---- Type checker (Untyped expressions -> Typed IR) ----------------------*)

type ('t, 'f, 'b) branch =
  { branch : 'r. ('t, 'r) descr -> ('f, 'r) descr -> ('b, 'r) descr } [@@unboxed]


let merge_branches
  : type bef a b. legacy: bool -> context -> int -> a judgement -> b judgement ->
  (a, b, bef) branch ->
  (bef judgement * context) tzresult Lwt.t
  = fun ~legacy ctxt loc btr bfr { branch } ->
    match btr, bfr with
    | Typed ({ aft = aftbt ; _ } as dbt), Typed ({ aft = aftbf ; _ } as dbf) ->
        let unmatched_branches () =
          serialize_stack_for_error ctxt aftbt >>=? fun (aftbt, ctxt) ->
          serialize_stack_for_error ctxt aftbf >>|? fun (aftbf, _ctxt) ->
          Unmatched_branches (loc, aftbt, aftbf) in
        trace_eval unmatched_branches
          (Lwt.return (stack_ty_eq ctxt 1 aftbt aftbf) >>=? fun (Eq, ctxt) ->
           Lwt.return (merge_stacks ~legacy loc ctxt aftbt aftbf) >>=? fun (merged_stack, ctxt) ->
           return (
             Typed (branch {dbt with aft=merged_stack} {dbf with aft=merged_stack}),
             ctxt))
    | Failed { descr = descrt }, Failed { descr = descrf } ->
        let descr ret =
          branch (descrt ret) (descrf ret) in
        return (Failed { descr }, ctxt)
    | Typed dbt, Failed { descr = descrf } ->
        return (Typed (branch dbt (descrf dbt.aft)), ctxt)
    | Failed { descr = descrt }, Typed dbf ->
        return (Typed (branch (descrt dbf.aft) dbf), ctxt)

let rec parse_comparable_ty
  : context -> Script.node -> (ex_comparable_ty * context) tzresult
  = fun ctxt ty ->
    Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
    Gas.consume ctxt (Typecheck_costs.type_ 0) >>? fun ctxt ->
    match ty with
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Int_key tname ), ctxt
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Nat_key tname ), ctxt
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( String_key tname ), ctxt
    | Prim (loc, T_bytes, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Bytes_key tname ), ctxt
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Mutez_key tname ), ctxt
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Bool_key tname ), ctxt
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Key_hash_key tname ), ctxt
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Timestamp_key tname ), ctxt
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot >|? fun tname ->
        Ex_comparable_ty ( Address_key tname ), ctxt
    | Prim (loc, (T_int | T_nat
                 | T_string | T_mutez | T_bool
                 | T_key | T_address | T_timestamp as prim), l, _) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, (T_pair | T_or | T_set | T_map
                 | T_list | T_option  | T_lambda
                 | T_unit | T_signature  | T_contract), _, _) ->
        error (Comparable_type_expected (loc, Micheline.strip_locations ty))
    | expr ->
        error @@ unexpected expr [] Type_namespace
          [ T_int ; T_nat ;
            T_string ; T_mutez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ]

and parse_packable_ty :
  context -> legacy:bool ->
  Script.node -> (ex_ty * context) tzresult
  = fun ctxt ~legacy ->
    parse_ty ctxt ~legacy ~allow_big_map:false ~allow_operation:false ~allow_contract:legacy

and parse_parameter_ty :
  context -> legacy:bool ->
  Script.node -> (ex_ty * context) tzresult
  = fun ctxt ~legacy ->
    parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:false ~allow_contract:true

and parse_any_ty :
  context -> legacy:bool ->
  Script.node -> (ex_ty * context) tzresult
  = fun ctxt ~legacy ->
    parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:true ~allow_contract:true

and parse_ty :
  context ->
  legacy: bool ->
  allow_big_map: bool ->
  allow_operation: bool ->
  allow_contract: bool ->
  Script.node -> (ex_ty * context) tzresult
  = fun ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract node ->
    Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
    match node with
    | Prim (loc, T_unit, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Unit_t ty_name), ctxt
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Int_t ty_name), ctxt
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Nat_t ty_name), ctxt
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (String_t ty_name), ctxt
    | Prim (loc, T_bytes, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Bytes_t ty_name), ctxt
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Mutez_t ty_name), ctxt
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Bool_t ty_name), ctxt
    | Prim (loc, T_key, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Key_t ty_name), ctxt
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Key_hash_t ty_name), ctxt
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Timestamp_t ty_name), ctxt
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Address_t ty_name), ctxt
    | Prim (loc, T_signature, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Signature_t ty_name), ctxt
    | Prim (loc, T_operation, [], annot) ->
        if allow_operation then
          parse_type_annot loc annot >>? fun ty_name ->
          Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
          Ex_ty (Operation_t ty_name), ctxt
        else
          error (Unexpected_operation loc)
    | Prim (loc, T_chain_id, [], annot) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 0) >|? fun ctxt ->
        Ex_ty (Chain_id_t ty_name), ctxt
    | Prim (loc, T_contract, [ utl ], annot) ->
        if allow_contract then
          parse_parameter_ty ctxt ~legacy utl >>? fun (Ex_ty tl, ctxt) ->
          parse_type_annot loc annot >>? fun ty_name ->
          Gas.consume ctxt (Typecheck_costs.type_ 1) >|? fun ctxt ->
          Ex_ty (Contract_t (tl, ty_name)), ctxt
        else
          error (Unexpected_contract loc)
    | Prim (loc, T_pair, [ utl; utr ], annot) ->
        extract_field_annot utl >>? fun (utl, left_field) ->
        extract_field_annot utr >>? fun (utr, right_field) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract utl >>? fun (Ex_ty tl, ctxt) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract utr >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        Ex_ty (Pair_t ((tl, left_field, None), (tr, right_field, None), ty_name, has_big_map tl || has_big_map tr)), ctxt
    | Prim (loc, T_or, [ utl; utr ], annot) ->
        extract_field_annot utl >>? fun (utl, left_constr) ->
        extract_field_annot utr >>? fun (utr, right_constr) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract  utl >>? fun (Ex_ty tl, ctxt) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract  utr >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        Ex_ty (Union_t ((tl, left_constr), (tr, right_constr), ty_name, has_big_map tl || has_big_map tr)), ctxt
    | Prim (loc, T_lambda, [ uta; utr ], annot) ->
        parse_any_ty ctxt ~legacy uta >>? fun (Ex_ty ta, ctxt) ->
        parse_any_ty ctxt ~legacy utr >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        Ex_ty (Lambda_t (ta, tr, ty_name)), ctxt
    | Prim (loc, T_option, [ ut ], annot) ->
        begin if legacy then
            (* legacy semantics with (broken) field annotations *)
            extract_field_annot ut >>? fun (ut, _some_constr) ->
            parse_composed_type_annot loc annot >>? fun (ty_name, _none_constr, _) ->
            ok (ut, ty_name)
          else
            parse_type_annot loc annot >>? fun ty_name ->
            ok (ut, ty_name)
        end >>? fun (ut, ty_name) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract ut >>? fun (Ex_ty t, ctxt) ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        Ex_ty (Option_t (t, ty_name, has_big_map t)), ctxt
    | Prim (loc, T_list, [ ut ], annot) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract ut >>? fun (Ex_ty t, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 1) >|? fun ctxt ->
        Ex_ty (List_t (t, ty_name, has_big_map t)), ctxt
    | Prim (loc, T_set, [ ut ], annot) ->
        parse_comparable_ty ctxt ut >>? fun (Ex_comparable_ty t, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 1) >|? fun ctxt ->
        Ex_ty (Set_t (t, ty_name)), ctxt
    | Prim (loc, T_map, [ uta; utr ], annot) ->
        parse_comparable_ty ctxt uta >>? fun (Ex_comparable_ty ta, ctxt) ->
        parse_ty ctxt ~legacy ~allow_big_map ~allow_operation ~allow_contract utr >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot >>? fun ty_name ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        Ex_ty (Map_t (ta, tr, ty_name, has_big_map tr)), ctxt
    | Prim (loc, T_big_map, args, annot)
      when allow_big_map ->
        parse_big_map_ty ctxt ~legacy loc args annot >>? fun (big_map_ty, ctxt) ->
        Gas.consume ctxt (Typecheck_costs.type_ 2) >|? fun ctxt ->
        big_map_ty, ctxt
    | Prim (loc, T_big_map, _, _) ->
        error (Unexpected_big_map loc)
    | Prim (loc, (T_unit | T_signature
                 | T_int | T_nat
                 | T_string | T_bytes | T_mutez | T_bool
                 | T_key | T_key_hash
                 | T_timestamp | T_address as prim), l, _) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, (T_set | T_list | T_option | T_contract as prim), l, _) ->
        error (Invalid_arity (loc, prim, 1, List.length l))
    | Prim (loc, (T_pair | T_or | T_map | T_lambda as prim), l, _) ->
        error (Invalid_arity (loc, prim, 2, List.length l))
    | expr ->
        error @@ unexpected expr [] Type_namespace
          [ T_pair ; T_or ; T_set ; T_map ;
            T_list ; T_option  ; T_lambda ;
            T_unit ; T_signature  ; T_contract ;
            T_int ; T_nat ; T_operation ;
            T_string ; T_bytes ; T_mutez ; T_bool ;
            T_key ; T_key_hash ; T_timestamp ; T_chain_id ]

and parse_big_map_ty ctxt ~legacy big_map_loc args map_annot =
  Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
  begin match args with
    | [ key_ty ; value_ty ] ->
        parse_comparable_ty ctxt key_ty >>? fun (Ex_comparable_ty key_ty, ctxt) ->
        parse_packable_ty ctxt ~legacy value_ty
        >>? fun (Ex_ty value_ty, ctxt) ->
        parse_type_annot big_map_loc map_annot >|? fun map_name ->
        let big_map_ty = Big_map_t (key_ty, value_ty, map_name) in
        Ex_ty big_map_ty, ctxt
    | args -> error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)
  end

and parse_storage_ty :
  context -> legacy:bool -> Script.node -> (ex_ty * context) tzresult
  = fun ctxt ~legacy node ->
    match node with
    | Prim (loc, T_pair,
            [ Prim (big_map_loc, T_big_map, args, map_annot) ; remaining_storage ],
            storage_annot) when legacy ->
        begin match storage_annot with
          | [] ->
              parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:false ~allow_contract:legacy node
          | [ single ] when Compare.Int.(String.length single > 0) && Compare.Char.(String.get single 0 = '%') ->
              parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:false ~allow_contract:legacy node
          | _ ->
              (* legacy semantics of big maps used the wrong annotation parser *)
              Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
              parse_big_map_ty ctxt ~legacy big_map_loc args map_annot >>? fun (Ex_ty big_map_ty, ctxt) ->
              parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:false ~allow_contract:legacy remaining_storage
              >>? fun (Ex_ty remaining_storage, ctxt) ->
              parse_composed_type_annot loc storage_annot
              >>? fun (ty_name, map_field, storage_field) ->
              Gas.consume ctxt (Typecheck_costs.type_ 5) >|? fun ctxt ->
              Ex_ty (Pair_t ((big_map_ty, map_field, None),
                             (remaining_storage, storage_field, None),
                             ty_name, true)),
              ctxt
        end
    | _ ->
        parse_ty ctxt ~legacy ~allow_big_map:true ~allow_operation:false ~allow_contract:legacy node

let check_packable ~legacy loc root =
  let rec check : type t. t ty -> unit tzresult = function
    | Big_map_t _ -> error (Unexpected_big_map loc)
    | Operation_t _ -> error (Unexpected_operation loc)
    | Unit_t _ -> ok ()
    | Int_t _ -> ok ()
    | Nat_t _ -> ok ()
    | Signature_t _ -> ok ()
    | String_t _ -> ok ()
    | Bytes_t _ -> ok ()
    | Mutez_t _ -> ok ()
    | Key_hash_t _ -> ok ()
    | Key_t _ -> ok ()
    | Timestamp_t _ -> ok ()
    | Address_t _ -> ok ()
    | Bool_t _ -> ok ()
    | Chain_id_t _ -> ok ()
    | Pair_t ((l_ty, _, _), (r_ty, _, _), _, _) ->
        check l_ty >>? fun () -> check r_ty
    | Union_t ((l_ty, _), (r_ty, _), _, _) ->
        check l_ty >>? fun () -> check r_ty
    | Option_t (v_ty, _, _) -> check v_ty
    | List_t (elt_ty, _, _) -> check elt_ty
    | Set_t (_, _) -> ok ()
    | Map_t (_, elt_ty, _, _) -> check elt_ty
    | Lambda_t (_l_ty, _r_ty, _) -> ok ()
    | Contract_t (_, _) when legacy -> ok ()
    | Contract_t (_, _) -> error (Unexpected_contract loc) in
  check root

type ex_script = Ex_script : ('a, 'c) script -> ex_script

type _ dig_proof_argument =
    Dig_proof_argument
    : ((('x * 'rest), 'rest, 'bef, 'aft) stack_prefix_preservation_witness
       * ('x ty * var_annot option)
       * 'aft stack_ty)
    -> 'bef dig_proof_argument

type (_, _) dug_proof_argument =
    Dug_proof_argument
    : (('rest, ('x * 'rest), 'bef, 'aft) stack_prefix_preservation_witness
       * unit
       * 'aft stack_ty)
    -> ('bef, 'x) dug_proof_argument

type (_) dipn_proof_argument =
    Dipn_proof_argument
    : (('fbef, 'faft, 'bef, 'aft) stack_prefix_preservation_witness
       * (context * ('fbef, 'faft) descr)
       * 'aft stack_ty)
    -> 'bef dipn_proof_argument

type (_) dropn_proof_argument =
    Dropn_proof_argument
    : (('rest, 'rest, 'bef, 'aft) stack_prefix_preservation_witness
       * 'rest stack_ty
       * 'aft stack_ty)
    -> 'bef dropn_proof_argument

(* Lwt versions *)
let parse_var_annot loc ?default annot =
  Lwt.return (parse_var_annot loc ?default annot)
let parse_entrypoint_annot loc ?default annot =
  Lwt.return (parse_entrypoint_annot loc ?default annot)
let parse_constr_annot loc ?if_special_first ?if_special_second annot =
  Lwt.return (parse_constr_annot loc ?if_special_first ?if_special_second annot)
let parse_two_var_annot loc annot =
  Lwt.return (parse_two_var_annot loc annot)
let parse_destr_annot loc annot ~default_accessor ~field_name ~pair_annot ~value_annot =
  Lwt.return (parse_destr_annot loc annot ~default_accessor ~field_name ~pair_annot ~value_annot)
let parse_var_type_annot loc annot =
  Lwt.return (parse_var_type_annot loc annot)


let find_entrypoint (type full) (full : full ty) ~root_name entrypoint =
  let rec find_entrypoint
    : type t. t ty -> string -> ((Script.node -> Script.node) * ex_ty)
    = fun t entrypoint -> match t with
      | Union_t ((tl, al), (tr, ar), _, _) ->
          if match al with None -> false | Some (`Field_annot l) -> Compare.String.(l = entrypoint) then
            ((fun e -> Prim (0, D_Left, [ e ], [])), Ex_ty tl)
          else if match ar with None -> false | Some (`Field_annot r) -> Compare.String.(r = entrypoint) then
            ((fun e -> Prim (0, D_Right, [ e ], [])), Ex_ty tr)
          else begin try
              let (f, t) = find_entrypoint tl entrypoint in
              ((fun e -> Prim (0, D_Left, [ f e ], [])), t)
            with Not_found ->
              let (f, t) = find_entrypoint tr entrypoint in
              ((fun e -> Prim (0, D_Right, [ f e ], [])), t)
          end
      | _ -> raise Not_found in
  let entrypoint = if Compare.String.(entrypoint = "") then "default" else entrypoint in
  if Compare.Int.(String.length entrypoint > 31) then
    error (Entrypoint_name_too_long entrypoint)
  else match root_name with
    | Some root_name when Compare.String.(entrypoint = root_name) ->
        ok ((fun e -> e), Ex_ty full)
    | _ ->
        try ok (find_entrypoint full entrypoint) with Not_found ->
        match entrypoint with
        | "default" -> ok ((fun e -> e), Ex_ty full)
        | _ -> error (No_such_entrypoint entrypoint)

let find_entrypoint_for_type
    (type full) (type exp) ~(full : full ty) ~(expected : exp ty) ~root_name entrypoint ctxt
  : (context * string * exp ty) tzresult =
  match entrypoint, root_name with
  | "default", Some "root" ->
      begin match find_entrypoint full ~root_name entrypoint with
        | Error _ as err -> err
        | Ok (_, Ex_ty ty) ->
            match ty_eq ctxt expected ty with
            | Ok (Eq, ctxt) ->
                ok (ctxt, "default", (ty : exp ty))
            | Error _ ->
                ty_eq ctxt expected full >>? fun (Eq, ctxt) ->
                ok (ctxt, "root", (full : exp ty))
      end
  | _ ->
      find_entrypoint full ~root_name entrypoint >>? fun (_, Ex_ty ty) ->
      ty_eq ctxt expected ty >>? fun (Eq, ctxt) ->
      ok (ctxt, entrypoint, (ty : exp ty))


module Entrypoints = Set.Make (String)

exception Duplicate of string
exception Too_long of string

let well_formed_entrypoints (type full) (full : full ty) ~root_name =
  let merge path annot (type t) (ty : t ty) reachable ((first_unreachable, all) as acc) =
    match annot with
    | None | Some (`Field_annot "") ->
        if reachable then acc
        else begin match ty with
          | Union_t _ -> acc
          | _ -> match first_unreachable with
            | None -> (Some (List.rev path), all)
            | Some _ -> acc
        end
    | Some (`Field_annot name) ->
        if Compare.Int.(String.length name > 31) then raise (Too_long name)
        else if Entrypoints.mem name all then raise (Duplicate name)
        else (first_unreachable, Entrypoints.add name all) in
  let rec check
    : type t. t ty -> prim list -> bool -> (prim list) option * Entrypoints.t -> (prim list) option * Entrypoints.t
    = fun t path reachable acc ->
      match t with
      | Union_t ((tl, al), (tr, ar), _, _) ->
          let acc = merge (D_Left :: path) al tl reachable acc in
          let acc = merge (D_Right :: path) ar tr reachable acc in
          let acc = check tl (D_Left :: path) (match al with Some _ -> true | None -> reachable) acc in
          check tr (D_Right :: path) (match ar with Some _ -> true | None -> reachable) acc
      | _ -> acc in
  try
    let init, reachable = match root_name with
      | None | Some "" -> Entrypoints.empty, false
      | Some name -> Entrypoints.singleton name, true in
    let first_unreachable, all = check full [] reachable (None, init) in
    if not (Entrypoints.mem "default" all) then ok ()
    else match first_unreachable with
      | None -> ok ()
      | Some path -> error (Unreachable_entrypoint path)
  with
  | Duplicate name -> error (Duplicate_entrypoint name)
  | Too_long name -> error (Entrypoint_name_too_long name)

let rec parse_data
  : type a.
    ?type_logger: type_logger ->
    context -> legacy: bool -> a ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun ?type_logger ctxt ~legacy ty script_data ->
    Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
    let error () =
      Lwt.return (serialize_ty_for_error ctxt ty) >>|? fun (ty, _ctxt) ->
      Invalid_constant (location script_data, strip_locations script_data, ty) in
    let traced body =
      trace_eval error body in
    let parse_items ?type_logger loc ctxt expr key_type value_type items item_wrapper =
      let length = List.length items in
      fold_left_s
        (fun (last_value, map, ctxt) item ->
           Lwt.return (Gas.consume ctxt (Typecheck_costs.map_element length)) >>=? fun ctxt ->
           match item with
           | Prim (_, D_Elt, [ k; v ], _) ->
               parse_comparable_data ?type_logger ctxt key_type k >>=? fun (k, ctxt) ->
               parse_data ?type_logger ctxt ~legacy value_type v >>=? fun (v, ctxt) ->
               begin match last_value with
                 | Some value ->
                     if Compare.Int.(0 <= (compare_comparable key_type value k))
                     then
                       if Compare.Int.(0 = (compare_comparable key_type value k))
                       then fail (Duplicate_map_keys (loc, strip_locations expr))
                       else fail (Unordered_map_keys (loc, strip_locations expr))
                     else return_unit
                 | None -> return_unit
               end >>=? fun () ->
               return (Some k, map_update k (Some (item_wrapper v)) map, ctxt)
           | Prim (loc, D_Elt, l, _) ->
               fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
           | Prim (loc, name, _, _) ->
               fail @@ Invalid_primitive (loc, [ D_Elt ], name)
           | Int _ | String _ | Bytes _ | Seq _ ->
               error () >>=? fail)
        (None, empty_map key_type, ctxt) items |> traced >>|? fun (_, items, ctxt) ->
      (items, ctxt) in
    match ty, script_data with
    (* Unit *)
    | Unit_t _, Prim (loc, D_Unit, [], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.unit) >>|? fun ctxt ->
        ((() : a), ctxt)
    | Unit_t _, Prim (loc, D_Unit, l, _) ->
        traced (fail (Invalid_arity (loc, D_Unit, 0, List.length l)))
    | Unit_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Unit ]))
    (* Booleans *)
    | Bool_t _, Prim (loc, D_True, [], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (true, ctxt)
    | Bool_t _, Prim (loc, D_False, [], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.bool) >>|? fun ctxt ->
        (false, ctxt)
    | Bool_t _, Prim (loc, (D_True | D_False as c), l, _) ->
        traced (fail (Invalid_arity (loc, c, 0, List.length l)))
    | Bool_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_True ; D_False ]))
    (* Strings *)
    | String_t _, String (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.string (String.length v))) >>=? fun ctxt ->
        let rec check_printable_ascii i =
          if Compare.Int.(i < 0) then true
          else match String.get v i with
            | '\n' | '\x20'..'\x7E' -> check_printable_ascii (i - 1)
            | _ -> false in
        if check_printable_ascii (String.length v - 1) then
          return (v, ctxt)
        else
          error () >>=? fail
    | String_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ], kind expr)))
    (* Byte sequences *)
    | Bytes_t _, Bytes (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.string (MBytes.length v))) >>=? fun ctxt ->
        return (v, ctxt)
    | Bytes_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Bytes_kind ], kind expr)))
    (* Integers *)
    | Int_t _, Int (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.z v)) >>=? fun ctxt ->
        return (Script_int.of_zint v, ctxt)
    | Nat_t _, Int (_, v) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.z v)) >>=? fun ctxt ->
        let v = Script_int.of_zint v in
        if Compare.Int.(Script_int.compare v Script_int.zero >= 0) then
          return (Script_int.abs v, ctxt)
        else
          error () >>=? fail
    | Int_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    | Nat_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Tez amounts *)
    | Mutez_t _, Int (_, v) ->
        Lwt.return (
          Gas.consume ctxt Typecheck_costs.tez >>? fun ctxt ->
          Gas.consume ctxt Michelson_v1_gas.Cost_of.Legacy.z_to_int64
        ) >>=? fun ctxt ->
        begin try
            match Tez.of_mutez (Z.to_int64 v) with
            | None -> raise Exit
            | Some tez -> return (tez, ctxt)
          with _ ->
            error () >>=? fail
        end
    | Mutez_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Int_kind ], kind expr)))
    (* Timestamps *)
    | Timestamp_t _, (Int (_, v)) (* As unparsed with [Optimized] or out of bounds [Readable]. *) ->
        Lwt.return (Gas.consume ctxt (Typecheck_costs.z v)) >>=? fun ctxt ->
        return (Script_timestamp.of_zint v, ctxt)
    | Timestamp_t _, String (_, s) (* As unparsed with [Redable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.string_timestamp) >>=? fun ctxt ->
        begin match Script_timestamp.of_string s with
          | Some v -> return (v, ctxt)
          | None -> error () >>=? fail
        end
    | Timestamp_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Int_kind ], kind expr)))
    (* IDs *)
    | Key_t _, Bytes (_, bytes) -> (* As unparsed with [Optimized]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key) >>=? fun ctxt ->
        begin match Data_encoding.Binary.of_bytes Signature.Public_key.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Key_t _, String (_, s) -> (* As unparsed with [Readable]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key) >>=? fun ctxt ->
        begin match Signature.Public_key.of_b58check_opt s with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Key_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    | Key_hash_t _, Bytes (_, bytes) -> (* As unparsed with [Optimized]. *)
        Lwt.return (Gas.consume ctxt Typecheck_costs.key_hash) >>=? fun ctxt ->
        begin
          match Data_encoding.Binary.of_bytes Signature.Public_key_hash.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Key_hash_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.key_hash) >>=? fun ctxt ->
        begin match Signature.Public_key_hash.of_b58check_opt s with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Key_hash_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Signatures *)
    | Signature_t _, Bytes (_, bytes) (* As unparsed with [Optimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.signature) >>=? fun ctxt ->
        begin match Data_encoding.Binary.of_bytes Signature.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Signature_t _, String (_, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.signature) >>=? fun ctxt ->
        begin match Signature.of_b58check_opt s with
          | Some s -> return (s, ctxt)
          | None -> error () >>=? fail
        end
    | Signature_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Operations *)
    | Operation_t _, _ ->
        (* operations cannot appear in parameters or storage,
           the protocol should never parse the bytes of an operation *)
        assert false
    (* Chain_ids *)
    | Chain_id_t _, Bytes (_, bytes) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.chain_id) >>=? fun ctxt ->
        begin match Data_encoding.Binary.of_bytes Chain_id.encoding bytes with
          | Some k -> return (k, ctxt)
          | None -> error () >>=? fail
        end
    | Chain_id_t _, String (_,  s) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.chain_id) >>=? fun ctxt ->
        begin match Chain_id.of_b58check_opt s with
          | Some s -> return (s, ctxt)
          | None -> error () >>=? fail
        end
    | Chain_id_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Addresses *)
    | Address_t _, Bytes (loc, bytes) (* As unparsed with [O[ptimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin
          match Data_encoding.Binary.of_bytes
                  Data_encoding.(tup2 Contract.encoding Variable.string)
                  bytes with
          | Some (c, entrypoint) ->
              if Compare.Int.(String.length entrypoint > 31) then
                fail (Entrypoint_name_too_long entrypoint)
              else
                begin match entrypoint with
                  | "" -> return "default"
                  | "default" -> fail (Unexpected_annotation loc)
                  | name -> return name end >>=? fun entrypoint ->
                return ((c, entrypoint), ctxt)
          | None -> error () >>=? fail
        end
    | Address_t _, String (loc, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin match String.index_opt s '%' with
          | None -> return (s, "default")
          | Some pos ->
              let len = String.length s - pos - 1 in
              let name = String.sub s (pos + 1) len in
              if Compare.Int.(len > 31) then
                fail (Entrypoint_name_too_long name)
              else
                match String.sub s 0 pos, name with
                | _, "default" -> traced (fail (Unexpected_annotation loc))
                | addr_and_name -> return addr_and_name
        end >>=? fun (addr, entrypoint) ->
        Lwt.return (Contract.of_b58check addr) >>=? fun c ->
        return ((c, entrypoint), ctxt)
    | Address_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Contracts *)
    | Contract_t (ty, _), Bytes (loc, bytes) (* As unparsed with [Optimized]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin
          match Data_encoding.Binary.of_bytes
                  Data_encoding.(tup2 Contract.encoding Variable.string)
                  bytes with
          | Some (c, entrypoint) ->
              if Compare.Int.(String.length entrypoint > 31) then
                fail (Entrypoint_name_too_long entrypoint)
              else
                begin match entrypoint with
                  | "" -> return "default"
                  | "default" -> traced (fail (Unexpected_annotation loc))
                  | name -> return name end >>=? fun entrypoint ->
                traced (parse_contract ~legacy ctxt loc ty c ~entrypoint) >>=? fun (ctxt, _) ->
                return ((ty, (c, entrypoint)), ctxt)
          | None -> error () >>=? fail
        end
    | Contract_t (ty, _), String (loc, s) (* As unparsed with [Readable]. *) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.contract) >>=? fun ctxt ->
        begin match String.index_opt s '%' with
          | None -> return (s, "default")
          | Some pos ->
              let len = String.length s - pos - 1 in
              let name = String.sub s (pos + 1) len in
              if Compare.Int.(len > 31) then
                fail (Entrypoint_name_too_long name)
              else
                match String.sub s 0 pos, name with
                | _, "default" -> traced (fail (Unexpected_annotation loc))
                | addr_and_name -> return addr_and_name
        end >>=? fun (addr, entrypoint) ->
        traced (Lwt.return (Contract.of_b58check addr)) >>=? fun c ->
        parse_contract ~legacy ctxt loc ty c ~entrypoint >>=? fun (ctxt, _) ->
        return ((ty, (c, entrypoint)), ctxt)
    | Contract_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ String_kind ; Bytes_kind ], kind expr)))
    (* Pairs *)
    | Pair_t ((ta, _, _), (tb, _, _), _, _), Prim (loc, D_Pair, [ va; vb ], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.pair) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~legacy ta va >>=? fun (va, ctxt) ->
        parse_data ?type_logger ctxt ~legacy tb vb >>=? fun (vb, ctxt) ->
        return ((va, vb), ctxt)
    | Pair_t _, Prim (loc, D_Pair, l, _) ->
        fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
    | Pair_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Pair ]))
    (* Unions *)
    | Union_t ((tl, _), _, _, _), Prim (loc, D_Left, [ v ], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~legacy tl v >>=? fun (v, ctxt) ->
        return (L v, ctxt)
    | Union_t _, Prim (loc, D_Left, l, _) ->
        fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
    | Union_t (_, (tr, _), _, _), Prim (loc, D_Right, [ v ], annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.union) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~legacy tr v >>=? fun (v, ctxt) ->
        return (R v, ctxt)
    | Union_t _, Prim (loc, D_Right, l, _) ->
        fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
    | Union_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Left ; D_Right ]))
    (* Lambdas *)
    | Lambda_t (ta, tr, _ty_name), (Seq (_loc, _) as script_instr) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.lambda) >>=? fun ctxt ->
        traced @@
        parse_returning Lambda ?type_logger ctxt ~legacy (ta, Some (`Var_annot "@arg")) tr script_instr
    | Lambda_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Options *)
    | Option_t (t, _, _), Prim (loc, D_Some, [ v ], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.some) >>=? fun ctxt ->
        traced @@
        parse_data ?type_logger ctxt ~legacy t v >>=? fun (v, ctxt) ->
        return (Some v, ctxt)
    | Option_t _, Prim (loc, D_Some, l, _) ->
        fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
    | Option_t (_, _, _), Prim (loc, D_None, [], annot) ->
        (if legacy then return () else
           fail_unexpected_annot loc annot) >>=? fun () ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.none) >>=? fun ctxt ->
        return (None, ctxt)
    | Option_t _, Prim (loc, D_None, l, _) ->
        fail @@ Invalid_arity (loc, D_None, 0, List.length l)
    | Option_t _, expr ->
        traced (fail (unexpected expr [] Constant_namespace [ D_Some ; D_None ]))
    (* Lists *)
    | List_t (t, _ty_name, _), Seq (_loc, items) ->
        traced @@
        fold_right_s
          (fun v (rest, ctxt) ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.list_element) >>=? fun ctxt ->
             parse_data ?type_logger ctxt ~legacy  t v >>=? fun (v, ctxt) ->
             return ((v :: rest), ctxt))
          items ([], ctxt)
    | List_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Sets *)
    | Set_t (t, _ty_name), (Seq (loc, vs) as expr) ->
        let length = List.length vs in
        traced @@
        fold_left_s
          (fun (last_value, set, ctxt) v ->
             Lwt.return (Gas.consume ctxt (Typecheck_costs.set_element length)) >>=? fun ctxt ->
             parse_comparable_data ?type_logger ctxt t v >>=? fun (v, ctxt) ->
             begin match last_value with
               | Some value ->
                   if Compare.Int.(0 <= (compare_comparable t value v))
                   then
                     if Compare.Int.(0 = (compare_comparable t value v))
                     then fail (Duplicate_set_values (loc, strip_locations expr))
                     else fail (Unordered_set_values (loc, strip_locations expr))
                   else return_unit
               | None -> return_unit
             end >>=? fun () ->
             Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.Legacy.set_update v false set)) >>=? fun ctxt ->
             return (Some v, set_update v true set, ctxt))
          (None, empty_set t, ctxt) vs >>|? fun (_, set, ctxt) ->
        (set, ctxt)
    | Set_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    (* Maps *)
    | Map_t (tk, tv, _ty_name, _), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> x)
    | Map_t _, expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ], kind expr)))
    | Big_map_t (tk, tv, _ty_name), (Seq (loc, vs) as expr) ->
        parse_items ?type_logger loc ctxt expr tk tv vs (fun x -> Some x) >>|? fun (diff, ctxt) ->
        ({ id = None ; diff ; key_type = ty_of_comparable_ty tk ; value_type = tv }, ctxt)
    | Big_map_t (tk, tv, _ty_name), Int (loc, id) ->
        Big_map.exists ctxt id >>=? begin function
          | _, None ->
              traced (fail (Invalid_big_map (loc, id)))
          | ctxt, Some (btk, btv) ->
              Lwt.return begin
                parse_comparable_ty ctxt (Micheline.root btk) >>? fun (Ex_comparable_ty btk, ctxt) ->
                parse_packable_ty ctxt ~legacy (Micheline.root btv) >>? fun (Ex_ty btv, ctxt) ->
                comparable_ty_eq ctxt tk btk >>? fun Eq ->
                ty_eq ctxt tv btv >>? fun (Eq, ctxt) ->
                ok ({ id = Some id ; diff = empty_map tk ; key_type = ty_of_comparable_ty tk ; value_type = tv }, ctxt)
              end
        end
    | Big_map_t (_tk, _tv, _), expr ->
        traced (fail (Invalid_kind (location expr, [ Seq_kind ; Int_kind ], kind expr)))

and parse_comparable_data
  : type a.
    ?type_logger:type_logger ->
    context -> a comparable_ty -> Script.node -> (a * context) tzresult Lwt.t
  = fun ?type_logger ctxt ty script_data ->
    parse_data ?type_logger ctxt ~legacy: false (ty_of_comparable_ty ty) script_data

and parse_returning
  : type arg ret.
    ?type_logger: type_logger ->
    tc_context -> context -> legacy:bool ->
  arg ty * var_annot option -> ret ty -> Script.node ->
  ((arg, ret) lambda * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt ~legacy (arg, arg_annot) ret script_instr ->
    parse_instr ?type_logger tc_context ctxt ~legacy
      script_instr (Item_t (arg, Empty_t, arg_annot)) >>=? function
    | (Typed ({ loc ; aft = (Item_t (ty, Empty_t, _) as stack_ty) ; _ } as descr), ctxt) ->
        trace_eval
          (fun () ->
             Lwt.return (serialize_ty_for_error ctxt ret) >>=? fun (ret, ctxt) ->
             serialize_stack_for_error ctxt stack_ty >>|? fun (stack_ty, _ctxt) ->
             Bad_return (loc, stack_ty, ret))
          (Lwt.return (ty_eq ctxt ty ret) >>=? fun (Eq, ctxt) ->
           Lwt.return (merge_types ~legacy ctxt loc ty ret) >>=? fun (_ret, ctxt) ->
           return ((Lam (descr, script_instr) : (arg, ret) lambda), ctxt))
    | (Typed { loc ; aft = stack_ty ; _ }, ctxt) ->
        Lwt.return (serialize_ty_for_error ctxt ret) >>=? fun (ret, ctxt) ->
        serialize_stack_for_error ctxt stack_ty >>=? fun (stack_ty, _ctxt) ->
        fail (Bad_return (loc, stack_ty, ret))
    | (Failed { descr }, ctxt) ->
        return ((Lam (descr (Item_t (ret, Empty_t, None)), script_instr)
                 : (arg, ret) lambda), ctxt)

and parse_int32 (n : (location, prim) Micheline.node) : int tzresult =
  let error' () =
    Invalid_syntactic_constant (location n, strip_locations n,
                                "a positive 32-bit integer (between 0 and "
                                ^ (Int32.to_string Int32.max_int) ^ ")") in
  match n with
  | Micheline.Int (_, n') ->
      begin try
          let n'' = Z.to_int n' in
          if (Compare.Int.(0 <= n'')) && (Compare.Int.(n'' <= Int32.to_int Int32.max_int)) then
            ok n''
          else
            error @@ error' ()
        with _ ->
          error @@ error' ()
      end
  | _ -> error @@ error' ()

and parse_instr
  : type bef.
    ?type_logger: type_logger ->
    tc_context -> context -> legacy: bool ->
  Script.node -> bef stack_ty -> (bef judgement * context) tzresult Lwt.t =
  fun ?type_logger tc_context ctxt ~legacy script_instr stack_ty ->
    let _check_item check loc name n m =
      trace_eval (fun () ->
          serialize_stack_for_error ctxt stack_ty >>|? fun (stack_ty, _ctxt) ->
          Bad_stack (loc, name, m, stack_ty)) @@
      trace (Bad_stack_item n) @@
      Lwt.return check in
    let check_item_ty
        (type a) (type b)
        ctxt (exp : a ty) (got : b ty) loc name n m
      : ((a, b) eq * a ty * context) tzresult Lwt.t =
      trace_eval (fun () ->
          serialize_stack_for_error ctxt stack_ty >>|? fun (stack_ty, _ctxt) ->
          Bad_stack (loc, name, m, stack_ty)) @@
      trace (Bad_stack_item n) @@ Lwt.return begin
        ty_eq ctxt exp got >>? fun (Eq, ctxt) ->
        merge_types ~legacy ctxt loc exp got >>? fun (ty, ctxt) ->
        ok ((Eq : (a, b) eq), (ty : a ty), ctxt)
      end in
    let check_item_comparable_ty
        (type a) (type b)
        (exp : a comparable_ty) (got : b comparable_ty) loc name n m
      : ((a, b) eq * a comparable_ty) tzresult Lwt.t =
      trace_eval (fun () ->
          serialize_stack_for_error ctxt stack_ty >>|? fun (stack_ty, _ctxt) ->
          Bad_stack (loc, name, m, stack_ty)) @@
      trace (Bad_stack_item n) @@ Lwt.return begin
        comparable_ty_eq ctxt exp got >>? fun Eq ->
        merge_comparable_types ~legacy exp got >>? fun ty ->
        ok ((Eq : (a, b) eq), (ty : a comparable_ty))
      end in
    let log_stack ctxt loc stack_ty aft =
      match type_logger, script_instr with
      | None, _
      | Some _, (Seq (-1, _) | Int _ | String _ | Bytes _) -> return_unit
      | Some log, (Prim _ | Seq _) ->
          (* Unparsing for logging done in an unlimited context as this
             is used only by the client and not the protocol *)
          let ctxt = Gas.set_unlimited ctxt in
          unparse_stack ctxt stack_ty >>=? fun (stack_ty, _) ->
          unparse_stack ctxt aft >>=? fun (aft, _) ->
          log loc stack_ty aft;
          return_unit
    in
    let outer_return = return in
    let return : type bef .
      context -> bef judgement -> (bef judgement * context) tzresult Lwt.t = fun ctxt judgement ->
      match judgement with
      | Typed { instr ; loc ; aft ; _ } ->
          let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
          let type_size =
            type_size_of_stack_head aft
              ~up_to:(number_of_generated_growing_types instr) in
          if Compare.Int.(type_size > maximum_type_size) then
            fail (Type_too_large (loc, type_size, maximum_type_size))
          else
            return (judgement, ctxt)
      | Failed _ ->
          return (judgement, ctxt) in
    let typed ctxt loc instr aft =
      log_stack ctxt loc stack_ty aft >>=? fun () ->
      Lwt.return @@ Gas.consume ctxt (Typecheck_costs.instr instr) >>=? fun ctxt ->
      return ctxt (Typed { loc ; instr ; bef = stack_ty ; aft }) in
    Lwt.return @@ Gas.consume ctxt Typecheck_costs.cycle >>=? fun ctxt ->
    match script_instr, stack_ty with
    (* stack ops *)
    | Prim (loc, I_DROP, [], annot),
      Item_t (_, rest, _) ->
        (fail_unexpected_annot loc annot >>=? fun () ->
         typed ctxt loc Drop rest : (bef judgement * context) tzresult Lwt.t)
    | Prim (loc, I_DROP, [n], result_annot), whole_stack ->
        Lwt.return (parse_int32 n) >>=? fun whole_n ->
        let rec make_proof_argument
          : type tstk . int -> (tstk stack_ty) -> (tstk dropn_proof_argument) tzresult Lwt.t =
          fun n stk ->
            match (Compare.Int.(n = 0)), stk with
              true, rest ->
                outer_return @@ (Dropn_proof_argument (Rest, rest, rest))
            | false, Item_t (v, rest, annot) ->
                make_proof_argument (n - 1) rest
                >>=? fun (Dropn_proof_argument (n', stack_after_drops, aft')) ->
                outer_return @@ (Dropn_proof_argument (Prefix n', stack_after_drops, Item_t (v, aft', annot)))
            | _, _ ->
                serialize_stack_for_error ctxt whole_stack >>=? fun (whole_stack, _ctxt) ->
                fail (Bad_stack (loc, I_DROP, whole_n, whole_stack))
        in
        fail_unexpected_annot loc result_annot >>=? fun () ->
        make_proof_argument whole_n whole_stack >>=? fun (Dropn_proof_argument (n', stack_after_drops, _aft)) ->
        typed ctxt loc (Dropn (whole_n, n')) stack_after_drops
    | Prim (loc, I_DROP, (_ :: _ :: _ as l), _), _ ->
        (* Technically, the arities 0 and 1 are allowed but the error only mentions 1.
           However, DROP is equivalent to DROP 1 so hinting at an arity of 1 makes sense. *)
        fail (Invalid_arity (loc, I_DROP, 1, List.length l))
    | Prim (loc, I_DUP, [], annot),
      Item_t (v, rest, stack_annot) ->
        parse_var_annot loc annot ~default:stack_annot >>=? fun annot ->
        typed ctxt loc Dup
          (Item_t (v, Item_t (v, rest, stack_annot), annot))
    | Prim (loc, I_DIG, [n], result_annot), stack ->
        let rec make_proof_argument
          : type tstk . int -> (tstk stack_ty) -> (tstk dig_proof_argument) tzresult Lwt.t =
          fun n stk ->
            match (Compare.Int.(n = 0)), stk with
              true, Item_t (v, rest, annot) ->
                outer_return @@ (Dig_proof_argument (Rest, (v, annot), rest))
            | false, Item_t (v, rest, annot) ->
                make_proof_argument (n - 1) rest
                >>=? fun (Dig_proof_argument (n', (x, xv), aft')) ->
                outer_return @@ (Dig_proof_argument (Prefix n', (x, xv), Item_t (v, aft', annot)))
            | _, _ ->
                serialize_stack_for_error ctxt stack >>=? fun (whole_stack, _ctxt) ->
                fail (Bad_stack (loc, I_DIG, 1, whole_stack))
        in
        Lwt.return (parse_int32 n) >>=? fun n ->
        fail_unexpected_annot loc result_annot >>=? fun () ->
        make_proof_argument n stack >>=? fun (Dig_proof_argument (n', (x, stack_annot), aft)) ->
        typed ctxt loc (Dig (n, n')) (Item_t (x, aft, stack_annot))
    | Prim (loc, I_DIG, ([] | _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, I_DIG, 1, List.length l))
    | Prim (loc, I_DUG, [n], result_annot), Item_t (x, whole_stack, stack_annot) ->
        Lwt.return (parse_int32 n) >>=? fun whole_n ->
        let rec make_proof_argument
          : type tstk x . int -> x ty -> var_annot option -> (tstk stack_ty)
            -> ((tstk, x) dug_proof_argument) tzresult Lwt.t =
          fun n x stack_annot stk ->
            match (Compare.Int.(n = 0)), stk with
              true, rest ->
                outer_return @@ (Dug_proof_argument (Rest, (), Item_t (x, rest, stack_annot)))
            | false, Item_t (v, rest, annot) ->
                make_proof_argument (n - 1) x stack_annot rest
                >>=? fun (Dug_proof_argument (n', (), aft')) ->
                outer_return @@ (Dug_proof_argument (Prefix n', (), Item_t (v, aft', annot)))
            | _, _ ->
                serialize_stack_for_error ctxt whole_stack >>=? fun (whole_stack, _ctxt) ->
                fail (Bad_stack (loc, I_DUG, whole_n, whole_stack))
        in
        fail_unexpected_annot loc result_annot >>=? fun () ->
        make_proof_argument whole_n x stack_annot whole_stack >>=? fun (Dug_proof_argument (n', (), aft)) ->
        typed ctxt loc (Dug (whole_n, n')) aft
    | Prim (loc, I_DUG, [_], result_annot), (Empty_t as stack) ->
        fail_unexpected_annot loc result_annot >>=? fun () ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, I_DUG, 1, stack))
    | Prim (loc, I_DUG, ([] | _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, I_DUG, 1, List.length l))
    | Prim (loc, I_SWAP, [], annot),
      Item_t (v,  Item_t (w, rest, stack_annot), cur_top_annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        typed ctxt loc Swap
          (Item_t (w, Item_t (v, rest, cur_top_annot), stack_annot))
    | Prim (loc, I_PUSH, [ t ; d ], annot),
      stack ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ parse_packable_ty ctxt ~legacy t >>=? fun (Ex_ty t, ctxt) ->
        parse_data ?type_logger ctxt ~legacy t d >>=? fun (v, ctxt) ->
        typed ctxt loc (Const v) (Item_t (t, stack, annot))
    | Prim (loc, I_UNIT, [], annot),
      stack ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc (Const ()) (Item_t (Unit_t ty_name, stack, annot))
    (* options *)
    | Prim (loc, I_SOME, [], annot),
      Item_t (t, rest, _) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc Cons_some
          (Item_t (Option_t (t, ty_name, has_big_map t), rest, annot))
    | Prim (loc, I_NONE, [ t ], annot),
      stack ->
        Lwt.return @@ parse_any_ty ctxt ~legacy t >>=? fun (Ex_ty t, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc (Cons_none t)
          (Item_t (Option_t (t, ty_name, has_big_map t), stack, annot))
    | Prim (loc, I_IF_NONE, [ bt ; bf ], annot),
      (Item_t (Option_t (t, _, _), rest, option_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let annot = gen_access_annot option_annot default_some_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy bt rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~legacy bf (Item_t (t, rest, annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_none (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches ~legacy ctxt loc btr bfr { branch } >>=? fun (judgement, ctxt) ->
        return ctxt judgement
    (* pairs *)
    | Prim (loc, I_PAIR, [], annot),
      Item_t (a, Item_t (b, rest, snd_annot), fst_annot) ->
        parse_constr_annot loc annot
          ~if_special_first:(var_to_field_annot fst_annot)
          ~if_special_second:(var_to_field_annot snd_annot)
        >>=? fun (annot, ty_name, l_field, r_field) ->
        typed ctxt loc Cons_pair
          (Item_t (Pair_t((a, l_field, fst_annot), (b, r_field, snd_annot), ty_name, has_big_map a || has_big_map b), rest, annot))
    | Prim (loc, I_CAR, [], annot),
      Item_t (Pair_t ((a, expected_field_annot, a_annot), _, _, _), rest, pair_annot) ->
        parse_destr_annot loc annot
          ~pair_annot
          ~value_annot:a_annot
          ~field_name:expected_field_annot
          ~default_accessor:default_car_annot
        >>=? fun (annot, field_annot) ->
        Lwt.return @@ check_correct_field field_annot expected_field_annot >>=? fun () ->
        typed ctxt loc Car (Item_t (a, rest, annot))
    | Prim (loc, I_CDR, [], annot),
      Item_t (Pair_t (_, (b, expected_field_annot, b_annot), _, _), rest, pair_annot) ->
        parse_destr_annot loc annot
          ~pair_annot
          ~value_annot:b_annot
          ~field_name:expected_field_annot
          ~default_accessor:default_cdr_annot
        >>=? fun (annot, field_annot) ->
        Lwt.return @@ check_correct_field field_annot expected_field_annot >>=? fun () ->
        typed ctxt loc Cdr (Item_t (b, rest, annot))
    (* unions *)
    | Prim (loc, I_LEFT, [ tr ], annot),
      Item_t (tl, rest, stack_annot) ->
        Lwt.return @@ parse_any_ty ctxt ~legacy tr >>=? fun (Ex_ty tr, ctxt) ->
        parse_constr_annot loc annot
          ~if_special_first:(var_to_field_annot stack_annot)
        >>=? fun (annot, tname, l_field, r_field) ->
        typed ctxt loc Left (Item_t (Union_t ((tl, l_field), (tr, r_field), tname, has_big_map tl || has_big_map tr), rest, annot))
    | Prim (loc, I_RIGHT, [ tl ], annot),
      Item_t (tr, rest, stack_annot) ->
        Lwt.return @@ parse_any_ty ctxt ~legacy tl >>=? fun (Ex_ty tl, ctxt) ->
        parse_constr_annot loc annot
          ~if_special_second:(var_to_field_annot stack_annot)
        >>=? fun (annot, tname, l_field, r_field) ->
        typed ctxt loc Right (Item_t (Union_t ((tl, l_field), (tr, r_field), tname, has_big_map tl || has_big_map tr), rest, annot))
    | Prim (loc, I_IF_LEFT, [ bt ; bf ], annot),
      (Item_t (Union_t ((tl, l_field), (tr, r_field), _, _), rest, union_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let left_annot = gen_access_annot union_annot l_field ~default:default_left_annot in
        let right_annot = gen_access_annot union_annot r_field ~default:default_right_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy bt (Item_t (tl, rest, left_annot)) >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~legacy bf (Item_t (tr, rest, right_annot)) >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_left (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches ~legacy ctxt loc btr bfr { branch } >>=? fun (judgement, ctxt) ->
        return ctxt judgement
    (* lists *)
    | Prim (loc, I_NIL, [ t ], annot),
      stack ->
        Lwt.return @@ parse_any_ty ctxt ~legacy t >>=? fun (Ex_ty t, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc Nil (Item_t (List_t (t, ty_name, has_big_map t), stack, annot))
    | Prim (loc, I_CONS, [], annot),
      Item_t (tv, Item_t (List_t (t, ty_name, has_big_map), rest, _), _) ->
        check_item_ty ctxt tv t loc I_CONS 1 2 >>=? fun (Eq, t, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Cons_list (Item_t (List_t (t, ty_name, has_big_map), rest, annot))
    | Prim (loc, I_IF_CONS, [ bt ; bf ], annot),
      (Item_t (List_t (t, ty_name, has_big_map), rest, list_annot) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let hd_annot = gen_access_annot list_annot default_hd_annot in
        let tl_annot = gen_access_annot list_annot default_tl_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy bt
          (Item_t (t, Item_t (List_t (t, ty_name, has_big_map), rest, tl_annot), hd_annot))
        >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~legacy bf
          rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If_cons (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches ~legacy ctxt loc btr bfr { branch } >>=? fun (judgement, ctxt) ->
        return ctxt judgement
    | Prim (loc, I_SIZE, [], annot),
      Item_t (List_t _, rest, _) ->
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        typed ctxt loc List_size (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_MAP, [ body ], annot),
      (Item_t (List_t (elt, _, _), starting_rest, list_annot)) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_var_type_annot loc annot
        >>=? fun (ret_annot, list_ty_name) ->
        let elt_annot = gen_access_annot list_annot default_elt_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy
          body (Item_t (elt, starting_rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft = Item_t (ret, rest, _) ; _ } as ibody) ->
              let invalid_map_body () =
                serialize_stack_for_error ctxt ibody.aft >>|? fun (aft, _ctxt) ->
                Invalid_map_body (loc, aft) in
              trace_eval invalid_map_body
                (Lwt.return @@ stack_ty_eq ctxt 1 rest starting_rest >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt rest starting_rest >>=? fun (rest, ctxt) ->
                 typed ctxt loc (List_map ibody)
                   (Item_t (List_t (ret, list_ty_name, has_big_map ret), rest, ret_annot)))
          | Typed { aft ; _ } ->
              serialize_stack_for_error ctxt aft >>=? fun (aft, _ctxt) ->
              fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (List_t (elt, _, _), rest, list_annot) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let elt_annot = gen_access_annot list_annot default_elt_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy
          body (Item_t (elt, rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              let invalid_iter_body () =
                serialize_stack_for_error ctxt ibody.aft >>=? fun (aft, ctxt) ->
                serialize_stack_for_error ctxt rest >>|? fun (rest, _ctxt) ->
                Invalid_iter_body (loc, rest, aft) in
              trace_eval invalid_iter_body
                (Lwt.return @@ stack_ty_eq ctxt 1 aft rest >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt aft rest >>=? fun (rest, ctxt) ->
                 typed ctxt loc (List_iter ibody) rest)
          | Failed { descr } ->
              typed ctxt loc (List_iter (descr rest)) rest
        end
    (* sets *)
    | Prim (loc, I_EMPTY_SET, [ t ], annot),
      rest ->
        Lwt.return @@ parse_comparable_ty ctxt t >>=? fun (Ex_comparable_ty t, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        typed ctxt loc (Empty_set t) (Item_t (Set_t (t, tname), rest, annot))
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Set_t (comp_elt, _), rest, set_annot) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let elt_annot = gen_access_annot set_annot default_elt_annot in
        let elt = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt ~legacy
          body (Item_t (elt, rest, elt_annot)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              let invalid_iter_body () =
                serialize_stack_for_error ctxt ibody.aft >>=? fun (aft, ctxt) ->
                serialize_stack_for_error ctxt rest >>|? fun (rest, _ctxt) ->
                Invalid_iter_body (loc, rest, aft) in
              trace_eval invalid_iter_body
                (Lwt.return @@ stack_ty_eq ctxt 1 aft rest >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt aft rest >>=? fun (rest, ctxt) ->
                 typed ctxt loc (Set_iter ibody) rest)
          | Failed { descr } ->
              typed ctxt loc (Set_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], annot),
      Item_t (v, Item_t (Set_t (elt, _), rest, _), _) ->
        let elt = ty_of_comparable_ty elt in
        parse_var_type_annot loc annot >>=? fun (annot, tname) ->
        check_item_ty ctxt elt v loc I_MEM 1 2 >>=? fun (Eq, _, ctxt) ->
        typed ctxt loc Set_mem (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (v, Item_t (Bool_t _, Item_t (Set_t (elt, tname), rest, set_annot), _), _) ->
        begin match comparable_ty_of_ty v with
          | None ->
              unparse_ty ctxt v >>=? fun (v, _ctxt) ->
              fail (Comparable_type_expected (loc, Micheline.strip_locations v))
          | Some v ->
              parse_var_annot loc annot ~default:set_annot >>=? fun annot ->
              check_item_comparable_ty elt v loc I_UPDATE 1 3 >>=? fun (Eq, elt) ->
              typed ctxt loc Set_update (Item_t (Set_t (elt, tname), rest, annot))
        end
    | Prim (loc, I_SIZE, [], annot),
      Item_t (Set_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Set_size (Item_t (Nat_t None, rest, annot))
    (* maps *)
    | Prim (loc, I_EMPTY_MAP, [ tk ; tv ], annot),
      stack ->
        Lwt.return @@ parse_comparable_ty ctxt tk >>=? fun (Ex_comparable_ty tk, ctxt) ->
        Lwt.return @@ parse_any_ty ctxt ~legacy tv >>=? fun (Ex_ty tv, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc (Empty_map (tk, tv)) (Item_t (Map_t (tk, tv, ty_name, has_big_map tv), stack, annot))
    | Prim (loc, I_MAP, [ body ], annot),
      Item_t (Map_t (ck, elt, _, _), starting_rest, _map_annot) ->
        let k = ty_of_comparable_ty ck in
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_var_type_annot loc annot >>=? fun (ret_annot, ty_name) ->
        let k_name = field_to_var_annot default_key_annot in
        let e_name = field_to_var_annot default_elt_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy
          body (Item_t (Pair_t ((k, None, k_name), (elt, None, e_name), None, has_big_map elt),
                        starting_rest, None)) >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft = Item_t (ret, rest, _) ; _ } as ibody) ->
              let invalid_map_body () =
                serialize_stack_for_error ctxt ibody.aft >>|? fun (aft, _ctxt) ->
                Invalid_map_body (loc, aft) in
              trace_eval invalid_map_body
                (Lwt.return @@ stack_ty_eq ctxt 1 rest starting_rest >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt rest starting_rest >>=? fun (rest, ctxt) ->
                 typed ctxt loc (Map_map ibody)
                   (Item_t (Map_t (ck, ret, ty_name, has_big_map ret), rest, ret_annot)))
          | Typed { aft ; _ } ->
              serialize_stack_for_error ctxt aft >>=? fun (aft, _ctxt) ->
              fail (Invalid_map_body (loc, aft))
          | Failed _ -> fail (Invalid_map_block_fail loc)
        end
    | Prim (loc, I_ITER, [ body ], annot),
      Item_t (Map_t (comp_elt, element_ty, _, _), rest, _map_annot) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let k_name = field_to_var_annot default_key_annot in
        let e_name = field_to_var_annot default_elt_annot in
        let key = ty_of_comparable_ty comp_elt in
        parse_instr ?type_logger tc_context ctxt ~legacy body
          (Item_t (Pair_t ((key, None, k_name), (element_ty, None, e_name), None, has_big_map element_ty),
                   rest, None))
        >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ({ aft ; _ } as ibody) ->
              let invalid_iter_body () =
                serialize_stack_for_error ctxt ibody.aft >>=? fun (aft, ctxt) ->
                serialize_stack_for_error ctxt rest >>|? fun (rest, _ctxt) ->
                Invalid_iter_body (loc, rest, aft) in
              trace_eval invalid_iter_body
                (Lwt.return @@ stack_ty_eq ctxt 1 aft rest >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt aft rest >>=? fun (rest, ctxt) ->
                 typed ctxt loc (Map_iter ibody) rest)
          | Failed { descr } ->
              typed ctxt loc (Map_iter (descr rest)) rest
        end
    | Prim (loc, I_MEM, [], annot),
      Item_t (vk, Item_t (Map_t (ck, _, _, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty ctxt vk k loc I_MEM 1 2 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_mem (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Map_t (ck, elt, _, has_big_map), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty ctxt vk k loc I_GET 1 2 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_get (Item_t (Option_t (elt, None, has_big_map), rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (vk, Item_t (Option_t (vv, _, _),
                          Item_t (Map_t (ck, v, map_name, has_big_map), rest, map_annot), _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty ctxt vk k loc I_UPDATE 1 3 >>=? fun (Eq, _, ctxt) ->
        check_item_ty ctxt vv v loc I_UPDATE 2 3 >>=? fun (Eq, v, ctxt) ->
        parse_var_annot loc annot ~default:map_annot >>=? fun annot ->
        typed ctxt loc Map_update (Item_t (Map_t (ck, v, map_name, has_big_map), rest, annot))
    | Prim (loc, I_SIZE, [], annot),
      Item_t (Map_t (_, _, _, _), rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Map_size (Item_t (Nat_t None, rest, annot))
    (* big_map *)
    | Prim (loc, I_EMPTY_BIG_MAP, [ tk ; tv ], annot),
      stack ->
        Lwt.return @@ parse_comparable_ty ctxt tk >>=? fun (Ex_comparable_ty tk, ctxt) ->
        Lwt.return @@ parse_packable_ty ctxt ~legacy tv >>=? fun (Ex_ty tv, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        typed ctxt loc (Empty_big_map (tk, tv)) (Item_t (Big_map_t (tk, tv, ty_name), stack, annot))
    | Prim (loc, I_MEM, [], annot),
      Item_t (set_key, Item_t (Big_map_t (map_key, _, _), rest, _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty ctxt set_key k loc I_MEM 1 2 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Big_map_mem (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Big_map_t (ck, elt, _), rest, _), _) ->
        let k = ty_of_comparable_ty ck in
        check_item_ty ctxt vk k loc I_GET 1 2 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Big_map_get (Item_t (Option_t (elt, None, has_big_map elt), rest, annot))
    | Prim (loc, I_UPDATE, [], annot),
      Item_t (set_key,
              Item_t (Option_t (set_value, _, _),
                      Item_t (Big_map_t (map_key, map_value, map_name), rest, map_annot), _), _) ->
        let k = ty_of_comparable_ty map_key in
        check_item_ty ctxt set_key k loc I_UPDATE 1 3 >>=? fun (Eq, _, ctxt) ->
        check_item_ty ctxt set_value map_value loc I_UPDATE 2 3 >>=? fun (Eq, map_value, ctxt) ->
        parse_var_annot loc annot ~default:map_annot >>=? fun annot ->
        typed ctxt loc Big_map_update (Item_t (Big_map_t (map_key, map_value, map_name), rest, annot))
    (* control *)
    | Seq (loc, []),
      stack ->
        typed ctxt loc Nop stack
    | Seq (loc, [ single ]),
      stack ->
        parse_instr ?type_logger tc_context ctxt ~legacy single
          stack >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ({ aft ; _ } as instr) ->
              let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
              typed ctxt loc (Seq (instr, nop)) aft
          | Failed { descr ; _ } ->
              let descr aft =
                let nop = { bef = aft ; loc = loc ; aft ; instr = Nop } in
                let descr = descr aft in
                { descr with instr = Seq (descr, nop) } in
              return ctxt (Failed { descr })
        end
    | Seq (loc, hd :: tl),
      stack ->
        parse_instr ?type_logger tc_context ctxt ~legacy hd
          stack >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Failed _ ->
              fail (Fail_not_in_tail_position (Micheline.location hd))
          | Typed ({ aft = middle ; _ } as ihd) ->
              parse_instr ?type_logger tc_context ctxt ~legacy (Seq (-1, tl))
                middle >>=? fun (judgement, ctxt) ->
              match judgement with
              | Failed { descr } ->
                  let descr ret =
                    { loc ; instr = Seq (ihd, descr ret) ;
                      bef = stack ; aft = ret } in
                  return ctxt (Failed { descr })
              | Typed itl ->
                  typed ctxt loc (Seq (ihd, itl)) itl.aft
        end
    | Prim (loc, I_IF, [ bt ; bf ], annot),
      (Item_t (Bool_t _, rest, _) as bef) ->
        check_kind [ Seq_kind ] bt >>=? fun () ->
        check_kind [ Seq_kind ] bf >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~legacy bt rest >>=? fun (btr, ctxt) ->
        parse_instr ?type_logger tc_context ctxt ~legacy bf rest >>=? fun (bfr, ctxt) ->
        let branch ibt ibf =
          { loc ; instr = If (ibt, ibf) ; bef ; aft = ibt.aft } in
        merge_branches ~legacy ctxt loc btr bfr { branch } >>=? fun (judgement, ctxt) ->
        return ctxt judgement
    | Prim (loc, I_LOOP, [ body ], annot),
      (Item_t (Bool_t _, rest, _stack_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        fail_unexpected_annot loc annot >>=? fun () ->
        parse_instr ?type_logger tc_context ctxt ~legacy body
          rest >>=? begin fun (judgement, ctxt) ->
          match judgement with
          | Typed ibody ->
              let unmatched_branches () =
                serialize_stack_for_error ctxt ibody.aft >>=? fun (aft, ctxt) ->
                serialize_stack_for_error ctxt stack >>|? fun (stack, _ctxt) ->
                Unmatched_branches (loc, aft, stack) in
              trace_eval unmatched_branches
                (Lwt.return @@ stack_ty_eq ctxt 1 ibody.aft stack >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt ibody.aft stack >>=? fun (_stack, ctxt) ->
                 typed ctxt loc (Loop ibody) rest)
          | Failed { descr } ->
              let ibody = descr stack in
              typed ctxt loc (Loop ibody) rest
        end
    | Prim (loc, I_LOOP_LEFT, [ body ], annot),
      (Item_t (Union_t ((tl, l_field), (tr, _), _, _), rest, union_annot) as stack) ->
        check_kind [ Seq_kind ] body >>=? fun () ->
        parse_var_annot loc annot >>=? fun annot ->
        let l_annot = gen_access_annot union_annot l_field ~default:default_left_annot in
        parse_instr ?type_logger tc_context ctxt ~legacy body
          (Item_t (tl, rest, l_annot)) >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed ibody ->
              let unmatched_branches () =
                serialize_stack_for_error ctxt ibody.aft >>=? fun (aft, ctxt) ->
                serialize_stack_for_error ctxt stack >>|? fun (stack, _ctxt) ->
                Unmatched_branches (loc, aft, stack) in
              trace_eval unmatched_branches
                (Lwt.return @@ stack_ty_eq ctxt 1 ibody.aft stack >>=? fun (Eq, ctxt) ->
                 Lwt.return @@ merge_stacks ~legacy loc ctxt ibody.aft stack >>=? fun (_stack, ctxt) ->
                 typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, annot)))
          | Failed { descr } ->
              let ibody = descr stack in
              typed ctxt loc (Loop_left ibody) (Item_t (tr, rest, annot))
        end
    | Prim (loc, I_LAMBDA, [ arg ; ret ; code ], annot),
      stack ->
        Lwt.return @@ parse_any_ty ctxt ~legacy arg
        >>=? fun (Ex_ty arg, ctxt) ->
        Lwt.return @@ parse_any_ty ctxt ~legacy ret
        >>=? fun (Ex_ty ret, ctxt) ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_var_annot loc annot >>=? fun annot ->
        parse_returning Lambda ?type_logger ctxt ~legacy
          (arg, default_arg_annot) ret code >>=? fun (lambda, ctxt) ->
        typed ctxt loc (Lambda lambda) (Item_t (Lambda_t (arg, ret, None), stack, annot))
    | Prim (loc, I_EXEC, [], annot),
      Item_t (arg, Item_t (Lambda_t (param, ret, _), rest, _), _) ->
        check_item_ty ctxt arg param loc I_EXEC 1 2 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Exec (Item_t (ret, rest, annot))
    | Prim (loc, I_APPLY, [], annot),
      Item_t (capture, Item_t (Lambda_t (Pair_t ((capture_ty, _, _), (arg_ty, _, _), lam_annot, _), ret, _), rest, _), _) ->
        Lwt.return @@ check_packable ~legacy:false loc capture_ty >>=? fun () ->
        check_item_ty ctxt capture capture_ty loc I_APPLY 1 2 >>=? fun (Eq, capture_ty, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc (Apply capture_ty) (Item_t (Lambda_t (arg_ty, ret, lam_annot), rest, annot))
    | Prim (loc, I_DIP, [ code ], annot),
      Item_t (v, rest, stack_annot) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        check_kind [ Seq_kind ] code >>=? fun () ->
        parse_instr ?type_logger (add_dip v stack_annot tc_context) ctxt ~legacy code
          rest >>=? begin fun (judgement, ctxt) -> match judgement with
          | Typed descr ->
              typed ctxt loc (Dip descr) (Item_t (v, descr.aft, stack_annot))
          | Failed _ ->
              fail (Fail_not_in_tail_position loc)
        end
    | Prim (loc, I_DIP, [n; code], result_annot), stack
      when (match parse_int32 n with Ok _ -> true | Error _ -> false) ->
        let rec make_proof_argument
          : type tstk . int
            (* -> (fbef stack_ty -> (fbef judgement * context) tzresult Lwt.t) *)
            -> tc_context
            -> (tstk stack_ty)
            -> (tstk dipn_proof_argument) tzresult Lwt.t =
          fun n inner_tc_context stk ->
            match (Compare.Int.(n = 0)), stk with
              true, rest ->
                (parse_instr ?type_logger inner_tc_context ctxt ~legacy code
                   rest) >>=? begin fun (judgement, ctxt) -> match judgement with
                  | Typed descr ->
                      outer_return @@ (Dipn_proof_argument (Rest, (ctxt, descr), descr.aft))
                  | Failed _ ->
                      fail (Fail_not_in_tail_position loc)
                end
            | false, Item_t (v, rest, annot) ->
                make_proof_argument (n - 1) (add_dip v annot tc_context) rest
                >>=? fun (Dipn_proof_argument (n', descr, aft')) ->
                outer_return @@ (Dipn_proof_argument (Prefix n', descr, Item_t (v, aft', annot)))
            | _, _ ->
                serialize_stack_for_error ctxt stack >>=? fun (whole_stack, _ctxt) ->
                fail (Bad_stack (loc, I_DIP, 1, whole_stack))
        in
        Lwt.return (parse_int32 n) >>=? fun n ->
        fail_unexpected_annot loc result_annot >>=? fun () ->
        make_proof_argument n tc_context stack >>=? fun (Dipn_proof_argument (n', (new_ctxt, descr), aft)) ->
        (* TODO: which context should be used in the next line? new_ctxt or the old ctxt? *)
        typed new_ctxt loc (Dipn (n, n', descr)) aft
    | Prim (loc, I_DIP, ([] | _ :: _ :: _ :: _ as l), _), _ ->
        (* Technically, the arities 1 and 2 are allowed but the error only mentions 2.
           However, DIP {code} is equivalent to DIP 1 {code} so hinting at an arity of 2 makes sense. *)
        fail (Invalid_arity (loc, I_DIP, 2, List.length l))
    | Prim (loc, I_FAILWITH, [], annot),
      Item_t (v, _rest, _) ->
        fail_unexpected_annot loc annot >>=? fun () ->
        let descr aft = { loc ; instr = Failwith v ; bef = stack_ty ; aft } in
        log_stack ctxt loc stack_ty Empty_t >>=? fun () ->
        return ctxt (Failed { descr })
    (* timestamp operations *)
    | Prim (loc, I_ADD, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_timestamp_to_seconds
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t _, Item_t (Timestamp_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_seconds_to_timestamp
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sub_timestamp_seconds
          (Item_t (Timestamp_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Timestamp_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Diff_timestamps
          (Item_t (Int_t tname, rest, annot))
    (* string operations *)
    | Prim (loc, I_CONCAT, [], annot),
      Item_t (String_t tn1, Item_t (String_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Concat_string_pair
          (Item_t (String_t tname, rest, annot))
    | Prim (loc, I_CONCAT, [], annot),
      Item_t (List_t (String_t tname, _, _), rest, list_annot) ->
        parse_var_annot ~default:list_annot loc annot >>=? fun annot ->
        typed ctxt loc Concat_string
          (Item_t (String_t tname, rest, annot))
    | Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t _, Item_t (Nat_t _, Item_t (String_t tname, rest, string_annot), _), _) ->
        parse_var_annot
          ~default:(gen_access_annot string_annot default_slice_annot)
          loc annot >>=? fun annot ->
        typed ctxt loc Slice_string
          (Item_t (Option_t (String_t tname, None, false), rest, annot))
    | Prim (loc, I_SIZE, [], annot),
      Item_t (String_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc String_size (Item_t (Nat_t None, rest, annot))
    (* bytes operations *)
    | Prim (loc, I_CONCAT, [], annot),
      Item_t (Bytes_t tn1, Item_t (Bytes_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Concat_bytes_pair
          (Item_t (Bytes_t tname, rest, annot))
    | Prim (loc, I_CONCAT, [], annot),
      Item_t (List_t (Bytes_t tname, _, _), rest, list_annot) ->
        parse_var_annot ~default:list_annot loc annot >>=? fun annot ->
        typed ctxt loc Concat_bytes
          (Item_t (Bytes_t tname, rest, annot))
    | Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t _, Item_t (Nat_t _, Item_t (Bytes_t tname, rest, bytes_annot), _), _) ->
        parse_var_annot
          ~default:(gen_access_annot bytes_annot default_slice_annot)
          loc annot >>=? fun annot ->
        typed ctxt loc Slice_bytes
          (Item_t (Option_t (Bytes_t tname, None, false), rest, annot))
    | Prim (loc, I_SIZE, [], annot),
      Item_t (Bytes_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Bytes_size (Item_t (Nat_t None, rest, annot))
    (* currency operations *)
    | Prim (loc, I_ADD, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Add_tez
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Sub_tez
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) -> (* no type name check *)
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_teznat
          (Item_t (Mutez_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Mutez_t tname, rest, _), _) -> (* no type name check *)
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_nattez
          (Item_t (Mutez_t tname, rest, annot))
    (* boolean operations *)
    | Prim (loc, I_OR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Or
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc And
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_XOR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Xor
          (Item_t (Bool_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Bool_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not
          (Item_t (Bool_t tname, rest, annot))
    (* integer operations *)
    | Prim (loc, I_ABS, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Abs_int
          (Item_t (Nat_t None, rest, annot))
    | Prim (loc, I_ISNAT, [], annot),
      Item_t (Int_t _, rest, int_annot) ->
        parse_var_annot loc annot ~default:int_annot >>=? fun annot ->
        typed ctxt loc Is_nat
          (Item_t (Option_t (Nat_t None, None, false), rest, annot))
    | Prim (loc, I_INT, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Int_nat
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_NEG, [], annot),
      Item_t (Int_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neg_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_NEG, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neg_nat
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Add_intint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_intnat
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Add_natint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Add_natnat
          (Item_t (Nat_t  tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sub_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun _tname ->
        typed ctxt loc Sub_int
          (Item_t (Int_t None, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Mul_intint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_intnat
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Mul_natint
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Mul_natnat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_teznat
          (Item_t (Option_t
                     (Pair_t ((Mutez_t tname, None, None),
                              (Mutez_t tname, None, None), None, false),
                      None, false), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Ediv_tez
          (Item_t (Option_t (Pair_t ((Nat_t None, None, None),
                                     (Mutez_t tname, None, None), None, false),
                             None, false), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Ediv_intint
          (Item_t (Option_t
                     (Pair_t ((Int_t tname, None, None),
                              (Nat_t None, None, None), None, false),
                      None, false), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_intnat
          (Item_t (Option_t
                     (Pair_t ((Int_t tname, None, None),
                              (Nat_t None, None, None), None, false),
                      None, false), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tname, Item_t (Int_t _, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ediv_natint
          (Item_t (Option_t (Pair_t ((Int_t None, None, None),
                                     (Nat_t tname, None, None), None, false),
                             None, false), rest, annot))
    | Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Ediv_natnat
          (Item_t (Option_t (Pair_t ((Nat_t tname, None, None),
                                     (Nat_t tname, None, None), None, false),
                             None, false), rest, annot))
    | Prim (loc, I_LSL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Lsl_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_LSR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Lsr_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_OR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Or_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc And_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_AND, [], annot),
      Item_t (Int_t _, Item_t (Nat_t tname, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc And_int_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_XOR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        Lwt.return @@ merge_type_annot ~legacy tn1 tn2 >>=? fun tname ->
        typed ctxt loc Xor_nat
          (Item_t (Nat_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Int_t tname, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not_int
          (Item_t (Int_t tname, rest, annot))
    | Prim (loc, I_NOT, [], annot),
      Item_t (Nat_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Not_nat
          (Item_t (Int_t None, rest, annot))
    (* comparison *)
    | Prim (loc, I_COMPARE, [], annot),
      Item_t (t1, Item_t (t2, rest, _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        check_item_ty ctxt t1 t2 loc I_COMPARE 1 2 >>=? fun (Eq, t, ctxt) ->
        begin match comparable_ty_of_ty t with
          | None ->
              Lwt.return (serialize_ty_for_error ctxt t) >>=? fun (t, _ctxt) ->
              fail (Comparable_type_expected (loc, t))
          | Some key ->
              typed ctxt loc (Compare key)
                (Item_t (Int_t None, rest, annot))
        end
    (* comparators *)
    | Prim (loc, I_EQ, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Eq
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_NEQ, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Neq
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_LT, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Lt
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GT, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Gt
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_LE, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Le
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_GE, [], annot),
      Item_t (Int_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Ge
          (Item_t (Bool_t None, rest, annot))
    (* annotations *)
    | Prim (loc, I_CAST, [ cast_t ], annot),
      Item_t (t, stack, item_annot) ->
        parse_var_annot loc annot ~default:item_annot >>=? fun annot ->
        (Lwt.return @@ parse_any_ty ctxt ~legacy cast_t)
        >>=? fun (Ex_ty cast_t, ctxt) ->
        Lwt.return @@ ty_eq ctxt cast_t t >>=? fun (Eq, ctxt) ->
        Lwt.return @@ merge_types ~legacy ctxt loc cast_t t >>=? fun (_, ctxt) ->
        typed ctxt loc Nop (Item_t (cast_t, stack, annot))
    | Prim (loc, I_RENAME, [], annot),
      Item_t (t, stack, _) ->
        parse_var_annot loc annot >>=? fun annot -> (* can erase annot *)
        typed ctxt loc Nop (Item_t (t, stack, annot))
    (* packing *)
    | Prim (loc, I_PACK, [], annot),
      Item_t (t, rest, unpacked_annot) ->
        Lwt.return (check_packable ~legacy:true (* allow to pack contracts for hash/signature checks *) loc t) >>=? fun () ->
        parse_var_annot loc annot ~default:(gen_access_annot unpacked_annot default_pack_annot)
        >>=? fun annot ->
        typed ctxt loc (Pack t)
          (Item_t (Bytes_t None, rest, annot))
    | Prim (loc, I_UNPACK, [ ty ], annot),
      Item_t (Bytes_t _, rest, packed_annot) ->
        Lwt.return @@ parse_packable_ty ctxt ~legacy ty >>=? fun (Ex_ty t, ctxt) ->
        parse_var_type_annot loc annot >>=? fun (annot, ty_name) ->
        let annot = default_annot annot ~default:(gen_access_annot packed_annot default_unpack_annot) in
        typed ctxt loc (Unpack t) (Item_t (Option_t (t, ty_name, false (* cannot unpack big_maps *)), rest, annot))
    (* protocol *)
    | Prim (loc, I_ADDRESS, [], annot),
      Item_t (Contract_t _, rest, contract_annot) ->
        parse_var_annot loc annot ~default:(gen_access_annot contract_annot default_addr_annot)
        >>=? fun annot ->
        typed ctxt loc Address
          (Item_t (Address_t None, rest, annot))
    | Prim (loc, I_CONTRACT, [ ty ], annot),
      Item_t (Address_t _, rest, addr_annot) ->
        Lwt.return @@ parse_parameter_ty ctxt ~legacy ty >>=? fun (Ex_ty t, ctxt) ->
        parse_entrypoint_annot loc annot ~default:(gen_access_annot addr_annot default_contract_annot)
        >>=? fun (annot, entrypoint) ->
        Lwt.return @@ begin match entrypoint with
          | None -> Ok "default"
          | Some (`Field_annot "default") -> error (Unexpected_annotation loc)
          | Some (`Field_annot entrypoint) ->
              if Compare.Int.(String.length entrypoint > 31) then
                error (Entrypoint_name_too_long entrypoint)
              else Ok entrypoint
        end >>=? fun entrypoint ->
        typed ctxt loc (Contract (t, entrypoint))
          (Item_t (Option_t (Contract_t (t, None), None, false), rest, annot))
    | Prim (loc, I_TRANSFER_TOKENS, [], annot),
      Item_t (p, Item_t
                (Mutez_t _, Item_t
                   (Contract_t (cp, _), rest, _), _), _) ->
        check_item_ty ctxt p cp loc I_TRANSFER_TOKENS 1 4 >>=? fun (Eq, _, ctxt) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Transfer_tokens (Item_t (Operation_t None, rest, annot))
    | Prim (loc, I_SET_DELEGATE, [], annot),
      Item_t (Option_t (Key_hash_t _, _, _), rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Set_delegate (Item_t (Operation_t None, rest, annot))
    | Prim (loc, I_CREATE_ACCOUNT, [], annot),
      Item_t
        (Key_hash_t _, Item_t
           (Option_t (Key_hash_t _, _, _), Item_t
              (Bool_t _, Item_t
                 (Mutez_t _, rest, _), _), _), _) ->
        if legacy
        then begin
          (* For existing contracts, this instruction is still allowed *)
          parse_two_var_annot loc annot >>=? fun (op_annot, addr_annot) ->
          typed ctxt loc Create_account
            (Item_t (Operation_t None, Item_t (Address_t None, rest, addr_annot), op_annot))
        end
        else
          (* For new contracts this instruction is not allowed anymore *)
          fail (Deprecated_instruction I_CREATE_ACCOUNT)
    | Prim (loc, I_IMPLICIT_ACCOUNT, [], annot),
      Item_t (Key_hash_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Implicit_account
          (Item_t (Contract_t (Unit_t None, None), rest, annot))
    | Prim (loc, I_CREATE_CONTRACT, [ (Seq _ as code)], annot),
      Item_t
        (Key_hash_t _, Item_t
           (Option_t (Key_hash_t _, _, _), Item_t
              (Bool_t _, Item_t
                 (Bool_t _, Item_t
                    (Mutez_t _, Item_t
                       (ginit, rest, _), _), _), _), _), _) ->
        if legacy then begin
          (* For existing contracts, this instruction is still allowed *)
          parse_two_var_annot loc annot >>=? fun (op_annot, addr_annot) ->
          let cannonical_code = fst @@ Micheline.extract_locations code in
          Lwt.return @@ parse_toplevel ~legacy cannonical_code >>=? fun (arg_type, storage_type, code_field, root_name) ->
          trace
            (Ill_formed_type (Some "parameter", cannonical_code, location arg_type))
            (Lwt.return @@ parse_parameter_ty ctxt ~legacy arg_type)
          >>=? fun (Ex_ty arg_type, ctxt) ->
          begin
            if legacy then Error_monad.return () else
              Lwt.return (well_formed_entrypoints ~root_name arg_type)
          end >>=? fun () ->
          trace
            (Ill_formed_type (Some "storage", cannonical_code, location storage_type))
            (Lwt.return @@ parse_storage_ty ctxt ~legacy storage_type)
          >>=? fun (Ex_ty storage_type, ctxt) ->
          let arg_annot = default_annot (type_to_var_annot (name_of_ty arg_type))
              ~default:default_param_annot in
          let storage_annot = default_annot (type_to_var_annot (name_of_ty storage_type))
              ~default:default_storage_annot in
          let arg_type_full = Pair_t ((arg_type, None, arg_annot),
                                      (storage_type, None, storage_annot), None,
                                     has_big_map arg_type || has_big_map storage_type) in
          let ret_type_full =
            Pair_t ((List_t (Operation_t None, None, false), None, None),
                    (storage_type, None, None), None,
                    has_big_map storage_type) in
          trace
            (Ill_typed_contract (cannonical_code, []))
            (parse_returning (Toplevel { storage_type ; param_type = arg_type ; root_name ;
                                         legacy_create_contract_literal = true })
               ctxt ~legacy ?type_logger (arg_type_full, None) ret_type_full code_field) >>=?
          fun (Lam ({ bef = Item_t (arg, Empty_t, _) ;
                      aft = Item_t (ret, Empty_t, _) ; _ }, _) as lambda, ctxt) ->
          Lwt.return @@ ty_eq ctxt arg arg_type_full >>=? fun (Eq, ctxt) ->
          Lwt.return @@ merge_types ~legacy ctxt loc arg arg_type_full >>=? fun (_, ctxt) ->
          Lwt.return @@ ty_eq ctxt ret ret_type_full >>=? fun (Eq, ctxt) ->
          Lwt.return @@ merge_types ~legacy ctxt loc ret ret_type_full >>=? fun (_, ctxt) ->
          Lwt.return @@ ty_eq ctxt storage_type ginit >>=? fun (Eq, ctxt) ->
          Lwt.return @@ merge_types ~legacy ctxt loc storage_type ginit >>=? fun (_, ctxt) ->
          typed ctxt loc (Create_contract (storage_type, arg_type, lambda, root_name))
            (Item_t (Operation_t None, Item_t (Address_t None, rest, addr_annot), op_annot))
        end
        else
          (* For new contracts this instruction is not allowed anymore *)
          fail (Deprecated_instruction I_CREATE_CONTRACT)
    | Prim (loc, I_CREATE_CONTRACT, [ (Seq _ as code)], annot),
      (* Removed the instruction's arguments manager, spendable and delegatable *)
      Item_t
        (Option_t (Key_hash_t _, _, _), Item_t
           (Mutez_t _, Item_t
              (ginit, rest, _), _), _) ->
        parse_two_var_annot loc annot >>=? fun (op_annot, addr_annot) ->
        let cannonical_code = fst @@ Micheline.extract_locations code in
        Lwt.return @@ parse_toplevel ~legacy cannonical_code >>=? fun (arg_type, storage_type, code_field, root_name) ->
        trace
          (Ill_formed_type (Some "parameter", cannonical_code, location arg_type))
          (Lwt.return @@ parse_parameter_ty ctxt ~legacy arg_type)
        >>=? fun (Ex_ty arg_type, ctxt) ->
        begin
          if legacy then Error_monad.return () else
            Lwt.return (well_formed_entrypoints ~root_name arg_type)
        end >>=? fun () ->
        trace
          (Ill_formed_type (Some "storage", cannonical_code, location storage_type))
          (Lwt.return @@ parse_storage_ty ctxt ~legacy storage_type)
        >>=? fun (Ex_ty storage_type, ctxt) ->
        let arg_annot = default_annot (type_to_var_annot (name_of_ty arg_type))
            ~default:default_param_annot in
        let storage_annot = default_annot (type_to_var_annot (name_of_ty storage_type))
            ~default:default_storage_annot in
        let arg_type_full = Pair_t ((arg_type, None, arg_annot),
                                    (storage_type, None, storage_annot), None,
                                    has_big_map arg_type || has_big_map storage_type) in
        let ret_type_full =
          Pair_t ((List_t (Operation_t None, None, false), None, None),
                  (storage_type, None, None), None, has_big_map storage_type) in
        trace
          (Ill_typed_contract (cannonical_code, []))
          (parse_returning (Toplevel { storage_type ; param_type = arg_type ; root_name ;
                                       legacy_create_contract_literal = false })
             ctxt ~legacy ?type_logger (arg_type_full, None) ret_type_full code_field) >>=?
        fun (Lam ({ bef = Item_t (arg, Empty_t, _) ;
                    aft = Item_t (ret, Empty_t, _) ; _ }, _) as lambda, ctxt) ->
        Lwt.return @@ ty_eq ctxt arg arg_type_full >>=? fun (Eq, ctxt) ->
        Lwt.return @@ merge_types ~legacy ctxt loc arg arg_type_full >>=? fun (_, ctxt) ->
        Lwt.return @@ ty_eq ctxt ret ret_type_full >>=? fun (Eq, ctxt) ->
        Lwt.return @@ merge_types ~legacy ctxt loc ret ret_type_full >>=? fun (_, ctxt) ->
        Lwt.return @@ ty_eq ctxt storage_type ginit >>=? fun (Eq, ctxt) ->
        Lwt.return @@ merge_types ~legacy ctxt loc storage_type ginit >>=? fun (_, ctxt) ->
        typed ctxt loc (Create_contract_2 (storage_type, arg_type, lambda, root_name))
          (Item_t (Operation_t None, Item_t (Address_t None, rest, addr_annot), op_annot))
    | Prim (loc, I_NOW, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_now_annot >>=? fun annot ->
        typed ctxt loc Now (Item_t (Timestamp_t None, stack, annot))
    | Prim (loc, I_AMOUNT, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_amount_annot >>=? fun annot ->
        typed ctxt loc Amount
          (Item_t (Mutez_t None, stack, annot))
    | Prim (loc, I_CHAIN_ID, [], annot),
      stack ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc ChainId
          (Item_t (Chain_id_t None, stack, annot))
    | Prim (loc, I_BALANCE, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_balance_annot >>=? fun annot ->
        typed ctxt loc Balance
          (Item_t (Mutez_t None, stack, annot))
    | Prim (loc, I_HASH_KEY, [], annot),
      Item_t (Key_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Hash_key
          (Item_t (Key_hash_t None, rest, annot))
    | Prim (loc, I_CHECK_SIGNATURE, [], annot),
      Item_t (Key_t _, Item_t (Signature_t _, Item_t (Bytes_t _, rest, _), _), _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Check_signature
          (Item_t (Bool_t None, rest, annot))
    | Prim (loc, I_BLAKE2B, [], annot),
      Item_t (Bytes_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Blake2b
          (Item_t (Bytes_t None, rest, annot))
    | Prim (loc, I_SHA256, [], annot),
      Item_t (Bytes_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sha256
          (Item_t (Bytes_t None, rest, annot))
    | Prim (loc, I_SHA512, [], annot),
      Item_t (Bytes_t _, rest, _) ->
        parse_var_annot loc annot >>=? fun annot ->
        typed ctxt loc Sha512
          (Item_t (Bytes_t None, rest, annot))
    | Prim (loc, I_STEPS_TO_QUOTA, [], annot),
      stack ->
        if legacy
        then begin
          (* For existing contracts, this instruction is still allowed *)
          parse_var_annot loc annot ~default:default_steps_annot >>=? fun annot ->
          typed ctxt loc Steps_to_quota
            (Item_t (Nat_t None, stack, annot))
        end
        else
          (* For new contracts this instruction is not allowed anymore *)
          fail (Deprecated_instruction I_STEPS_TO_QUOTA)
    | Prim (loc, I_SOURCE, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_source_annot >>=? fun annot ->
        typed ctxt loc Source
          (Item_t (Address_t None, stack, annot))
    | Prim (loc, I_SENDER, [], annot),
      stack ->
        parse_var_annot loc annot ~default:default_sender_annot >>=? fun annot ->
        typed ctxt loc Sender
          (Item_t (Address_t None, stack, annot))
    | Prim (loc, I_SELF, [], annot),
      stack ->
        parse_entrypoint_annot loc annot ~default:default_self_annot
        >>=? fun (annot, entrypoint) ->
        let entrypoint = Option.unopt_map ~f:(fun (`Field_annot annot) -> annot) ~default:"default" entrypoint in
        let rec get_toplevel_type : tc_context -> (bef judgement * context) tzresult Lwt.t = function
          | Lambda -> fail (Self_in_lambda loc)
          | Dip (_, prev) -> get_toplevel_type prev
          | Toplevel { param_type ; root_name ; legacy_create_contract_literal = false} ->
              Lwt.return (find_entrypoint param_type ~root_name entrypoint) >>=? fun (_, Ex_ty param_type) ->
              typed ctxt loc (Self (param_type, entrypoint))
                (Item_t (Contract_t (param_type, None), stack, annot))
          | Toplevel { param_type ; root_name = _ ; legacy_create_contract_literal = true} ->
              typed ctxt loc (Self (param_type, "default"))
                (Item_t (Contract_t (param_type, None), stack, annot)) in
        get_toplevel_type tc_context
    (* Primitive parsing errors *)
    | Prim (loc, (I_DUP | I_SWAP | I_SOME | I_UNIT
                 | I_PAIR | I_CAR | I_CDR | I_CONS | I_CONCAT | I_SLICE
                 | I_MEM | I_UPDATE | I_MAP
                 | I_GET | I_EXEC | I_FAILWITH | I_SIZE
                 | I_ADD | I_SUB
                 | I_MUL | I_EDIV | I_OR | I_AND | I_XOR
                 | I_NOT
                 | I_ABS | I_NEG | I_LSL | I_LSR
                 | I_COMPARE | I_EQ | I_NEQ
                 | I_LT | I_GT | I_LE | I_GE
                 | I_TRANSFER_TOKENS | I_CREATE_ACCOUNT
                 | I_SET_DELEGATE | I_NOW
                 | I_IMPLICIT_ACCOUNT | I_AMOUNT | I_BALANCE
                 | I_CHECK_SIGNATURE | I_HASH_KEY | I_SOURCE | I_SENDER
                 | I_BLAKE2B | I_SHA256 | I_SHA512 | I_STEPS_TO_QUOTA | I_ADDRESS
                 as name), (_ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 0, List.length l))
    | Prim (loc, (I_NONE | I_LEFT | I_RIGHT | I_NIL | I_MAP | I_ITER
                 | I_EMPTY_SET | I_DIP | I_LOOP | I_LOOP_LEFT | I_CONTRACT
                 as name), ([]
                           | _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 1, List.length l))
    | Prim (loc, (I_PUSH | I_IF_NONE | I_IF_LEFT | I_IF_CONS
                 | I_EMPTY_MAP | I_IF
                 as name), ([] | [ _ ]
                           | _ :: _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, name, 2, List.length l))
    | Prim (loc, I_LAMBDA, ([] | [ _ ]
                           | _ :: _ :: _ :: _ :: _ as l), _), _ ->
        fail (Invalid_arity (loc, I_LAMBDA, 3, List.length l))
    (* Stack errors *)
    | Prim (loc, (I_ADD | I_SUB | I_MUL | I_EDIV
                 | I_AND | I_OR | I_XOR | I_LSL | I_LSR as name), [], _),
      Item_t (ta, Item_t (tb, _, _), _) ->
        Lwt.return @@ serialize_ty_for_error ctxt ta >>=? fun (ta, ctxt) ->
        Lwt.return @@ serialize_ty_for_error ctxt tb >>=? fun (tb, _ctxt) ->
        fail (Undefined_binop (loc, name, ta, tb))
    | Prim (loc, (I_NEG | I_ABS | I_NOT | I_CONCAT | I_SIZE
                 | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE as name),
            [], _),
      Item_t (t, _, _) ->
        Lwt.return @@ serialize_ty_for_error ctxt t >>=? fun (t, _ctxt) ->
        fail (Undefined_unop (loc, name, t))
    | Prim (loc, (I_UPDATE | I_SLICE as name), [], _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, name, 3, stack))
    | Prim (loc, I_CREATE_CONTRACT, _, _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, I_CREATE_CONTRACT, 7, stack))
    | Prim (loc, I_CREATE_ACCOUNT, [], _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, I_CREATE_ACCOUNT, 4, stack))
    | Prim (loc, I_TRANSFER_TOKENS, [], _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, I_TRANSFER_TOKENS, 4, stack))
    | Prim (loc, (I_DROP | I_DUP | I_CAR | I_CDR | I_SOME
                 | I_BLAKE2B | I_SHA256 | I_SHA512 | I_DIP
                 | I_IF_NONE | I_LEFT | I_RIGHT | I_IF_LEFT | I_IF
                 | I_LOOP | I_IF_CONS | I_IMPLICIT_ACCOUNT
                 | I_NEG | I_ABS | I_INT | I_NOT | I_HASH_KEY
                 | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE as name), _, _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, name, 1, stack))
    | Prim (loc, (I_SWAP | I_PAIR | I_CONS
                 | I_GET | I_MEM | I_EXEC
                 | I_CHECK_SIGNATURE | I_ADD | I_SUB | I_MUL
                 | I_EDIV | I_AND | I_OR | I_XOR
                 | I_LSL | I_LSR as name), _, _),
      stack ->
        serialize_stack_for_error ctxt stack >>=? fun (stack, _ctxt) ->
        fail (Bad_stack (loc, name, 2, stack))
    (* Generic parsing errors *)
    | expr, _ ->
        fail @@ unexpected expr [ Seq_kind ] Instr_namespace
          [ I_DROP ; I_DUP; I_DIG; I_DUG;
            I_SWAP ; I_SOME ; I_UNIT ;
            I_PAIR ; I_CAR ; I_CDR ; I_CONS ;
            I_MEM ; I_UPDATE ; I_MAP ; I_ITER ;
            I_GET ; I_EXEC ; I_FAILWITH ; I_SIZE ;
            I_CONCAT ; I_ADD ; I_SUB ;
            I_MUL ; I_EDIV ; I_OR ; I_AND ; I_XOR ;
            I_NOT ;
            I_ABS ; I_INT; I_NEG ; I_LSL ; I_LSR ;
            I_COMPARE ; I_EQ ; I_NEQ ;
            I_LT ; I_GT ; I_LE ; I_GE ;
            I_TRANSFER_TOKENS ; I_CREATE_ACCOUNT ;
            I_CREATE_CONTRACT ; I_NOW ; I_AMOUNT ; I_BALANCE ;
            I_IMPLICIT_ACCOUNT ; I_CHECK_SIGNATURE ;
            I_BLAKE2B ; I_SHA256 ; I_SHA512 ; I_HASH_KEY ;
            I_STEPS_TO_QUOTA ;
            I_PUSH ; I_NONE ; I_LEFT ; I_RIGHT ; I_NIL ;
            I_EMPTY_SET ; I_DIP ; I_LOOP ;
            I_IF_NONE ; I_IF_LEFT ; I_IF_CONS ;
            I_EMPTY_MAP ; I_IF ; I_SOURCE ; I_SENDER ; I_SELF ; I_LAMBDA ]

and parse_contract
  : type arg. legacy: bool -> context -> Script.location -> arg ty -> Contract.t -> entrypoint:string ->
  (context * arg typed_contract) tzresult Lwt.t
  = fun ~legacy ctxt loc arg contract ~entrypoint ->
    Lwt.return @@ Gas.consume ctxt Typecheck_costs.contract_exists >>=? fun ctxt ->
    Contract.exists ctxt contract >>=? function
    | false -> fail (Invalid_contract (loc, contract))
    | true ->
        Lwt.return @@ Gas.consume ctxt Typecheck_costs.get_script >>=? fun ctxt ->
        trace
          (Invalid_contract (loc, contract)) @@
        Contract.get_script_code ctxt contract >>=? fun (ctxt, code) -> match code with
        | None ->
            Lwt.return
              (ty_eq ctxt arg (Unit_t None) >>? fun (Eq, ctxt) ->
               match entrypoint with
               | "default" ->
                   let contract : arg typed_contract = (arg, (contract, entrypoint)) in
                   ok (ctxt, contract)
               | entrypoint -> error (No_such_entrypoint entrypoint))
        | Some code ->
            Script.force_decode ctxt code >>=? fun (code, ctxt) ->
            Lwt.return
              (parse_toplevel ~legacy:true code >>? fun (arg_type, _, _, root_name) ->
               parse_parameter_ty ctxt ~legacy:true arg_type >>? fun (Ex_ty targ, ctxt) ->
               let return ctxt targ entrypoint =
                 merge_types ~legacy ctxt loc targ arg >>? fun (arg, ctxt) ->
                 let contract : arg typed_contract = (arg, (contract, entrypoint)) in
                 ok (ctxt, contract) in
               find_entrypoint_for_type ~full:targ ~expected:arg ~root_name entrypoint ctxt >>? fun (ctxt, entrypoint, targ) ->
               merge_types ~legacy ctxt loc targ arg >>? fun (targ, ctxt) ->
               return ctxt targ entrypoint)

(* Same as the one above, but does not fail when the contact is missing or
   if the expected type doesn't match the actual one. In that case None is
   returned and some overapproximation of the typechecking gas is consumed.
   This can still fail on gas exhaustion. *)
and parse_contract_for_script
  : type arg. legacy: bool -> context -> Script.location -> arg ty -> Contract.t -> entrypoint:string ->
  (context * arg typed_contract option) tzresult Lwt.t
  = fun ~legacy ctxt loc arg contract ~entrypoint ->
    Lwt.return @@ Gas.consume ctxt Typecheck_costs.contract_exists >>=? fun ctxt ->
    Contract.exists ctxt contract >>=? function
    | false -> return (ctxt, None)
    | true ->
        Lwt.return @@ Gas.consume ctxt Typecheck_costs.get_script >>=? fun ctxt ->
        trace
          (Invalid_contract (loc, contract)) @@
        Contract.get_script_code ctxt contract >>=? fun (ctxt, code) -> match code with (* can only fail because of gas *)
        | None ->
            begin match entrypoint with
              | "default" ->
                  Lwt.return
                    (match ty_eq ctxt arg (Unit_t None) with
                     | Ok (Eq, ctxt) ->
                         let contract : arg typed_contract = (arg, (contract, entrypoint)) in
                         ok (ctxt, Some contract)
                     | Error _ ->
                         Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
                         ok (ctxt, None))
              | _ -> return (ctxt, None)
            end
        | Some code ->
            Script.force_decode ctxt code >>=? fun (code, ctxt) -> (* can only fail because of gas *)
            Lwt.return
              (match parse_toplevel ~legacy:true code with
               | Error _ -> error (Invalid_contract (loc, contract))
               | Ok (arg_type, _, _, root_name) ->
                   match parse_parameter_ty ctxt ~legacy:true arg_type with
                   | Error _ ->
                       error (Invalid_contract (loc, contract))
                   | Ok (Ex_ty targ, ctxt) ->
                       match
                         find_entrypoint_for_type ~full:targ ~expected:arg ~root_name entrypoint ctxt >>? fun (ctxt, entrypoint, targ) ->
                         merge_types ~legacy ctxt loc targ arg >>? fun (targ, ctxt) ->
                         merge_types ~legacy ctxt loc targ arg >>? fun (arg, ctxt) ->
                         let contract : arg typed_contract = (arg, (contract, entrypoint)) in
                         ok (ctxt, Some contract)
                       with
                       | Ok res -> ok res
                       | Error _ ->
                           (* overapproximation by checking if targ = targ,
                                                       can only fail because of gas *)
                           ty_eq ctxt targ targ >>? fun (Eq, ctxt) ->
                           merge_types ~legacy ctxt loc targ targ >>? fun (_, ctxt) ->
                           ok (ctxt, None))

and parse_toplevel
  : legacy: bool -> Script.expr -> (Script.node * Script.node * Script.node * string option) tzresult
  = fun ~legacy toplevel ->
    record_trace (Ill_typed_contract (toplevel, [])) @@
    match root toplevel with
    | Int (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], Int_kind))
    | String (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], String_kind))
    | Bytes (loc, _) -> error (Invalid_kind (loc, [ Seq_kind ], Bytes_kind))
    | Prim (loc, _, _, _) -> error (Invalid_kind (loc, [ Seq_kind ], Prim_kind))
    | Seq (_, fields) ->
        let rec find_fields p s c fields =
          match fields with
          | [] -> ok (p, s, c)
          | Int (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Int_kind))
          | String (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], String_kind))
          | Bytes (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Bytes_kind))
          | Seq (loc, _) :: _ -> error (Invalid_kind (loc, [ Prim_kind ], Seq_kind))
          | Prim (loc, K_parameter, [ arg ], annot) :: rest ->
              begin match p with
                | None -> find_fields (Some (arg, loc, annot)) s c rest
                | Some _ -> error (Duplicate_field (loc, K_parameter))
              end
          | Prim (loc, K_storage, [ arg ], annot) :: rest ->
              begin match s with
                | None -> find_fields p (Some (arg, loc, annot)) c rest
                | Some _ -> error (Duplicate_field (loc, K_storage))
              end
          | Prim (loc, K_code, [ arg ], annot) :: rest ->
              begin match c with
                | None -> find_fields p s (Some (arg, loc, annot)) rest
                | Some _ -> error (Duplicate_field (loc, K_code))
              end
          | Prim (loc, (K_parameter | K_storage | K_code as name), args, _) :: _ ->
              error (Invalid_arity (loc, name, 1, List.length args))
          | Prim (loc, name, _, _) :: _ ->
              let allowed = [ K_parameter ; K_storage ; K_code ] in
              error (Invalid_primitive (loc, allowed, name))
        in
        find_fields None None None fields >>? function
        | (None, _, _) -> error (Missing_field K_parameter)
        | (Some _, None, _) -> error (Missing_field K_storage)
        | (Some _, Some _, None) -> error (Missing_field K_code)
        | (Some (p, ploc, pannot), Some (s, sloc, sannot), Some (c, cloc, carrot)) ->
            let maybe_root_name =
              (* root name can be attached to either the parameter
                 primitive or the toplevel constructor *)
              Script_ir_annot.extract_field_annot p >>? fun (p, root_name) ->
              match root_name with
              | Some (`Field_annot root_name) ->
                  ok (p, pannot, Some root_name)
              | None ->
                  match pannot with
                  | [ single ] when Compare.Int.(String.length single > 0) && Compare.Char.(String.get single 0 = '%') ->
                      ok (p, [], Some (String.sub single 1 (String.length single - 1)))
                  | _ -> ok (p, pannot, None) in
            if legacy then
              (* legacy semantics ignores spurious annotations *)
              let p, root_name = match maybe_root_name with Ok (p, _, root_name) -> (p, root_name) | Error _ -> (p, None) in
              ok (p, s, c, root_name)
            else
              (* only one field annot is allowed to set the root entrypoint name *)
              maybe_root_name >>? fun (p, pannot, root_name) ->
              Script_ir_annot.error_unexpected_annot ploc pannot >>? fun () ->
              Script_ir_annot.error_unexpected_annot cloc carrot >>? fun () ->
              Script_ir_annot.error_unexpected_annot sloc sannot >>? fun () ->
              ok (p, s, c, root_name)

let parse_script
  : ?type_logger: type_logger ->
    context -> legacy: bool -> Script.t -> (ex_script * context) tzresult Lwt.t
  = fun ?type_logger ctxt ~legacy { code ; storage } ->
    Script.force_decode ctxt code >>=? fun (code, ctxt) ->
    Script.force_decode ctxt storage >>=? fun (storage, ctxt) ->
    Lwt.return @@ parse_toplevel ~legacy code >>=? fun (arg_type, storage_type, code_field, root_name) ->
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_parameter_ty ctxt ~legacy arg_type))
    >>=? fun (Ex_ty arg_type, ctxt) ->
    begin
      if legacy then return () else
        Lwt.return (well_formed_entrypoints ~root_name arg_type)
    end >>=? fun () ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_storage_ty ctxt ~legacy storage_type))
    >>=? fun (Ex_ty storage_type, ctxt) ->
    let arg_annot = default_annot (type_to_var_annot (name_of_ty arg_type))
        ~default:default_param_annot in
    let storage_annot = default_annot (type_to_var_annot (name_of_ty storage_type))
        ~default:default_storage_annot in
    let arg_type_full = Pair_t ((arg_type, None, arg_annot),
                                (storage_type, None, storage_annot), None,
                                has_big_map arg_type || has_big_map storage_type) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None, false), None, None),
              (storage_type, None, None), None, has_big_map storage_type) in
    trace_eval
      (fun () ->
         Lwt.return @@ serialize_ty_for_error ctxt storage_type >>|? fun (storage_type, _ctxt) ->
         Ill_typed_data (None, storage, storage_type))
      (parse_data ?type_logger ctxt ~legacy storage_type (root storage)) >>=? fun (storage, ctxt) ->
    trace
      (Ill_typed_contract (code, []))
      (parse_returning (Toplevel { storage_type ; param_type = arg_type ; root_name ;
                                   legacy_create_contract_literal = false})
         ctxt ~legacy ?type_logger (arg_type_full, None) ret_type_full code_field) >>=? fun (code, ctxt) ->
    return (Ex_script { code ; arg_type ; storage ; storage_type ; root_name }, ctxt)

let typecheck_code
  : context -> Script.expr -> (type_map * context) tzresult Lwt.t
  = fun ctxt code ->
    let legacy = false in
    Lwt.return @@ parse_toplevel ~legacy code >>=? fun (arg_type, storage_type, code_field, root_name) ->
    let type_map = ref [] in
    trace
      (Ill_formed_type (Some "parameter", code, location arg_type))
      (Lwt.return (parse_parameter_ty ctxt ~legacy  arg_type))
    >>=? fun (Ex_ty arg_type, ctxt) ->
    begin
      if legacy then return () else
        Lwt.return (well_formed_entrypoints ~root_name arg_type)
    end >>=? fun () ->
    trace
      (Ill_formed_type (Some "storage", code, location storage_type))
      (Lwt.return (parse_storage_ty ctxt ~legacy storage_type))
    >>=? fun (Ex_ty storage_type, ctxt) ->
    let arg_annot = default_annot (type_to_var_annot (name_of_ty arg_type))
        ~default:default_param_annot in
    let storage_annot = default_annot (type_to_var_annot (name_of_ty storage_type))
        ~default:default_storage_annot in
    let arg_type_full = Pair_t ((arg_type, None, arg_annot),
                                (storage_type, None, storage_annot), None,
                                has_big_map arg_type || has_big_map storage_type) in
    let ret_type_full =
      Pair_t ((List_t (Operation_t None, None, false), None, None),
              (storage_type, None, None), None,
              has_big_map storage_type) in
    let result =
      parse_returning
        (Toplevel { storage_type ; param_type = arg_type ; root_name ;
                    legacy_create_contract_literal = false })
        ctxt ~legacy
        ~type_logger: (fun loc bef aft -> type_map := (loc, (bef, aft)) :: !type_map)
        (arg_type_full, None) ret_type_full code_field in
    trace
      (Ill_typed_contract (code, !type_map))
      result >>=? fun (Lam _, ctxt) ->
    return (!type_map, ctxt)

let typecheck_data
  : ?type_logger: type_logger ->
    context -> Script.expr * Script.expr -> context tzresult Lwt.t
  = fun ?type_logger ctxt (data, exp_ty) ->
    let legacy = false in
    trace
      (Ill_formed_type (None, exp_ty, 0))
      (Lwt.return @@ parse_packable_ty ctxt ~legacy (root exp_ty))
    >>=? fun (Ex_ty exp_ty, ctxt) ->
    trace_eval
      (fun () ->
         Lwt.return @@ serialize_ty_for_error ctxt exp_ty >>|? fun (exp_ty, _ctxt) ->
         Ill_typed_data (None, data, exp_ty))
      (parse_data ?type_logger ctxt ~legacy exp_ty (root data)) >>=? fun (_, ctxt) ->
    return ctxt

module Entrypoints_map = Map.Make (String)

let list_entrypoints  (type full) (full : full ty) ctxt ~root_name =
  let merge path annot (type t) (ty : t ty) reachable ((unreachables, all) as acc) =
    match annot with
    | None | Some (`Field_annot "") ->
        ok @@
        if reachable then acc else
          begin match ty with
            | Union_t _ -> acc
            | _ ->  ( (List.rev path)::unreachables, all )
          end
    | Some (`Field_annot name) ->
        if Compare.Int.(String.length name > 31) then ok ((List.rev path)::unreachables, all)
        else if Entrypoints_map.mem name all then ok ((List.rev path)::unreachables, all)
        else unparse_ty_no_lwt ctxt ty >>? fun (unparsed_ty , _) ->
          ok (unreachables, Entrypoints_map.add name ((List.rev path),unparsed_ty)  all)
  in
  let rec fold_tree
    : type t. t ty ->
      prim list ->
      bool ->
      prim list list * (prim list * Script.node)  Entrypoints_map.t ->
      (prim list list * (prim list * Script.node) Entrypoints_map.t) tzresult
    = fun t path reachable acc ->
      match t with
      | Union_t ((tl, al), (tr, ar), _, _) ->
          merge (D_Left :: path) al tl reachable acc >>? fun acc ->
          merge (D_Right :: path) ar tr reachable acc  >>? fun acc ->
          fold_tree tl (D_Left :: path) (match al with Some _ -> true | None -> reachable) acc >>? fun acc ->
          fold_tree tr (D_Right :: path) (match ar with Some _ -> true | None -> reachable) acc
      | _ -> ok acc in
  unparse_ty_no_lwt ctxt full >>? fun (unparsed_full , _) ->
  let init, reachable = match root_name with
    | None | Some "" -> Entrypoints_map.empty, false
    | Some name ->  Entrypoints_map.singleton name ([],unparsed_full), true in
  fold_tree full [] reachable ([], init)

(* ---- Unparsing (Typed IR -> Untyped expressions) --------------------------*)

let rec unparse_data
  : type a. context -> unparsing_mode -> a ty -> a -> (Script.node * context) tzresult Lwt.t
  = fun ctxt mode ty a ->
    Lwt.return (Gas.consume ctxt Unparse_costs.cycle) >>=? fun ctxt ->
    match ty, a with
    | Unit_t _, () ->
        Lwt.return (Gas.consume ctxt Unparse_costs.unit) >>=? fun ctxt ->
        return (Prim (-1, D_Unit, [], []), ctxt)
    | Int_t _, v ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
        return (Int (-1, Script_int.to_zint v), ctxt)
    | Nat_t _, v ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.int v)) >>=? fun ctxt ->
        return (Int (-1, Script_int.to_zint v), ctxt)
    | String_t _, s ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.string s)) >>=? fun ctxt ->
        return (String (-1, s), ctxt)
    | Bytes_t _, s ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.bytes s)) >>=? fun ctxt ->
        return (Bytes (-1, s), ctxt)
    | Bool_t _, true ->
        Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
        return (Prim (-1, D_True, [], []), ctxt)
    | Bool_t _, false ->
        Lwt.return (Gas.consume ctxt Unparse_costs.bool) >>=? fun ctxt ->
        return (Prim (-1, D_False, [], []), ctxt)
    | Timestamp_t _, t ->
        Lwt.return (Gas.consume ctxt (Unparse_costs.timestamp t)) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
          | Readable ->
              match Script_timestamp.to_notation t with
              | None -> return (Int (-1, Script_timestamp.to_zint t), ctxt)
              | Some s -> return (String (-1, s), ctxt)
        end
    | Address_t _, (c, entrypoint)  ->
        Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let entrypoint = match entrypoint with "default" -> "" | name -> name in
              let bytes = Data_encoding.Binary.to_bytes_exn
                  Data_encoding.(tup2 Contract.encoding Variable.string)
                  (c, entrypoint) in
              return (Bytes (-1, bytes), ctxt)
          | Readable ->
              let notation = match entrypoint with
                | "default" -> Contract.to_b58check c
                | entrypoint -> Contract.to_b58check c ^ "%" ^ entrypoint in
              return (String (-1, notation), ctxt)
        end
    | Contract_t _, (_, (c, entrypoint))  ->
        Lwt.return (Gas.consume ctxt Unparse_costs.contract) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let entrypoint = match entrypoint with "default" -> "" | name -> name in
              let bytes = Data_encoding.Binary.to_bytes_exn
                  Data_encoding.(tup2 Contract.encoding Variable.string)
                  (c, entrypoint) in
              return (Bytes (-1, bytes), ctxt)
          | Readable ->
              let notation = match entrypoint with
                | "default" -> Contract.to_b58check c
                | entrypoint -> Contract.to_b58check c ^ "%" ^ entrypoint in
              return (String (-1, notation), ctxt)
        end
    | Signature_t _, s ->
        Lwt.return (Gas.consume ctxt Unparse_costs.signature) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
              return (Bytes (-1, bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.to_b58check s), ctxt)
        end
    | Mutez_t _, v ->
        Lwt.return (Gas.consume ctxt Unparse_costs.tez) >>=? fun ctxt ->
        return (Int (-1, Z.of_int64 (Tez.to_mutez v)), ctxt)
    | Key_t _, k ->
        Lwt.return (Gas.consume ctxt Unparse_costs.key) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k in
              return (Bytes (-1, bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.Public_key.to_b58check k), ctxt)
        end
    | Key_hash_t _, k ->
        Lwt.return (Gas.consume ctxt Unparse_costs.key_hash) >>=? fun ctxt ->
        begin
          match mode with
          | Optimized ->
              let bytes = Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k in
              return (Bytes (-1, bytes), ctxt)
          | Readable ->
              return (String (-1, Signature.Public_key_hash.to_b58check k), ctxt)
        end
    | Operation_t _, (op, _big_map_diff) ->
        let bytes = Data_encoding.Binary.to_bytes_exn Operation.internal_operation_encoding op in
        Lwt.return (Gas.consume ctxt (Unparse_costs.operation bytes)) >>=? fun ctxt ->
        return (Bytes (-1, bytes), ctxt)
    | Chain_id_t _, chain_id ->
        let bytes = Data_encoding.Binary.to_bytes_exn Chain_id.encoding chain_id in
        Lwt.return (Gas.consume ctxt (Unparse_costs.chain_id bytes)) >>=? fun ctxt ->
        return (Bytes (-1, bytes), ctxt)
    | Pair_t ((tl, _, _), (tr, _, _), _, _), (l, r) ->
        Lwt.return (Gas.consume ctxt Unparse_costs.pair) >>=? fun ctxt ->
        unparse_data ctxt mode tl l >>=? fun (l, ctxt) ->
        unparse_data ctxt mode tr r >>=? fun (r, ctxt) ->
        return (Prim (-1, D_Pair, [ l; r ], []), ctxt)
    | Union_t ((tl, _), _, _, _), L l ->
        Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
        unparse_data ctxt mode tl l >>=? fun (l, ctxt) ->
        return (Prim (-1, D_Left, [ l ], []), ctxt)
    | Union_t (_, (tr, _), _, _), R r ->
        Lwt.return (Gas.consume ctxt Unparse_costs.union) >>=? fun ctxt ->
        unparse_data ctxt mode tr r >>=? fun (r, ctxt) ->
        return (Prim (-1, D_Right, [ r ], []), ctxt)
    | Option_t (t, _, _), Some v ->
        Lwt.return (Gas.consume ctxt Unparse_costs.some) >>=? fun ctxt ->
        unparse_data ctxt mode t v >>=? fun (v, ctxt) ->
        return (Prim (-1, D_Some, [ v ], []), ctxt)
    | Option_t _, None ->
        Lwt.return (Gas.consume ctxt Unparse_costs.none) >>=? fun ctxt ->
        return (Prim (-1, D_None, [], []), ctxt)
    | List_t (t, _, _), items ->
        fold_left_s
          (fun (l, ctxt) element ->
             Lwt.return (Gas.consume ctxt Unparse_costs.list_element) >>=? fun ctxt ->
             unparse_data ctxt mode t element >>=? fun (unparsed, ctxt) ->
             return (unparsed :: l, ctxt))
          ([], ctxt)
          items >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, List.rev items), ctxt)
    | Set_t (t, _), set ->
        let t = ty_of_comparable_ty t in
        fold_left_s
          (fun (l, ctxt) item ->
             Lwt.return (Gas.consume ctxt Unparse_costs.set_element) >>=? fun ctxt ->
             unparse_data ctxt mode t item >>=? fun (item, ctxt) ->
             return (item :: l, ctxt))
          ([], ctxt)
          (set_fold (fun e acc -> e :: acc) set []) >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, items), ctxt)
    | Map_t (kt, vt, _, _), map ->
        let kt = ty_of_comparable_ty kt in
        fold_left_s
          (fun (l, ctxt) (k, v) ->
             Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
             unparse_data ctxt mode kt k >>=? fun (key, ctxt) ->
             unparse_data ctxt mode vt v >>=? fun (value, ctxt) ->
             return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
          ([], ctxt)
          (map_fold (fun k v acc -> (k, v) :: acc) map []) >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, items), ctxt)
    | Big_map_t (kt, vt, _), { id = None ; diff = (module Diff) ; _ } ->
        (* this branch is to allow roundtrip of big map literals *)
        let kt = ty_of_comparable_ty kt in
        fold_left_s
          (fun (l, ctxt) (k, v) ->
             Lwt.return (Gas.consume ctxt Unparse_costs.map_element) >>=? fun ctxt ->
             unparse_data ctxt mode kt k >>=? fun (key, ctxt) ->
             unparse_data ctxt mode vt v >>=? fun (value, ctxt) ->
             return (Prim (-1, D_Elt, [ key ; value ], []) :: l, ctxt))
          ([], ctxt)
          (Diff.OPS.fold
             (fun k v acc -> match v with | None -> acc | Some v -> (k, v) :: acc)
             (fst Diff.boxed) []) >>=? fun (items, ctxt) ->
        return (Micheline.Seq (-1, items), ctxt)
    | Big_map_t (_kt, _kv, _), { id = Some id ; diff = (module Diff) ; _ } ->
        if Compare.Int.(Diff.OPS.cardinal (fst Diff.boxed) = 0) then
          return (Micheline.Int (-1, id), ctxt)
        else
          (* this can only be the result of an execution and the map
             must have been flushed at this point *)
          assert false
    | Lambda_t _, Lam (_, original_code) ->
        unparse_code ctxt mode original_code

(* Gas accounting may not be perfect in this function, as it is only called by RPCs. *)
and unparse_code ctxt mode =
  let legacy = true in
  function
  | Prim (loc, I_PUSH, [ ty ; data ], annot) ->
      Lwt.return (parse_packable_ty ctxt ~legacy ty) >>=? fun (Ex_ty t, ctxt) ->
      parse_data ctxt ~legacy t data >>=? fun (data, ctxt) ->
      unparse_data ctxt mode t data >>=? fun (data, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 2 annot)) >>=? fun ctxt ->
      return (Prim (loc, I_PUSH, [ ty ; data ], annot), ctxt)
  | Seq (loc, items) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.seq_cost (List.length items))) >>=? fun ctxt ->
      return (Micheline.Seq (loc, List.rev items), ctxt)
  | Prim (loc, prim, items, annot) ->
      fold_left_s
        (fun (l, ctxt) item ->
           unparse_code ctxt mode item >>=? fun (item, ctxt) ->
           return (item :: l, ctxt))
        ([], ctxt) items >>=? fun (items, ctxt) ->
      Lwt.return (Gas.consume ctxt (Unparse_costs.prim_cost 3 annot)) >>=? fun ctxt ->
      return (Prim (loc, prim, List.rev items, annot), ctxt)
  | Int _ | String _ | Bytes _ as atom -> return (atom, ctxt)

(* Gas accounting may not be perfect in this function, as it is only called by RPCs. *)
let unparse_script ctxt mode { code ; arg_type ; storage ; storage_type ; root_name } =
  let Lam (_, original_code) = code in
  unparse_code ctxt mode original_code >>=? fun (code, ctxt) ->
  unparse_data ctxt mode storage_type storage >>=? fun (storage, ctxt) ->
  unparse_ty ctxt arg_type >>=? fun (arg_type, ctxt) ->
  unparse_ty ctxt storage_type >>=? fun (storage_type, ctxt) ->
  let arg_type = add_field_annot (Option.map ~f:(fun n -> `Field_annot n) root_name) None arg_type in
  let open Micheline in
  let code =
    Seq (-1, [ Prim (-1, K_parameter, [ arg_type ], []) ;
               Prim (-1, K_storage, [ storage_type ], []) ;
               Prim (-1, K_code, [ code ], []) ]) in
  Lwt.return
    (Gas.consume ctxt (Unparse_costs.seq_cost 3) >>? fun ctxt ->
     Gas.consume ctxt (Unparse_costs.prim_cost 1 []) >>? fun ctxt ->
     Gas.consume ctxt (Unparse_costs.prim_cost 1 []) >>? fun ctxt ->
     Gas.consume ctxt (Unparse_costs.prim_cost 1 [])) >>=? fun ctxt ->
  return ({ code = lazy_expr (strip_locations code) ;
            storage = lazy_expr (strip_locations storage) }, ctxt)

let pack_data ctxt typ data =
  unparse_data ctxt Optimized typ data >>=? fun (unparsed, ctxt) ->
  let bytes = Data_encoding.Binary.to_bytes_exn expr_encoding (Micheline.strip_locations unparsed) in
  Lwt.return @@ Gas.consume ctxt (Script.serialized_cost bytes) >>=? fun ctxt ->
  let bytes = MBytes.concat "" [ MBytes.of_string "\005" ; bytes ] in
  Lwt.return @@ Gas.consume ctxt (Script.serialized_cost bytes) >>=? fun ctxt ->
  return (bytes, ctxt)

let hash_data ctxt typ data =
  pack_data ctxt typ data >>=? fun (bytes, ctxt) ->
  Lwt.return @@ Gas.consume ctxt
    (Michelson_v1_gas.Cost_of.Legacy.hash bytes Script_expr_hash.size) >>=? fun ctxt ->
  return (Script_expr_hash.(hash_bytes [ bytes ]), ctxt)

(* ---------------- Big map -------------------------------------------------*)

let empty_big_map tk tv =
  { id = None ; diff = empty_map tk ; key_type = ty_of_comparable_ty tk ; value_type = tv }

let big_map_mem ctxt key { id ; diff ; key_type ; _ } =
  match map_get key diff, id with
  | None, None -> return (false, ctxt)
  | None, Some id -> hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Big_map.mem ctxt id hash >>=? fun (ctxt, res) ->
      return (res, ctxt)
  | Some None, _ -> return (false, ctxt)
  | Some (Some _), _ -> return (true, ctxt)

let big_map_get ctxt key { id ; diff ; key_type ; value_type } =
  match map_get key diff, id with
  | Some x, _ -> return (x, ctxt)
  | None, None -> return (None, ctxt)
  | None, Some id ->
      hash_data ctxt key_type key >>=? fun (hash, ctxt) ->
      Alpha_context.Big_map.get_opt
        ctxt id hash >>=? begin function
        | (ctxt, None) -> return (None, ctxt)
        | (ctxt, Some value) ->
            parse_data ctxt ~legacy:true value_type
              (Micheline.root value) >>=? fun (x, ctxt) ->
            return (Some x, ctxt)
      end

let big_map_update key value ({ diff ; _ } as map) =
  { map with diff = map_set key value diff }

module Ids = Set.Make (Compare.Z)

type big_map_ids = Ids.t

let no_big_map_id = Ids.empty

let diff_of_big_map ctxt fresh mode ~ids { id ; key_type ; value_type ; diff } =
  Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.Legacy.map_to_list diff)) >>=? fun ctxt ->
  begin match id with
    | Some id ->
        if Ids.mem id ids then
          fresh ctxt >>=? fun (ctxt, duplicate) ->
          return (ctxt, [ Contract.Copy (id, duplicate) ], duplicate)
        else
          (* The first occurence encountered of a big_map reuses the
             ID. This way, the payer is only charged for the diff.
             For this to work, this diff has to be put at the end of
             the global diff, otherwise the duplicates will use the
             updated version as a base. This is true because we add
             this diff first in the accumulator of
             `extract_big_map_updates`, and this accumulator is not
             reversed before being flattened. *)
          return (ctxt, [], id)
    | None ->
        fresh ctxt >>=? fun (ctxt, id) ->
        unparse_ty ctxt key_type >>=? fun (kt, ctxt) ->
        unparse_ty ctxt value_type >>=? fun (kv, ctxt) ->
        return (ctxt, [ Contract.Alloc { big_map = id ;
                                         key_type = Micheline.strip_locations kt ;
                                         value_type = Micheline.strip_locations kv } ], id)
  end >>=? fun (ctxt, init, big_map) ->
  let pairs = map_fold (fun key value acc -> (key, value) :: acc) diff [] in
  fold_left_s
    (fun (acc, ctxt) (key, value) ->
       Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
       hash_data ctxt key_type key >>=? fun (diff_key_hash, ctxt) ->
       unparse_data ctxt mode key_type key >>=? fun (key_node, ctxt) ->
       let diff_key = Micheline.strip_locations key_node in
       begin
         match value with
         | None -> return (None, ctxt)
         | Some x ->
             begin
               unparse_data ctxt mode value_type x >>=? fun (node, ctxt) ->
               return (Some (Micheline.strip_locations node), ctxt)
             end
       end >>=? fun (diff_value, ctxt) ->
       let diff_item = Contract.Update { big_map ; diff_key ; diff_key_hash ; diff_value } in
       return (diff_item :: acc, ctxt))
    ([], ctxt) pairs >>=? fun (diff, ctxt) ->
  return (init @ diff, big_map, ctxt)

let rec extract_big_map_updates
  : type a. context -> (context -> (context * Big_map.id) tzresult Lwt.t) ->
    unparsing_mode -> Ids.t -> Contract.big_map_diff list -> a ty -> a ->
    (context * a * Ids.t * Contract.big_map_diff list) tzresult Lwt.t
  = fun ctxt fresh mode ids acc ty x ->
    match (ty, x) with
    | Big_map_t (_, _, _), map ->
        diff_of_big_map ctxt fresh mode ids map >>=? fun (diff, id, ctxt) ->
        let (module Map) = map.diff in
        let map = { map with diff = empty_map Map.key_ty ; id = Some id } in
        return (ctxt, map, Ids.add id ids, diff :: acc)
    | Pair_t ((tyl, _, _), (tyr, _, _), _, true), (xl, xr) ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
        extract_big_map_updates ctxt fresh mode ids acc tyl xl >>=? fun (ctxt, xl, ids, acc) ->
        extract_big_map_updates ctxt fresh mode ids acc tyr xr >>=? fun (ctxt, xr, ids, acc) ->
        return (ctxt, (xl, xr), ids, acc)
    | Union_t ((ty, _), (_, _), _, true), L x ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
        extract_big_map_updates ctxt fresh mode ids acc ty x >>=? fun (ctxt, x, ids, acc) ->
        return (ctxt, L x, ids, acc)
    | Union_t ((_, _), (ty, _), _, true), R x ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
        extract_big_map_updates ctxt fresh mode ids acc ty x >>=? fun (ctxt, x, ids, acc) ->
        return (ctxt, R x, ids, acc)
    | Option_t (ty, _, true), Some x ->
        Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
        extract_big_map_updates ctxt fresh mode ids acc ty x >>=? fun (ctxt, x, ids, acc) ->
        return (ctxt, Some x, ids, acc)
    | List_t (ty, _, true), l ->
        fold_left_s
          (fun (ctxt, l, ids, acc) x ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
             extract_big_map_updates ctxt fresh mode ids acc ty x >>=? fun (ctxt, x, ids, acc) ->
             return (ctxt, x :: l, ids, acc))
          (ctxt, [], ids, acc) l >>=? fun (ctxt, l, ids, acc) ->
        return (ctxt, List.rev l, ids, acc)
    | Map_t (_, ty, _, true), ((module M) as m) ->
        Lwt.return (Gas.consume ctxt (Michelson_v1_gas.Cost_of.Legacy.map_to_list m)) >>=? fun ctxt ->
        fold_left_s
          (fun (ctxt, m, ids, acc) (k, x) ->
             Lwt.return (Gas.consume ctxt Typecheck_costs.cycle) >>=? fun ctxt ->
             extract_big_map_updates ctxt fresh mode ids acc ty x >>=? fun (ctxt, x, ids, acc) ->
             return (ctxt, M.OPS.add k x m, ids, acc))
          (ctxt, M.OPS.empty, ids, acc) (M.OPS.bindings (fst M.boxed)) >>=? fun (ctxt, m, ids, acc) ->
        let module M = struct
          module OPS = M.OPS
          type key = M.key
          type value = M.value
          let key_ty = M.key_ty
          let boxed = m, (snd M.boxed)
        end in
        return (ctxt, (module M : Boxed_map with type key = M.key and type value = M.value), ids, acc)
    | Option_t (_, _, true), None -> return (ctxt, None, ids, acc)
    | List_t (_, _, false), v -> return (ctxt, v, ids, acc)
    | Map_t (_, _, _, false), v -> return (ctxt, v, ids, acc)
    | Option_t (_, _, false), None -> return (ctxt, None, ids, acc)
    | Pair_t (_, _, _, false), v -> return (ctxt, v, ids, acc)
    | Union_t (_, _, _, false), v -> return (ctxt, v, ids, acc)
    | Option_t (_, _, false), v -> return (ctxt, v, ids, acc)
    | Chain_id_t _, v -> return (ctxt, v, ids, acc)
    | Set_t (_, _), v -> return (ctxt, v, ids, acc)
    | Unit_t _, v -> return (ctxt, v, ids, acc)
    | Int_t _, v -> return (ctxt, v, ids, acc)
    | Nat_t _, v -> return (ctxt, v, ids, acc)
    | Signature_t _, v -> return (ctxt, v, ids, acc)
    | String_t _, v -> return (ctxt, v, ids, acc)
    | Bytes_t _, v -> return (ctxt, v, ids, acc)
    | Mutez_t _, v -> return (ctxt, v, ids, acc)
    | Key_hash_t _, v -> return (ctxt, v, ids, acc)
    | Key_t _, v -> return (ctxt, v, ids, acc)
    | Timestamp_t _, v -> return (ctxt, v, ids, acc)
    | Address_t _, v -> return (ctxt, v, ids, acc)
    | Bool_t _, v -> return (ctxt, v, ids, acc)
    | Lambda_t (_, _, _), v -> return (ctxt, v, ids, acc)
    | Contract_t (_, _), v -> return (ctxt, v, ids, acc)
    | Operation_t _, _ -> assert false (* called only on parameters and storage, which cannot contain operations *)

let collect_big_maps ctxt ty x =
  let rec collect
    : type a. context -> a ty -> a -> Ids.t -> (Ids.t * context) tzresult
    = fun ctxt ty x acc ->
      match (ty, x) with
      | Big_map_t (_, _, _), { id = Some id } ->
          Gas.consume ctxt Typecheck_costs.cycle >>? fun ctxt ->
          ok (Ids.add id acc, ctxt)
      | Pair_t ((tyl, _, _), (tyr, _, _), _, true), (xl, xr) ->
          collect ctxt tyl xl acc >>? fun (acc, ctxt) ->
          collect ctxt tyr xr acc
      | Union_t ((ty, _), (_, _), _, true), L x ->
          collect ctxt ty x acc
      | Union_t ((_, _), (ty, _), _, true), R x ->
          collect ctxt ty x acc
      | Option_t (ty, _, true), Some x ->
          collect ctxt ty x acc
      | List_t (ty, _, true), l ->
          List.fold_left (fun acc x -> acc >>? fun (acc, ctxt) -> collect ctxt ty x acc) (ok (acc, ctxt)) l
      | Map_t (_, ty, _, true), m ->
          map_fold (fun _ v acc -> acc >>? fun (acc, ctxt) -> collect ctxt ty v acc) m (ok (acc, ctxt))
      | List_t (_, _, false), _ -> ok (acc, ctxt)
      | Map_t (_, _, _, false), _ -> ok (acc, ctxt)
      | Big_map_t (_, _, _), { id = None } -> ok (acc, ctxt)
      | Option_t (_, _, true), None -> ok (acc, ctxt)
      | Option_t (_, _, false), _ -> ok (acc, ctxt)
      | Union_t (_, _, _, false), _ -> ok (acc, ctxt)
      | Pair_t (_, _, _, false), _ -> ok (acc, ctxt)
      | Chain_id_t _, _ -> ok (acc, ctxt)
      | Set_t (_, _), _ -> ok (acc, ctxt)
      | Unit_t _, _ -> ok (acc, ctxt)
      | Int_t _, _ -> ok (acc, ctxt)
      | Nat_t _, _ -> ok (acc, ctxt)
      | Signature_t _, _ -> ok (acc, ctxt)
      | String_t _, _ -> ok (acc, ctxt)
      | Bytes_t _, _ -> ok (acc, ctxt)
      | Mutez_t _, _ -> ok (acc, ctxt)
      | Key_hash_t _, _ -> ok (acc, ctxt)
      | Key_t _, _ -> ok (acc, ctxt)
      | Timestamp_t _, _ -> ok (acc, ctxt)
      | Address_t _, _ -> ok (acc, ctxt)
      | Bool_t _, _ -> ok (acc, ctxt)
      | Lambda_t (_, _, _), _ -> ok (acc, ctxt)
      | Contract_t (_, _), _ -> ok (acc, ctxt)
      | Operation_t _, _ -> assert false (* called only on parameters and storage, which cannot contain operations *) in
  Lwt.return (collect ctxt ty x no_big_map_id)

let extract_big_map_diff ctxt mode
    ~temporary ~to_duplicate ~to_update
    ty v =
  let to_duplicate = Ids.diff to_duplicate to_update in
  let fresh = if temporary then (fun c -> return (Big_map.fresh_temporary c)) else Big_map.fresh in
  extract_big_map_updates ctxt fresh mode to_duplicate [] ty v >>=? fun (ctxt, v, alive, diffs) ->
  let diffs = if temporary then diffs else
      let dead = Ids.diff to_update alive in
      Ids.fold (fun id acc -> Contract.Clear id :: acc) dead [] :: diffs in
  match diffs with
  | [] -> return (v, None, ctxt)
  | diffs -> return (v, Some (List.flatten diffs (* do not reverse *)), ctxt)

let list_of_big_map_ids ids = Ids.elements ids
