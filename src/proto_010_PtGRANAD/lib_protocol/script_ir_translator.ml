(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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
module Typecheck_costs = Michelson_v1_gas.Cost_of.Typechecking
module Unparse_costs = Michelson_v1_gas.Cost_of.Unparsing

type ex_comparable_ty =
  | Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty

type ex_ty = Ex_ty : 'a ty -> ex_ty

type ex_stack_ty = Ex_stack_ty : ('a, 's) stack_ty -> ex_stack_ty

(*

   The following type represents an instruction parameterized by its
   continuation. During the elaboration of the typed term, a sequence
   of instructions in Micheline is read from left to right: hence, the
   elaboration needs to wait for the next instruction to be elaborated
   to be able to construct the current instruction.

*)
type ('a, 's, 'b, 'u) cinstr = {
  apply :
    'r 'f. ('a, 's) kinfo -> ('b, 'u, 'r, 'f) kinstr -> ('a, 's, 'r, 'f) kinstr;
}

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
  let kinfo = {iloc = loc; kstack_ty = aft} in
  let kinfo' = {iloc = loc; kstack_ty = bef} in
  let kinstr = instr.apply kinfo' (IHalt kinfo) in
  {kloc = loc; kbef = bef; kaft = aft; kinstr}

let kinfo_of_descr {loc; bef; _} = {iloc = loc; kstack_ty = bef}

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
    instr =
      {
        apply =
          (fun _ k ->
            d1.instr.apply
              (kinfo_of_descr d1)
              (d2.instr.apply (kinfo_of_descr d2) k));
      };
  }

type tc_context =
  | Lambda : tc_context
  | Dip : ('a, 's) stack_ty * tc_context -> tc_context
  | Toplevel : {
      storage_type : 'sto ty;
      param_type : 'param ty;
      root_name : field_annot option;
      legacy_create_contract_literal : bool;
    }
      -> tc_context

type unparsing_mode = Optimized | Readable | Optimized_legacy

type type_logger =
  int ->
  (Script.expr * Script.annot) list ->
  (Script.expr * Script.annot) list ->
  unit

let add_dip ty annot prev =
  match prev with
  | Lambda | Toplevel _ ->
      Dip (Item_t (ty, Item_t (Unit_t None, Bot_t, None), annot), prev)
  | Dip (stack, _) ->
      Dip (Item_t (ty, stack, annot), prev)

(* ---- Type size accounting ------------------------------------------------*)

(**
  [deduce_comparable_type_size ~remaining ty] returns [remaining] minus the size of type [ty].
  It is guaranteed to not grow the stack by more than [remaining] non-tail calls.
*)
let rec deduce_comparable_type_size :
    type t. remaining:int -> t comparable_ty -> int =
 fun ~remaining ty ->
  if Compare.Int.(remaining < 0) then remaining
  else
    match ty with
    | Unit_key _
    | Never_key _
    | Int_key _
    | Nat_key _
    | Signature_key _
    | String_key _
    | Bytes_key _
    | Mutez_key _
    | Bool_key _
    | Key_hash_key _
    | Key_key _
    | Timestamp_key _
    | Chain_id_key _
    | Address_key _ ->
        remaining - 1
    | Pair_key ((t1, _), (t2, _), _) ->
        let remaining = remaining - 1 in
        let remaining = deduce_comparable_type_size ~remaining t1 in
        deduce_comparable_type_size ~remaining t2
    | Union_key ((t1, _), (t2, _), _) ->
        let remaining = remaining - 1 in
        let remaining = deduce_comparable_type_size ~remaining t1 in
        deduce_comparable_type_size ~remaining t2
    | Option_key (t, _) ->
        let remaining = remaining - 1 in
        deduce_comparable_type_size ~remaining t

(**
  [deduce_type_size ~remaining ty] returns [remaining] minus the size of type [ty].
  It is guaranteed to not grow the stack by more than [remaining] non-tail calls.
*)
let rec deduce_type_size : type t. remaining:int -> t ty -> int =
 fun ~remaining ty ->
  match ty with
  | Unit_t _
  | Int_t _
  | Nat_t _
  | Signature_t _
  | Bytes_t _
  | String_t _
  | Mutez_t _
  | Key_hash_t _
  | Key_t _
  | Timestamp_t _
  | Address_t _
  | Bool_t _
  | Operation_t _
  | Chain_id_t _
  | Never_t _
  | Bls12_381_g1_t _
  | Bls12_381_g2_t _
  | Bls12_381_fr_t _
  | Sapling_transaction_t _
  | Sapling_state_t _ ->
      remaining - 1
  | Pair_t ((l, _, _), (r, _, _), _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining l in
      deduce_type_size ~remaining r
  | Union_t ((l, _), (r, _), _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining l in
      deduce_type_size ~remaining r
  | Lambda_t (arg, ret, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_type_size ~remaining arg in
      deduce_type_size ~remaining ret
  | Option_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining t
  | List_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining t
  | Ticket_t (t, _) ->
      let remaining = remaining - 1 in
      deduce_comparable_type_size ~remaining t
  | Set_t (k, _) ->
      let remaining = remaining - 1 in
      deduce_comparable_type_size ~remaining k
  | Map_t (k, v, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_comparable_type_size ~remaining k in
      deduce_type_size ~remaining v
  | Big_map_t (k, v, _) ->
      let remaining = remaining - 1 in
      let remaining = deduce_comparable_type_size ~remaining k in
      deduce_type_size ~remaining v
  | Contract_t (arg, _) ->
      let remaining = remaining - 1 in
      deduce_type_size ~remaining arg

let check_type_size ~loc ~maximum_type_size ty =
  if Compare.Int.(deduce_type_size ~remaining:maximum_type_size ty >= 0) then
    ok_unit
  else error (Type_too_large (loc, maximum_type_size))

let rec check_type_size_of_stack_head :
    type a s.
    loc:Script.location ->
    maximum_type_size:int ->
    (a, s) stack_ty ->
    up_to:int ->
    unit tzresult =
 fun ~loc ~maximum_type_size stack ~up_to ->
  if Compare.Int.(up_to <= 0) then ok_unit
  else
    match stack with
    | Bot_t ->
        ok_unit
    | Item_t (head, tail, _annot) ->
        check_type_size ~loc ~maximum_type_size head
        >>? fun () ->
        (check_type_size_of_stack_head [@tailcall])
          ~loc
          ~maximum_type_size
          tail
          ~up_to:(up_to - 1)

let check_comparable_type_size ~legacy ctxt ~loc ty =
  if legacy then ok_unit
  else
    let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
    if
      Compare.Int.(
        deduce_comparable_type_size ~remaining:maximum_type_size ty >= 0)
    then ok_unit
    else error (Type_too_large (loc, maximum_type_size))

let check_type_size ~legacy ctxt ~loc ty =
  if legacy then ok_unit
  else
    let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
    check_type_size ~loc ~maximum_type_size ty

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
  | (Int_kind, Int_kind)
  | (String_kind, String_kind)
  | (Bytes_kind, Bytes_kind)
  | (Prim_kind, Prim_kind)
  | (Seq_kind, Seq_kind) ->
      true
  | _ ->
      false

let kind = function
  | Int _ ->
      Int_kind
  | String _ ->
      String_kind
  | Bytes _ ->
      Bytes_kind
  | Prim _ ->
      Prim_kind
  | Seq _ ->
      Seq_kind

let unexpected expr exp_kinds exp_ns exp_prims =
  match expr with
  | Int (loc, _) ->
      Invalid_kind (loc, Prim_kind :: exp_kinds, Int_kind)
  | String (loc, _) ->
      Invalid_kind (loc, Prim_kind :: exp_kinds, String_kind)
  | Bytes (loc, _) ->
      Invalid_kind (loc, Prim_kind :: exp_kinds, Bytes_kind)
  | Seq (loc, _) ->
      Invalid_kind (loc, Prim_kind :: exp_kinds, Seq_kind)
  | Prim (loc, name, _, _) -> (
      let open Michelson_v1_primitives in
      match (namespace name, exp_ns) with
      | (Type_namespace, Type_namespace)
      | (Instr_namespace, Instr_namespace)
      | (Constant_namespace, Constant_namespace) ->
          Invalid_primitive (loc, exp_prims, name)
      | (ns, _) ->
          Invalid_namespace (loc, name, exp_ns, ns) )

let check_kind kinds expr =
  let kind = kind expr in
  if List.exists (kind_equal kind) kinds then ok_unit
  else
    let loc = location expr in
    error (Invalid_kind (loc, kinds, kind))

(* ---- Lists, Sets and Maps ----------------------------------------------- *)

let list_empty : 'a Script_typed_ir.boxed_list =
  let open Script_typed_ir in
  {elements = []; length = 0}

let list_cons :
    'a -> 'a Script_typed_ir.boxed_list -> 'a Script_typed_ir.boxed_list =
 fun elt l ->
  let open Script_typed_ir in
  {length = 1 + l.length; elements = elt :: l.elements}

let compare_address (x, ex) (y, ey) =
  let lres = Contract.compare x y in
  if Compare.Int.(lres = 0) then Compare.String.compare ex ey else lres

type compare_comparable_cont =
  | Compare_comparable :
      'a comparable_ty * 'a * 'a * compare_comparable_cont
      -> compare_comparable_cont
  | Compare_comparable_return : compare_comparable_cont

let compare_comparable : type a. a comparable_ty -> a -> a -> int =
  let rec compare_comparable :
      type a. a comparable_ty -> compare_comparable_cont -> a -> a -> int =
   fun kind k x y ->
    match (kind, x, y) with
    | (Unit_key _, (), ()) ->
        (apply [@tailcall]) 0 k
    | (Never_key _, _, _) ->
        .
    | (Signature_key _, x, y) ->
        (apply [@tailcall]) (Signature.compare x y) k
    | (String_key _, x, y) ->
        (apply [@tailcall]) (Compare.String.compare x y) k
    | (Bool_key _, x, y) ->
        (apply [@tailcall]) (Compare.Bool.compare x y) k
    | (Mutez_key _, x, y) ->
        (apply [@tailcall]) (Tez.compare x y) k
    | (Key_hash_key _, x, y) ->
        (apply [@tailcall]) (Signature.Public_key_hash.compare x y) k
    | (Key_key _, x, y) ->
        (apply [@tailcall]) (Signature.Public_key.compare x y) k
    | (Int_key _, x, y) ->
        (apply [@tailcall]) (Script_int.compare x y) k
    | (Nat_key _, x, y) ->
        (apply [@tailcall]) (Script_int.compare x y) k
    | (Timestamp_key _, x, y) ->
        (apply [@tailcall]) (Script_timestamp.compare x y) k
    | (Address_key _, x, y) ->
        (apply [@tailcall]) (compare_address x y) k
    | (Bytes_key _, x, y) ->
        (apply [@tailcall]) (Compare.Bytes.compare x y) k
    | (Chain_id_key _, x, y) ->
        (apply [@tailcall]) (Chain_id.compare x y) k
    | (Pair_key ((tl, _), (tr, _), _), (lx, rx), (ly, ry)) ->
        (compare_comparable [@tailcall])
          tl
          (Compare_comparable (tr, rx, ry, k))
          lx
          ly
    | (Union_key ((tl, _), _, _), L x, L y) ->
        (compare_comparable [@tailcall]) tl k x y
    | (Union_key _, L _, R _) ->
        -1
    | (Union_key _, R _, L _) ->
        1
    | (Union_key (_, (tr, _), _), R x, R y) ->
        (compare_comparable [@tailcall]) tr k x y
    | (Option_key _, None, None) ->
        0
    | (Option_key _, None, Some _) ->
        -1
    | (Option_key _, Some _, None) ->
        1
    | (Option_key (t, _), Some x, Some y) ->
        (compare_comparable [@tailcall]) t k x y
  and apply ret k =
    match (ret, k) with
    | (0, Compare_comparable (ty, x, y, k)) ->
        (compare_comparable [@tailcall]) ty k x y
    | (0, Compare_comparable_return) ->
        0
    | (ret, _) ->
        (* ret <> 0, we perform an early exit *)
        if Compare.Int.(ret > 0) then 1 else -1
  in
  fun t -> compare_comparable t Compare_comparable_return

let empty_set : type a. a comparable_ty -> a set =
 fun ty ->
  let module OPS = Set.Make (struct
    type t = a

    let compare = compare_comparable ty
  end) in
  ( module struct
    type elt = a

    let elt_ty = ty

    module OPS = OPS

    let boxed = OPS.empty

    let size = 0
  end )

let set_update : type a. a -> bool -> a set -> a set =
 fun v b (module Box) ->
  ( module struct
    type elt = a

    let elt_ty = Box.elt_ty

    module OPS = Box.OPS

    let boxed =
      if b then Box.OPS.add v Box.boxed else Box.OPS.remove v Box.boxed

    let size =
      let mem = Box.OPS.mem v Box.boxed in
      if mem then if b then Box.size else Box.size - 1
      else if b then Box.size + 1
      else Box.size
  end )

let set_mem : type elt. elt -> elt set -> bool =
 fun v (module Box) -> Box.OPS.mem v Box.boxed

let set_fold : type elt acc. (elt -> acc -> acc) -> elt set -> acc -> acc =
 fun f (module Box) -> Box.OPS.fold f Box.boxed

let set_size : type elt. elt set -> Script_int.n Script_int.num =
 fun (module Box) -> Script_int.(abs (of_int Box.size))

let map_key_ty : type a b. (a, b) map -> a comparable_ty =
 fun (module Box) -> Box.key_ty

let empty_map : type a b. a comparable_ty -> (a, b) map =
 fun ty ->
  let module OPS = Map.Make (struct
    type t = a

    let compare = compare_comparable ty
  end) in
  ( module struct
    type key = a

    type value = b

    let key_ty = ty

    module OPS = OPS

    let boxed = (OPS.empty, 0)
  end )

let map_get : type key value. key -> (key, value) map -> value option =
 fun k (module Box) -> Box.OPS.find_opt k (fst Box.boxed)

let map_update : type a b. a -> b option -> (a, b) map -> (a, b) map =
 fun k v (module Box) ->
  ( module struct
    type key = a

    type value = b

    let key_ty = Box.key_ty

    module OPS = Box.OPS

    let boxed =
      let (map, size) = Box.boxed in
      let contains = Box.OPS.mem k map in
      match v with
      | Some v ->
          (Box.OPS.add k v map, size + if contains then 0 else 1)
      | None ->
          (Box.OPS.remove k map, size - if contains then 1 else 0)
  end )

let map_mem : type key value. key -> (key, value) map -> bool =
 fun k (module Box) -> Box.OPS.mem k (fst Box.boxed)

let map_fold :
    type key value acc.
    (key -> value -> acc -> acc) -> (key, value) map -> acc -> acc =
 fun f (module Box) -> Box.OPS.fold f (fst Box.boxed)

let map_size : type key value. (key, value) map -> Script_int.n Script_int.num
    =
 fun (module Box) -> Script_int.(abs (of_int (snd Box.boxed)))

(* ---- Unparsing (Typed IR -> Untyped expressions) of types -----------------*)

(* This part contains the unparsing that does not depend on parsing
   (everything that cannot contain a lambda). The rest is located at
   the end of the file. *)

let rec ty_of_comparable_ty : type a. a comparable_ty -> a ty = function
  | Unit_key tname ->
      Unit_t tname
  | Never_key tname ->
      Never_t tname
  | Int_key tname ->
      Int_t tname
  | Nat_key tname ->
      Nat_t tname
  | Signature_key tname ->
      Signature_t tname
  | String_key tname ->
      String_t tname
  | Bytes_key tname ->
      Bytes_t tname
  | Mutez_key tname ->
      Mutez_t tname
  | Bool_key tname ->
      Bool_t tname
  | Key_hash_key tname ->
      Key_hash_t tname
  | Key_key tname ->
      Key_t tname
  | Timestamp_key tname ->
      Timestamp_t tname
  | Address_key tname ->
      Address_t tname
  | Chain_id_key tname ->
      Chain_id_t tname
  | Pair_key ((l, al), (r, ar), tname) ->
      Pair_t
        ( (ty_of_comparable_ty l, al, None),
          (ty_of_comparable_ty r, ar, None),
          tname )
  | Union_key ((l, al), (r, ar), tname) ->
      Union_t ((ty_of_comparable_ty l, al), (ty_of_comparable_ty r, ar), tname)
  | Option_key (t, tname) ->
      Option_t (ty_of_comparable_ty t, tname)

let add_field_annot a var = function
  | Prim (loc, prim, args, annots) ->
      Prim
        ( loc,
          prim,
          args,
          annots @ unparse_field_annot a @ unparse_var_annot var )
  | expr ->
      expr

let rec unparse_comparable_ty : type a. a comparable_ty -> Script.node =
  function
  | Unit_key tname ->
      Prim (-1, T_unit, [], unparse_type_annot tname)
  | Never_key tname ->
      Prim (-1, T_never, [], unparse_type_annot tname)
  | Int_key tname ->
      Prim (-1, T_int, [], unparse_type_annot tname)
  | Nat_key tname ->
      Prim (-1, T_nat, [], unparse_type_annot tname)
  | Signature_key tname ->
      Prim (-1, T_signature, [], unparse_type_annot tname)
  | String_key tname ->
      Prim (-1, T_string, [], unparse_type_annot tname)
  | Bytes_key tname ->
      Prim (-1, T_bytes, [], unparse_type_annot tname)
  | Mutez_key tname ->
      Prim (-1, T_mutez, [], unparse_type_annot tname)
  | Bool_key tname ->
      Prim (-1, T_bool, [], unparse_type_annot tname)
  | Key_hash_key tname ->
      Prim (-1, T_key_hash, [], unparse_type_annot tname)
  | Key_key tname ->
      Prim (-1, T_key, [], unparse_type_annot tname)
  | Timestamp_key tname ->
      Prim (-1, T_timestamp, [], unparse_type_annot tname)
  | Address_key tname ->
      Prim (-1, T_address, [], unparse_type_annot tname)
  | Chain_id_key tname ->
      Prim (-1, T_chain_id, [], unparse_type_annot tname)
  | Pair_key ((l, al), (r, ar), pname) -> (
      let tl = add_field_annot al None (unparse_comparable_ty l) in
      let tr = add_field_annot ar None (unparse_comparable_ty r) in
      (* Fold [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an] *)
      (* Note that the folding does not happen if the pair on the right has a
         field annotation because this annotation would be lost *)
      match tr with
      | Prim (_, T_pair, ts, []) ->
          Prim (-1, T_pair, tl :: ts, unparse_type_annot pname)
      | _ ->
          Prim (-1, T_pair, [tl; tr], unparse_type_annot pname) )
  | Union_key ((l, al), (r, ar), tname) ->
      let tl = add_field_annot al None (unparse_comparable_ty l) in
      let tr = add_field_annot ar None (unparse_comparable_ty r) in
      Prim (-1, T_or, [tl; tr], unparse_type_annot tname)
  | Option_key (t, tname) ->
      Prim (-1, T_option, [unparse_comparable_ty t], unparse_type_annot tname)

let unparse_memo_size memo_size =
  let z = Sapling.Memo_size.unparse_to_z memo_size in
  Int (-1, z)

let rec unparse_ty :
    type a. context -> a ty -> (Script.node * context) tzresult =
 fun ctxt ty ->
  Gas.consume ctxt Unparse_costs.unparse_type_cycle
  >>? fun ctxt ->
  let return ctxt (name, args, annot) =
    let result = Prim (-1, name, args, annot) in
    ok (result, ctxt)
  in
  match ty with
  | Unit_t tname ->
      return ctxt (T_unit, [], unparse_type_annot tname)
  | Int_t tname ->
      return ctxt (T_int, [], unparse_type_annot tname)
  | Nat_t tname ->
      return ctxt (T_nat, [], unparse_type_annot tname)
  | Signature_t tname ->
      return ctxt (T_signature, [], unparse_type_annot tname)
  | String_t tname ->
      return ctxt (T_string, [], unparse_type_annot tname)
  | Bytes_t tname ->
      return ctxt (T_bytes, [], unparse_type_annot tname)
  | Mutez_t tname ->
      return ctxt (T_mutez, [], unparse_type_annot tname)
  | Bool_t tname ->
      return ctxt (T_bool, [], unparse_type_annot tname)
  | Key_hash_t tname ->
      return ctxt (T_key_hash, [], unparse_type_annot tname)
  | Key_t tname ->
      return ctxt (T_key, [], unparse_type_annot tname)
  | Timestamp_t tname ->
      return ctxt (T_timestamp, [], unparse_type_annot tname)
  | Address_t tname ->
      return ctxt (T_address, [], unparse_type_annot tname)
  | Operation_t tname ->
      return ctxt (T_operation, [], unparse_type_annot tname)
  | Chain_id_t tname ->
      return ctxt (T_chain_id, [], unparse_type_annot tname)
  | Never_t tname ->
      return ctxt (T_never, [], unparse_type_annot tname)
  | Bls12_381_g1_t tname ->
      return ctxt (T_bls12_381_g1, [], unparse_type_annot tname)
  | Bls12_381_g2_t tname ->
      return ctxt (T_bls12_381_g2, [], unparse_type_annot tname)
  | Bls12_381_fr_t tname ->
      return ctxt (T_bls12_381_fr, [], unparse_type_annot tname)
  | Contract_t (ut, tname) ->
      unparse_ty ctxt ut
      >>? fun (t, ctxt) ->
      return ctxt (T_contract, [t], unparse_type_annot tname)
  | Pair_t ((utl, l_field, l_var), (utr, r_field, r_var), tname) ->
      let annot = unparse_type_annot tname in
      unparse_ty ctxt utl
      >>? fun (utl, ctxt) ->
      let tl = add_field_annot l_field l_var utl in
      unparse_ty ctxt utr
      >>? fun (utr, ctxt) ->
      let tr = add_field_annot r_field r_var utr in
      (* Fold [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an] *)
      (* Note that the folding does not happen if the pair on the right has an
         annotation because this annotation would be lost *)
      return
        ctxt
        ( match tr with
        | Prim (_, T_pair, ts, []) ->
            (T_pair, tl :: ts, annot)
        | _ ->
            (T_pair, [tl; tr], annot) )
  | Union_t ((utl, l_field), (utr, r_field), tname) ->
      let annot = unparse_type_annot tname in
      unparse_ty ctxt utl
      >>? fun (utl, ctxt) ->
      let tl = add_field_annot l_field None utl in
      unparse_ty ctxt utr
      >>? fun (utr, ctxt) ->
      let tr = add_field_annot r_field None utr in
      return ctxt (T_or, [tl; tr], annot)
  | Lambda_t (uta, utr, tname) ->
      unparse_ty ctxt uta
      >>? fun (ta, ctxt) ->
      unparse_ty ctxt utr
      >>? fun (tr, ctxt) ->
      return ctxt (T_lambda, [ta; tr], unparse_type_annot tname)
  | Option_t (ut, tname) ->
      let annot = unparse_type_annot tname in
      unparse_ty ctxt ut
      >>? fun (ut, ctxt) -> return ctxt (T_option, [ut], annot)
  | List_t (ut, tname) ->
      unparse_ty ctxt ut
      >>? fun (t, ctxt) -> return ctxt (T_list, [t], unparse_type_annot tname)
  | Ticket_t (ut, tname) ->
      let t = unparse_comparable_ty ut in
      return ctxt (T_ticket, [t], unparse_type_annot tname)
  | Set_t (ut, tname) ->
      let t = unparse_comparable_ty ut in
      return ctxt (T_set, [t], unparse_type_annot tname)
  | Map_t (uta, utr, tname) ->
      let ta = unparse_comparable_ty uta in
      unparse_ty ctxt utr
      >>? fun (tr, ctxt) ->
      return ctxt (T_map, [ta; tr], unparse_type_annot tname)
  | Big_map_t (uta, utr, tname) ->
      let ta = unparse_comparable_ty uta in
      unparse_ty ctxt utr
      >>? fun (tr, ctxt) ->
      return ctxt (T_big_map, [ta; tr], unparse_type_annot tname)
  | Sapling_transaction_t (memo_size, tname) ->
      return
        ctxt
        ( T_sapling_transaction,
          [unparse_memo_size memo_size],
          unparse_type_annot tname )
  | Sapling_state_t (memo_size, tname) ->
      return
        ctxt
        ( T_sapling_state,
          [unparse_memo_size memo_size],
          unparse_type_annot tname )

let rec strip_var_annots = function
  | (Int _ | String _ | Bytes _) as atom ->
      atom
  | Seq (loc, args) ->
      Seq (loc, List.map strip_var_annots args)
  | Prim (loc, name, args, annots) ->
      let not_var_annot s = Compare.Char.(s.[0] <> '@') in
      let annots = List.filter not_var_annot annots in
      Prim (loc, name, List.map strip_var_annots args, annots)

let serialize_ty_for_error ctxt ty =
  unparse_ty ctxt ty
  >>? (fun (ty, ctxt) ->
        Gas.consume ctxt (Script.strip_locations_cost ty)
        >|? fun ctxt -> (Micheline.strip_locations (strip_var_annots ty), ctxt))
  |> record_trace Cannot_serialize_error

let rec comparable_ty_of_ty :
    type a.
    context -> Script.location -> a ty -> (a comparable_ty * context) tzresult
    =
 fun ctxt loc ty ->
  Gas.consume ctxt Typecheck_costs.comparable_ty_of_ty_cycle
  >>? fun ctxt ->
  match ty with
  | Unit_t tname ->
      ok ((Unit_key tname : a comparable_ty), ctxt)
  | Never_t tname ->
      ok (Never_key tname, ctxt)
  | Int_t tname ->
      ok (Int_key tname, ctxt)
  | Nat_t tname ->
      ok (Nat_key tname, ctxt)
  | Signature_t tname ->
      ok (Signature_key tname, ctxt)
  | String_t tname ->
      ok (String_key tname, ctxt)
  | Bytes_t tname ->
      ok (Bytes_key tname, ctxt)
  | Mutez_t tname ->
      ok (Mutez_key tname, ctxt)
  | Bool_t tname ->
      ok (Bool_key tname, ctxt)
  | Key_hash_t tname ->
      ok (Key_hash_key tname, ctxt)
  | Key_t tname ->
      ok (Key_key tname, ctxt)
  | Timestamp_t tname ->
      ok (Timestamp_key tname, ctxt)
  | Address_t tname ->
      ok (Address_key tname, ctxt)
  | Chain_id_t tname ->
      ok (Chain_id_key tname, ctxt)
  | Pair_t ((l, al, _), (r, ar, _), pname) ->
      comparable_ty_of_ty ctxt loc l
      >>? fun (lty, ctxt) ->
      comparable_ty_of_ty ctxt loc r
      >|? fun (rty, ctxt) -> (Pair_key ((lty, al), (rty, ar), pname), ctxt)
  | Union_t ((l, al), (r, ar), tname) ->
      comparable_ty_of_ty ctxt loc l
      >>? fun (lty, ctxt) ->
      comparable_ty_of_ty ctxt loc r
      >|? fun (rty, ctxt) -> (Union_key ((lty, al), (rty, ar), tname), ctxt)
  | Option_t (tt, tname) ->
      comparable_ty_of_ty ctxt loc tt
      >|? fun (ty, ctxt) -> (Option_key (ty, tname), ctxt)
  | Lambda_t _
  | List_t _
  | Ticket_t _
  | Set_t _
  | Map_t _
  | Big_map_t _
  | Contract_t _
  | Operation_t _
  | Bls12_381_fr_t _
  | Bls12_381_g1_t _
  | Bls12_381_g2_t _
  | Sapling_state_t _
  | Sapling_transaction_t _ ->
      serialize_ty_for_error ctxt ty
      >>? fun (t, _ctxt) -> error (Comparable_type_expected (loc, t))

let rec unparse_stack :
    type a s.
    context ->
    (a, s) stack_ty ->
    ((Script.expr * Script.annot) list * context) tzresult =
 fun ctxt -> function
  | Bot_t ->
      ok ([], ctxt)
  | Item_t (ty, rest, annot) ->
      unparse_ty ctxt ty
      >>? fun (uty, ctxt) ->
      unparse_stack ctxt rest
      >|? fun (urest, ctxt) ->
      ((strip_locations uty, unparse_var_annot annot) :: urest, ctxt)

let serialize_stack_for_error ctxt stack_ty =
  record_trace Cannot_serialize_error (unparse_stack ctxt stack_ty)

let name_of_ty : type a. a ty -> type_annot option = function
  | Unit_t tname ->
      tname
  | Int_t tname ->
      tname
  | Nat_t tname ->
      tname
  | String_t tname ->
      tname
  | Bytes_t tname ->
      tname
  | Mutez_t tname ->
      tname
  | Bool_t tname ->
      tname
  | Key_hash_t tname ->
      tname
  | Key_t tname ->
      tname
  | Timestamp_t tname ->
      tname
  | Address_t tname ->
      tname
  | Signature_t tname ->
      tname
  | Operation_t tname ->
      tname
  | Chain_id_t tname ->
      tname
  | Never_t tname ->
      tname
  | Contract_t (_, tname) ->
      tname
  | Pair_t (_, _, tname) ->
      tname
  | Union_t (_, _, tname) ->
      tname
  | Lambda_t (_, _, tname) ->
      tname
  | Option_t (_, tname) ->
      tname
  | List_t (_, tname) ->
      tname
  | Ticket_t (_, tname) ->
      tname
  | Set_t (_, tname) ->
      tname
  | Map_t (_, _, tname) ->
      tname
  | Big_map_t (_, _, tname) ->
      tname
  | Bls12_381_g1_t tname ->
      tname
  | Bls12_381_g2_t tname ->
      tname
  | Bls12_381_fr_t tname ->
      tname
  | Sapling_state_t (_, tname) ->
      tname
  | Sapling_transaction_t (_, tname) ->
      tname

let unparse_unit ctxt () = ok (Prim (-1, D_Unit, [], []), ctxt)

let unparse_int ctxt v = ok (Int (-1, Script_int.to_zint v), ctxt)

let unparse_nat ctxt v = ok (Int (-1, Script_int.to_zint v), ctxt)

let unparse_string ctxt s = ok (String (-1, s), ctxt)

let unparse_bytes ctxt s = ok (Bytes (-1, s), ctxt)

let unparse_bool ctxt b =
  ok (Prim (-1, (if b then D_True else D_False), [], []), ctxt)

let unparse_timestamp ctxt mode t =
  match mode with
  | Optimized | Optimized_legacy ->
      ok (Int (-1, Script_timestamp.to_zint t), ctxt)
  | Readable -> (
      Gas.consume ctxt Unparse_costs.timestamp_readable
      >>? fun ctxt ->
      match Script_timestamp.to_notation t with
      | None ->
          ok (Int (-1, Script_timestamp.to_zint t), ctxt)
      | Some s ->
          ok (String (-1, s), ctxt) )

let unparse_address ctxt mode (c, entrypoint) =
  Gas.consume ctxt Unparse_costs.contract
  >>? fun ctxt ->
  ( match entrypoint with
  (* given parse_address, this should not happen *)
  | "" ->
      error Unparsing_invariant_violated
  | _ ->
      ok () )
  >|? fun () ->
  match mode with
  | Optimized | Optimized_legacy ->
      let entrypoint =
        match entrypoint with "default" -> "" | name -> name
      in
      let bytes =
        Data_encoding.Binary.to_bytes_exn
          Data_encoding.(tup2 Contract.encoding Variable.string)
          (c, entrypoint)
      in
      (Bytes (-1, bytes), ctxt)
  | Readable ->
      let notation =
        match entrypoint with
        | "default" ->
            Contract.to_b58check c
        | entrypoint ->
            Contract.to_b58check c ^ "%" ^ entrypoint
      in
      (String (-1, notation), ctxt)

let unparse_contract ctxt mode (_, address) = unparse_address ctxt mode address

let unparse_signature ctxt mode s =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.signature_optimized
      >|? fun ctxt ->
      let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
      (Bytes (-1, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.signature_readable
      >|? fun ctxt -> (String (-1, Signature.to_b58check s), ctxt)

let unparse_mutez ctxt v = ok (Int (-1, Z.of_int64 (Tez.to_mutez v)), ctxt)

let unparse_key ctxt mode k =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.public_key_optimized
      >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k
      in
      (Bytes (-1, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.public_key_readable
      >|? fun ctxt -> (String (-1, Signature.Public_key.to_b58check k), ctxt)

let unparse_key_hash ctxt mode k =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.key_hash_optimized
      >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k
      in
      (Bytes (-1, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.key_hash_readable
      >|? fun ctxt ->
      (String (-1, Signature.Public_key_hash.to_b58check k), ctxt)

let unparse_operation ctxt (op, _big_map_diff) =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Operation.internal_operation_encoding op
  in
  Gas.consume ctxt (Unparse_costs.operation bytes)
  >|? fun ctxt -> (Bytes (-1, bytes), ctxt)

let unparse_chain_id ctxt mode chain_id =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.chain_id_optimized
      >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Chain_id.encoding chain_id
      in
      (Bytes (-1, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.chain_id_readable
      >|? fun ctxt -> (String (-1, Chain_id.to_b58check chain_id), ctxt)

let unparse_bls12_381_g1 ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_g1
  >|? fun ctxt ->
  let bytes = Bls12_381.G1.to_bytes x in
  (Bytes (-1, bytes), ctxt)

let unparse_bls12_381_g2 ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_g2
  >|? fun ctxt ->
  let bytes = Bls12_381.G2.to_bytes x in
  (Bytes (-1, bytes), ctxt)

let unparse_bls12_381_fr ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_fr
  >|? fun ctxt ->
  let bytes = Bls12_381.Fr.to_bytes x in
  (Bytes (-1, bytes), ctxt)

(* -- Unparsing data of complex types -- *)

type ('ty, 'depth) comb_witness =
  | Comb_Pair : ('t, 'd) comb_witness -> (_ * 't, unit -> 'd) comb_witness
  | Comb_Any : (_, _) comb_witness

let unparse_pair (type r) unparse_l unparse_r ctxt mode
    (r_comb_witness : (r, unit -> unit -> _) comb_witness) (l, (r : r)) =
  unparse_l ctxt l
  >>=? fun (l, ctxt) ->
  unparse_r ctxt r
  >|=? fun (r, ctxt) ->
  (* Fold combs.
     For combs, three notations are supported:
     - a) [Pair x1 (Pair x2 ... (Pair xn-1 xn) ...)],
     - b) [Pair x1 x2 ... xn-1 xn], and
     - c) [{x1; x2; ...; xn-1; xn}].
     In readable mode, we always use b),
     in optimized mode we use the shortest to serialize:
     - for n=2, [Pair x1 x2],
     - for n=3, [Pair x1 (Pair x2 x3)],
     - for n>=4, [{x1; x2; ...; xn}].
     *)
  let res =
    match (mode, r_comb_witness, r) with
    | (Optimized, Comb_Pair _, Micheline.Seq (_, r)) ->
        (* Optimized case n > 4 *)
        Micheline.Seq (-1, l :: r)
    | ( Optimized,
        Comb_Pair (Comb_Pair _),
        Prim (_, D_Pair, [x2; Prim (_, D_Pair, [x3; x4], [])], []) ) ->
        (* Optimized case n = 4 *)
        Micheline.Seq (-1, [l; x2; x3; x4])
    | (Readable, Comb_Pair _, Prim (_, D_Pair, xs, [])) ->
        (* Readable case n > 2 *)
        Prim (-1, D_Pair, l :: xs, [])
    | _ ->
        (* The remaining cases are:
            - Optimized n = 2,
            - Optimized n = 3, and
            - Readable n = 2,
            - Optimized_legacy, any n *)
        Prim (-1, D_Pair, [l; r], [])
  in
  (res, ctxt)

let unparse_union unparse_l unparse_r ctxt = function
  | L l ->
      unparse_l ctxt l >|=? fun (l, ctxt) -> (Prim (-1, D_Left, [l], []), ctxt)
  | R r ->
      unparse_r ctxt r >|=? fun (r, ctxt) -> (Prim (-1, D_Right, [r], []), ctxt)

let unparse_option unparse_v ctxt = function
  | Some v ->
      unparse_v ctxt v >|=? fun (v, ctxt) -> (Prim (-1, D_Some, [v], []), ctxt)
  | None ->
      return (Prim (-1, D_None, [], []), ctxt)

(* -- Unparsing data of comparable types -- *)

let comparable_comb_witness2 :
    type t. t comparable_ty -> (t, unit -> unit -> unit) comb_witness =
  function
  | Pair_key (_, (Pair_key _, _), _) ->
      Comb_Pair (Comb_Pair Comb_Any)
  | Pair_key _ ->
      Comb_Pair Comb_Any
  | _ ->
      Comb_Any

let rec unparse_comparable_data :
    type a.
    context ->
    unparsing_mode ->
    a comparable_ty ->
    a ->
    (Script.node * context) tzresult Lwt.t =
 fun ctxt mode ty a ->
  (* No need for stack_depth here. Unlike [unparse_data],
     [unparse_comparable_data] doesn't call [unparse_code].
     The stack depth is bounded by the type depth, currently bounded
     by 1000 (michelson_maximum_type_size). *)
  Gas.consume ctxt Unparse_costs.unparse_data_cycle
  (* We could have a smaller cost but let's keep it consistent with
     [unparse_data] for now. *)
  >>?= fun ctxt ->
  match (ty, a) with
  | (Unit_key _, v) ->
      Lwt.return @@ unparse_unit ctxt v
  | (Int_key _, v) ->
      Lwt.return @@ unparse_int ctxt v
  | (Nat_key _, v) ->
      Lwt.return @@ unparse_nat ctxt v
  | (String_key _, s) ->
      Lwt.return @@ unparse_string ctxt s
  | (Bytes_key _, s) ->
      Lwt.return @@ unparse_bytes ctxt s
  | (Bool_key _, b) ->
      Lwt.return @@ unparse_bool ctxt b
  | (Timestamp_key _, t) ->
      Lwt.return @@ unparse_timestamp ctxt mode t
  | (Address_key _, address) ->
      Lwt.return @@ unparse_address ctxt mode address
  | (Signature_key _, s) ->
      Lwt.return @@ unparse_signature ctxt mode s
  | (Mutez_key _, v) ->
      Lwt.return @@ unparse_mutez ctxt v
  | (Key_key _, k) ->
      Lwt.return @@ unparse_key ctxt mode k
  | (Key_hash_key _, k) ->
      Lwt.return @@ unparse_key_hash ctxt mode k
  | (Chain_id_key _, chain_id) ->
      Lwt.return @@ unparse_chain_id ctxt mode chain_id
  | (Pair_key ((tl, _), (tr, _), _), pair) ->
      let r_witness = comparable_comb_witness2 tr in
      let unparse_l ctxt v = unparse_comparable_data ctxt mode tl v in
      let unparse_r ctxt v = unparse_comparable_data ctxt mode tr v in
      unparse_pair unparse_l unparse_r ctxt mode r_witness pair
  | (Union_key ((tl, _), (tr, _), _), v) ->
      let unparse_l ctxt v = unparse_comparable_data ctxt mode tl v in
      let unparse_r ctxt v = unparse_comparable_data ctxt mode tr v in
      unparse_union unparse_l unparse_r ctxt v
  | (Option_key (t, _), v) ->
      let unparse_v ctxt v = unparse_comparable_data ctxt mode t v in
      unparse_option unparse_v ctxt v
  | (Never_key _, _) ->
      .

let pack_node unparsed ctxt =
  Gas.consume ctxt (Script.strip_locations_cost unparsed)
  >>? fun ctxt ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      expr_encoding
      (Micheline.strip_locations unparsed)
  in
  Gas.consume ctxt (Script.serialized_cost bytes)
  >>? fun ctxt ->
  let bytes = Bytes.cat (Bytes.of_string "\005") bytes in
  Gas.consume ctxt (Script.serialized_cost bytes) >|? fun ctxt -> (bytes, ctxt)

let pack_comparable_data ctxt typ data ~mode =
  unparse_comparable_data ctxt mode typ data
  >>=? fun (unparsed, ctxt) -> Lwt.return @@ pack_node unparsed ctxt

let hash_bytes ctxt bytes =
  Gas.consume ctxt (Michelson_v1_gas.Cost_of.Interpreter.blake2b bytes)
  >|? fun ctxt -> (Script_expr_hash.(hash_bytes [bytes]), ctxt)

let hash_comparable_data ctxt typ data =
  pack_comparable_data ctxt typ data ~mode:Optimized_legacy
  >>=? fun (bytes, ctxt) -> Lwt.return @@ hash_bytes ctxt bytes

(* ---- Tickets ------------------------------------------------------------ *)

(*
   All comparable types are dupable, this function exists only to not forget
   checking this property when adding new types.
*)
let check_dupable_comparable_ty : type a. a comparable_ty -> unit = function
  | Unit_key _
  | Never_key _
  | Int_key _
  | Nat_key _
  | Signature_key _
  | String_key _
  | Bytes_key _
  | Mutez_key _
  | Bool_key _
  | Key_hash_key _
  | Key_key _
  | Timestamp_key _
  | Chain_id_key _
  | Address_key _
  | Pair_key _
  | Union_key _
  | Option_key _ ->
      ()

let rec check_dupable_ty :
    type a. context -> location -> a ty -> context tzresult =
 fun ctxt loc ty ->
  Gas.consume ctxt Typecheck_costs.check_dupable_cycle
  >>? fun ctxt ->
  match ty with
  | Unit_t _ ->
      ok ctxt
  | Int_t _ ->
      ok ctxt
  | Nat_t _ ->
      ok ctxt
  | Signature_t _ ->
      ok ctxt
  | String_t _ ->
      ok ctxt
  | Bytes_t _ ->
      ok ctxt
  | Mutez_t _ ->
      ok ctxt
  | Key_hash_t _ ->
      ok ctxt
  | Key_t _ ->
      ok ctxt
  | Timestamp_t _ ->
      ok ctxt
  | Address_t _ ->
      ok ctxt
  | Bool_t _ ->
      ok ctxt
  | Contract_t (_, _) ->
      ok ctxt
  | Operation_t _ ->
      ok ctxt
  | Chain_id_t _ ->
      ok ctxt
  | Never_t _ ->
      ok ctxt
  | Bls12_381_g1_t _ ->
      ok ctxt
  | Bls12_381_g2_t _ ->
      ok ctxt
  | Bls12_381_fr_t _ ->
      ok ctxt
  | Sapling_state_t _ ->
      ok ctxt
  | Sapling_transaction_t _ ->
      ok ctxt
  | Ticket_t _ ->
      error (Unexpected_ticket loc)
  | Pair_t ((ty_a, _, _), (ty_b, _, _), _) ->
      check_dupable_ty ctxt loc ty_a
      >>? fun ctxt -> check_dupable_ty ctxt loc ty_b
  | Union_t ((ty_a, _), (ty_b, _), _) ->
      check_dupable_ty ctxt loc ty_a
      >>? fun ctxt -> check_dupable_ty ctxt loc ty_b
  | Lambda_t (_, _, _) ->
      (*
        Lambda are dupable as long as:
          - they don't contain non-dupable values, e.g. in `PUSH`
            (mosty non-dupable values should probably be considered forged)
          - they are not the result of a partial application on a non-dupable
            value. `APPLY` rejects non-packable types (because of `PUSH`).
            Hence non-dupable should imply non-packable.
      *)
      ok ctxt
  | Option_t (ty, _) ->
      check_dupable_ty ctxt loc ty
  | List_t (ty, _) ->
      check_dupable_ty ctxt loc ty
  | Set_t (key_ty, _) ->
      let () = check_dupable_comparable_ty key_ty in
      ok ctxt
  | Map_t (key_ty, val_ty, _) ->
      let () = check_dupable_comparable_ty key_ty in
      check_dupable_ty ctxt loc val_ty
  | Big_map_t (key_ty, val_ty, _) ->
      let () = check_dupable_comparable_ty key_ty in
      check_dupable_ty ctxt loc val_ty

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

let record_inconsistent ctxt ta tb =
  record_trace_eval (fun () ->
      serialize_ty_for_error ctxt ta
      >>? fun (ta, ctxt) ->
      serialize_ty_for_error ctxt tb
      >|? fun (tb, _ctxt) -> Inconsistent_types (ta, tb))

let record_inconsistent_type_annotations ctxt loc ta tb =
  record_trace_eval (fun () ->
      serialize_ty_for_error ctxt ta
      >>? fun (ta, ctxt) ->
      serialize_ty_for_error ctxt tb
      >|? fun (tb, _ctxt) -> Inconsistent_type_annotations (loc, ta, tb))

let rec merge_comparable_types :
    type ta tb.
    legacy:bool ->
    context ->
    ta comparable_ty ->
    tb comparable_ty ->
    ((ta comparable_ty, tb comparable_ty) eq * ta comparable_ty * context)
    tzresult =
 fun ~legacy ctxt ta tb ->
  Gas.consume ctxt Typecheck_costs.merge_cycle
  >>? fun ctxt ->
  match (ta, tb) with
  | (Unit_key annot_a, Unit_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot ->
      ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
        (Unit_key annot : ta comparable_ty),
        ctxt )
  | (Never_key annot_a, Never_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Never_key annot, ctxt)
  | (Int_key annot_a, Int_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Int_key annot, ctxt)
  | (Nat_key annot_a, Nat_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Nat_key annot, ctxt)
  | (Signature_key annot_a, Signature_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Signature_key annot, ctxt)
  | (String_key annot_a, String_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, String_key annot, ctxt)
  | (Bytes_key annot_a, Bytes_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Bytes_key annot, ctxt)
  | (Mutez_key annot_a, Mutez_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Mutez_key annot, ctxt)
  | (Bool_key annot_a, Bool_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Bool_key annot, ctxt)
  | (Key_hash_key annot_a, Key_hash_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Key_hash_key annot, ctxt)
  | (Key_key annot_a, Key_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Key_key annot, ctxt)
  | (Timestamp_key annot_a, Timestamp_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Timestamp_key annot, ctxt)
  | (Chain_id_key annot_a, Chain_id_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Chain_id_key annot, ctxt)
  | (Address_key annot_a, Address_key annot_b) ->
      merge_type_annot ~legacy annot_a annot_b
      >|? fun annot -> (Eq, Address_key annot, ctxt)
  | ( Pair_key ((left_a, annot_left_a), (right_a, annot_right_a), annot_a),
      Pair_key ((left_b, annot_left_b), (right_b, annot_right_b), annot_b) ) ->
      merge_type_annot ~legacy annot_a annot_b
      >>? fun annot ->
      merge_field_annot ~legacy annot_left_a annot_left_b
      >>? fun annot_left ->
      merge_field_annot ~legacy annot_right_a annot_right_b
      >>? fun annot_right ->
      merge_comparable_types ~legacy ctxt left_a left_b
      >>? fun (Eq, left, ctxt) ->
      merge_comparable_types ~legacy ctxt right_a right_b
      >|? fun (Eq, right, ctxt) ->
      ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
        Pair_key ((left, annot_left), (right, annot_right), annot),
        ctxt )
  | ( Union_key ((left_a, annot_left_a), (right_a, annot_right_a), annot_a),
      Union_key ((left_b, annot_left_b), (right_b, annot_right_b), annot_b) )
    ->
      merge_type_annot ~legacy annot_a annot_b
      >>? fun annot ->
      merge_field_annot ~legacy annot_left_a annot_left_b
      >>? fun annot_left ->
      merge_field_annot ~legacy annot_right_a annot_right_b
      >>? fun annot_right ->
      merge_comparable_types ~legacy ctxt left_a left_b
      >>? fun (Eq, left, ctxt) ->
      merge_comparable_types ~legacy ctxt right_a right_b
      >|? fun (Eq, right, ctxt) ->
      ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
        Union_key ((left, annot_left), (right, annot_right), annot),
        ctxt )
  | (Option_key (ta, annot_a), Option_key (tb, annot_b)) ->
      merge_type_annot ~legacy annot_a annot_b
      >>? fun annot ->
      merge_comparable_types ~legacy ctxt ta tb
      >|? fun (Eq, t, ctxt) ->
      ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
        Option_key (t, annot),
        ctxt )
  | (_, _) ->
      serialize_ty_for_error ctxt (ty_of_comparable_ty ta)
      >>? fun (ta, ctxt) ->
      serialize_ty_for_error ctxt (ty_of_comparable_ty tb)
      >>? fun (tb, _ctxt) -> error (Inconsistent_types (ta, tb))

let comparable_ty_eq :
    type ta tb.
    context ->
    ta comparable_ty ->
    tb comparable_ty ->
    ((ta comparable_ty, tb comparable_ty) eq * context) tzresult =
 fun ctxt ta tb ->
  merge_comparable_types ~legacy:true ctxt ta tb
  >|? fun (eq, _ty, ctxt) -> (eq, ctxt)

let merge_memo_sizes ms1 ms2 =
  if Sapling.Memo_size.equal ms1 ms2 then ok ms1
  else error (Inconsistent_memo_sizes (ms1, ms2))

let merge_types :
    type a b.
    legacy:bool ->
    context ->
    Script.location ->
    a ty ->
    b ty ->
    ((a ty, b ty) eq * a ty * context) tzresult =
 fun ~legacy ctxt loc ty1 ty2 ->
  let merge_type_annot tn1 tn2 =
    merge_type_annot ~legacy tn1 tn2
    |> record_inconsistent_type_annotations ctxt loc ty1 ty2
  in
  let rec help :
      type ta tb.
      context ->
      ta ty ->
      tb ty ->
      ((ta ty, tb ty) eq * ta ty * context) tzresult =
   fun ctxt ty1 ty2 -> help0 ctxt ty1 ty2 |> record_inconsistent ctxt ty1 ty2
  and help0 :
      type ta tb.
      context ->
      ta ty ->
      tb ty ->
      ((ta ty, tb ty) eq * ta ty * context) tzresult =
   fun ctxt ty1 ty2 ->
    Gas.consume ctxt Typecheck_costs.merge_cycle
    >>? fun ctxt ->
    match (ty1, ty2) with
    | (Unit_t tn1, Unit_t tn2) ->
        merge_type_annot tn1 tn2
        >|? fun tname ->
        ((Eq : (ta ty, tb ty) eq), (Unit_t tname : ta ty), ctxt)
    | (Int_t tn1, Int_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Int_t tname, ctxt)
    | (Nat_t tn1, Nat_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Nat_t tname, ctxt)
    | (Key_t tn1, Key_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Key_t tname, ctxt)
    | (Key_hash_t tn1, Key_hash_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Key_hash_t tname, ctxt)
    | (String_t tn1, String_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, String_t tname, ctxt)
    | (Bytes_t tn1, Bytes_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Bytes_t tname, ctxt)
    | (Signature_t tn1, Signature_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Signature_t tname, ctxt)
    | (Mutez_t tn1, Mutez_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Mutez_t tname, ctxt)
    | (Timestamp_t tn1, Timestamp_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Timestamp_t tname, ctxt)
    | (Address_t tn1, Address_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Address_t tname, ctxt)
    | (Bool_t tn1, Bool_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Bool_t tname, ctxt)
    | (Chain_id_t tn1, Chain_id_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Chain_id_t tname, ctxt)
    | (Never_t tn1, Never_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Never_t tname, ctxt)
    | (Operation_t tn1, Operation_t tn2) ->
        merge_type_annot tn1 tn2 >|? fun tname -> (Eq, Operation_t tname, ctxt)
    | (Bls12_381_g1_t tn1, Bls12_381_g1_t tn2) ->
        merge_type_annot tn1 tn2
        >|? fun tname -> (Eq, Bls12_381_g1_t tname, ctxt)
    | (Bls12_381_g2_t tn1, Bls12_381_g2_t tn2) ->
        merge_type_annot tn1 tn2
        >|? fun tname -> (Eq, Bls12_381_g2_t tname, ctxt)
    | (Bls12_381_fr_t tn1, Bls12_381_fr_t tn2) ->
        merge_type_annot tn1 tn2
        >|? fun tname -> (Eq, Bls12_381_fr_t tname, ctxt)
    | (Map_t (tal, tar, tn1), Map_t (tbl, tbr, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tar tbr
        >>? fun (Eq, value, ctxt) ->
        merge_comparable_types ~legacy ctxt tal tbl
        >|? fun (Eq, tk, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Map_t (tk, value, tname), ctxt)
    | (Big_map_t (tal, tar, tn1), Big_map_t (tbl, tbr, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tar tbr
        >>? fun (Eq, value, ctxt) ->
        merge_comparable_types ~legacy ctxt tal tbl
        >|? fun (Eq, tk, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Big_map_t (tk, value, tname), ctxt)
    | (Set_t (ea, tn1), Set_t (eb, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_comparable_types ~legacy ctxt ea eb
        >|? fun (Eq, e, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Set_t (e, tname), ctxt)
    | (Ticket_t (ea, tn1), Ticket_t (eb, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_comparable_types ~legacy ctxt ea eb
        >|? fun (Eq, e, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Ticket_t (e, tname), ctxt)
    | ( Pair_t ((tal, l_field1, l_var1), (tar, r_field1, r_var1), tn1),
        Pair_t ((tbl, l_field2, l_var2), (tbr, r_field2, r_var2), tn2) ) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_field_annot ~legacy l_field1 l_field2
        >>? fun l_field ->
        merge_field_annot ~legacy r_field1 r_field2
        >>? fun r_field ->
        let l_var = merge_var_annot l_var1 l_var2 in
        let r_var = merge_var_annot r_var1 r_var2 in
        help ctxt tal tbl
        >>? fun (Eq, left_ty, ctxt) ->
        help ctxt tar tbr
        >|? fun (Eq, right_ty, ctxt) ->
        ( (Eq : (ta ty, tb ty) eq),
          Pair_t ((left_ty, l_field, l_var), (right_ty, r_field, r_var), tname),
          ctxt )
    | ( Union_t ((tal, tal_annot), (tar, tar_annot), tn1),
        Union_t ((tbl, tbl_annot), (tbr, tbr_annot), tn2) ) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_field_annot ~legacy tal_annot tbl_annot
        >>? fun left_annot ->
        merge_field_annot ~legacy tar_annot tbr_annot
        >>? fun right_annot ->
        help ctxt tal tbl
        >>? fun (Eq, left_ty, ctxt) ->
        help ctxt tar tbr
        >|? fun (Eq, right_ty, ctxt) ->
        ( (Eq : (ta ty, tb ty) eq),
          Union_t ((left_ty, left_annot), (right_ty, right_annot), tname),
          ctxt )
    | (Lambda_t (tal, tar, tn1), Lambda_t (tbl, tbr, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tal tbl
        >>? fun (Eq, left_ty, ctxt) ->
        help ctxt tar tbr
        >|? fun (Eq, right_ty, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Lambda_t (left_ty, right_ty, tname), ctxt)
    | (Contract_t (tal, tn1), Contract_t (tbl, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tal tbl
        >|? fun (Eq, arg_ty, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Contract_t (arg_ty, tname), ctxt)
    | (Option_t (tva, tn1), Option_t (tvb, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tva tvb
        >|? fun (Eq, ty, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), Option_t (ty, tname), ctxt)
    | (List_t (tva, tn1), List_t (tvb, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        help ctxt tva tvb
        >|? fun (Eq, ty, ctxt) ->
        ((Eq : (ta ty, tb ty) eq), List_t (ty, tname), ctxt)
    | (Sapling_state_t (ms1, tn1), Sapling_state_t (ms2, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_memo_sizes ms1 ms2
        >|? fun ms -> (Eq, Sapling_state_t (ms, tname), ctxt)
    | (Sapling_transaction_t (ms1, tn1), Sapling_transaction_t (ms2, tn2)) ->
        merge_type_annot tn1 tn2
        >>? fun tname ->
        merge_memo_sizes ms1 ms2
        >|? fun ms -> (Eq, Sapling_transaction_t (ms, tname), ctxt)
    | (_, _) ->
        serialize_ty_for_error ctxt ty1
        >>? fun (ty1, ctxt) ->
        serialize_ty_for_error ctxt ty2
        >>? fun (ty2, _ctxt) -> error (Inconsistent_types (ty1, ty2))
  in
  help ctxt ty1 ty2
 [@@coq_axiom_with_reason "non-top-level mutual recursion"]

let ty_eq :
    type ta tb.
    context ->
    Script.location ->
    ta ty ->
    tb ty ->
    ((ta ty, tb ty) eq * context) tzresult =
 fun ctxt loc ta tb ->
  merge_types ~legacy:true ctxt loc ta tb >|? fun (eq, _ty, ctxt) -> (eq, ctxt)

let merge_stacks :
    type ta tb ts tu.
    legacy:bool ->
    Script.location ->
    context ->
    int ->
    (ta, ts) stack_ty ->
    (tb, tu) stack_ty ->
    (((ta, ts) stack_ty, (tb, tu) stack_ty) eq * (ta, ts) stack_ty * context)
    tzresult =
 fun ~legacy loc ->
  let rec help :
      type ta tb ts tu.
      context ->
      int ->
      (ta, ts) stack_ty ->
      (tb, tu) stack_ty ->
      (((ta, ts) stack_ty, (tb, tu) stack_ty) eq * (ta, ts) stack_ty * context)
      tzresult =
   fun ctxt lvl stack1 stack2 ->
    match (stack1, stack2) with
    | (Bot_t, Bot_t) ->
        ok (Eq, Bot_t, ctxt)
    | (Item_t (ty1, rest1, annot1), Item_t (ty2, rest2, annot2)) ->
        merge_types ~legacy ctxt loc ty1 ty2
        |> record_trace (Bad_stack_item lvl)
        >>? fun (Eq, ty, ctxt) ->
        help ctxt (lvl + 1) rest1 rest2
        >|? fun (Eq, rest, ctxt) ->
        let annot = merge_var_annot annot1 annot2 in
        ( (Eq : ((ta, ts) stack_ty, (tb, tu) stack_ty) eq),
          Item_t (ty, rest, annot),
          ctxt )
    | (_, _) ->
        error Bad_stack_length
  in
  help

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
    'r 'f. ('a, 's, 'r, 'f) descr -> ('b, 'u, 'r, 'f) descr ->
    ('c, 'v, 'r, 'f) descr;
}
[@@unboxed]

let merge_branches :
    type a s b u c v.
    legacy:bool ->
    context ->
    int ->
    (a, s) judgement ->
    (b, u) judgement ->
    (a, s, b, u, c, v) branch ->
    ((c, v) judgement * context) tzresult =
 fun ~legacy ctxt loc btr bfr {branch} ->
  match (btr, bfr) with
  | (Typed ({aft = aftbt; _} as dbt), Typed ({aft = aftbf; _} as dbf)) ->
      let unmatched_branches () =
        serialize_stack_for_error ctxt aftbt
        >>? fun (aftbt, ctxt) ->
        serialize_stack_for_error ctxt aftbf
        >|? fun (aftbf, _ctxt) -> Unmatched_branches (loc, aftbt, aftbf)
      in
      record_trace_eval
        unmatched_branches
        ( merge_stacks ~legacy loc ctxt 1 aftbt aftbf
        >|? fun (Eq, merged_stack, ctxt) ->
        ( Typed
            (branch {dbt with aft = merged_stack} {dbf with aft = merged_stack}),
          ctxt ) )
  | (Failed {descr = descrt}, Failed {descr = descrf}) ->
      let descr ret = branch (descrt ret) (descrf ret) in
      ok (Failed {descr}, ctxt)
  | (Typed dbt, Failed {descr = descrf}) ->
      ok (Typed (branch dbt (descrf dbt.aft)), ctxt)
  | (Failed {descr = descrt}, Typed dbf) ->
      ok (Typed (branch (descrt dbf.aft) dbf), ctxt)

let parse_memo_size (n : (location, _) Micheline.node) :
    Sapling.Memo_size.t tzresult =
  match n with
  | Int (_, z) -> (
    match Sapling.Memo_size.parse_z z with
    | Ok _ as ok_memo_size ->
        ok_memo_size
    | Error msg ->
        error @@ Invalid_syntactic_constant (location n, strip_locations n, msg)
    )
  | _ ->
      error @@ Invalid_kind (location n, [Int_kind], kind n)

let rec parse_comparable_ty :
    stack_depth:int ->
    context ->
    Script.node ->
    (ex_comparable_ty * context) tzresult =
 fun ~stack_depth ctxt ty ->
  Gas.consume ctxt Typecheck_costs.parse_type_cycle
  >>? fun ctxt ->
  if Compare.Int.(stack_depth > 10000) then
    error Typechecking_too_many_recursive_calls
  else
    match ty with
    | Prim (loc, T_unit, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Unit_key tname), ctxt)
    | Prim (loc, T_never, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Never_key tname), ctxt)
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Int_key tname), ctxt)
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Nat_key tname), ctxt)
    | Prim (loc, T_signature, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Signature_key tname), ctxt)
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (String_key tname), ctxt)
    | Prim (loc, T_bytes, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Bytes_key tname), ctxt)
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Mutez_key tname), ctxt)
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Bool_key tname), ctxt)
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Key_hash_key tname), ctxt)
    | Prim (loc, T_key, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Key_key tname), ctxt)
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Timestamp_key tname), ctxt)
    | Prim (loc, T_chain_id, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Chain_id_key tname), ctxt)
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot
        >|? fun tname -> (Ex_comparable_ty (Address_key tname), ctxt)
    | Prim
        ( loc,
          ( ( T_unit
            | T_never
            | T_int
            | T_nat
            | T_string
            | T_bytes
            | T_mutez
            | T_bool
            | T_key_hash
            | T_timestamp
            | T_address
            | T_chain_id
            | T_signature
            | T_key ) as prim ),
          l,
          _ ) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, T_pair, left :: right, annot) ->
        parse_type_annot loc annot
        >>? fun pname ->
        extract_field_annot left
        >>? fun (left, left_annot) ->
        ( match right with
        | [right] ->
            extract_field_annot right
        | right ->
            (* Unfold [pair t1 ... tn] as [pair t1 (... (pair tn-1 tn))] *)
            ok (Prim (loc, T_pair, right, []), None) )
        >>? fun (right, right_annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt right
        >>? fun (Ex_comparable_ty right, ctxt) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt left
        >|? fun (Ex_comparable_ty left, ctxt) ->
        ( Ex_comparable_ty
            (Pair_key ((left, left_annot), (right, right_annot), pname)),
          ctxt )
    | Prim (loc, T_or, [left; right], annot) ->
        parse_type_annot loc annot
        >>? fun pname ->
        extract_field_annot left
        >>? fun (left, left_annot) ->
        extract_field_annot right
        >>? fun (right, right_annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt right
        >>? fun (Ex_comparable_ty right, ctxt) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt left
        >|? fun (Ex_comparable_ty left, ctxt) ->
        ( Ex_comparable_ty
            (Union_key ((left, left_annot), (right, right_annot), pname)),
          ctxt )
    | Prim (loc, ((T_pair | T_or) as prim), l, _) ->
        error (Invalid_arity (loc, prim, 2, List.length l))
    | Prim (loc, T_option, [t], annot) ->
        parse_type_annot loc annot
        >>? fun tname ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt t
        >|? fun (Ex_comparable_ty t, ctxt) ->
        (Ex_comparable_ty (Option_key (t, tname)), ctxt)
    | Prim (loc, T_option, l, _) ->
        error (Invalid_arity (loc, T_option, 1, List.length l))
    | Prim
        ( loc,
          (T_set | T_map | T_list | T_lambda | T_contract | T_operation),
          _,
          _ ) ->
        error (Comparable_type_expected (loc, Micheline.strip_locations ty))
    | expr ->
        error
        @@ unexpected
             expr
             []
             Type_namespace
             [ T_unit;
               T_never;
               T_int;
               T_nat;
               T_string;
               T_bytes;
               T_mutez;
               T_bool;
               T_key_hash;
               T_timestamp;
               T_address;
               T_pair;
               T_or;
               T_option;
               T_chain_id;
               T_signature;
               T_key ]

let rec parse_packable_ty :
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
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:legacy
    ~allow_ticket:false

and parse_parameter_ty :
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
    ~allow_operation:false
    ~allow_contract:true
    ~allow_ticket:true

and parse_normal_storage_ty :
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
    ~allow_operation:false
    ~allow_contract:legacy
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

and parse_ty :
    context ->
    stack_depth:int ->
    legacy:bool ->
    allow_lazy_storage:bool ->
    allow_operation:bool ->
    allow_contract:bool ->
    allow_ticket:bool ->
    Script.node ->
    (ex_ty * context) tzresult =
 fun ctxt
     ~stack_depth
     ~legacy
     ~allow_lazy_storage
     ~allow_operation
     ~allow_contract
     ~allow_ticket
     node ->
  Gas.consume ctxt Typecheck_costs.parse_type_cycle
  >>? fun ctxt ->
  if Compare.Int.(stack_depth > 10000) then
    error Typechecking_too_many_recursive_calls
  else
    match node with
    | Prim (loc, T_unit, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Unit_t ty_name), ctxt)
    | Prim (loc, T_int, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Int_t ty_name), ctxt)
    | Prim (loc, T_nat, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Nat_t ty_name), ctxt)
    | Prim (loc, T_string, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (String_t ty_name), ctxt)
    | Prim (loc, T_bytes, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Bytes_t ty_name), ctxt)
    | Prim (loc, T_mutez, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Mutez_t ty_name), ctxt)
    | Prim (loc, T_bool, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Bool_t ty_name), ctxt)
    | Prim (loc, T_key, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Key_t ty_name), ctxt)
    | Prim (loc, T_key_hash, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Key_hash_t ty_name), ctxt)
    | Prim (loc, T_timestamp, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Timestamp_t ty_name), ctxt)
    | Prim (loc, T_address, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Address_t ty_name), ctxt)
    | Prim (loc, T_signature, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Signature_t ty_name), ctxt)
    | Prim (loc, T_operation, [], annot) ->
        if allow_operation then
          parse_type_annot loc annot
          >>? fun ty_name -> ok (Ex_ty (Operation_t ty_name), ctxt)
        else error (Unexpected_operation loc)
    | Prim (loc, T_chain_id, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Chain_id_t ty_name), ctxt)
    | Prim (loc, T_never, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Never_t ty_name), ctxt)
    | Prim (loc, T_bls12_381_g1, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Bls12_381_g1_t ty_name), ctxt)
    | Prim (loc, T_bls12_381_g2, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Bls12_381_g2_t ty_name), ctxt)
    | Prim (loc, T_bls12_381_fr, [], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Bls12_381_fr_t ty_name), ctxt)
    | Prim (loc, T_contract, [utl], annot) ->
        if allow_contract then
          parse_parameter_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy utl
          >>? fun (Ex_ty tl, ctxt) ->
          parse_type_annot loc annot
          >>? fun ty_name -> ok (Ex_ty (Contract_t (tl, ty_name)), ctxt)
        else error (Unexpected_contract loc)
    | Prim (loc, T_pair, utl :: utr, annot) ->
        extract_field_annot utl
        >>? fun (utl, left_field) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          utl
        >>? fun (Ex_ty tl, ctxt) ->
        ( match utr with
        | [utr] ->
            extract_field_annot utr
        | utr ->
            (* Unfold [pair t1 ... tn] as [pair t1 (... (pair tn-1 tn))] *)
            ok (Prim (loc, T_pair, utr, []), None) )
        >>? fun (utr, right_field) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          utr
        >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name ->
        ok
          ( Ex_ty
              (Pair_t ((tl, left_field, None), (tr, right_field, None), ty_name)),
            ctxt )
    | Prim (loc, T_or, [utl; utr], annot) ->
        extract_field_annot utl
        >>? fun (utl, left_constr) ->
        extract_field_annot utr
        >>? fun (utr, right_constr) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          utl
        >>? fun (Ex_ty tl, ctxt) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          utr
        >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name ->
        ok
          ( Ex_ty (Union_t ((tl, left_constr), (tr, right_constr), ty_name)),
            ctxt )
    | Prim (loc, T_lambda, [uta; utr], annot) ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy uta
        >>? fun (Ex_ty ta, ctxt) ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy utr
        >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Lambda_t (ta, tr, ty_name)), ctxt)
    | Prim (loc, T_option, [ut], annot) ->
        ( if legacy then
          (* legacy semantics with (broken) field annotations *)
          extract_field_annot ut
          >>? fun (ut, _some_constr) ->
          parse_composed_type_annot loc annot
          >>? fun (ty_name, _none_constr, _) -> ok (ut, ty_name)
        else parse_type_annot loc annot >>? fun ty_name -> ok (ut, ty_name) )
        >>? fun (ut, ty_name) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ut
        >>? fun (Ex_ty t, ctxt) -> ok (Ex_ty (Option_t (t, ty_name)), ctxt)
    | Prim (loc, T_list, [ut], annot) ->
        parse_ty
          ctxt
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ~allow_lazy_storage
          ~allow_operation
          ~allow_contract
          ~allow_ticket
          ut
        >>? fun (Ex_ty t, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (List_t (t, ty_name)), ctxt)
    | Prim (loc, T_ticket, [ut], annot) ->
        if allow_ticket then
          parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
          >>? fun (Ex_comparable_ty t, ctxt) ->
          parse_type_annot loc annot
          >>? fun ty_name -> ok (Ex_ty (Ticket_t (t, ty_name)), ctxt)
        else error (Unexpected_ticket loc)
    | Prim (loc, T_set, [ut], annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
        >>? fun (Ex_comparable_ty t, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Set_t (t, ty_name)), ctxt)
    | Prim (loc, T_map, [uta; utr], annot) ->
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
          utr
        >>? fun (Ex_ty tr, ctxt) ->
        parse_type_annot loc annot
        >>? fun ty_name -> ok (Ex_ty (Map_t (ta, tr, ty_name)), ctxt)
    | Prim (loc, T_sapling_transaction, [memo_size], annot) ->
        parse_type_annot loc annot
        >>? fun ty_name ->
        parse_memo_size memo_size
        >|? fun memo_size ->
        (Ex_ty (Sapling_transaction_t (memo_size, ty_name)), ctxt)
    (*
    /!\ When adding new lazy storage kinds, be careful to use
    [when allow_lazy_storage] /!\
    Lazy storage should not be packable to avoid stealing a lazy storage
    from another contract with `PUSH t id` or `UNPACK`.
  *)
    | Prim (loc, T_big_map, args, annot) when allow_lazy_storage ->
        (parse_big_map_ty [@tailcall]) ctxt ~stack_depth ~legacy loc args annot
    | Prim (loc, T_sapling_state, [memo_size], annot) when allow_lazy_storage
      ->
        parse_type_annot loc annot
        >>? fun ty_name ->
        parse_memo_size memo_size
        >|? fun memo_size ->
        (Ex_ty (Sapling_state_t (memo_size, ty_name)), ctxt)
    | Prim (loc, (T_big_map | T_sapling_state), _, _) ->
        error (Unexpected_lazy_storage loc)
    | Prim
        ( loc,
          ( ( T_unit
            | T_signature
            | T_int
            | T_nat
            | T_string
            | T_bytes
            | T_mutez
            | T_bool
            | T_key
            | T_key_hash
            | T_timestamp
            | T_address
            | T_chain_id
            | T_operation
            | T_never ) as prim ),
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
             [ T_pair;
               T_or;
               T_set;
               T_map;
               T_list;
               T_option;
               T_lambda;
               T_unit;
               T_signature;
               T_contract;
               T_int;
               T_nat;
               T_operation;
               T_string;
               T_bytes;
               T_mutez;
               T_bool;
               T_key;
               T_key_hash;
               T_timestamp;
               T_chain_id;
               T_never;
               T_bls12_381_g1;
               T_bls12_381_g2;
               T_bls12_381_fr;
               T_ticket ]

and parse_big_map_ty ctxt ~stack_depth ~legacy big_map_loc args map_annot =
  Gas.consume ctxt Typecheck_costs.parse_type_cycle
  >>? fun ctxt ->
  match args with
  | [key_ty; value_ty] ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt key_ty
      >>? fun (Ex_comparable_ty key_ty, ctxt) ->
      parse_big_map_value_ty
        ctxt
        ~stack_depth:(stack_depth + 1)
        ~legacy
        value_ty
      >>? fun (Ex_ty value_ty, ctxt) ->
      parse_type_annot big_map_loc map_annot
      >|? fun map_name ->
      let big_map_ty = Big_map_t (key_ty, value_ty, map_name) in
      (Ex_ty big_map_ty, ctxt)
  | args ->
      error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)

and parse_big_map_value_ty ctxt ~stack_depth ~legacy value_ty =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:legacy
    ~allow_ticket:true
    value_ty

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
        Gas.consume ctxt Typecheck_costs.parse_type_cycle
        >>? fun ctxt ->
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
        parse_composed_type_annot loc storage_annot
        >>? fun (ty_name, map_field, storage_field) ->
        ok
          ( Ex_ty
              (Pair_t
                 ( (big_map_ty, map_field, None),
                   (remaining_storage, storage_field, None),
                   ty_name )),
            ctxt ) )
  | _ ->
      (parse_normal_storage_ty [@tailcall]) ctxt ~stack_depth ~legacy node

let check_packable ~legacy loc root =
  let rec check : type t. t ty -> unit tzresult = function
    (* /!\ When adding new lazy storage kinds, be sure to return an error. /!\
    Lazy storage should not be packable. *)
    | Big_map_t _ ->
        error (Unexpected_lazy_storage loc)
    | Sapling_state_t _ ->
        error (Unexpected_lazy_storage loc)
    | Operation_t _ ->
        error (Unexpected_operation loc)
    | Unit_t _ ->
        ok_unit
    | Int_t _ ->
        ok_unit
    | Nat_t _ ->
        ok_unit
    | Signature_t _ ->
        ok_unit
    | String_t _ ->
        ok_unit
    | Bytes_t _ ->
        ok_unit
    | Mutez_t _ ->
        ok_unit
    | Key_hash_t _ ->
        ok_unit
    | Key_t _ ->
        ok_unit
    | Timestamp_t _ ->
        ok_unit
    | Address_t _ ->
        ok_unit
    | Bool_t _ ->
        ok_unit
    | Chain_id_t _ ->
        ok_unit
    | Never_t _ ->
        ok_unit
    | Set_t (_, _) ->
        ok_unit
    | Ticket_t _ ->
        error (Unexpected_ticket loc)
    | Lambda_t (_, _, _) ->
        ok_unit
    | Bls12_381_g1_t _ ->
        ok_unit
    | Bls12_381_g2_t _ ->
        ok_unit
    | Bls12_381_fr_t _ ->
        ok_unit
    | Pair_t ((l_ty, _, _), (r_ty, _, _), _) ->
        check l_ty >>? fun () -> check r_ty
    | Union_t ((l_ty, _), (r_ty, _), _) ->
        check l_ty >>? fun () -> check r_ty
    | Option_t (v_ty, _) ->
        check v_ty
    | List_t (elt_ty, _) ->
        check elt_ty
    | Map_t (_, elt_ty, _) ->
        check elt_ty
    | Contract_t (_, _) when legacy ->
        ok_unit
    | Contract_t (_, _) ->
        error (Unexpected_contract loc)
    | Sapling_transaction_t _ ->
        ok ()
  in
  check root

type ('arg, 'storage) code = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage_type : 'storage ty;
  root_name : field_annot option;
}

type ex_script = Ex_script : ('a, 'c) script -> ex_script

type ex_code = Ex_code : ('a, 'c) code -> ex_code

type (_, _) dig_proof_argument =
  | Dig_proof_argument :
      ('x, 'a * 's, 'a, 's, 'b, 't, 'c, 'u) stack_prefix_preservation_witness
      * 'x ty
      * var_annot option
      * ('c, 'u) stack_ty
      -> ('b, 't) dig_proof_argument

type (_, _, _) dug_proof_argument =
  | Dug_proof_argument :
      ( ('a, 's, 'x, 'a * 's, 'b, 't, 'c, 'u) stack_prefix_preservation_witness
      * unit
      * ('c, 'u) stack_ty )
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

type 'before comb_proof_argument =
  | Comb_proof_argument :
      ('a * 's, 'b * 'u) comb_gadt_witness * ('b, 'u) stack_ty
      -> ('a * 's) comb_proof_argument

type 'before uncomb_proof_argument =
  | Uncomb_proof_argument :
      ('a * 's, 'b * 'u) uncomb_gadt_witness * ('b, 'u) stack_ty
      -> ('a * 's) uncomb_proof_argument

type 'before comb_get_proof_argument =
  | Comb_get_proof_argument :
      ('before, 'after) comb_get_gadt_witness * 'after ty
      -> 'before comb_get_proof_argument

type ('rest, 'before) comb_set_proof_argument =
  | Comb_set_proof_argument :
      ('rest, 'before, 'after) comb_set_gadt_witness * 'after ty
      -> ('rest, 'before) comb_set_proof_argument

type 'before dup_n_proof_argument =
  | Dup_n_proof_argument :
      ('before, 'a) dup_n_gadt_witness * 'a ty
      -> 'before dup_n_proof_argument

let find_entrypoint (type full) (full : full ty) ~root_name entrypoint =
  let annot_is_entrypoint entrypoint = function
    | None ->
        false
    | Some (Field_annot l) ->
        Compare.String.(l = entrypoint)
  in
  let rec find_entrypoint :
      type t. t ty -> string -> ((Script.node -> Script.node) * ex_ty) option =
   fun t entrypoint ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) -> (
        if annot_is_entrypoint entrypoint al then
          Some ((fun e -> Prim (0, D_Left, [e], [])), Ex_ty tl)
        else if annot_is_entrypoint entrypoint ar then
          Some ((fun e -> Prim (0, D_Right, [e], [])), Ex_ty tr)
        else
          match find_entrypoint tl entrypoint with
          | Some (f, t) ->
              Some ((fun e -> Prim (0, D_Left, [f e], [])), t)
          | None -> (
            match find_entrypoint tr entrypoint with
            | Some (f, t) ->
                Some ((fun e -> Prim (0, D_Right, [f e], [])), t)
            | None ->
                None ) )
    | _ ->
        None
  in
  let entrypoint =
    if Compare.String.(entrypoint = "") then "default" else entrypoint
  in
  if Compare.Int.(String.length entrypoint > 31) then
    error (Entrypoint_name_too_long entrypoint)
  else
    match root_name with
    | Some (Field_annot root_name) when Compare.String.(entrypoint = root_name)
      ->
        ok ((fun e -> e), Ex_ty full)
    | _ -> (
      match find_entrypoint full entrypoint with
      | Some result ->
          ok result
      | None -> (
        match entrypoint with
        | "default" ->
            ok ((fun e -> e), Ex_ty full)
        | _ ->
            error (No_such_entrypoint entrypoint) ) )

let find_entrypoint_for_type (type full exp) ~legacy ~(full : full ty)
    ~(expected : exp ty) ~root_name entrypoint ctxt loc :
    (context * string * exp ty) tzresult =
  match (entrypoint, root_name) with
  | ("default", Some (Field_annot "root")) -> (
    match find_entrypoint full ~root_name entrypoint with
    | Error _ as err ->
        err
    | Ok (_, Ex_ty ty) -> (
      match merge_types ~legacy ctxt loc ty expected with
      | Ok (Eq, ty, ctxt) ->
          ok (ctxt, "default", ty)
      | Error _ ->
          merge_types ~legacy ctxt loc full expected
          >>? fun (Eq, full, ctxt) -> ok (ctxt, "root", (full : exp ty)) ) )
  | _ ->
      find_entrypoint full ~root_name entrypoint
      >>? fun (_, Ex_ty ty) ->
      merge_types ~legacy ctxt loc ty expected
      >>? fun (Eq, ty, ctxt) -> ok (ctxt, entrypoint, (ty : exp ty))
  [@@coq_axiom_with_reason "cast on err"]

module Entrypoints = Set.Make (String)

exception Duplicate of string

exception Too_long of string

let well_formed_entrypoints (type full) (full : full ty) ~root_name =
  let merge path annot (type t) (ty : t ty) reachable
      ((first_unreachable, all) as acc) =
    match annot with
    | None | Some (Field_annot "") -> (
        if reachable then acc
        else
          match ty with
          | Union_t _ ->
              acc
          | _ -> (
            match first_unreachable with
            | None ->
                (Some (List.rev path), all)
            | Some _ ->
                acc ) )
    | Some (Field_annot name) ->
        if Compare.Int.(String.length name > 31) then raise (Too_long name)
        else if Entrypoints.mem name all then raise (Duplicate name)
        else (first_unreachable, Entrypoints.add name all)
  in
  let rec check :
      type t.
      t ty ->
      prim list ->
      bool ->
      prim list option * Entrypoints.t ->
      prim list option * Entrypoints.t =
   fun t path reachable acc ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) ->
        let acc = merge (D_Left :: path) al tl reachable acc in
        let acc = merge (D_Right :: path) ar tr reachable acc in
        let acc =
          check
            tl
            (D_Left :: path)
            (match al with Some _ -> true | None -> reachable)
            acc
        in
        check
          tr
          (D_Right :: path)
          (match ar with Some _ -> true | None -> reachable)
          acc
    | _ ->
        acc
  in
  try
    let (init, reachable) =
      match root_name with
      | None | Some (Field_annot "") ->
          (Entrypoints.empty, false)
      | Some (Field_annot name) ->
          (Entrypoints.singleton name, true)
    in
    let (first_unreachable, all) = check full [] reachable (None, init) in
    if not (Entrypoints.mem "default" all) then ok_unit
    else
      match first_unreachable with
      | None ->
          ok_unit
      | Some path ->
          error (Unreachable_entrypoint path)
  with
  | Duplicate name ->
      error (Duplicate_entrypoint name)
  | Too_long name ->
      error (Entrypoint_name_too_long name)

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

(* This type is used to:
   - serialize and deserialize tickets when they are stored or transferred,
   - type the READ_TICKET instruction. *)
let opened_ticket_type ty =
  Pair_key
    ( (Address_key None, None),
      (Pair_key ((ty, None), (Nat_key None, None), None), None),
      None )

(* -- parse data of primitive types -- *)

let parse_unit ctxt ~legacy = function
  | Prim (loc, D_Unit, [], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.unit >|? fun ctxt -> ((), ctxt)
  | Prim (loc, D_Unit, l, _) ->
      error @@ Invalid_arity (loc, D_Unit, 0, List.length l)
  | expr ->
      error @@ unexpected expr [] Constant_namespace [D_Unit]

let parse_bool ctxt ~legacy = function
  | Prim (loc, D_True, [], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.bool >|? fun ctxt -> (true, ctxt)
  | Prim (loc, D_False, [], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>? fun () ->
      Gas.consume ctxt Typecheck_costs.bool >|? fun ctxt -> (false, ctxt)
  | Prim (loc, ((D_True | D_False) as c), l, _) ->
      error @@ Invalid_arity (loc, c, 0, List.length l)
  | expr ->
      error @@ unexpected expr [] Constant_namespace [D_True; D_False]

let parse_string ctxt = function
  | String (loc, v) as expr ->
      Gas.consume ctxt (Typecheck_costs.check_printable v)
      >>? fun ctxt ->
      let rec check_printable_ascii i =
        if Compare.Int.(i < 0) then true
        else
          match v.[i] with
          | '\n' | '\x20' .. '\x7E' ->
              check_printable_ascii (i - 1)
          | _ ->
              false
      in
      if check_printable_ascii (String.length v - 1) then ok (v, ctxt)
      else
        error
        @@ Invalid_syntactic_constant
             (loc, strip_locations expr, "a printable ascii string")
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind], kind expr)

let parse_bytes ctxt = function
  | Bytes (_, v) ->
      ok (v, ctxt)
  | expr ->
      error @@ Invalid_kind (location expr, [Bytes_kind], kind expr)

let parse_int ctxt = function
  | Int (_, v) ->
      ok (Script_int.of_zint v, ctxt)
  | expr ->
      error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_nat ctxt = function
  | Int (loc, v) as expr -> (
      let v = Script_int.of_zint v in
      match Script_int.is_nat v with
      | Some nat ->
          ok (nat, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a non-negative integer") )
  | expr ->
      error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_mutez ctxt = function
  | Int (loc, v) as expr -> (
    try
      match Tez.of_mutez (Z.to_int64 v) with
      | None ->
          raise Exit
      | Some tez ->
          ok (tez, ctxt)
    with _ ->
      error
      @@ Invalid_syntactic_constant
           (loc, strip_locations expr, "a valid mutez amount") )
  | expr ->
      error @@ Invalid_kind (location expr, [Int_kind], kind expr)

let parse_timestamp ctxt = function
  | Int (_, v) (* As unparsed with [Optimized] or out of bounds [Readable]. *)
    ->
      ok (Script_timestamp.of_zint v, ctxt)
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.timestamp_readable
      >>? fun ctxt ->
      match Script_timestamp.of_string s with
      | Some v ->
          ok (v, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid timestamp") )
  | expr ->
      error @@ Invalid_kind (location expr, [String_kind; Int_kind], kind expr)

let parse_key ctxt = function
  | Bytes (loc, bytes) as expr -> (
      (* As unparsed with [Optimized]. *)
      Gas.consume ctxt Typecheck_costs.public_key_optimized
      >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes Signature.Public_key.encoding bytes
      with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid public key") )
  | String (loc, s) as expr -> (
      (* As unparsed with [Readable]. *)
      Gas.consume ctxt Typecheck_costs.public_key_readable
      >>? fun ctxt ->
      match Signature.Public_key.of_b58check_opt s with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid public key") )
  | expr ->
      error
      @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_key_hash ctxt = function
  | Bytes (loc, bytes) as expr -> (
      (* As unparsed with [Optimized]. *)
      Gas.consume ctxt Typecheck_costs.key_hash_optimized
      >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes Signature.Public_key_hash.encoding bytes
      with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid key hash") )
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.key_hash_readable
      >>? fun ctxt ->
      match Signature.Public_key_hash.of_b58check_opt s with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid key hash") )
  | expr ->
      error
      @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_signature ctxt = function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.signature_optimized
      >>? fun ctxt ->
      match Data_encoding.Binary.of_bytes Signature.encoding bytes with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid signature") )
  | String (loc, s) as expr (* As unparsed with [Readable]. *) -> (
      Gas.consume ctxt Typecheck_costs.signature_readable
      >>? fun ctxt ->
      match Signature.of_b58check_opt s with
      | Some s ->
          ok (s, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid signature") )
  | expr ->
      error
      @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_chain_id ctxt = function
  | Bytes (loc, bytes) as expr -> (
      Gas.consume ctxt Typecheck_costs.chain_id_optimized
      >>? fun ctxt ->
      match Data_encoding.Binary.of_bytes Chain_id.encoding bytes with
      | Some k ->
          ok (k, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid chain id") )
  | String (loc, s) as expr -> (
      Gas.consume ctxt Typecheck_costs.chain_id_readable
      >>? fun ctxt ->
      match Chain_id.of_b58check_opt s with
      | Some s ->
          ok (s, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid chain id") )
  | expr ->
      error
      @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_address ctxt = function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.contract
      >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes
          Data_encoding.(tup2 Contract.encoding Variable.string)
          bytes
      with
      | Some (c, entrypoint) -> (
          if Compare.Int.(String.length entrypoint > 31) then
            error (Entrypoint_name_too_long entrypoint)
          else
            match entrypoint with
            | "" ->
                ok ((c, "default"), ctxt)
            | "default" ->
                error (Unexpected_annotation loc)
            | name ->
                ok ((c, name), ctxt) )
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid address") )
  | String (loc, s) (* As unparsed with [Readable]. *) ->
      Gas.consume ctxt Typecheck_costs.contract
      >>? fun ctxt ->
      ( match String.index_opt s '%' with
      | None ->
          ok (s, "default")
      | Some pos -> (
          let len = String.length s - pos - 1 in
          let name = String.sub s (pos + 1) len in
          if Compare.Int.(len > 31) then error (Entrypoint_name_too_long name)
          else
            match (String.sub s 0 pos, name) with
            | (addr, "") ->
                ok (addr, "default")
            | (_, "default") ->
                error @@ Unexpected_annotation loc
            | addr_and_name ->
                ok addr_and_name ) )
      >>? fun (addr, entrypoint) ->
      Contract.of_b58check addr >|? fun c -> ((c, entrypoint), ctxt)
  | expr ->
      error
      @@ Invalid_kind (location expr, [String_kind; Bytes_kind], kind expr)

let parse_never expr = error @@ Invalid_never_expr (location expr)

(* -- parse data of complex types -- *)

let parse_pair (type r) parse_l parse_r ctxt ~legacy
    (r_comb_witness : (r, unit -> _) comb_witness) expr =
  let parse_comb loc l rs =
    parse_l ctxt l
    >>=? fun (l, ctxt) ->
    ( match (rs, r_comb_witness) with
    | ([r], _) ->
        ok r
    | ([], _) ->
        error @@ Invalid_arity (loc, D_Pair, 2, 1)
    | (_ :: _, Comb_Pair _) ->
        (* Unfold [Pair x1 ... xn] as [Pair x1 (Pair x2 ... xn-1 xn))]
          for type [pair ta (pair tb1 tb2)] and n >= 3 only *)
        ok (Prim (loc, D_Pair, rs, []))
    | _ ->
        error @@ Invalid_arity (loc, D_Pair, 2, 1 + List.length rs) )
    >>?= fun r -> parse_r ctxt r >|=? fun (r, ctxt) -> ((l, r), ctxt)
  in
  match expr with
  | Prim (loc, D_Pair, l :: rs, annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>?= fun () -> parse_comb loc l rs
  | Prim (loc, D_Pair, l, _) ->
      fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
  (* Unfold [{x1; ...; xn}] as [Pair x1 x2 ... xn-1 xn] for n >= 2 *)
  | Seq (loc, l :: (_ :: _ as rs)) ->
      parse_comb loc l rs
  | Seq (loc, l) ->
      fail @@ Invalid_seq_arity (loc, 2, List.length l)
  | expr ->
      fail @@ unexpected expr [] Constant_namespace [D_Pair]

let parse_union parse_l parse_r ctxt ~legacy = function
  | Prim (loc, D_Left, [v], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>?= fun () -> parse_l ctxt v >|=? fun (v, ctxt) -> (L v, ctxt)
  | Prim (loc, D_Left, l, _) ->
      fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
  | Prim (loc, D_Right, [v], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>?= fun () -> parse_r ctxt v >|=? fun (v, ctxt) -> (R v, ctxt)
  | Prim (loc, D_Right, l, _) ->
      fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
  | expr ->
      fail @@ unexpected expr [] Constant_namespace [D_Left; D_Right]

let parse_option parse_v ctxt ~legacy = function
  | Prim (loc, D_Some, [v], annot) ->
      (if legacy then ok_unit else error_unexpected_annot loc annot)
      >>?= fun () -> parse_v ctxt v >|=? fun (v, ctxt) -> (Some v, ctxt)
  | Prim (loc, D_Some, l, _) ->
      fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
  | Prim (loc, D_None, [], annot) ->
      Lwt.return
        ( (if legacy then ok_unit else error_unexpected_annot loc annot)
        >|? fun () -> (None, ctxt) )
  | Prim (loc, D_None, l, _) ->
      fail @@ Invalid_arity (loc, D_None, 0, List.length l)
  | expr ->
      fail @@ unexpected expr [] Constant_namespace [D_Some; D_None]

(* -- parse data of comparable types -- *)

let comparable_comb_witness1 :
    type t. t comparable_ty -> (t, unit -> unit) comb_witness = function
  | Pair_key _ ->
      Comb_Pair Comb_Any
  | _ ->
      Comb_Any

let rec parse_comparable_data :
    type a.
    ?type_logger:type_logger ->
    context ->
    a comparable_ty ->
    Script.node ->
    (a * context) tzresult Lwt.t =
 fun ?type_logger ctxt ty script_data ->
  (* No need for stack_depth here. Unlike [parse_data],
     [parse_comparable_data] doesn't call [parse_returning].
     The stack depth is bounded by the type depth, bounded by 1024. *)
  let parse_data_error () =
    serialize_ty_for_error ctxt (ty_of_comparable_ty ty)
    >|? fun (ty, _ctxt) ->
    Invalid_constant (location script_data, strip_locations script_data, ty)
  in
  let traced_no_lwt body = record_trace_eval parse_data_error body in
  let traced body =
    trace_eval (fun () -> Lwt.return @@ parse_data_error ()) body
  in
  Gas.consume ctxt Typecheck_costs.parse_data_cycle
  (* We could have a smaller cost but let's keep it consistent with
     [parse_data] for now. *)
  >>?= fun ctxt ->
  let legacy = false in
  match (ty, script_data) with
  | (Unit_key _, expr) ->
      Lwt.return @@ traced_no_lwt
      @@ (parse_unit ctxt ~legacy expr : (a * context) tzresult)
  | (Bool_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_bool ctxt ~legacy expr
  | (String_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_string ctxt expr
  | (Bytes_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_bytes ctxt expr
  | (Int_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_int ctxt expr
  | (Nat_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_nat ctxt expr
  | (Mutez_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_mutez ctxt expr
  | (Timestamp_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_timestamp ctxt expr
  | (Key_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_key ctxt expr
  | (Key_hash_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_key_hash ctxt expr
  | (Signature_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_signature ctxt expr
  | (Chain_id_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_chain_id ctxt expr
  | (Address_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_address ctxt expr
  | (Pair_key ((tl, _), (tr, _), _), expr) ->
      let r_witness = comparable_comb_witness1 tr in
      let parse_l ctxt v = parse_comparable_data ?type_logger ctxt tl v in
      let parse_r ctxt v = parse_comparable_data ?type_logger ctxt tr v in
      traced @@ parse_pair parse_l parse_r ctxt ~legacy r_witness expr
  | (Union_key ((tl, _), (tr, _), _), expr) ->
      let parse_l ctxt v = parse_comparable_data ?type_logger ctxt tl v in
      let parse_r ctxt v = parse_comparable_data ?type_logger ctxt tr v in
      traced @@ parse_union parse_l parse_r ctxt ~legacy expr
  | (Option_key (t, _), expr) ->
      let parse_v ctxt v = parse_comparable_data ?type_logger ctxt t v in
      traced @@ parse_option parse_v ctxt ~legacy expr
  | (Never_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_never expr

(* -- parse data of any type -- *)

let comb_witness1 : type t. t ty -> (t, unit -> unit) comb_witness = function
  | Pair_t _ ->
      Comb_Pair Comb_Any
  | _ ->
      Comb_Any

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
    type a.
    ?type_logger:type_logger ->
    stack_depth:int ->
    context ->
    legacy:bool ->
    allow_forged:bool ->
    a ty ->
    Script.node ->
    (a * context) tzresult Lwt.t =
 fun ?type_logger ~stack_depth ctxt ~legacy ~allow_forged ty script_data ->
  Gas.consume ctxt Typecheck_costs.parse_data_cycle
  >>?= fun ctxt ->
  let non_terminal_recursion ?type_logger ctxt ~legacy ty script_data =
    if Compare.Int.(stack_depth > 10_000) then
      fail Typechecking_too_many_recursive_calls
    else
      parse_data
        ?type_logger
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~legacy
        ~allow_forged
        ty
        script_data
  in
  let parse_data_error () =
    serialize_ty_for_error ctxt ty
    >|? fun (ty, _ctxt) ->
    Invalid_constant (location script_data, strip_locations script_data, ty)
  in
  let fail_parse_data () = parse_data_error () >>?= fail in
  let traced_no_lwt body = record_trace_eval parse_data_error body in
  let traced body =
    trace_eval (fun () -> Lwt.return @@ parse_data_error ()) body
  in
  let traced_fail err = Lwt.return @@ traced_no_lwt (error err) in
  let parse_items ?type_logger ctxt expr key_type value_type items item_wrapper
      =
    fold_left_s
      (fun (last_value, map, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if legacy then ok_unit else error_unexpected_annot loc annot)
            >>?= fun () ->
            parse_comparable_data ?type_logger ctxt key_type k
            >>=? fun (k, ctxt) ->
            non_terminal_recursion ?type_logger ctxt ~legacy value_type v
            >>=? fun (v, ctxt) ->
            Lwt.return
              ( ( match last_value with
                | Some value ->
                    Gas.consume
                      ctxt
                      (Michelson_v1_gas.Cost_of.Interpreter.compare
                         key_type
                         value
                         k)
                    >>? fun ctxt ->
                    let c = compare_comparable key_type value k in
                    if Compare.Int.(0 <= c) then
                      if Compare.Int.(0 = c) then
                        error (Duplicate_map_keys (loc, strip_locations expr))
                      else
                        error (Unordered_map_keys (loc, strip_locations expr))
                    else ok ctxt
                | None ->
                    ok ctxt )
              >>? fun ctxt ->
              Gas.consume
                ctxt
                (Michelson_v1_gas.Cost_of.Interpreter.map_update k map)
              >|? fun ctxt ->
              (Some k, map_update k (Some (item_wrapper v)) map, ctxt) )
        | Prim (loc, D_Elt, l, _) ->
            fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            fail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ ->
            fail_parse_data ())
      (None, empty_map key_type, ctxt)
      items
    |> traced
    >|=? fun (_, items, ctxt) -> (items, ctxt)
  in
  let parse_big_map_items (type t) ?type_logger ctxt expr
      (key_type : t comparable_ty) value_type items item_wrapper =
    fold_left_s
      (fun (last_key, {map; size}, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if legacy then ok_unit else error_unexpected_annot loc annot)
            >>?= fun () ->
            parse_comparable_data ?type_logger ctxt key_type k
            >>=? fun (k, ctxt) ->
            hash_comparable_data ctxt key_type k
            >>=? fun (key_hash, ctxt) ->
            non_terminal_recursion ?type_logger ctxt ~legacy value_type v
            >>=? fun (v, ctxt) ->
            Lwt.return
              ( ( match last_key with
                | Some last_key ->
                    Gas.consume
                      ctxt
                      (Michelson_v1_gas.Cost_of.Interpreter.compare
                         key_type
                         last_key
                         k)
                    >>? fun ctxt ->
                    let c = compare_comparable key_type last_key k in
                    if Compare.Int.(0 <= c) then
                      if Compare.Int.(0 = c) then
                        error (Duplicate_map_keys (loc, strip_locations expr))
                      else
                        error (Unordered_map_keys (loc, strip_locations expr))
                    else ok ctxt
                | None ->
                    ok ctxt )
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
                      map = Big_map_overlay.add key_hash (k, item_wrapper v) map;
                      size = size + 1;
                    },
                    ctxt ) )
        | Prim (loc, D_Elt, l, _) ->
            fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            fail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ ->
            fail_parse_data ())
      (None, {map = Big_map_overlay.empty; size = 0}, ctxt)
      items
    |> traced
    >|=? fun (_, map, ctxt) -> (map, ctxt)
  in
  match (ty, script_data) with
  | (Unit_t _, expr) ->
      Lwt.return @@ traced_no_lwt
      @@ (parse_unit ctxt ~legacy expr : (a * context) tzresult)
  | (Bool_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_bool ctxt ~legacy expr
  | (String_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_string ctxt expr
  | (Bytes_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_bytes ctxt expr
  | (Int_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_int ctxt expr
  | (Nat_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_nat ctxt expr
  | (Mutez_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_mutez ctxt expr
  | (Timestamp_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_timestamp ctxt expr
  | (Key_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_key ctxt expr
  | (Key_hash_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_key_hash ctxt expr
  | (Signature_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_signature ctxt expr
  | (Operation_t _, _) ->
      (* operations cannot appear in parameters or storage,
         the protocol should never parse the bytes of an operation *)
      assert false
  | (Chain_id_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_chain_id ctxt expr
  | (Address_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_address ctxt expr
  | (Contract_t (ty, _), expr) ->
      traced
        ( parse_address ctxt expr
        >>?= fun ((c, entrypoint), ctxt) ->
        let loc = location expr in
        parse_contract
          ~stack_depth:(stack_depth + 1)
          ~legacy
          ctxt
          loc
          ty
          c
          ~entrypoint
        >|=? fun (ctxt, _) -> ((ty, (c, entrypoint)), ctxt) )
  (* Pairs *)
  | (Pair_t ((tl, _, _), (tr, _, _), _), expr) ->
      let r_witness = comb_witness1 tr in
      let parse_l ctxt v =
        non_terminal_recursion ?type_logger ctxt ~legacy tl v
      in
      let parse_r ctxt v =
        non_terminal_recursion ?type_logger ctxt ~legacy tr v
      in
      traced @@ parse_pair parse_l parse_r ctxt ~legacy r_witness expr
  (* Unions *)
  | (Union_t ((tl, _), (tr, _), _), expr) ->
      let parse_l ctxt v =
        non_terminal_recursion ?type_logger ctxt ~legacy tl v
      in
      let parse_r ctxt v =
        non_terminal_recursion ?type_logger ctxt ~legacy tr v
      in
      traced @@ parse_union parse_l parse_r ctxt ~legacy expr
  (* Lambdas *)
  | (Lambda_t (ta, tr, _ty_name), (Seq (_loc, _) as script_instr)) ->
      traced
      @@ parse_returning
           Lambda
           ?type_logger
           ~stack_depth:(stack_depth + 1)
           ctxt
           ~legacy
           (ta, Some (Var_annot "@arg"))
           tr
           script_instr
  | (Lambda_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Options *)
  | (Option_t (t, _), expr) ->
      let parse_v ctxt v =
        non_terminal_recursion ?type_logger ctxt ~legacy t v
      in
      traced @@ parse_option parse_v ctxt ~legacy expr
  (* Lists *)
  | (List_t (t, _ty_name), Seq (_loc, items)) ->
      traced
      @@ fold_right_s
           (fun v (rest, ctxt) ->
             non_terminal_recursion ?type_logger ctxt ~legacy t v
             >|=? fun (v, ctxt) -> (list_cons v rest, ctxt))
           items
           (list_empty, ctxt)
  | (List_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Tickets *)
  | (Ticket_t (t, _ty_name), expr) ->
      if allow_forged then
        parse_comparable_data ?type_logger ctxt (opened_ticket_type t) expr
        >|=? fun ((ticketer, (contents, amount)), ctxt) ->
        ({ticketer; contents; amount}, ctxt)
      else traced_fail (Unexpected_forged_value (location expr))
  (* Sets *)
  | (Set_t (t, _ty_name), (Seq (loc, vs) as expr)) ->
      traced
      @@ fold_left_s
           (fun (last_value, set, ctxt) v ->
             parse_comparable_data ?type_logger ctxt t v
             >>=? fun (v, ctxt) ->
             Lwt.return
               ( ( match last_value with
                 | Some value ->
                     Gas.consume
                       ctxt
                       (Michelson_v1_gas.Cost_of.Interpreter.compare t value v)
                     >>? fun ctxt ->
                     let c = compare_comparable t value v in
                     if Compare.Int.(0 <= c) then
                       if Compare.Int.(0 = c) then
                         error
                           (Duplicate_set_values (loc, strip_locations expr))
                       else
                         error
                           (Unordered_set_values (loc, strip_locations expr))
                     else ok ctxt
                 | None ->
                     ok ctxt )
               >>? fun ctxt ->
               Gas.consume
                 ctxt
                 (Michelson_v1_gas.Cost_of.Interpreter.set_update v set)
               >|? fun ctxt -> (Some v, set_update v true set, ctxt) ))
           (None, empty_set t, ctxt)
           vs
      >|=? fun (_, set, ctxt) -> (set, ctxt)
  | (Set_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Maps *)
  | (Map_t (tk, tv, _ty_name), (Seq (_, vs) as expr)) ->
      parse_items ?type_logger ctxt expr tk tv vs (fun x -> x)
  | (Map_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  | (Big_map_t (tk, tv, _ty_name), expr) ->
      ( match expr with
      | Int (loc, id) ->
          return (Some (id, loc), {map = Big_map_overlay.empty; size = 0}, ctxt)
      | Seq (_, vs) ->
          parse_big_map_items ?type_logger ctxt expr tk tv vs (fun x -> Some x)
          >|=? fun (diff, ctxt) -> (None, diff, ctxt)
      | Prim (loc, D_Pair, [Int (loc_id, id); Seq (_, vs)], annot) ->
          error_unexpected_annot loc annot
          >>?= fun () ->
          let tv_opt = Option_t (tv, None) in
          parse_big_map_items ?type_logger ctxt expr tk tv_opt vs (fun x -> x)
          >|=? fun (diff, ctxt) -> (Some (id, loc_id), diff, ctxt)
      | Prim (_, D_Pair, [Int _; expr], _) ->
          traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
      | Prim (_, D_Pair, [expr; _], _) ->
          traced_fail (Invalid_kind (location expr, [Int_kind], kind expr))
      | Prim (loc, D_Pair, l, _) ->
          traced_fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
      | _ ->
          traced_fail
            (unexpected expr [Seq_kind; Int_kind] Constant_namespace [D_Pair])
      )
      >>=? fun (id_opt, diff, ctxt) ->
      ( match id_opt with
      | None ->
          return @@ (None, ctxt)
      | Some (id, loc) ->
          if allow_forged then
            let id = Big_map.Id.parse_z id in
            Big_map.exists ctxt id
            >>=? function
            | (_, None) ->
                traced_fail (Invalid_big_map (loc, id))
            | (ctxt, Some (btk, btv)) ->
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
                  comparable_ty_eq ctxt tk btk
                  >>? fun (Eq, ctxt) ->
                  ty_eq ctxt loc tv btv >>? fun (Eq, ctxt) -> ok (Some id, ctxt)
                  )
          else traced_fail (Unexpected_forged_value loc) )
      >|=? fun (id, ctxt) -> ({id; diff; key_type = tk; value_type = tv}, ctxt)
  | (Never_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_never expr
  (* Bls12_381 types *)
  | (Bls12_381_g1_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g1
      >>?= fun ctxt ->
      match Bls12_381.G1.of_bytes_opt bs with
      | Some pt ->
          return (pt, ctxt)
      | None ->
          fail_parse_data () )
  | (Bls12_381_g1_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Bls12_381_g2_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g2
      >>?= fun ctxt ->
      match Bls12_381.G2.of_bytes_opt bs with
      | Some pt ->
          return (pt, ctxt)
      | None ->
          fail_parse_data () )
  | (Bls12_381_g2_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Bls12_381_fr_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_fr
      >>?= fun ctxt ->
      match Bls12_381.Fr.of_bytes_opt bs with
      | Some pt ->
          return (pt, ctxt)
      | None ->
          fail_parse_data () )
  | (Bls12_381_fr_t _, Int (_, v)) ->
      Gas.consume ctxt Typecheck_costs.bls12_381_fr
      >>?= fun ctxt -> return (Bls12_381.Fr.of_z v, ctxt)
  | (Bls12_381_fr_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  (*
    /!\ When adding new lazy storage kinds, you may want to guard the parsing
    of identifiers with [allow_forged].
  *)
  (* Sapling *)
  | (Sapling_transaction_t (memo_size, _), Bytes (_, bytes)) -> (
    match Data_encoding.Binary.of_bytes Sapling.transaction_encoding bytes with
    | Some transaction -> (
      match Sapling.transaction_get_memo_size transaction with
      | None ->
          return (transaction, ctxt)
      | Some transac_memo_size ->
          Lwt.return
            ( merge_memo_sizes memo_size transac_memo_size
            >|? fun _ms -> (transaction, ctxt) ) )
    | None ->
        fail_parse_data () )
  | (Sapling_transaction_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Sapling_state_t (memo_size, _), Int (loc, id)) ->
      if allow_forged then
        let id = Sapling.Id.parse_z id in
        Sapling.state_from_id ctxt id
        >>=? fun (state, ctxt) ->
        Lwt.return
          ( traced_no_lwt @@ merge_memo_sizes memo_size state.Sapling.memo_size
          >|? fun _memo_size -> (state, ctxt) )
      else traced_fail (Unexpected_forged_value loc)
  | (Sapling_state_t (memo_size, _), Seq (_, [])) ->
      return (Sapling.empty_state ~memo_size (), ctxt)
  | (Sapling_state_t _, expr) ->
      (* Do not allow to input diffs as they are untrusted and may not be the
         result of a verify_update. *)
      traced_fail
        (Invalid_kind (location expr, [Int_kind; Seq_kind], kind expr))

and parse_returning :
    type arg ret.
    ?type_logger:type_logger ->
    stack_depth:int ->
    tc_context ->
    context ->
    legacy:bool ->
    arg ty * var_annot option ->
    ret ty ->
    Script.node ->
    ((arg, ret) lambda * context) tzresult Lwt.t =
 fun ?type_logger
     ~stack_depth
     tc_context
     ctxt
     ~legacy
     (arg, arg_annot)
     ret
     script_instr ->
  parse_instr
    ?type_logger
    tc_context
    ctxt
    ~legacy
    ~stack_depth:(stack_depth + 1)
    script_instr
    (Item_t (arg, Bot_t, arg_annot))
  >>=? function
  | (Typed ({loc; aft = Item_t (ty, Bot_t, _) as stack_ty; _} as descr), ctxt)
    ->
      Lwt.return
      @@ record_trace_eval
           (fun () ->
             serialize_ty_for_error ctxt ret
             >>? fun (ret, ctxt) ->
             serialize_stack_for_error ctxt stack_ty
             >|? fun (stack_ty, _ctxt) -> Bad_return (loc, stack_ty, ret))
           ( merge_types ~legacy ctxt loc ty ret
           >|? fun (Eq, _ret, ctxt) ->
           ((Lam (close_descr descr, script_instr) : (arg, ret) lambda), ctxt)
           )
  | (Typed {loc; aft = stack_ty; _}, ctxt) ->
      Lwt.return
        ( serialize_ty_for_error ctxt ret
        >>? fun (ret, ctxt) ->
        serialize_stack_for_error ctxt stack_ty
        >>? fun (stack_ty, _ctxt) -> error (Bad_return (loc, stack_ty, ret)) )
  | (Failed {descr}, ctxt) ->
      return
        ( ( Lam (close_descr (descr (Item_t (ret, Bot_t, None))), script_instr)
            : (arg, ret) lambda ),
          ctxt )

and parse_instr :
    type a s.
    ?type_logger:type_logger ->
    stack_depth:int ->
    tc_context ->
    context ->
    legacy:bool ->
    Script.node ->
    (a, s) stack_ty ->
    ((a, s) judgement * context) tzresult Lwt.t =
 fun ?type_logger ~stack_depth tc_context ctxt ~legacy script_instr stack_ty ->
  let check_item_ty (type a b) ctxt (exp : a ty) (got : b ty) loc name n m :
      ((a, b) eq * a ty * context) tzresult =
    record_trace_eval (fun () ->
        serialize_stack_for_error ctxt stack_ty
        >|? fun (stack_ty, _ctxt) -> Bad_stack (loc, name, m, stack_ty))
    @@ record_trace
         (Bad_stack_item n)
         ( merge_types ~legacy ctxt loc exp got
         >>? fun (Eq, ty, ctxt) -> ok ((Eq : (a, b) eq), (ty : a ty), ctxt) )
  in
  let log_stack ctxt loc stack_ty aft =
    match (type_logger, script_instr) with
    | (None, _) | (Some _, (Seq (-1, _) | Int _ | String _ | Bytes _)) ->
        ok_unit
    | (Some log, (Prim _ | Seq _)) ->
        (* Unparsing for logging done in an unlimited context as this
              is used only by the client and not the protocol *)
        let ctxt = Gas.set_unlimited ctxt in
        unparse_stack ctxt stack_ty
        >>? fun (stack_ty, _) ->
        unparse_stack ctxt aft >|? fun (aft, _) -> log loc stack_ty aft ; ()
  in
  (*

     In the following functions, [number_of_generated_growing_types] is the
     depth of the stack to inspect for sizes overflow. We only need to check
     the produced types that can be larger than the arguments. That's why Swap
     is 0 for instance as no type grows.
     Constant sized types are not checked: it is assumed they are lower than
     the bound (otherwise every program would be rejected).

  *)
  let return_no_lwt :
      type a s.
      context ->
      int ->
      (a, s) judgement ->
      ((a, s) judgement * context) tzresult =
   fun ctxt number_of_generated_growing_types judgement ->
    match judgement with
    | Typed {loc; aft; _} ->
        let maximum_type_size = Constants.michelson_maximum_type_size ctxt in
        check_type_size_of_stack_head
          ~loc
          ~maximum_type_size
          aft
          ~up_to:number_of_generated_growing_types
        >|? fun () -> (judgement, ctxt)
    | Failed _ ->
        ok (judgement, ctxt)
  in
  let return :
      type a s.
      context ->
      int ->
      (a, s) judgement ->
      ((a, s) judgement * context) tzresult Lwt.t =
   fun ctxt number_of_generated_growing_types judgement ->
    Lwt.return
    @@ return_no_lwt ctxt number_of_generated_growing_types judgement
  in
  let typed_no_lwt ctxt number_of_generated_growing_types loc instr aft : ((a, s) judgement * context) tzresult =
    log_stack ctxt loc stack_ty aft
    >>? fun () ->
    let j = Typed {loc; instr; bef = stack_ty; aft} in
    return_no_lwt ctxt number_of_generated_growing_types j
  in
  let typed ctxt number_of_generated_growing_types loc instr aft =
    Lwt.return
    @@ typed_no_lwt ctxt number_of_generated_growing_types loc instr aft
  in
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle
  >>?= fun ctxt ->
  let non_terminal_recursion ?type_logger tc_context ctxt ~legacy script_instr
      stack_ty =
    if Compare.Int.(stack_depth > 10000) then
      fail Typechecking_too_many_recursive_calls
    else
      parse_instr
        ?type_logger
        tc_context
        ctxt
        ~stack_depth:(stack_depth + 1)
        ~legacy
        script_instr
        stack_ty
  in
  match (script_instr, stack_ty) with
  (* stack ops *)
  | (Prim (loc, I_DROP, [], annot), Item_t (_, rest, _)) ->
      ( error_unexpected_annot loc annot
        >>?= fun () ->
        typed ctxt 0 loc {apply = (fun kinfo k -> IDrop (kinfo, k))} rest
        : ((a, s) judgement * context) tzresult Lwt.t )
  | (Prim (loc, I_DROP, [n], result_annot), whole_stack) ->
      parse_uint10 n
      >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n)
      >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dropn_proof_argument tzresult =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) ->
            ok @@ Dropn_proof_argument (KRest, rest)
        | (false, Item_t (_, rest, _)) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dropn_proof_argument (n', stack_after_drops)) ->
            let kinfo = {iloc = loc; kstack_ty = rest} in
            Dropn_proof_argument (KPrefix (kinfo, n'), stack_after_drops)
        | (_, _) ->
            serialize_stack_for_error ctxt whole_stack
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_DROP, whole_n, whole_stack))
      in
      error_unexpected_annot loc result_annot
      >>?= fun () ->
      make_proof_argument whole_n whole_stack
      >>?= fun (Dropn_proof_argument (n', stack_after_drops)) ->
      let kdropn kinfo k = IDropn (kinfo, whole_n, n', k) in
      typed ctxt 0 loc {apply = kdropn} stack_after_drops
  | (Prim (loc, I_DROP, (_ :: _ :: _ as l), _), _) ->
      (* Technically, the arities 0 and 1 are allowed but the error only mentions 1.
            However, DROP is equivalent to DROP 1 so hinting at an arity of 1 makes sense. *)
      fail (Invalid_arity (loc, I_DROP, 1, List.length l))
  | (Prim (loc, I_DUP, [], annot), Item_t (v, rest, stack_annot)) ->
      parse_var_annot loc annot ~default:stack_annot
      >>?= fun annot ->
      record_trace_eval
        (fun () ->
          serialize_ty_for_error ctxt v
          >|? fun (t, _ctxt) -> Non_dupable_type (loc, t))
        (check_dupable_ty ctxt loc v)
      >>?= fun ctxt ->
      let dup = {apply = (fun kinfo k -> IDup (kinfo, k))} in
      typed ctxt 0 loc dup (Item_t (v, Item_t (v, rest, stack_annot), annot))
  | (Prim (loc, I_DUP, [n], v_annot), stack_ty) ->
      parse_var_annot loc v_annot
      >>?= fun annot ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) dup_n_proof_argument tzresult =
       fun n (stack_ty : (a, s) stack_ty) ->
        match (n, stack_ty) with
        | (1, Item_t (hd_ty, _, _)) ->
            ok @@ Dup_n_proof_argument (Dup_n_zero, hd_ty)
        | (n, Item_t (_, tl_ty, _)) ->
            make_proof_argument (n - 1) tl_ty
            >|? fun (Dup_n_proof_argument (dup_n_witness, b_ty)) ->
            Dup_n_proof_argument (Dup_n_succ dup_n_witness, b_ty)
        | _ ->
            serialize_stack_for_error ctxt stack_ty
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_DUP, 1, whole_stack))
      in
      parse_uint10 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 0) (Dup_n_bad_argument loc)
      >>?= fun () ->
      record_trace (Dup_n_bad_stack loc) (make_proof_argument n stack_ty)
      >>?= fun (Dup_n_proof_argument (witness, after_ty)) ->
      record_trace_eval
        (fun () ->
          serialize_ty_for_error ctxt after_ty
          >|? fun (t, _ctxt) -> Non_dupable_type (loc, t))
        (check_dupable_ty ctxt loc after_ty)
      >>?= fun ctxt ->
      let dupn = {apply = (fun kinfo k -> IDup_n (kinfo, n, witness, k))} in
      typed ctxt 0 loc dupn (Item_t (after_ty, stack_ty, annot))
  | (Prim (loc, I_DIG, [n], result_annot), stack) ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dig_proof_argument tzresult =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, Item_t (v, rest, annot)) ->
            ok @@ Dig_proof_argument (KRest, v, annot, rest)
        | (false, Item_t (v, rest, annot)) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dig_proof_argument (n', x, xv, aft')) ->
            let kinfo = {iloc = loc; kstack_ty = aft'} in
            Dig_proof_argument
              (KPrefix (kinfo, n'), x, xv, Item_t (v, aft', annot))
        | (_, _) ->
            serialize_stack_for_error ctxt stack
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_DIG, 3, whole_stack))
      in
      parse_uint10 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      error_unexpected_annot loc result_annot
      >>?= fun () ->
      make_proof_argument n stack
      >>?= fun (Dig_proof_argument (n', x, stack_annot, aft)) ->
      let dig = {apply = (fun kinfo k -> IDig (kinfo, n, n', k))} in
      typed ctxt 0 loc dig (Item_t (x, aft, stack_annot))
  | (Prim (loc, I_DIG, (([] | _ :: _ :: _) as l), _), _) ->
      fail (Invalid_arity (loc, I_DIG, 1, List.length l))
  | (Prim (loc, I_DUG, [n], result_annot), Item_t (x, whole_stack, stack_annot))
    ->
      parse_uint10 n
      >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n)
      >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s x.
          int ->
          x ty ->
          var_annot option ->
          (a, s) stack_ty ->
          (a, s, x) dug_proof_argument tzresult =
       fun n x stack_annot stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) ->
            ok @@ Dug_proof_argument (KRest, (), Item_t (x, rest, stack_annot))
        | (false, Item_t (v, rest, annot)) ->
            make_proof_argument (n - 1) x stack_annot rest
            >|? fun (Dug_proof_argument (n', (), aft')) ->
            let kinfo = {iloc = loc; kstack_ty = aft'} in
            Dug_proof_argument
              (KPrefix (kinfo, n'), (), Item_t (v, aft', annot))
        | (_, _) ->
            serialize_stack_for_error ctxt whole_stack
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_DUG, whole_n, whole_stack))
      in
      error_unexpected_annot loc result_annot
      >>?= fun () ->
      make_proof_argument whole_n x stack_annot whole_stack
      >>?= fun (Dug_proof_argument (n', (), aft)) ->
      let dug = {apply = (fun kinfo k -> IDug (kinfo, whole_n, n', k))} in
      typed ctxt 0 loc dug aft
  | (Prim (loc, I_DUG, [_], result_annot), stack) ->
      Lwt.return
        ( error_unexpected_annot loc result_annot
        >>? fun () ->
        serialize_stack_for_error ctxt stack
        >>? fun (stack, _ctxt) -> error (Bad_stack (loc, I_DUG, 1, stack)) )
  | (Prim (loc, I_DUG, (([] | _ :: _ :: _) as l), _), _) ->
      fail (Invalid_arity (loc, I_DUG, 1, List.length l))
  | ( Prim (loc, I_SWAP, [], annot),
      Item_t (v, Item_t (w, rest, stack_annot), cur_top_annot) ) ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let swap = {apply = (fun kinfo k -> ISwap (kinfo, k))} in
      let stack_ty =
        Item_t (w, Item_t (v, rest, cur_top_annot), stack_annot)
      in
      typed ctxt 0 loc swap stack_ty
  | (Prim (loc, I_PUSH, [t; d], annot), stack) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      check_type_size ~legacy ctxt ~loc t
      >>?= fun () ->
      parse_data
        ?type_logger
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~legacy
        ~allow_forged:false
        t
        d
      >>=? fun (v, ctxt) ->
      let const = {apply = (fun kinfo k -> IConst (kinfo, v, k))} in
      typed
        ctxt
        0
        (* type size already checked *) loc
        const
        (Item_t (t, stack, annot))
  | (Prim (loc, I_UNIT, [], annot), stack) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let const = {apply = (fun kinfo k -> IConst (kinfo, (), k))} in
      typed ctxt 0 loc const (Item_t (Unit_t ty_name, stack, annot))
  (* options *)
  | (Prim (loc, I_SOME, [], annot), Item_t (t, rest, _)) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let cons_some = {apply = (fun kinfo k -> ICons_some (kinfo, k))} in
      typed ctxt 1 loc cons_some (Item_t (Option_t (t, ty_name), rest, annot))
  | (Prim (loc, I_NONE, [t], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let cons_none = {apply = (fun kinfo k -> ICons_none (kinfo, t, k))} in
      let stack_ty = Item_t (Option_t (t, ty_name), stack, annot) in
      typed ctxt 1 loc cons_none stack_ty
  | ( Prim (loc, I_IF_NONE, [bt; bf], annot),
      (Item_t (Option_t (t, _), rest, option_annot) as bef) ) ->
      check_kind [Seq_kind] bt
      >>?= fun () ->
      check_kind [Seq_kind] bf
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let annot = gen_access_annot option_annot default_some_annot in
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bt rest
      >>=? fun (btr, ctxt) ->
      let stack_ty = Item_t (t, rest, annot) in
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bf stack_ty
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let ifnone =
          {
            apply =
              (fun kinfo k ->
                let btinfo = kinfo_of_descr ibt
                and bfinfo = kinfo_of_descr ibf in
                let branch_if_none = ibt.instr.apply btinfo k
                and branch_if_some = ibf.instr.apply bfinfo k in
                IIf_none {kinfo; branch_if_none; branch_if_some});
          }
        in
        {loc; instr = ifnone; bef; aft = ibt.aft}
      in
      merge_branches ~legacy ctxt loc btr bfr {branch}
      >>?= fun (judgement, ctxt) -> return ctxt 0 judgement
  (* pairs *)
  | ( Prim (loc, I_PAIR, [], annot),
      Item_t (a, Item_t (b, rest, snd_annot), fst_annot) ) ->
      parse_constr_annot
        loc
        annot
        ~if_special_first:(var_to_field_annot fst_annot)
        ~if_special_second:(var_to_field_annot snd_annot)
      >>?= fun (annot, ty_name, l_field, r_field) ->
      let stack_ty =
        Item_t
          ( Pair_t ((a, l_field, fst_annot), (b, r_field, snd_annot), ty_name),
            rest,
            annot )
      in
      let cons_pair = {apply = (fun kinfo k -> ICons_pair (kinfo, k))} in
      typed ctxt 1 loc cons_pair stack_ty
  | (Prim (loc, I_PAIR, [n], annot), stack_ty) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) comb_proof_argument tzresult =
       fun n stack_ty ->
        match (n, stack_ty) with
        | (1, Item_t (a_ty, tl_ty, _a_annot_opt)) ->
            ok (Comb_proof_argument (Comb_one, Item_t (a_ty, tl_ty, annot)))
        | (n, Item_t (a_ty, tl_ty, _prop_annot_opt)) ->
            make_proof_argument (n - 1) tl_ty
            >|? fun (Comb_proof_argument
                      (comb_witness, Item_t (b_ty, tl_ty', annot))) ->
            let pair_t =
              Pair_t ((a_ty, None, None), (b_ty, None, None), None)
            in
            Comb_proof_argument
              (Comb_succ comb_witness, Item_t (pair_t, tl_ty', annot))
        | _ ->
            serialize_stack_for_error ctxt stack_ty
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_PAIR, 1, whole_stack))
      in
      parse_uint10 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Pair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Comb_proof_argument (witness, after_ty)) ->
      let comb = {apply = (fun kinfo k -> IComb (kinfo, n, witness, k))} in
      typed ctxt 1 loc comb after_ty
  | (Prim (loc, I_UNPAIR, [n], annot), stack_ty) ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) uncomb_proof_argument tzresult =
       fun n stack_ty ->
        match (n, stack_ty) with
        | (1, Item_t (a_ty, tl_ty, annot)) ->
            ok
            @@ Uncomb_proof_argument (Uncomb_one, Item_t (a_ty, tl_ty, annot))
        | ( n,
            Item_t
              ( Pair_t ((a_ty, field_opt, _), (b_ty, b_field_opt, _), _),
                tl_ty,
                _ ) ) ->
            let b_annot = Script_ir_annot.field_to_var_annot b_field_opt in
            make_proof_argument (n - 1) (Item_t (b_ty, tl_ty, b_annot))
            >|? fun (Uncomb_proof_argument (uncomb_witness, after_ty)) ->
            Uncomb_proof_argument
              ( Uncomb_succ uncomb_witness,
                Item_t
                  (a_ty, after_ty, Script_ir_annot.field_to_var_annot field_opt)
              )
        | _ ->
            serialize_stack_for_error ctxt stack_ty
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_UNPAIR, 1, whole_stack))
      in
      parse_uint10 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Unpair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Uncomb_proof_argument (witness, after_ty)) ->
      let uncomb = {apply = (fun kinfo k -> IUncomb (kinfo, n, witness, k))} in
      typed ctxt 0 loc uncomb after_ty
  | (Prim (loc, I_GET, [n], annot), Item_t (comb_ty, rest_ty, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let rec make_proof_argument :
          type b. int -> b ty -> b comb_get_proof_argument tzresult =
       fun n ty ->
        match (n, ty) with
        | (0, value_ty) ->
            ok @@ Comb_get_proof_argument (Comb_get_zero, value_ty)
        | (1, Pair_t ((hd_ty, _at1, _at2), _, _annot)) ->
            ok @@ Comb_get_proof_argument (Comb_get_one, hd_ty)
        | (n, Pair_t (_, (tl_ty, _bt1, _bt2), _annot)) ->
            make_proof_argument (n - 2) tl_ty
            >|? fun (Comb_get_proof_argument (comb_get_left_witness, ty')) ->
            Comb_get_proof_argument
              (Comb_get_plus_two comb_get_left_witness, ty')
        | _ ->
            serialize_stack_for_error ctxt stack_ty
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_GET, 1, whole_stack))
      in
      parse_uint11 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      make_proof_argument n comb_ty
      >>?= fun (Comb_get_proof_argument (witness, ty')) ->
      let after_stack_ty = Item_t (ty', rest_ty, annot) in
      let comb_get =
        {apply = (fun kinfo k -> IComb_get (kinfo, n, witness, k))}
      in
      typed ctxt 0 loc comb_get after_stack_ty
  | ( Prim (loc, I_UPDATE, [n], annot),
      Item_t (value_ty, Item_t (comb_ty, rest_ty, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let rec make_proof_argument :
          type value before.
          int ->
          value ty ->
          before ty ->
          (value, before) comb_set_proof_argument tzresult =
       fun n value_ty ty ->
        match (n, ty) with
        | (0, _) ->
            ok @@ Comb_set_proof_argument (Comb_set_zero, value_ty)
        | (1, Pair_t ((_hd_ty, at1, at2), (tl_ty, bt1, bt2), annot)) ->
            let after_ty =
              Pair_t ((value_ty, at1, at2), (tl_ty, bt1, bt2), annot)
            in
            ok @@ Comb_set_proof_argument (Comb_set_one, after_ty)
        | (n, Pair_t ((hd_ty, at1, at2), (tl_ty, bt1, bt2), annot)) ->
            make_proof_argument (n - 2) value_ty tl_ty
            >|? fun (Comb_set_proof_argument (comb_set_left_witness, tl_ty')) ->
            let after_ty =
              Pair_t ((hd_ty, at1, at2), (tl_ty', bt1, bt2), annot)
            in
            Comb_set_proof_argument
              (Comb_set_plus_two comb_set_left_witness, after_ty)
        | _ ->
            serialize_stack_for_error ctxt stack_ty
            >>? fun (whole_stack, _ctxt) ->
            error (Bad_stack (loc, I_UPDATE, 2, whole_stack))
      in
      parse_uint11 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      make_proof_argument n value_ty comb_ty
      >>?= fun (Comb_set_proof_argument (witness, after_ty)) ->
      let after_stack_ty = Item_t (after_ty, rest_ty, annot) in
      let comb_set =
        {apply = (fun kinfo k -> IComb_set (kinfo, n, witness, k))}
      in
      typed ctxt 0 loc comb_set after_stack_ty
  | ( Prim (loc, I_UNPAIR, [], annot),
      Item_t
        ( Pair_t
            ( (a, expected_field_annot_a, a_annot),
              (b, expected_field_annot_b, b_annot),
              _ ),
          rest,
          pair_annot ) ) ->
      parse_unpair_annot
        loc
        annot
        ~pair_annot
        ~value_annot_car:a_annot
        ~value_annot_cdr:b_annot
        ~field_name_car:expected_field_annot_a
        ~field_name_cdr:expected_field_annot_b
      >>?= fun (annot_a, annot_b, field_a, field_b) ->
      check_correct_field field_a expected_field_annot_a
      >>?= fun () ->
      check_correct_field field_b expected_field_annot_b
      >>?= fun () ->
      let unpair = {apply = (fun kinfo k -> IUnpair (kinfo, k))} in
      typed ctxt 0 loc unpair (Item_t (a, Item_t (b, rest, annot_b), annot_a))
  | ( Prim (loc, I_CAR, [], annot),
      Item_t
        (Pair_t ((a, expected_field_annot, a_annot), _, _), rest, pair_annot)
    ) ->
      parse_destr_annot
        loc
        annot
        ~pair_annot
        ~value_annot:a_annot
        ~field_name:expected_field_annot
        ~default_accessor:default_car_annot
      >>?= fun (annot, field_annot) ->
      check_correct_field field_annot expected_field_annot
      >>?= fun () ->
      let car = {apply = (fun kinfo k -> ICar (kinfo, k))} in
      typed ctxt 0 loc car (Item_t (a, rest, annot))
  | ( Prim (loc, I_CDR, [], annot),
      Item_t
        (Pair_t (_, (b, expected_field_annot, b_annot), _), rest, pair_annot)
    ) ->
      parse_destr_annot
        loc
        annot
        ~pair_annot
        ~value_annot:b_annot
        ~field_name:expected_field_annot
        ~default_accessor:default_cdr_annot
      >>?= fun (annot, field_annot) ->
      check_correct_field field_annot expected_field_annot
      >>?= fun () ->
      let cdr = {apply = (fun kinfo k -> ICdr (kinfo, k))} in
      typed ctxt 0 loc cdr (Item_t (b, rest, annot))
  (* unions *)
  | (Prim (loc, I_LEFT, [tr], annot), Item_t (tl, rest, stack_annot)) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tr
      >>?= fun (Ex_ty tr, ctxt) ->
      parse_constr_annot
        loc
        annot
        ~if_special_first:(var_to_field_annot stack_annot)
      >>?= fun (annot, tname, l_field, r_field) ->
      let cons_left = {apply = (fun kinfo k -> ICons_left (kinfo, k))} in
      let stack_ty =
        Item_t (Union_t ((tl, l_field), (tr, r_field), tname), rest, annot)
      in
      typed ctxt 1 loc cons_left stack_ty
  | (Prim (loc, I_RIGHT, [tl], annot), Item_t (tr, rest, stack_annot)) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tl
      >>?= fun (Ex_ty tl, ctxt) ->
      parse_constr_annot
        loc
        annot
        ~if_special_second:(var_to_field_annot stack_annot)
      >>?= fun (annot, tname, l_field, r_field) ->
      let cons_right = {apply = (fun kinfo k -> ICons_right (kinfo, k))} in
      let stack_ty =
        Item_t (Union_t ((tl, l_field), (tr, r_field), tname), rest, annot)
      in
      typed ctxt 1 loc cons_right stack_ty
  | ( Prim (loc, I_IF_LEFT, [bt; bf], annot),
      ( Item_t (Union_t ((tl, l_field), (tr, r_field), _), rest, union_annot)
      as bef ) ) ->
      check_kind [Seq_kind] bt
      >>?= fun () ->
      check_kind [Seq_kind] bf
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let left_annot =
        gen_access_annot union_annot l_field ~default:default_left_annot
      in
      let right_annot =
        gen_access_annot union_annot r_field ~default:default_right_annot
      in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bt
        (Item_t (tl, rest, left_annot))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bf
        (Item_t (tr, rest, right_annot))
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let infobt = kinfo_of_descr ibt and infobf = kinfo_of_descr ibf in
        let instr =
          {
            apply =
              (fun kinfo k ->
                let branch_if_left = ibt.instr.apply infobt k
                and branch_if_right = ibf.instr.apply infobf k in
                IIf_left {kinfo; branch_if_left; branch_if_right});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      merge_branches ~legacy ctxt loc btr bfr {branch}
      >>?= fun (judgement, ctxt) -> return ctxt 0 judgement
  (* lists *)
  | (Prim (loc, I_NIL, [t], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let nil = {apply = (fun kinfo k -> INil (kinfo, k))} in
      typed ctxt 1 loc nil (Item_t (List_t (t, ty_name), stack, annot))
  | ( Prim (loc, I_CONS, [], annot),
      Item_t (tv, Item_t (List_t (t, ty_name), rest, _), _) ) ->
      check_item_ty ctxt tv t loc I_CONS 1 2
      >>?= fun (Eq, t, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let cons_list = {apply = (fun kinfo k -> ICons_list (kinfo, k))} in
      ( typed ctxt 0 loc cons_list (Item_t (List_t (t, ty_name), rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_IF_CONS, [bt; bf], annot),
      (Item_t (List_t (t, ty_name), rest, list_annot) as bef) ) ->
      check_kind [Seq_kind] bt
      >>?= fun () ->
      check_kind [Seq_kind] bf
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let hd_annot = gen_access_annot list_annot default_hd_annot in
      let tl_annot = gen_access_annot list_annot default_tl_annot in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bt
        (Item_t (t, Item_t (List_t (t, ty_name), rest, tl_annot), hd_annot))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bf rest
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let infobt = kinfo_of_descr ibt and infobf = kinfo_of_descr ibf in
        let instr =
          {
            apply =
              (fun kinfo k ->
                let branch_if_cons = ibt.instr.apply infobt k
                and branch_if_nil = ibf.instr.apply infobf k in
                IIf_cons {kinfo; branch_if_nil; branch_if_cons});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      merge_branches ~legacy ctxt loc btr bfr {branch}
      >>?= fun (judgement, ctxt) -> return ctxt 0 judgement
  | (Prim (loc, I_SIZE, [], annot), Item_t (List_t _, rest, _)) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, tname) ->
      let list_size = {apply = (fun kinfo k -> IList_size (kinfo, k))} in
      typed ctxt 0 loc list_size (Item_t (Nat_t tname, rest, annot))
  | ( Prim (loc, I_MAP, [body], annot),
      Item_t (List_t (elt, _), starting_rest, list_annot) ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      parse_var_type_annot loc annot
      >>?= fun (ret_annot, list_ty_name) ->
      let elt_annot = gen_access_annot list_annot default_elt_annot in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, starting_rest, elt_annot))
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed ({aft = Item_t (ret, rest, _); _} as kibody) ->
          let invalid_map_body () =
            serialize_stack_for_error ctxt kibody.aft
            >|? fun (aft, _ctxt) -> Invalid_map_body (loc, aft)
          in
          Lwt.return
          @@ record_trace_eval
               invalid_map_body
               ( merge_stacks ~legacy loc ctxt 1 rest starting_rest
               >>? fun (Eq, rest, ctxt) ->
               let binfo = kinfo_of_descr kibody in
               let hinfo =
                 {iloc = loc; kstack_ty = Item_t (ret, rest, ret_annot)}
               in
               let ibody = kibody.instr.apply binfo (IHalt hinfo) in
               let list_map =
                 {apply = (fun kinfo k -> IList_map (kinfo, ibody, k))}
               in
               let stack =
                 Item_t (List_t (ret, list_ty_name), rest, ret_annot)
               in
               typed_no_lwt ctxt 1 loc list_map stack )
      | Typed {aft; _} ->
          Lwt.return
            ( serialize_stack_for_error ctxt aft
            >>? fun (aft, _ctxt) -> error (Invalid_map_body (loc, aft)) )
      | Failed _ ->
          fail (Invalid_map_block_fail loc) )
  | ( Prim (loc, I_ITER, [body], annot),
      Item_t (List_t (elt, _), rest, list_annot) ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let elt_annot = gen_access_annot list_annot default_elt_annot in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, rest, elt_annot))
      >>=? fun (judgement, ctxt) ->
      let mk_list_iter ibody =
        {
          apply =
            (fun kinfo k ->
              let hinfo = {iloc = loc; kstack_ty = rest} in
              let binfo = kinfo_of_descr ibody in
              let ibody = ibody.instr.apply binfo (IHalt hinfo) in
              IList_iter (kinfo, ibody, k));
        }
      in
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            serialize_stack_for_error ctxt ibody.aft
            >>? fun (aft, ctxt) ->
            serialize_stack_for_error ctxt rest
            >|? fun (rest, _ctxt) -> Invalid_iter_body (loc, rest, aft)
          in
          Lwt.return
          @@ record_trace_eval
               invalid_iter_body
               ( merge_stacks ~legacy loc ctxt 1 aft rest
               >>? fun (Eq, rest, ctxt) ->
               ( typed_no_lwt ctxt 0 loc (mk_list_iter ibody) rest
                 : ((a, s) judgement * context) tzresult ) )
      | Failed {descr} ->
          typed ctxt 0 loc (mk_list_iter (descr rest)) rest )
  (* sets *)
  | (Prim (loc, I_EMPTY_SET, [t], annot), rest) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt t
      >>?= fun (Ex_comparable_ty t, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, tname) ->
      let instr = {apply = (fun kinfo k -> IEmpty_set (kinfo, t, k))} in
      typed ctxt 1 loc instr (Item_t (Set_t (t, tname), rest, annot))
  | ( Prim (loc, I_ITER, [body], annot),
      Item_t (Set_t (comp_elt, _), rest, set_annot) ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let elt_annot = gen_access_annot set_annot default_elt_annot in
      let elt = ty_of_comparable_ty comp_elt in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, rest, elt_annot))
      >>=? fun (judgement, ctxt) ->
      let mk_iset_iter ibody =
        {
          apply =
            (fun kinfo k ->
              let hinfo = {iloc = loc; kstack_ty = rest} in
              let binfo = kinfo_of_descr ibody in
              let ibody = ibody.instr.apply binfo (IHalt hinfo) in
              ISet_iter (kinfo, ibody, k));
        }
      in
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            serialize_stack_for_error ctxt ibody.aft
            >>? fun (aft, ctxt) ->
            serialize_stack_for_error ctxt rest
            >|? fun (rest, _ctxt) -> Invalid_iter_body (loc, rest, aft)
          in
          Lwt.return
          @@ record_trace_eval
               invalid_iter_body
               ( merge_stacks ~legacy loc ctxt 1 aft rest
               >>? fun (Eq, rest, ctxt) ->
               ( typed_no_lwt ctxt 0 loc (mk_iset_iter ibody) rest
                 : ((a, s) judgement * context) tzresult ) )
      | Failed {descr} ->
          typed ctxt 0 loc (mk_iset_iter (descr rest)) rest )
  | ( Prim (loc, I_MEM, [], annot),
      Item_t (v, Item_t (Set_t (elt, _), rest, _), _) ) ->
      let elt = ty_of_comparable_ty elt in
      parse_var_type_annot loc annot
      >>?= fun (annot, tname) ->
      check_item_ty ctxt elt v loc I_MEM 1 2
      >>?= fun (Eq, _, ctxt) ->
      let instr = {apply = (fun kinfo k -> ISet_mem (kinfo, k))} in
      ( typed ctxt 0 loc instr (Item_t (Bool_t tname, rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( v,
          Item_t (Bool_t _, Item_t (Set_t (elt, tname), rest, set_annot), _),
          _ ) ) ->
      check_item_ty ctxt (ty_of_comparable_ty elt) v loc I_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot ~default:set_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISet_update (kinfo, k))} in
      ( typed ctxt 0 loc instr (Item_t (Set_t (elt, tname), rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | (Prim (loc, I_SIZE, [], annot), Item_t (Set_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISet_size (kinfo, k))} in
      typed ctxt 0 loc instr (Item_t (Nat_t None, rest, annot))
  (* maps *)
  | (Prim (loc, I_EMPTY_MAP, [tk; tv], annot), stack) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let instr = {apply = (fun kinfo k -> IEmpty_map (kinfo, tk, tv, k))} in
      typed ctxt 1 loc instr (Item_t (Map_t (tk, tv, ty_name), stack, annot))
  | ( Prim (loc, I_MAP, [body], annot),
      Item_t (Map_t (ck, elt, _), starting_rest, _map_annot) ) -> (
      let k = ty_of_comparable_ty ck in
      check_kind [Seq_kind] body
      >>?= fun () ->
      parse_var_type_annot loc annot
      >>?= fun (ret_annot, ty_name) ->
      let k_name = field_to_var_annot default_key_annot in
      let e_name = field_to_var_annot default_elt_annot in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t
           ( Pair_t ((k, None, k_name), (elt, None, e_name), None),
             starting_rest,
             None ))
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed ({aft = Item_t (ret, rest, _); _} as ibody) ->
          let invalid_map_body () =
            serialize_stack_for_error ctxt ibody.aft
            >|? fun (aft, _ctxt) -> Invalid_map_body (loc, aft)
          in
          Lwt.return
          @@ record_trace_eval
               invalid_map_body
               ( merge_stacks ~legacy loc ctxt 1 rest starting_rest
               >>? fun (Eq, rest, ctxt) ->
               let instr =
                 {
                   apply =
                     (fun kinfo k ->
                       let binfo = kinfo_of_descr ibody in
                       let hinfo =
                         {
                           iloc = loc;
                           kstack_ty = Item_t (ret, rest, ret_annot);
                         }
                       in
                       let ibody = ibody.instr.apply binfo (IHalt hinfo) in
                       IMap_map (kinfo, ibody, k));
                 }
               in
               let stack =
                 Item_t (Map_t (ck, ret, ty_name), rest, ret_annot)
               in
               typed_no_lwt ctxt 1 loc instr stack )
      | Typed {aft; _} ->
          Lwt.return
            ( serialize_stack_for_error ctxt aft
            >>? fun (aft, _ctxt) -> error (Invalid_map_body (loc, aft)) )
      | Failed _ ->
          fail (Invalid_map_block_fail loc) )
  | ( Prim (loc, I_ITER, [body], annot),
      Item_t (Map_t (comp_elt, element_ty, _), rest, _map_annot) ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let k_name = field_to_var_annot default_key_annot in
      let e_name = field_to_var_annot default_elt_annot in
      let key = ty_of_comparable_ty comp_elt in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t
           ( Pair_t ((key, None, k_name), (element_ty, None, e_name), None),
             rest,
             None ))
      >>=? fun (judgement, ctxt) ->
      let make_instr ibody =
        {
          apply =
            (fun kinfo k ->
              let hinfo = {iloc = loc; kstack_ty = rest} in
              let binfo = kinfo_of_descr ibody in
              let ibody = ibody.instr.apply binfo (IHalt hinfo) in
              IMap_iter (kinfo, ibody, k));
        }
      in
      match judgement with
      | Typed ({aft; _} as ibody) ->
          let invalid_iter_body () =
            serialize_stack_for_error ctxt ibody.aft
            >>? fun (aft, ctxt) ->
            serialize_stack_for_error ctxt rest
            >|? fun (rest, _ctxt) -> Invalid_iter_body (loc, rest, aft)
          in
          Lwt.return
          @@ record_trace_eval
               invalid_iter_body
               ( merge_stacks ~legacy loc ctxt 1 aft rest
               >>? fun (Eq, rest, ctxt) ->
               ( typed_no_lwt ctxt 0 loc (make_instr ibody) rest
                 : ((a, s) judgement * context) tzresult ) )
      | Failed {descr} ->
          typed ctxt 0 loc (make_instr (descr rest)) rest )
  | ( Prim (loc, I_MEM, [], annot),
      Item_t (vk, Item_t (Map_t (ck, _, _), rest, _), _) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_MEM 1 2
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMap_mem (kinfo, k))} in
      ( typed ctxt 0 loc instr (Item_t (Bool_t None, rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Map_t (ck, elt, _), rest, _), _) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET 1 2
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMap_get (kinfo, k))} in
      ( typed ctxt 0 loc instr (Item_t (Option_t (elt, None), rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t
            ( Option_t (vv, _),
              Item_t (Map_t (ck, v, map_name), rest, map_annot),
              _ ),
          _ ) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_UPDATE 2 3
      >>?= fun (Eq, v, ctxt) ->
      parse_var_annot loc annot ~default:map_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMap_update (kinfo, k))} in
      ( typed ctxt 0 loc instr (Item_t (Map_t (ck, v, map_name), rest, annot))
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t
            ( Option_t (vv, vname),
              Item_t (Map_t (ck, v, map_name), rest, map_annot),
              v_annot ),
          _ ) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3
      >>?= fun (Eq, v, ctxt) ->
      parse_var_annot loc annot ~default:map_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMap_get_and_update (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t (vv, vname),
            Item_t (Map_t (ck, v, map_name), rest, annot),
            v_annot )
      in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | (Prim (loc, I_SIZE, [], annot), Item_t (Map_t (_, _, _), rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMap_size (kinfo, k))} in
      typed ctxt 0 loc instr (Item_t (Nat_t None, rest, annot))
  (* big_map *)
  | (Prim (loc, I_EMPTY_BIG_MAP, [tk; tv], annot), stack) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_big_map_value_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let instr =
        {apply = (fun kinfo k -> IEmpty_big_map (kinfo, tk, tv, k))}
      in
      let stack = Item_t (Big_map_t (tk, tv, ty_name), stack, annot) in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_MEM, [], annot),
      Item_t (set_key, Item_t (Big_map_t (map_key, _, _), rest, _), _) ) ->
      let k = ty_of_comparable_ty map_key in
      check_item_ty ctxt set_key k loc I_MEM 1 2
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBig_map_mem (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Big_map_t (ck, elt, _), rest, _), _) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET 1 2
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBig_map_get (kinfo, k))} in
      let stack = Item_t (Option_t (elt, None), rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( set_key,
          Item_t
            ( Option_t (set_value, _),
              Item_t (Big_map_t (map_key, map_value, map_name), rest, map_annot),
              _ ),
          _ ) ) ->
      let k = ty_of_comparable_ty map_key in
      check_item_ty ctxt set_key k loc I_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt set_value map_value loc I_UPDATE 2 3
      >>?= fun (Eq, map_value, ctxt) ->
      parse_var_annot loc annot ~default:map_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBig_map_update (kinfo, k))} in
      let stack =
        Item_t (Big_map_t (map_key, map_value, map_name), rest, annot)
      in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t
            ( Option_t (vv, vname),
              Item_t (Big_map_t (ck, v, map_name), rest, map_annot),
              v_annot ),
          _ ) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3
      >>?= fun (Eq, v, ctxt) ->
      parse_var_annot loc annot ~default:map_annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> IBig_map_get_and_update (kinfo, k))}
      in
      let stack =
        Item_t
          ( Option_t (vv, vname),
            Item_t (Big_map_t (ck, v, map_name), rest, annot),
            v_annot )
      in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  (* Sapling *)
  | (Prim (loc, I_SAPLING_EMPTY_STATE, [memo_size], annot), rest) ->
      parse_memo_size memo_size
      >>?= fun memo_size ->
      parse_var_annot loc annot ~default:default_sapling_state_annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> ISapling_empty_state (kinfo, memo_size, k))}
      in
      let stack = Item_t (Sapling_state_t (memo_size, None), rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SAPLING_VERIFY_UPDATE, [], _),
      Item_t
        ( Sapling_transaction_t (transaction_memo_size, _),
          Item_t
            ( (Sapling_state_t (state_memo_size, _) as state_ty),
              rest,
              stack_annot ),
          _ ) ) ->
      merge_memo_sizes state_memo_size transaction_memo_size
      >>?= fun _memo_size ->
      let instr =
        {apply = (fun kinfo k -> ISapling_verify_update (kinfo, k))}
      in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ( (Int_t None, None, default_sapling_balance_annot),
                    (state_ty, None, None),
                    None ),
                None ),
            rest,
            stack_annot )
      in
      typed ctxt 0 loc instr stack
  (* control *)
  | (Seq (loc, []), stack) ->
      let instr = {apply = (fun _kinfo k -> k)} in
      typed ctxt 0 loc instr stack
  | (Seq (_, [single]), stack) ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy single stack
  | (Seq (loc, hd :: tl), stack) -> (
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy hd stack
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Failed _ ->
          fail (Fail_not_in_tail_position (Micheline.location hd))
      | Typed ({aft = middle; _} as ihd) -> (
          non_terminal_recursion
            ?type_logger
            tc_context
            ctxt
            ~legacy
            (Seq (-1, tl))
            middle
          >>=? fun (judgement, ctxt) ->
          match judgement with
          | Failed {descr} ->
              let descr ret = compose_descr loc ihd (descr ret) in
              return ctxt 0 (Failed {descr})
          | Typed itl ->
              ( Lwt.return (Ok (Typed (compose_descr loc ihd itl), ctxt))
                : ((a, s) judgement * context) tzresult Lwt.t ) ) )
  | (Prim (loc, I_IF, [bt; bf], annot), (Item_t (Bool_t _, rest, _) as bef)) ->
      check_kind [Seq_kind] bt
      >>?= fun () ->
      check_kind [Seq_kind] bf
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bt rest
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bf rest
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let infobt = kinfo_of_descr ibt and infobf = kinfo_of_descr ibf in
        let instr =
          {
            apply =
              (fun kinfo k ->
                let branch_if_true = ibt.instr.apply infobt k
                and branch_if_false = ibf.instr.apply infobf k in
                IIf {kinfo; branch_if_true; branch_if_false});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      merge_branches ~legacy ctxt loc btr bfr {branch}
      >>?= fun (judgement, ctxt) -> return ctxt 0 judgement
  | ( Prim (loc, I_LOOP, [body], annot),
      (Item_t (Bool_t _, rest, _stack_annot) as stack) ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy body rest
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed ibody ->
          let unmatched_branches () =
            serialize_stack_for_error ctxt ibody.aft
            >>? fun (aft, ctxt) ->
            serialize_stack_for_error ctxt stack
            >|? fun (stack, _ctxt) -> Unmatched_branches (loc, aft, stack)
          in
          Lwt.return
          @@ record_trace_eval
               unmatched_branches
               ( merge_stacks ~legacy loc ctxt 1 ibody.aft stack
               >>? fun (Eq, _stack, ctxt) ->
               let instr =
                 {
                   apply =
                     (fun kinfo k ->
                       let ibody =
                         ibody.instr.apply (kinfo_of_descr ibody) (IHalt kinfo)
                       in
                       ILoop (kinfo, ibody, k));
                 }
               in
               typed_no_lwt ctxt 0 loc instr rest )
      | Failed {descr} ->
          let instr =
            {
              apply =
                (fun kinfo k ->
                  let ibody = descr stack in
                  let ibody =
                    ibody.instr.apply (kinfo_of_descr ibody) (IHalt kinfo)
                  in
                  ILoop (kinfo, ibody, k));
            }
          in
          typed ctxt loc 0 instr rest )
  | ( Prim (loc, I_LOOP_LEFT, [body], annot),
      (Item_t (Union_t ((tl, l_field), (tr, _), _), rest, union_annot) as stack)
    ) -> (
      check_kind [Seq_kind] body
      >>?= fun () ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let l_annot =
        gen_access_annot union_annot l_field ~default:default_left_annot
      in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (tl, rest, l_annot))
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed ibody ->
          let unmatched_branches () =
            serialize_stack_for_error ctxt ibody.aft
            >>? fun (aft, ctxt) ->
            serialize_stack_for_error ctxt stack
            >|? fun (stack, _ctxt) -> Unmatched_branches (loc, aft, stack)
          in
          Lwt.return
          @@ record_trace_eval
               unmatched_branches
               ( merge_stacks ~legacy loc ctxt 1 ibody.aft stack
               >>? fun (Eq, _stack, ctxt) ->
               let instr =
                 {
                   apply =
                     (fun kinfo k ->
                       let ibody =
                         ibody.instr.apply (kinfo_of_descr ibody) (IHalt kinfo)
                       in
                       ILoop_left (kinfo, ibody, k));
                 }
               in
               let stack = Item_t (tr, rest, annot) in
               typed_no_lwt ctxt 0 loc instr stack )
      | Failed {descr} ->
          let instr =
            {
              apply =
                (fun kinfo k ->
                  let ibody = descr stack in
                  let ibody =
                    ibody.instr.apply (kinfo_of_descr ibody) (IHalt kinfo)
                  in
                  ILoop_left (kinfo, ibody, k));
            }
          in
          let stack = Item_t (tr, rest, annot) in
          typed ctxt 0 loc instr stack )
  | (Prim (loc, I_LAMBDA, [arg; ret; code], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy arg
      >>?= fun (Ex_ty arg, ctxt) ->
      check_type_size ~legacy ctxt ~loc arg
      >>?= fun () ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ret
      >>?= fun (Ex_ty ret, ctxt) ->
      check_type_size ~legacy ctxt ~loc ret
      >>?= fun () ->
      check_kind [Seq_kind] code
      >>?= fun () ->
      parse_var_annot loc annot
      >>?= fun annot ->
      parse_returning
        Lambda
        ?type_logger
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~legacy
        (arg, default_arg_annot)
        ret
        code
      >>=? fun (lambda, ctxt) ->
      let instr = {apply = (fun kinfo k -> ILambda (kinfo, lambda, k))} in
      let stack = Item_t (Lambda_t (arg, ret, None), stack, annot) in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_EXEC, [], annot),
      Item_t (arg, Item_t (Lambda_t (param, ret, _), rest, _), _) ) ->
      check_item_ty ctxt arg param loc I_EXEC 1 2
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IExec (kinfo, k))} in
      let stack = Item_t (ret, rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_APPLY, [], annot),
      Item_t
        ( capture,
          Item_t
            ( Lambda_t
                (Pair_t ((capture_ty, _, _), (arg_ty, _, _), lam_annot), ret, _),
              rest,
              _ ),
          _ ) ) ->
      check_packable ~legacy:false loc capture_ty
      >>?= fun () ->
      check_item_ty ctxt capture capture_ty loc I_APPLY 1 2
      >>?= fun (Eq, capture_ty, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IApply (kinfo, capture_ty, k))} in
      let stack = Item_t (Lambda_t (arg_ty, ret, lam_annot), rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | (Prim (loc, I_DIP, [code], annot), Item_t (v, rest, stack_annot)) -> (
      error_unexpected_annot loc annot
      >>?= fun () ->
      check_kind [Seq_kind] code
      >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        (add_dip v stack_annot tc_context)
        ctxt
        ~legacy
        code
        rest
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Typed descr ->
          let instr =
            {
              apply =
                (fun kinfo k ->
                  let binfo = {iloc = descr.loc; kstack_ty = descr.bef} in
                  let kinfoh = {iloc = descr.loc; kstack_ty = descr.aft} in
                  let b = descr.instr.apply binfo (IHalt kinfoh) in
                  IDip (kinfo, b, k));
            }
          in
          let stack = Item_t (v, descr.aft, stack_annot) in
          typed ctxt 0 loc instr stack
      | Failed _ ->
          fail (Fail_not_in_tail_position loc) )
  | (Prim (loc, I_DIP, [n; code], result_annot), stack) ->
      parse_uint10 n
      >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n)
      >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int ->
          tc_context ->
          (a, s) stack_ty ->
          (a, s) dipn_proof_argument tzresult Lwt.t =
       fun n inner_tc_context stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) -> (
            non_terminal_recursion
              ?type_logger
              inner_tc_context
              ctxt
              ~legacy
              code
              rest
            >>=? fun (judgement, ctxt) ->
            match judgement with
            | Typed descr ->
                Lwt.return
                  (ok
                     ( Dipn_proof_argument (KRest, ctxt, descr, descr.aft)
                       : (a, s) dipn_proof_argument ))
            | Failed _ ->
                Lwt.return (error (Fail_not_in_tail_position loc)) )
        | (false, Item_t (v, rest, annot)) ->
            make_proof_argument (n - 1) (add_dip v annot tc_context) rest
            >|=? fun (Dipn_proof_argument (n', ctxt, descr, aft')) ->
            let kinfo' = {iloc = loc; kstack_ty = aft'} in
            let w = KPrefix (kinfo', n') in
            Dipn_proof_argument (w, ctxt, descr, Item_t (v, aft', annot))
        | (_, _) ->
            Lwt.return
              ( serialize_stack_for_error ctxt stack
              >>? fun (whole_stack, _ctxt) ->
              error (Bad_stack (loc, I_DIP, 1, whole_stack)) )
      in
      error_unexpected_annot loc result_annot
      >>?= fun () ->
      make_proof_argument n tc_context stack
      >>=? fun (Dipn_proof_argument (n', ctxt, descr, aft)) ->
      let kinfo = {iloc = descr.loc; kstack_ty = descr.bef} in
      let kinfoh = {iloc = descr.loc; kstack_ty = descr.aft} in
      let b = descr.instr.apply kinfo (IHalt kinfoh) in
      let res = {apply = (fun kinfo k -> IDipn (kinfo, n, n', b, k))} in
      typed ctxt 0 loc res aft
  | (Prim (loc, I_DIP, (([] | _ :: _ :: _ :: _) as l), _), _) ->
      (* Technically, the arities 1 and 2 are allowed but the error only mentions 2.
            However, DIP {code} is equivalent to DIP 1 {code} so hinting at an arity of 2 makes sense. *)
      fail (Invalid_arity (loc, I_DIP, 2, List.length l))
  | (Prim (loc, I_FAILWITH, [], annot), Item_t (v, _rest, _)) ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      (if legacy then ok_unit else check_packable ~legacy:false loc v)
      >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IFailwith (kinfo, loc, v, k))} in
      let descr aft = {loc; instr; bef = stack_ty; aft} in
      log_stack ctxt loc stack_ty Bot_t
      >>?= fun () -> return ctxt 0 (Failed {descr})
  | (Prim (loc, I_NEVER, [], annot), Item_t (Never_t _, _rest, _)) ->
      error_unexpected_annot loc annot
      >>?= fun () ->
      let instr = {apply = (fun kinfo _k -> INever kinfo)} in
      let descr aft = {loc; instr; bef = stack_ty; aft} in
      log_stack ctxt loc stack_ty Bot_t
      >>?= fun () -> return ctxt 0 (Failed {descr})
  (* timestamp operations *)
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> IAdd_timestamp_to_seconds (kinfo, k))}
      in
      typed ctxt 0 loc instr (Item_t (Timestamp_t tname, rest, annot))
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Int_t _, Item_t (Timestamp_t tname, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> IAdd_seconds_to_timestamp (kinfo, k))}
      in
      typed ctxt 0 loc instr (Item_t (Timestamp_t tname, rest, annot))
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> ISub_timestamp_seconds (kinfo, k))}
      in
      let stack = Item_t (Timestamp_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tn1, Item_t (Timestamp_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IDiff_timestamps (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  (* string operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (String_t tn1, Item_t (String_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IConcat_string_pair (kinfo, k))} in
      typed ctxt 0 loc instr (Item_t (String_t tname, rest, annot))
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (List_t (String_t tname, _), rest, list_annot) ) ->
      parse_var_annot ~default:list_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IConcat_string (kinfo, k))} in
      typed ctxt 0 loc instr (Item_t (String_t tname, rest, annot))
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t
        ( Nat_t _,
          Item_t (Nat_t _, Item_t (String_t tname, rest, string_annot), _),
          _ ) ) ->
      parse_var_annot
        ~default:(gen_access_annot string_annot default_slice_annot)
        loc
        annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISlice_string (kinfo, k))} in
      let stack = Item_t (Option_t (String_t tname, None), rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SIZE, [], annot), Item_t (String_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IString_size (kinfo, k))} in
      let stack = Item_t (Nat_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  (* bytes operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (Bytes_t tn1, Item_t (Bytes_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IConcat_bytes_pair (kinfo, k))} in
      let stack = Item_t (Bytes_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (List_t (Bytes_t tname, _), rest, list_annot) ) ->
      parse_var_annot ~default:list_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IConcat_bytes (kinfo, k))} in
      let stack = Item_t (Bytes_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t
        ( Nat_t _,
          Item_t (Nat_t _, Item_t (Bytes_t tname, rest, bytes_annot), _),
          _ ) ) ->
      parse_var_annot
        ~default:(gen_access_annot bytes_annot default_slice_annot)
        loc
        annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISlice_bytes (kinfo, k))} in
      let stack = Item_t (Option_t (Bytes_t tname, None), rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SIZE, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBytes_size (kinfo, k))} in
      let stack = Item_t (Nat_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  (* currency operations *)
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_tez (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ISub_tez (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      (* no type name check *)
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_teznat (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Mutez_t tname, rest, _), _) ) ->
      (* no type name check *)
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_nattez (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  (* boolean operations *)
  | ( Prim (loc, I_OR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IOr (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_AND, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAnd (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_XOR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IXor (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Bool_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INot (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  (* integer operations *)
  | (Prim (loc, I_ABS, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAbs_int (kinfo, k))} in
      let stack = Item_t (Nat_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_ISNAT, [], annot), Item_t (Int_t _, rest, int_annot)) ->
      parse_var_annot loc annot ~default:int_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IIs_nat (kinfo, k))} in
      let stack = Item_t (Option_t (Nat_t None, None), rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_INT, [], annot), Item_t (Nat_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IInt_nat (kinfo, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Int_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeg_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Nat_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeg_nat (kinfo, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_intint (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAdd_intnat (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAdd_natint (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_natnat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun _tname ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IMul_intint (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_intnat (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Int_t tname, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_natint (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IMul_natnat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IEdiv_teznat (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ( (Mutez_t tname, None, None),
                    (Mutez_t tname, None, None),
                    None ),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IEdiv_tez (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ((Nat_t None, None, None), (Mutez_t tname, None, None), None),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tn1, Item_t (Int_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IEdiv_intint (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ((Int_t tname, None, None), (Nat_t None, None, None), None),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Int_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IEdiv_intnat (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ((Int_t tname, None, None), (Nat_t None, None, None), None),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tname, Item_t (Int_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IEdiv_natint (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ((Int_t None, None, None), (Nat_t tname, None, None), None),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_EDIV, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IEdiv_natnat (kinfo, k))} in
      let stack =
        Item_t
          ( Option_t
              ( Pair_t
                  ((Nat_t tname, None, None), (Nat_t tname, None, None), None),
                None ),
            rest,
            annot )
      in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_LSL, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ILsl_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_LSR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ILsr_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_OR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IOr_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_AND, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAnd_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_AND, [], annot),
      Item_t (Int_t _, Item_t (Nat_t tname, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAnd_int_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_XOR, [], annot),
      Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IXor_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Int_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INot_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Nat_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INot_nat (kinfo, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  (* comparison *)
  | (Prim (loc, I_COMPARE, [], annot), Item_t (t1, Item_t (t2, rest, _), _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      check_item_ty ctxt t1 t2 loc I_COMPARE 1 2
      >>?= fun (Eq, t, ctxt) ->
      comparable_ty_of_ty ctxt loc t
      >>?= fun (key, ctxt) ->
      let instr = {apply = (fun kinfo k -> ICompare (kinfo, key, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  (* comparators *)
  | (Prim (loc, I_EQ, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IEq (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEQ, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeq (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_LT, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ILt (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_GT, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IGt (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_LE, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ILe (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_GE, [], annot), Item_t (Int_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IGe (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  (* annotations *)
  | (Prim (loc, I_CAST, [cast_t], annot), Item_t (t, stack, item_annot)) ->
      parse_var_annot loc annot ~default:item_annot
      >>?= fun annot ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy cast_t
      >>?= fun (Ex_ty cast_t, ctxt) ->
      merge_types ~legacy ctxt loc cast_t t
      >>?= fun (Eq, _, ctxt) ->
      let instr = {apply = (fun _ k -> k)} in
      let stack = Item_t (cast_t, stack, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | (Prim (loc, I_RENAME, [], annot), Item_t (t, stack, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      (* can erase annot *)
      let instr = {apply = (fun _ k -> k)} in
      let stack = Item_t (t, stack, annot) in
      typed ctxt 0 loc instr stack
  (* packing *)
  | (Prim (loc, I_PACK, [], annot), Item_t (t, rest, unpacked_annot)) ->
      check_packable
        ~legacy:true
        (* allow to pack contracts for hash/signature checks *) loc
        t
      >>?= fun () ->
      parse_var_annot
        loc
        annot
        ~default:(gen_access_annot unpacked_annot default_pack_annot)
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IPack (kinfo, t, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_UNPACK, [ty], annot), Item_t (Bytes_t _, rest, packed_annot))
    ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      parse_var_type_annot loc annot
      >>?= fun (annot, ty_name) ->
      let annot =
        default_annot
          annot
          ~default:(gen_access_annot packed_annot default_unpack_annot)
      in
      let instr = {apply = (fun kinfo k -> IUnpack (kinfo, t, k))} in
      let stack = Item_t (Option_t (t, ty_name), rest, annot) in
      typed ctxt 1 loc instr stack
  (* protocol *)
  | ( Prim (loc, I_ADDRESS, [], annot),
      Item_t (Contract_t _, rest, contract_annot) ) ->
      parse_var_annot
        loc
        annot
        ~default:(gen_access_annot contract_annot default_addr_annot)
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAddress (kinfo, k))} in
      let stack = Item_t (Address_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_CONTRACT, [ty], annot),
      Item_t (Address_t _, rest, addr_annot) ) ->
      parse_parameter_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      parse_entrypoint_annot
        loc
        annot
        ~default:(gen_access_annot addr_annot default_contract_annot)
      >>?= fun (annot, entrypoint) ->
      ( match entrypoint with
      | None ->
          Ok "default"
      | Some (Field_annot "default") ->
          error (Unexpected_annotation loc)
      | Some (Field_annot entrypoint) ->
          if Compare.Int.(String.length entrypoint > 31) then
            error (Entrypoint_name_too_long entrypoint)
          else Ok entrypoint )
      >>?= fun entrypoint ->
      let instr =
        {apply = (fun kinfo k -> IContract (kinfo, t, entrypoint, k))}
      in
      let stack =
        Item_t (Option_t (Contract_t (t, None), None), rest, annot)
      in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_TRANSFER_TOKENS, [], annot),
      Item_t (p, Item_t (Mutez_t _, Item_t (Contract_t (cp, _), rest, _), _), _)
    ) ->
      check_item_ty ctxt p cp loc I_TRANSFER_TOKENS 1 4
      >>?= fun (Eq, _, ctxt) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ITransfer_tokens (kinfo, k))} in
      let stack = Item_t (Operation_t None, rest, annot) in
      ( typed ctxt 0 loc instr stack
        : ((a, s) judgement * context) tzresult Lwt.t )
  | ( Prim (loc, I_SET_DELEGATE, [], annot),
      Item_t (Option_t (Key_hash_t _, _), rest, _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISet_delegate (kinfo, k))} in
      let stack = Item_t (Operation_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (_, I_CREATE_ACCOUNT, _, _), _) ->
      fail (Deprecated_instruction I_CREATE_ACCOUNT)
  | (Prim (loc, I_IMPLICIT_ACCOUNT, [], annot), Item_t (Key_hash_t _, rest, _))
    ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IImplicit_account (kinfo, k))} in
      let stack = Item_t (Contract_t (Unit_t None, None), rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_CREATE_CONTRACT, [(Seq _ as code)], annot),
      Item_t
        ( Option_t (Key_hash_t _, _),
          Item_t (Mutez_t _, Item_t (ginit, rest, _), _),
          _ ) ) ->
      parse_two_var_annot loc annot
      >>?= fun (op_annot, addr_annot) ->
      let canonical_code = fst @@ Micheline.extract_locations code in
      parse_toplevel ~legacy canonical_code
      >>?= fun (arg_type, storage_type, code_field, root_name) ->
      record_trace
        (Ill_formed_type (Some "parameter", canonical_code, location arg_type))
        (parse_parameter_ty
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           arg_type)
      >>?= fun (Ex_ty arg_type, ctxt) ->
      check_type_size ~legacy ctxt ~loc arg_type
      >>?= fun () ->
      (if legacy then ok_unit else well_formed_entrypoints ~root_name arg_type)
      >>?= fun () ->
      record_trace
        (Ill_formed_type (Some "storage", canonical_code, location storage_type))
        (parse_storage_ty
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           storage_type)
      >>?= fun (Ex_ty storage_type, ctxt) ->
      check_type_size ~legacy ctxt ~loc storage_type
      >>?= fun () ->
      let arg_annot =
        default_annot
          (type_to_var_annot (name_of_ty arg_type))
          ~default:default_param_annot
      in
      let storage_annot =
        default_annot
          (type_to_var_annot (name_of_ty storage_type))
          ~default:default_storage_annot
      in
      let arg_type_full =
        Pair_t
          ( (arg_type, None, arg_annot),
            (storage_type, None, storage_annot),
            None )
      in
      let ret_type_full =
        Pair_t
          ( (List_t (Operation_t None, None), None, None),
            (storage_type, None, None),
            None )
      in
      trace
        (Ill_typed_contract (canonical_code, []))
        (parse_returning
           (Toplevel
              {
                storage_type;
                param_type = arg_type;
                root_name;
                legacy_create_contract_literal = false;
              })
           ctxt
           ~legacy
           ?type_logger
           ~stack_depth:(stack_depth + 1)
           (arg_type_full, None)
           ret_type_full
           code_field)
      >>=? fun ( ( Lam
                     ( { kbef = Item_t (arg, Bot_t, _);
                         kaft = Item_t (ret, Bot_t, _);
                         _ },
                       _ ) as lambda ),
                 ctxt ) ->
      merge_types ~legacy ctxt loc arg arg_type_full
      >>?= fun (Eq, _, ctxt) ->
      merge_types ~legacy ctxt loc ret ret_type_full
      >>?= fun (Eq, _, ctxt) ->
      merge_types ~legacy ctxt loc storage_type ginit
      >>?= fun (Eq, _, ctxt) ->
      let instr =
        {
          apply =
            (fun kinfo k ->
              ICreate_contract
                {kinfo; storage_type; arg_type; lambda; root_name; k});
        }
      in
      let stack =
        Item_t
          ( Operation_t None,
            Item_t (Address_t None, rest, addr_annot),
            op_annot )
      in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NOW, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_now_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INow (kinfo, k))} in
      let stack = Item_t (Timestamp_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_AMOUNT, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_amount_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IAmount (kinfo, k))} in
      let stack = Item_t (Mutez_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_CHAIN_ID, [], annot), stack) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IChainId (kinfo, k))} in
      let stack = Item_t (Chain_id_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_BALANCE, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_balance_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBalance (kinfo, k))} in
      let stack = Item_t (Mutez_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_LEVEL, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_level_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ILevel (kinfo, k))} in
      let stack = Item_t (Nat_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_VOTING_POWER, [], annot), Item_t (Key_hash_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IVoting_power (kinfo, k))} in
      let stack = Item_t (Nat_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_TOTAL_VOTING_POWER, [], annot), stack) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ITotal_voting_power (kinfo, k))} in
      let stack = Item_t (Nat_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (_, I_STEPS_TO_QUOTA, _, _), _) ->
      fail (Deprecated_instruction I_STEPS_TO_QUOTA)
  | (Prim (loc, I_SOURCE, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_source_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISource (kinfo, k))} in
      let stack = Item_t (Address_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SENDER, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_sender_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISender (kinfo, k))} in
      let stack = Item_t (Address_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SELF, [], annot), stack) ->
      Lwt.return
        ( parse_entrypoint_annot loc annot ~default:default_self_annot
        >>? fun (annot, entrypoint) ->
        let entrypoint =
          Option.fold
            ~some:(fun (Field_annot annot) -> annot)
            ~none:"default"
            entrypoint
        in
        let rec get_toplevel_type :
            tc_context -> ((a, s) judgement * context) tzresult = function
          | Lambda ->
              error (Self_in_lambda loc)
          | Dip (_, prev) ->
              get_toplevel_type prev
          | Toplevel
              {param_type; root_name; legacy_create_contract_literal = false}
            ->
              find_entrypoint param_type ~root_name entrypoint
              >>? fun (_, Ex_ty param_type) ->
              let instr =
                {
                  apply =
                    (fun kinfo k -> ISelf (kinfo, param_type, entrypoint, k));
                }
              in
              let stack =
                Item_t (Contract_t (param_type, None), stack, annot)
              in
              typed_no_lwt ctxt 1 loc instr stack
          | Toplevel
              {param_type; root_name = _; legacy_create_contract_literal = true}
            ->
              let instr =
                {
                  apply =
                    (fun kinfo k -> ISelf (kinfo, param_type, "default", k));
                }
              in
              let stack =
                Item_t (Contract_t (param_type, None), stack, annot)
              in
              typed_no_lwt ctxt 1 loc instr stack
        in
        get_toplevel_type tc_context )
  | (Prim (loc, I_SELF_ADDRESS, [], annot), stack) ->
      parse_var_annot loc annot ~default:default_self_annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISelf_address (kinfo, k))} in
      let stack = Item_t (Address_t None, stack, annot) in
      typed ctxt 0 loc instr stack
  (* cryptography *)
  | (Prim (loc, I_HASH_KEY, [], annot), Item_t (Key_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IHash_key (kinfo, k))} in
      let stack = Item_t (Key_hash_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_CHECK_SIGNATURE, [], annot),
      Item_t
        (Key_t _, Item_t (Signature_t _, Item_t (Bytes_t _, rest, _), _), _) )
    ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ICheck_signature (kinfo, k))} in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_BLAKE2B, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IBlake2b (kinfo, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SHA256, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISha256 (kinfo, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SHA512, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISha512 (kinfo, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_KECCAK, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IKeccak (kinfo, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_SHA3, [], annot), Item_t (Bytes_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> ISha3 (kinfo, k))} in
      let stack = Item_t (Bytes_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g1_t tn1, Item_t (Bls12_381_g1_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g2_t tn1, Item_t (Bls12_381_g2_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_fr_t tn1, Item_t (Bls12_381_fr_t tn2, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_type_annot ~legacy tn1 tn2
      >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g1_t tname, Item_t (Bls12_381_fr_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g2_t tname, Item_t (Bls12_381_fr_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Bls12_381_fr_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t tname, Item_t (Bls12_381_fr_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr_z (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Int_t tname, Item_t (Bls12_381_fr_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr_z (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Int_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_z_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Nat_t _, rest, _), _) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_z_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_INT, [], annot), Item_t (Bls12_381_fr_t _, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> IInt_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Int_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_g1_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_g2_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_fr_t tname, rest, _)) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest, annot) in
      typed ctxt 0 loc instr stack
  | ( Prim (loc, I_PAIRING_CHECK, [], annot),
      Item_t
        ( List_t
            (Pair_t ((Bls12_381_g1_t _, _, _), (Bls12_381_g2_t _, _, _), _), _),
          rest,
          _ ) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let instr =
        {apply = (fun kinfo k -> IPairing_check_bls12_381 (kinfo, k))}
      in
      let stack = Item_t (Bool_t None, rest, annot) in
      typed ctxt 0 loc instr stack
  (* Tickets *)
  | (Prim (loc, I_TICKET, [], annot), Item_t (t, Item_t (Nat_t _, rest, _), _))
    ->
      parse_var_annot loc annot
      >>?= fun annot ->
      comparable_ty_of_ty ctxt loc t
      >>?= fun (ty, ctxt) ->
      let instr = {apply = (fun kinfo k -> ITicket (kinfo, k))} in
      let stack = Item_t (Ticket_t (ty, None), rest, annot) in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_READ_TICKET, [], annot),
      (Item_t (Ticket_t (t, _), _, _) as full_stack) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let () = check_dupable_comparable_ty t in
      let result = ty_of_comparable_ty @@ opened_ticket_type t in
      let instr = {apply = (fun kinfo k -> IRead_ticket (kinfo, k))} in
      let stack = Item_t (result, full_stack, annot) in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_SPLIT_TICKET, [], annot),
      Item_t
        ( (Ticket_t (t, _) as ticket_t),
          Item_t
            (Pair_t ((Nat_t _, fa_a, a_a), (Nat_t _, fa_b, a_b), _), rest, _),
          _ ) ) ->
      parse_var_annot loc annot
      >>?= fun annot ->
      let () = check_dupable_comparable_ty t in
      let result =
        Option_t
          (Pair_t ((ticket_t, fa_a, a_a), (ticket_t, fa_b, a_b), None), None)
      in
      let instr = {apply = (fun kinfo k -> ISplit_ticket (kinfo, k))} in
      let stack = Item_t (result, rest, annot) in
      typed ctxt 1 loc instr stack
  | ( Prim (loc, I_JOIN_TICKETS, [], annot),
      Item_t
        ( Pair_t (((Ticket_t _ as ty_a), _, _), ((Ticket_t _ as ty_b), _, _), _),
          rest,
          _ ) ) -> (
      parse_var_annot loc annot
      >>?= fun annot ->
      merge_types ~legacy ctxt loc ty_a ty_b
      >>?= fun (Eq, ty, ctxt) ->
      match ty with
      | Ticket_t (contents_ty, _) ->
          let instr =
            {apply = (fun kinfo k -> IJoin_tickets (kinfo, contents_ty, k))}
          in
          let stack = Item_t (Option_t (ty, None), rest, annot) in
          typed ctxt 0 loc instr stack
      | _ ->
          (* TODO: fix injectivity of types *) assert false )
  (* Primitive parsing errors *)
  | ( Prim
        ( loc,
          ( ( I_DUP
            | I_SWAP
            | I_SOME
            | I_UNIT
            | I_PAIR
            | I_UNPAIR
            | I_CAR
            | I_CDR
            | I_CONS
            | I_CONCAT
            | I_SLICE
            | I_MEM
            | I_UPDATE
            | I_GET
            | I_EXEC
            | I_FAILWITH
            | I_SIZE
            | I_ADD
            | I_SUB
            | I_MUL
            | I_EDIV
            | I_OR
            | I_AND
            | I_XOR
            | I_NOT
            | I_ABS
            | I_NEG
            | I_LSL
            | I_LSR
            | I_COMPARE
            | I_EQ
            | I_NEQ
            | I_LT
            | I_GT
            | I_LE
            | I_GE
            | I_TRANSFER_TOKENS
            | I_SET_DELEGATE
            | I_NOW
            | I_IMPLICIT_ACCOUNT
            | I_AMOUNT
            | I_BALANCE
            | I_LEVEL
            | I_CHECK_SIGNATURE
            | I_HASH_KEY
            | I_SOURCE
            | I_SENDER
            | I_BLAKE2B
            | I_SHA256
            | I_SHA512
            | I_ADDRESS
            | I_RENAME
            | I_PACK
            | I_ISNAT
            | I_INT
            | I_SELF
            | I_CHAIN_ID
            | I_NEVER
            | I_VOTING_POWER
            | I_TOTAL_VOTING_POWER
            | I_KECCAK
            | I_SHA3
            | I_PAIRING_CHECK
            | I_TICKET
            | I_READ_TICKET
            | I_SPLIT_TICKET
            | I_JOIN_TICKETS ) as name ),
          (_ :: _ as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 0, List.length l))
  | ( Prim
        ( loc,
          ( ( I_NONE
            | I_LEFT
            | I_RIGHT
            | I_NIL
            | I_MAP
            | I_ITER
            | I_EMPTY_SET
            | I_LOOP
            | I_LOOP_LEFT
            | I_CONTRACT
            | I_CAST
            | I_UNPACK
            | I_CREATE_CONTRACT ) as name ),
          (([] | _ :: _ :: _) as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 1, List.length l))
  | ( Prim
        ( loc,
          ( ( I_PUSH
            | I_IF_NONE
            | I_IF_LEFT
            | I_IF_CONS
            | I_EMPTY_MAP
            | I_EMPTY_BIG_MAP
            | I_IF ) as name ),
          (([] | [_] | _ :: _ :: _ :: _) as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 2, List.length l))
  | ( Prim
        (loc, I_LAMBDA, (([] | [_] | [_; _] | _ :: _ :: _ :: _ :: _) as l), _),
      _ ) ->
      fail (Invalid_arity (loc, I_LAMBDA, 3, List.length l))
  (* Stack errors *)
  | ( Prim
        ( loc,
          ( ( I_ADD
            | I_SUB
            | I_MUL
            | I_EDIV
            | I_AND
            | I_OR
            | I_XOR
            | I_LSL
            | I_LSR
            | I_CONCAT
            | I_PAIRING_CHECK ) as name ),
          [],
          _ ),
      Item_t (ta, Item_t (tb, _, _), _) ) ->
      serialize_ty_for_error ctxt ta
      >>?= fun (ta, ctxt) ->
      serialize_ty_for_error ctxt tb
      >>?= fun (tb, _ctxt) -> fail (Undefined_binop (loc, name, ta, tb))
  | ( Prim
        ( loc,
          ( ( I_NEG
            | I_ABS
            | I_NOT
            | I_SIZE
            | I_EQ
            | I_NEQ
            | I_LT
            | I_GT
            | I_LE
            | I_GE
            (* CONCAT is both unary and binary; this case can only be triggered
                on a singleton stack *)
            | I_CONCAT ) as name ),
          [],
          _ ),
      Item_t (t, _, _) ) ->
      serialize_ty_for_error ctxt t
      >>?= fun (t, _ctxt) -> fail (Undefined_unop (loc, name, t))
  | (Prim (loc, ((I_UPDATE | I_SLICE) as name), [], _), stack) ->
      Lwt.return
        ( serialize_stack_for_error ctxt stack
        >>? fun (stack, _ctxt) -> error (Bad_stack (loc, name, 3, stack)) )
  | (Prim (loc, I_CREATE_CONTRACT, _, _), stack) ->
      serialize_stack_for_error ctxt stack
      >>?= fun (stack, _ctxt) ->
      fail (Bad_stack (loc, I_CREATE_CONTRACT, 7, stack))
  | (Prim (loc, I_TRANSFER_TOKENS, [], _), stack) ->
      Lwt.return
        ( serialize_stack_for_error ctxt stack
        >>? fun (stack, _ctxt) ->
        error (Bad_stack (loc, I_TRANSFER_TOKENS, 4, stack)) )
  | ( Prim
        ( loc,
          ( ( I_DROP
            | I_DUP
            | I_CAR
            | I_CDR
            | I_UNPAIR
            | I_SOME
            | I_BLAKE2B
            | I_SHA256
            | I_SHA512
            | I_DIP
            | I_IF_NONE
            | I_LEFT
            | I_RIGHT
            | I_IF_LEFT
            | I_IF
            | I_LOOP
            | I_IF_CONS
            | I_IMPLICIT_ACCOUNT
            | I_NEG
            | I_ABS
            | I_INT
            | I_NOT
            | I_HASH_KEY
            | I_EQ
            | I_NEQ
            | I_LT
            | I_GT
            | I_LE
            | I_GE
            | I_SIZE
            | I_FAILWITH
            | I_RENAME
            | I_PACK
            | I_ISNAT
            | I_ADDRESS
            | I_SET_DELEGATE
            | I_CAST
            | I_MAP
            | I_ITER
            | I_LOOP_LEFT
            | I_UNPACK
            | I_CONTRACT
            | I_NEVER
            | I_KECCAK
            | I_SHA3
            | I_READ_TICKET
            | I_JOIN_TICKETS ) as name ),
          _,
          _ ),
      stack ) ->
      Lwt.return
        ( serialize_stack_for_error ctxt stack
        >>? fun (stack, _ctxt) -> error (Bad_stack (loc, name, 1, stack)) )
  | ( Prim
        ( loc,
          ( ( I_SWAP
            | I_PAIR
            | I_CONS
            | I_GET
            | I_MEM
            | I_EXEC
            | I_CHECK_SIGNATURE
            | I_ADD
            | I_SUB
            | I_MUL
            | I_EDIV
            | I_AND
            | I_OR
            | I_XOR
            | I_LSL
            | I_LSR
            | I_COMPARE
            | I_PAIRING_CHECK
            | I_TICKET
            | I_SPLIT_TICKET ) as name ),
          _,
          _ ),
      stack ) ->
      Lwt.return
        ( serialize_stack_for_error ctxt stack
        >>? fun (stack, _ctxt) -> error (Bad_stack (loc, name, 2, stack)) )
  (* Generic parsing errors *)
  | (expr, _) ->
      fail
      @@ unexpected
           expr
           [Seq_kind]
           Instr_namespace
           [ I_DROP;
             I_DUP;
             I_DIG;
             I_DUG;
             I_SWAP;
             I_SOME;
             I_UNIT;
             I_PAIR;
             I_UNPAIR;
             I_CAR;
             I_CDR;
             I_CONS;
             I_MEM;
             I_UPDATE;
             I_MAP;
             I_ITER;
             I_GET;
             I_GET_AND_UPDATE;
             I_EXEC;
             I_FAILWITH;
             I_SIZE;
             I_CONCAT;
             I_ADD;
             I_SUB;
             I_MUL;
             I_EDIV;
             I_OR;
             I_AND;
             I_XOR;
             I_NOT;
             I_ABS;
             I_INT;
             I_NEG;
             I_LSL;
             I_LSR;
             I_COMPARE;
             I_EQ;
             I_NEQ;
             I_LT;
             I_GT;
             I_LE;
             I_GE;
             I_TRANSFER_TOKENS;
             I_CREATE_CONTRACT;
             I_NOW;
             I_AMOUNT;
             I_BALANCE;
             I_LEVEL;
             I_IMPLICIT_ACCOUNT;
             I_CHECK_SIGNATURE;
             I_BLAKE2B;
             I_SHA256;
             I_SHA512;
             I_HASH_KEY;
             I_PUSH;
             I_NONE;
             I_LEFT;
             I_RIGHT;
             I_NIL;
             I_EMPTY_SET;
             I_DIP;
             I_LOOP;
             I_IF_NONE;
             I_IF_LEFT;
             I_IF_CONS;
             I_EMPTY_MAP;
             I_EMPTY_BIG_MAP;
             I_IF;
             I_SOURCE;
             I_SENDER;
             I_SELF;
             I_SELF_ADDRESS;
             I_LAMBDA;
             I_NEVER;
             I_VOTING_POWER;
             I_TOTAL_VOTING_POWER;
             I_KECCAK;
             I_SHA3;
             I_PAIRING_CHECK;
             I_SAPLING_EMPTY_STATE;
             I_SAPLING_VERIFY_UPDATE;
             I_TICKET;
             I_READ_TICKET;
             I_SPLIT_TICKET;
             I_JOIN_TICKETS ]

and parse_contract :
    type arg.
    stack_depth:int ->
    legacy:bool ->
    context ->
    Script.location ->
    arg ty ->
    Contract.t ->
    entrypoint:string ->
    (context * arg typed_contract) tzresult Lwt.t =
 fun ~stack_depth ~legacy ctxt loc arg contract ~entrypoint ->
  Gas.consume ctxt Typecheck_costs.contract_exists
  >>?= fun ctxt ->
  Contract.exists ctxt contract
  >>=? function
  | false ->
      fail (Invalid_contract (loc, contract))
  | true -> (
      trace (Invalid_contract (loc, contract))
      @@ Contract.get_script_code ctxt contract
      >>=? fun (ctxt, code) ->
      Lwt.return
      @@
      match code with
      | None -> (
          ty_eq ctxt loc arg (Unit_t None)
          >>? fun (Eq, ctxt) ->
          match entrypoint with
          | "default" ->
              let contract : arg typed_contract =
                (arg, (contract, entrypoint))
              in
              ok (ctxt, contract)
          | entrypoint ->
              error (No_such_entrypoint entrypoint) )
      | Some code ->
          Script.force_decode_in_context ctxt code
          >>? fun (code, ctxt) ->
          parse_toplevel ~legacy:true code
          >>? fun (arg_type, _, _, root_name) ->
          parse_parameter_ty
            ctxt
            ~stack_depth:(stack_depth + 1)
            ~legacy:true
            arg_type
          >>? fun (Ex_ty targ, ctxt) ->
          find_entrypoint_for_type
            ~legacy
            ~full:targ
            ~expected:arg
            ~root_name
            entrypoint
            ctxt
            loc
          >|? fun (ctxt, entrypoint, arg) ->
          let contract : arg typed_contract = (arg, (contract, entrypoint)) in
          (ctxt, contract) )

and parse_toplevel :
    legacy:bool ->
    Script.expr ->
    (Script.node * Script.node * Script.node * field_annot option) tzresult =
 fun ~legacy toplevel ->
  record_trace (Ill_typed_contract (toplevel, []))
  @@
  match root toplevel with
  | Int (loc, _) ->
      error (Invalid_kind (loc, [Seq_kind], Int_kind))
  | String (loc, _) ->
      error (Invalid_kind (loc, [Seq_kind], String_kind))
  | Bytes (loc, _) ->
      error (Invalid_kind (loc, [Seq_kind], Bytes_kind))
  | Prim (loc, _, _, _) ->
      error (Invalid_kind (loc, [Seq_kind], Prim_kind))
  | Seq (_, fields) -> (
      let rec find_fields p s c fields =
        match fields with
        | [] ->
            ok (p, s, c)
        | Int (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], Int_kind))
        | String (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], String_kind))
        | Bytes (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], Bytes_kind))
        | Seq (loc, _) :: _ ->
            error (Invalid_kind (loc, [Prim_kind], Seq_kind))
        | Prim (loc, K_parameter, [arg], annot) :: rest -> (
          match p with
          | None ->
              find_fields (Some (arg, loc, annot)) s c rest
          | Some _ ->
              error (Duplicate_field (loc, K_parameter)) )
        | Prim (loc, K_storage, [arg], annot) :: rest -> (
          match s with
          | None ->
              find_fields p (Some (arg, loc, annot)) c rest
          | Some _ ->
              error (Duplicate_field (loc, K_storage)) )
        | Prim (loc, K_code, [arg], annot) :: rest -> (
          match c with
          | None ->
              find_fields p s (Some (arg, loc, annot)) rest
          | Some _ ->
              error (Duplicate_field (loc, K_code)) )
        | Prim (loc, ((K_parameter | K_storage | K_code) as name), args, _)
          :: _ ->
            error (Invalid_arity (loc, name, 1, List.length args))
        | Prim (loc, name, _, _) :: _ ->
            let allowed = [K_parameter; K_storage; K_code] in
            error (Invalid_primitive (loc, allowed, name))
      in
      find_fields None None None fields
      >>? function
      | (None, _, _) ->
          error (Missing_field K_parameter)
      | (Some _, None, _) ->
          error (Missing_field K_storage)
      | (Some _, Some _, None) ->
          error (Missing_field K_code)
      | (Some (p, ploc, pannot), Some (s, sloc, sannot), Some (c, cloc, carrot))
        ->
          let maybe_root_name =
            (* root name can be attached to either the parameter
                 primitive or the toplevel constructor *)
            Script_ir_annot.extract_field_annot p
            >>? fun (p, root_name) ->
            match root_name with
            | Some _ ->
                ok (p, pannot, root_name)
            | None -> (
              match pannot with
              | [single]
                when Compare.Int.(String.length single > 0)
                     && Compare.Char.(single.[0] = '%') ->
                  parse_field_annot ploc [single]
                  >>? fun pannot -> ok (p, [], pannot)
              | _ ->
                  ok (p, pannot, None) )
          in
          if legacy then
            (* legacy semantics ignores spurious annotations *)
            let (p, root_name) =
              match maybe_root_name with
              | Ok (p, _, root_name) ->
                  (p, root_name)
              | Error _ ->
                  (p, None)
            in
            ok (p, s, c, root_name)
          else
            (* only one field annot is allowed to set the root entrypoint name *)
            maybe_root_name
            >>? fun (p, pannot, root_name) ->
            Script_ir_annot.error_unexpected_annot ploc pannot
            >>? fun () ->
            Script_ir_annot.error_unexpected_annot cloc carrot
            >>? fun () ->
            Script_ir_annot.error_unexpected_annot sloc sannot
            >>? fun () -> ok (p, s, c, root_name) )

(* Same as [parse_contract], but does not fail when the contact is missing or
   if the expected type doesn't match the actual one. In that case None is
   returned and some overapproximation of the typechecking gas is consumed.
   This can still fail on gas exhaustion. *)
let parse_contract_for_script :
    type arg.
    context ->
    Script.location ->
    arg ty ->
    Contract.t ->
    entrypoint:string ->
    (context * arg typed_contract option) tzresult Lwt.t =
 fun ctxt loc arg contract ~entrypoint ->
  Gas.consume ctxt Typecheck_costs.contract_exists
  >>?= fun ctxt ->
  match (Contract.is_implicit contract, entrypoint) with
  | (Some _, "default") ->
      (* An implicit account on the "default" entrypoint always exists and has type unit. *)
      Lwt.return
        ( match ty_eq ctxt loc arg (Unit_t None) with
        | Ok (Eq, ctxt) ->
            let contract : arg typed_contract =
              (arg, (contract, entrypoint))
            in
            ok (ctxt, Some contract)
        | Error _ ->
            Gas.consume ctxt Typecheck_costs.parse_instr_cycle
            >>? fun ctxt -> ok (ctxt, None) )
  | (Some _, _) ->
      Lwt.return
        ( Gas.consume ctxt Typecheck_costs.parse_instr_cycle
        >|? fun ctxt ->
        (* An implicit account on any other entrypoint is not a valid contract. *)
        (ctxt, None) )
  | (None, _) -> (
      (* Originated account *)
      Contract.exists ctxt contract
      >>=? function
      | false ->
          return (ctxt, None)
      | true -> (
          trace (Invalid_contract (loc, contract))
          @@ Contract.get_script_code ctxt contract
          >>=? fun (ctxt, code) ->
          match code with
          | None ->
              (* Since protocol 005, we have the invariant that all originated accounts have code *)
              assert false
          | Some code ->
              Lwt.return
                ( Script.force_decode_in_context ctxt code
                >>? fun (code, ctxt) ->
                (* can only fail because of gas *)
                match parse_toplevel ~legacy:true code with
                | Error _ ->
                    error (Invalid_contract (loc, contract))
                | Ok (arg_type, _, _, root_name) -> (
                  match
                    parse_parameter_ty
                      ctxt
                      ~stack_depth:0
                      ~legacy:true
                      arg_type
                  with
                  | Error _ ->
                      error (Invalid_contract (loc, contract))
                  | Ok (Ex_ty targ, ctxt) -> (
                    (* we don't check targ size here because it's a legacy contract code *)
                    match
                      find_entrypoint_for_type
                        ~legacy:false
                        ~full:targ
                        ~expected:arg
                        ~root_name
                        entrypoint
                        ctxt
                        loc
                      >|? fun (ctxt, entrypoint, arg) ->
                      let contract : arg typed_contract =
                        (arg, (contract, entrypoint))
                      in
                      (ctxt, Some contract)
                    with
                    | Ok res ->
                        ok res
                    | Error _ ->
                        (* overapproximation by checking if targ = targ,
                                                       can only fail because of gas *)
                        merge_types ~legacy:false ctxt loc targ targ
                        >|? fun (Eq, _, ctxt) -> (ctxt, None) ) ) ) ) )

let parse_code :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    code:lazy_expr ->
    (ex_code * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~code ->
  Script.force_decode_in_context ctxt code
  >>?= fun (code, ctxt) ->
  parse_toplevel ~legacy code
  >>?= fun (arg_type, storage_type, code_field, root_name) ->
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty ctxt ~stack_depth:0 ~legacy arg_type)
  >>?= fun (Ex_ty arg_type, ctxt) ->
  check_type_size ~legacy ctxt ~loc:arg_type_loc arg_type
  >>?= fun () ->
  (if legacy then ok_unit else well_formed_entrypoints ~root_name arg_type)
  >>?= fun () ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (Ex_ty storage_type, ctxt) ->
  check_type_size ~legacy ctxt ~loc:storage_type_loc storage_type
  >>?= fun () ->
  let arg_annot =
    default_annot
      (type_to_var_annot (name_of_ty arg_type))
      ~default:default_param_annot
  in
  let storage_annot =
    default_annot
      (type_to_var_annot (name_of_ty storage_type))
      ~default:default_storage_annot
  in
  let arg_type_full =
    Pair_t
      ((arg_type, None, arg_annot), (storage_type, None, storage_annot), None)
  in
  let ret_type_full =
    Pair_t
      ( (List_t (Operation_t None, None), None, None),
        (storage_type, None, None),
        None )
  in
  trace
    (Ill_typed_contract (code, []))
    (parse_returning
       (Toplevel
          {
            storage_type;
            param_type = arg_type;
            root_name;
            legacy_create_contract_literal = false;
          })
       ctxt
       ~legacy
       ~stack_depth:0
       ?type_logger
       (arg_type_full, None)
       ret_type_full
       code_field)
  >|=? fun (code, ctxt) ->
  (Ex_code {code; arg_type; storage_type; root_name}, ctxt)

let parse_storage :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    allow_forged:bool ->
    'storage ty ->
    storage:lazy_expr ->
    ('storage * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~allow_forged storage_type ~storage ->
  Script.force_decode_in_context ctxt storage
  >>?= fun (storage, ctxt) ->
  trace_eval
    (fun () ->
      Lwt.return
        ( serialize_ty_for_error ctxt storage_type
        >|? fun (storage_type, _ctxt) ->
        Ill_typed_data (None, storage, storage_type) ))
    (parse_data
       ?type_logger
       ~stack_depth:0
       ctxt
       ~legacy
       ~allow_forged
       storage_type
       (root storage))

let parse_script :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    allow_forged_in_storage:bool ->
    Script.t ->
    (ex_script * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~allow_forged_in_storage {code; storage} ->
  parse_code ~legacy ctxt ?type_logger ~code
  >>=? fun (Ex_code {code; arg_type; storage_type; root_name}, ctxt) ->
  parse_storage
    ?type_logger
    ctxt
    ~legacy
    ~allow_forged:allow_forged_in_storage
    storage_type
    ~storage
  >|=? fun (storage, ctxt) ->
  (Ex_script {code; arg_type; storage; storage_type; root_name}, ctxt)

let typecheck_code :
    legacy:bool ->
    context ->
    Script.expr ->
    (type_map * context) tzresult Lwt.t =
 fun ~legacy ctxt code ->
  parse_toplevel ~legacy code
  >>?= fun (arg_type, storage_type, code_field, root_name) ->
  let type_map = ref [] in
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty ctxt ~stack_depth:0 ~legacy arg_type)
  >>?= fun (Ex_ty arg_type, ctxt) ->
  check_type_size ~legacy ctxt ~loc:arg_type_loc arg_type
  >>?= fun () ->
  (if legacy then ok_unit else well_formed_entrypoints ~root_name arg_type)
  >>?= fun () ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (Ex_ty storage_type, ctxt) ->
  check_type_size ~legacy ctxt ~loc:storage_type_loc storage_type
  >>?= fun () ->
  let arg_annot =
    default_annot
      (type_to_var_annot (name_of_ty arg_type))
      ~default:default_param_annot
  in
  let storage_annot =
    default_annot
      (type_to_var_annot (name_of_ty storage_type))
      ~default:default_storage_annot
  in
  let arg_type_full =
    Pair_t
      ((arg_type, None, arg_annot), (storage_type, None, storage_annot), None)
  in
  let ret_type_full =
    Pair_t
      ( (List_t (Operation_t None, None), None, None),
        (storage_type, None, None),
        None )
  in
  let result =
    parse_returning
      (Toplevel
         {
           storage_type;
           param_type = arg_type;
           root_name;
           legacy_create_contract_literal = false;
         })
      ctxt
      ~legacy
      ~stack_depth:0
      ~type_logger:(fun loc bef aft ->
        type_map := (loc, (bef, aft)) :: !type_map)
      (arg_type_full, None)
      ret_type_full
      code_field
  in
  trace (Ill_typed_contract (code, !type_map)) result
  >|=? fun (Lam _, ctxt) -> (!type_map, ctxt)

module Entrypoints_map = Map.Make (String)

let list_entrypoints (type full) (full : full ty) ctxt ~root_name =
  let merge path annot (type t) (ty : t ty) reachable
      ((unreachables, all) as acc) =
    match annot with
    | None | Some (Field_annot "") -> (
        ok
        @@
        if reachable then acc
        else
          match ty with
          | Union_t _ ->
              acc
          | _ ->
              (List.rev path :: unreachables, all) )
    | Some (Field_annot name) ->
        if Compare.Int.(String.length name > 31) then
          ok (List.rev path :: unreachables, all)
        else if Entrypoints_map.mem name all then
          ok (List.rev path :: unreachables, all)
        else
          unparse_ty ctxt ty
          >>? fun (unparsed_ty, _) ->
          ok
            ( unreachables,
              Entrypoints_map.add name (List.rev path, unparsed_ty) all )
  in
  let rec fold_tree :
      type t.
      t ty ->
      prim list ->
      bool ->
      prim list list * (prim list * Script.node) Entrypoints_map.t ->
      (prim list list * (prim list * Script.node) Entrypoints_map.t) tzresult =
   fun t path reachable acc ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) ->
        merge (D_Left :: path) al tl reachable acc
        >>? fun acc ->
        merge (D_Right :: path) ar tr reachable acc
        >>? fun acc ->
        fold_tree
          tl
          (D_Left :: path)
          (match al with Some _ -> true | None -> reachable)
          acc
        >>? fun acc ->
        fold_tree
          tr
          (D_Right :: path)
          (match ar with Some _ -> true | None -> reachable)
          acc
    | _ ->
        ok acc
  in
  unparse_ty ctxt full
  >>? fun (unparsed_full, _) ->
  let (init, reachable) =
    match root_name with
    | None | Some (Field_annot "") ->
        (Entrypoints_map.empty, false)
    | Some (Field_annot name) ->
        (Entrypoints_map.singleton name ([], unparsed_full), true)
  in
  fold_tree full [] reachable ([], init)
  [@@coq_axiom_with_reason "unsupported syntax"]

(* ---- Unparsing (Typed IR -> Untyped expressions) --------------------------*)

(* -- Unparsing data of any type -- *)

let comb_witness2 : type t. t ty -> (t, unit -> unit -> unit) comb_witness =
  function
  | Pair_t (_, (Pair_t _, _, _), _) ->
      Comb_Pair (Comb_Pair Comb_Any)
  | Pair_t _ ->
      Comb_Pair Comb_Any
  | _ ->
      Comb_Any

let rec unparse_data :
    type a.
    context ->
    stack_depth:int ->
    unparsing_mode ->
    a ty ->
    a ->
    (Script.node * context) tzresult Lwt.t =
 fun ctxt ~stack_depth mode ty a ->
  Gas.consume ctxt Unparse_costs.unparse_data_cycle
  >>?= fun ctxt ->
  let non_terminal_recursion ctxt mode ty a =
    if Compare.Int.(stack_depth > 10_000) then
      fail Unparsing_too_many_recursive_calls
    else unparse_data ctxt ~stack_depth:(stack_depth + 1) mode ty a
  in
  match (ty, a) with
  | (Unit_t _, v) ->
      Lwt.return @@ unparse_unit ctxt v
  | (Int_t _, v) ->
      Lwt.return @@ unparse_int ctxt v
  | (Nat_t _, v) ->
      Lwt.return @@ unparse_nat ctxt v
  | (String_t _, s) ->
      Lwt.return @@ unparse_string ctxt s
  | (Bytes_t _, s) ->
      Lwt.return @@ unparse_bytes ctxt s
  | (Bool_t _, b) ->
      Lwt.return @@ unparse_bool ctxt b
  | (Timestamp_t _, t) ->
      Lwt.return @@ unparse_timestamp ctxt mode t
  | (Address_t _, address) ->
      Lwt.return @@ unparse_address ctxt mode address
  | (Contract_t _, contract) ->
      Lwt.return @@ unparse_contract ctxt mode contract
  | (Signature_t _, s) ->
      Lwt.return @@ unparse_signature ctxt mode s
  | (Mutez_t _, v) ->
      Lwt.return @@ unparse_mutez ctxt v
  | (Key_t _, k) ->
      Lwt.return @@ unparse_key ctxt mode k
  | (Key_hash_t _, k) ->
      Lwt.return @@ unparse_key_hash ctxt mode k
  | (Operation_t _, operation) ->
      Lwt.return @@ unparse_operation ctxt operation
  | (Chain_id_t _, chain_id) ->
      Lwt.return @@ unparse_chain_id ctxt mode chain_id
  | (Bls12_381_g1_t _, x) ->
      Lwt.return @@ unparse_bls12_381_g1 ctxt x
  | (Bls12_381_g2_t _, x) ->
      Lwt.return @@ unparse_bls12_381_g2 ctxt x
  | (Bls12_381_fr_t _, x) ->
      Lwt.return @@ unparse_bls12_381_fr ctxt x
  | (Pair_t ((tl, _, _), (tr, _, _), _), pair) ->
      let r_witness = comb_witness2 tr in
      let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
      let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
      unparse_pair unparse_l unparse_r ctxt mode r_witness pair
  | (Union_t ((tl, _), (tr, _), _), v) ->
      let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
      let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
      unparse_union unparse_l unparse_r ctxt v
  | (Option_t (t, _), v) ->
      let unparse_v ctxt v = non_terminal_recursion ctxt mode t v in
      unparse_option unparse_v ctxt v
  | (List_t (t, _), items) ->
      fold_left_s
        (fun (l, ctxt) element ->
          non_terminal_recursion ctxt mode t element
          >|=? fun (unparsed, ctxt) -> (unparsed :: l, ctxt))
        ([], ctxt)
        items.elements
      >|=? fun (items, ctxt) -> (Micheline.Seq (-1, List.rev items), ctxt)
  | (Ticket_t (t, _), {ticketer; contents; amount}) ->
      let t = ty_of_comparable_ty @@ opened_ticket_type t in
      (unparse_data [@tailcall])
        ctxt
        ~stack_depth
        mode
        t
        (ticketer, (contents, amount))
  | (Set_t (t, _), set) ->
      fold_left_s
        (fun (l, ctxt) item ->
          unparse_comparable_data ctxt mode t item
          >|=? fun (item, ctxt) -> (item :: l, ctxt))
        ([], ctxt)
        (set_fold (fun e acc -> e :: acc) set [])
      >|=? fun (items, ctxt) -> (Micheline.Seq (-1, items), ctxt)
  | (Map_t (kt, vt, _), map) ->
      let items = map_fold (fun k v acc -> (k, v) :: acc) map [] in
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) -> (Micheline.Seq (-1, items), ctxt)
  | (Big_map_t (_kt, _vt, _), {id = Some id; diff = {size}; _})
    when Compare.Int.( = ) size 0 ->
      return (Micheline.Int (-1, Big_map.Id.unparse_to_z id), ctxt)
  | (Big_map_t (kt, vt, _), {id = Some id; diff = {map; _}; _}) ->
      let items =
        Big_map_overlay.fold (fun _ (k, v) acc -> (k, v) :: acc) map []
      in
      let items =
        (* Sort the items in Michelson comparison order and not in key
           hash order. This code path is only exercized for tracing,
           so we don't bother carbonating this sort operation
           precisely. Also, the sort uses a reverse compare because
           [unparse_items] will reverse the result. *)
        List.sort (fun (a, _) (b, _) -> compare_comparable kt b a) items
      in
      let vt = Option_t (vt, None) in
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) ->
      ( Micheline.Prim
          ( -1,
            D_Pair,
            [Int (-1, Big_map.Id.unparse_to_z id); Seq (-1, items)],
            [] ),
        ctxt )
  | (Big_map_t (kt, vt, _), {id = None; diff = {map; _}; _}) ->
      let items =
        Big_map_overlay.fold
          (fun _ (k, v) acc ->
            match v with None -> acc | Some v -> (k, v) :: acc)
          map
          []
      in
      let items =
        (* See note above. *)
        List.sort (fun (a, _) (b, _) -> compare_comparable kt b a) items
      in
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) -> (Micheline.Seq (-1, items), ctxt)
  | (Lambda_t _, Lam (_, original_code)) ->
      unparse_code ctxt ~stack_depth:(stack_depth + 1) mode original_code
  | (Never_t _, _) ->
      .
  | (Sapling_transaction_t _, s) ->
      Lwt.return
        ( Gas.consume ctxt (Unparse_costs.sapling_transaction s)
        >|? fun ctxt ->
        let bytes =
          Data_encoding.Binary.to_bytes_exn Sapling.transaction_encoding s
        in
        (Bytes (-1, bytes), ctxt) )
  | (Sapling_state_t _, {id; diff; _}) ->
      Lwt.return
        ( Gas.consume ctxt (Unparse_costs.sapling_diff diff)
        >|? fun ctxt ->
        ( ( match diff with
          | {commitments_and_ciphertexts = []; nullifiers = []} -> (
            match id with
            | None ->
                Micheline.Seq (-1, [])
            | Some id ->
                let id = Sapling.Id.unparse_to_z id in
                Micheline.Int (-1, id) )
          | diff -> (
              let diff_bytes =
                Data_encoding.Binary.to_bytes_exn Sapling.diff_encoding diff
              in
              let unparsed_diff = Bytes (-1, diff_bytes) in
              match id with
              | None ->
                  unparsed_diff
              | Some id ->
                  let id = Sapling.Id.unparse_to_z id in
                  Micheline.Prim (-1, D_Pair, [Int (-1, id); unparsed_diff], [])
              ) ),
          ctxt ) )

and unparse_items :
    type k v.
    context ->
    stack_depth:int ->
    unparsing_mode ->
    k comparable_ty ->
    v ty ->
    (k * v) list ->
    (Script.node list * context) tzresult Lwt.t =
 fun ctxt ~stack_depth mode kt vt items ->
  fold_left_s
    (fun (l, ctxt) (k, v) ->
      unparse_comparable_data ctxt mode kt k
      >>=? fun (key, ctxt) ->
      unparse_data ctxt ~stack_depth:(stack_depth + 1) mode vt v
      >|=? fun (value, ctxt) -> (Prim (-1, D_Elt, [key; value], []) :: l, ctxt))
    ([], ctxt)
    items

and unparse_code ctxt ~stack_depth mode code =
  let legacy = true in
  Gas.consume ctxt Unparse_costs.unparse_instr_cycle
  >>?= fun ctxt ->
  let non_terminal_recursion ctxt mode code =
    if Compare.Int.(stack_depth > 10_000) then
      fail Unparsing_too_many_recursive_calls
    else unparse_code ctxt ~stack_depth:(stack_depth + 1) mode code
  in
  match code with
  | Prim (loc, I_PUSH, [ty; data], annot) ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      let allow_forged =
        false
        (* Forgeable in PUSH data are already forbidden at parsing,
         the only case for which this matters is storing a lambda resulting
         from APPLYing a non-forgeable but this cannot happen either as long
         as all packable values are also forgeable. *)
      in
      parse_data
        ctxt
        ~stack_depth:(stack_depth + 1)
        ~legacy
        ~allow_forged
        t
        data
      >>=? fun (data, ctxt) ->
      unparse_data ctxt ~stack_depth:(stack_depth + 1) mode t data
      >>=? fun (data, ctxt) ->
      return (Prim (loc, I_PUSH, [ty; data], annot), ctxt)
  | Seq (loc, items) ->
      fold_left_s
        (fun (l, ctxt) item ->
          non_terminal_recursion ctxt mode item
          >|=? fun (item, ctxt) -> (item :: l, ctxt))
        ([], ctxt)
        items
      >>=? fun (items, ctxt) ->
      return (Micheline.Seq (loc, List.rev items), ctxt)
  | Prim (loc, prim, items, annot) ->
      fold_left_s
        (fun (l, ctxt) item ->
          non_terminal_recursion ctxt mode item
          >|=? fun (item, ctxt) -> (item :: l, ctxt))
        ([], ctxt)
        items
      >>=? fun (items, ctxt) ->
      return (Prim (loc, prim, List.rev items, annot), ctxt)
  | (Int _ | String _ | Bytes _) as atom ->
      return (atom, ctxt)

(* Gas accounting may not be perfect in this function, as it is only called by RPCs. *)
let unparse_script ctxt mode {code; arg_type; storage; storage_type; root_name}
    =
  let (Lam (_, original_code)) = code in
  unparse_code ctxt ~stack_depth:0 mode original_code
  >>=? fun (code, ctxt) ->
  unparse_data ctxt ~stack_depth:0 mode storage_type storage
  >>=? fun (storage, ctxt) ->
  Lwt.return
    ( unparse_ty ctxt arg_type
    >>? fun (arg_type, ctxt) ->
    unparse_ty ctxt storage_type
    >>? fun (storage_type, ctxt) ->
    let arg_type = add_field_annot root_name None arg_type in
    let open Micheline in
    let code =
      Seq
        ( -1,
          [ Prim (-1, K_parameter, [arg_type], []);
            Prim (-1, K_storage, [storage_type], []);
            Prim (-1, K_code, [code], []) ] )
    in
    Gas.consume ctxt Unparse_costs.unparse_instr_cycle
    >>? fun ctxt ->
    Gas.consume ctxt Unparse_costs.unparse_instr_cycle
    >>? fun ctxt ->
    Gas.consume ctxt Unparse_costs.unparse_instr_cycle
    >>? fun ctxt ->
    Gas.consume ctxt Unparse_costs.unparse_instr_cycle
    >>? fun ctxt ->
    Gas.consume ctxt (Script.strip_locations_cost code)
    >>? fun ctxt ->
    Gas.consume ctxt (Script.strip_locations_cost storage)
    >|? fun ctxt ->
    ( {
        code = lazy_expr (strip_locations code);
        storage = lazy_expr (strip_locations storage);
      },
      ctxt ) )

let pack_data ctxt typ data ~mode =
  unparse_data ~stack_depth:0 ctxt mode typ data
  >>=? fun (unparsed, ctxt) -> Lwt.return @@ pack_node unparsed ctxt

let hash_data ctxt typ data =
  pack_data ctxt typ data ~mode:Optimized_legacy
  >>=? fun (bytes, ctxt) -> Lwt.return @@ hash_bytes ctxt bytes

let pack_data ctxt typ data = pack_data ctxt typ data ~mode:Optimized_legacy

(* ---------------- Big map -------------------------------------------------*)

let empty_big_map key_type value_type =
  {
    id = None;
    diff = {map = Big_map_overlay.empty; size = 0};
    key_type;
    value_type;
  }

let big_map_mem ctxt key {id; diff; key_type; _} =
  hash_comparable_data ctxt key_type key
  >>=? fun (key, ctxt) ->
  match (Big_map_overlay.find_opt key diff.map, id) with
  | (None, None) ->
      return (false, ctxt)
  | (None, Some id) ->
      Alpha_context.Big_map.mem ctxt id key >|=? fun (ctxt, res) -> (res, ctxt)
  | (Some (_, None), _) ->
      return (false, ctxt)
  | (Some (_, Some _), _) ->
      return (true, ctxt)

let big_map_get_by_hash ctxt key {id; diff; value_type} =
  match (Big_map_overlay.find_opt key diff.map, id) with
  | (Some (_, x), _) ->
      return (x, ctxt)
  | (None, None) ->
      return (None, ctxt)
  | (None, Some id) -> (
      Alpha_context.Big_map.get_opt ctxt id key
      >>=? function
      | (ctxt, None) ->
          return (None, ctxt)
      | (ctxt, Some value) ->
          parse_data
            ~stack_depth:0
            ctxt
            ~legacy:true
            ~allow_forged:true
            value_type
            (Micheline.root value)
          >|=? fun (x, ctxt) -> (Some x, ctxt) )

let big_map_get ctxt key map =
  hash_comparable_data ctxt map.key_type key
  >>=? fun (key_hash, ctxt) -> big_map_get_by_hash ctxt key_hash map

let big_map_update_by_hash ctxt key_hash key value map =
  let contains = Big_map_overlay.mem key_hash map.diff.map in
  return
    ( {
        map with
        diff =
          {
            map = Big_map_overlay.add key_hash (key, value) map.diff.map;
            size = (if contains then map.diff.size else map.diff.size + 1);
          };
      },
      ctxt )

let big_map_update ctxt key value map =
  hash_comparable_data ctxt map.key_type key
  >>=? fun (key_hash, ctxt) ->
  big_map_update_by_hash ctxt key_hash key value map

let big_map_get_and_update ctxt key value map =
  hash_comparable_data ctxt map.key_type key
  >>=? fun (key_hash, ctxt) ->
  big_map_update_by_hash ctxt key_hash key value map
  >>=? fun (map', ctxt) ->
  big_map_get_by_hash ctxt key_hash map
  >>=? fun (old_value, ctxt) -> return ((old_value, map'), ctxt)

(* ---------------- Lazy storage---------------------------------------------*)

type lazy_storage_ids = Lazy_storage.IdSet.t

let no_lazy_storage_id = Lazy_storage.IdSet.empty

let diff_of_big_map ctxt mode ~temporary ~ids_to_copy
    {id; key_type; value_type; diff} =
  ( match id with
  | Some id ->
      if Lazy_storage.IdSet.mem Big_map id ids_to_copy then
        Big_map.fresh ~temporary ctxt
        >|=? fun (ctxt, duplicate) ->
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
      Big_map.fresh ~temporary ctxt
      >>=? fun (ctxt, id) ->
      Lwt.return
        (let kt = unparse_comparable_ty key_type in
         Gas.consume ctxt (Script.strip_locations_cost kt)
         >>? fun ctxt ->
         unparse_ty ctxt value_type
         >>? fun (kv, ctxt) ->
         Gas.consume ctxt (Script.strip_locations_cost kv)
         >|? fun ctxt ->
         let key_type = Micheline.strip_locations kt in
         let value_type = Micheline.strip_locations kv in
         (ctxt, Lazy_storage.(Alloc Big_map.{key_type; value_type}), id)) )
  >>=? fun (ctxt, init, id) ->
  let pairs =
    Big_map_overlay.fold
      (fun key_hash (key, value) acc -> (key_hash, key, value) :: acc)
      diff.map
      []
  in
  fold_left_s
    (fun (acc, ctxt) (key_hash, key, value) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle
      >>?= fun ctxt ->
      unparse_comparable_data ctxt mode key_type key
      >>=? fun (key_node, ctxt) ->
      Gas.consume ctxt (Script.strip_locations_cost key_node)
      >>?= fun ctxt ->
      let key = Micheline.strip_locations key_node in
      ( match value with
      | None ->
          return (None, ctxt)
      | Some x ->
          unparse_data ~stack_depth:0 ctxt mode value_type x
          >>=? fun (node, ctxt) ->
          Lwt.return
            ( Gas.consume ctxt (Script.strip_locations_cost node)
            >|? fun ctxt -> (Some (Micheline.strip_locations node), ctxt) ) )
      >|=? fun (value, ctxt) ->
      let diff_item = Big_map.{key; key_hash; value} in
      (diff_item :: acc, ctxt))
    ([], ctxt)
    (List.rev pairs)
  >|=? fun (updates, ctxt) -> (Lazy_storage.Update {init; updates}, id, ctxt)

let diff_of_sapling_state ctxt ~temporary ~ids_to_copy
    ({id; diff; memo_size} : Sapling.state) =
  ( match id with
  | Some id ->
      if Lazy_storage.IdSet.mem Sapling_state id ids_to_copy then
        Sapling.fresh ~temporary ctxt
        >|=? fun (ctxt, duplicate) ->
        (ctxt, Lazy_storage.Copy {src = id}, duplicate)
      else return (ctxt, Lazy_storage.Existing, id)
  | None ->
      Sapling.fresh ~temporary ctxt
      >|=? fun (ctxt, id) -> (ctxt, Lazy_storage.Alloc Sapling.{memo_size}, id)
  )
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
  | True_f : _ has_lazy_storage
  | False_f : _ has_lazy_storage
  | Pair_f :
      'a has_lazy_storage * 'b has_lazy_storage
      -> ('a, 'b) pair has_lazy_storage
  | Union_f :
      'a has_lazy_storage * 'b has_lazy_storage
      -> ('a, 'b) union has_lazy_storage
  | Option_f : 'a has_lazy_storage -> 'a option has_lazy_storage
  | List_f : 'a has_lazy_storage -> 'a boxed_list has_lazy_storage
  | Map_f : 'v has_lazy_storage -> (_, 'v) map has_lazy_storage

(**
    This function is called only on storage and parameter types of contracts,
    once per typechecked contract. It has a complexity linear in the size of
    the types, which happen to be literally written types, so the gas for them
    has already been paid.
*)
let rec has_lazy_storage : type t. t ty -> t has_lazy_storage =
  let aux1 cons t =
    match has_lazy_storage t with False_f -> False_f | h -> cons h
  in
  let aux2 cons t1 t2 =
    match (has_lazy_storage t1, has_lazy_storage t2) with
    | (False_f, False_f) ->
        False_f
    | (h1, h2) ->
        cons h1 h2
  in
  function
  | Big_map_t (_, _, _) ->
      True_f
  | Sapling_state_t _ ->
      True_f
  | Unit_t _ ->
      False_f
  | Int_t _ ->
      False_f
  | Nat_t _ ->
      False_f
  | Signature_t _ ->
      False_f
  | String_t _ ->
      False_f
  | Bytes_t _ ->
      False_f
  | Mutez_t _ ->
      False_f
  | Key_hash_t _ ->
      False_f
  | Key_t _ ->
      False_f
  | Timestamp_t _ ->
      False_f
  | Address_t _ ->
      False_f
  | Bool_t _ ->
      False_f
  | Lambda_t (_, _, _) ->
      False_f
  | Set_t (_, _) ->
      False_f
  | Contract_t (_, _) ->
      False_f
  | Operation_t _ ->
      False_f
  | Chain_id_t _ ->
      False_f
  | Never_t _ ->
      False_f
  | Bls12_381_g1_t _ ->
      False_f
  | Bls12_381_g2_t _ ->
      False_f
  | Bls12_381_fr_t _ ->
      False_f
  | Sapling_transaction_t _ ->
      False_f
  | Ticket_t _ ->
      False_f
  | Pair_t ((l, _, _), (r, _, _), _) ->
      aux2 (fun l r -> Pair_f (l, r)) l r
  | Union_t ((l, _), (r, _), _) ->
      aux2 (fun l r -> Union_f (l, r)) l r
  | Option_t (t, _) ->
      aux1 (fun h -> Option_f h) t
  | List_t (t, _) ->
      aux1 (fun h -> List_f h) t
  | Map_t (_, t, _) ->
      aux1 (fun h -> Map_f h) t

(**
  Transforms a value potentially containing lazy storage in an intermediary
  state to a value containing lazy storage only represented by identifiers.

  Returns the updated value, the updated set of ids to copy, and the lazy
  storage diff to show on the receipt and apply on the storage.
*)
let extract_lazy_storage_updates ctxt mode ~temporary ids_to_copy acc ty x =
  let rec aux :
      type a.
      context ->
      unparsing_mode ->
      temporary:bool ->
      Lazy_storage.IdSet.t ->
      Lazy_storage.diffs ->
      a ty ->
      a ->
      has_lazy_storage:a has_lazy_storage ->
      (context * a * Lazy_storage.IdSet.t * Lazy_storage.diffs) tzresult Lwt.t
      =
   fun ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage ->
    Gas.consume ctxt Typecheck_costs.parse_instr_cycle
    >>?= fun ctxt ->
    match (has_lazy_storage, ty, x) with
    | (False_f, _, _) ->
        return (ctxt, x, ids_to_copy, acc)
    | (_, Big_map_t (_, _, _), map) ->
        diff_of_big_map ctxt mode ~temporary ~ids_to_copy map
        >|=? fun (diff, id, ctxt) ->
        let map =
          {
            map with
            diff = {map = Big_map_overlay.empty; size = 0};
            id = Some id;
          }
        in
        let diff = Lazy_storage.make Big_map id diff in
        let ids_to_copy = Lazy_storage.IdSet.add Big_map id ids_to_copy in
        (ctxt, map, ids_to_copy, diff :: acc)
    | (_, Sapling_state_t _, sapling_state) ->
        diff_of_sapling_state ctxt ~temporary ~ids_to_copy sapling_state
        >|=? fun (diff, id, ctxt) ->
        let sapling_state =
          Sapling.empty_state ~id ~memo_size:sapling_state.memo_size ()
        in
        let diff = Lazy_storage.make Sapling_state id diff in
        let ids_to_copy =
          Lazy_storage.IdSet.add Sapling_state id ids_to_copy
        in
        (ctxt, sapling_state, ids_to_copy, diff :: acc)
    | (Pair_f (hl, hr), Pair_t ((tyl, _, _), (tyr, _, _), _), (xl, xr)) ->
        aux ctxt mode ~temporary ids_to_copy acc tyl xl ~has_lazy_storage:hl
        >>=? fun (ctxt, xl, ids_to_copy, acc) ->
        aux ctxt mode ~temporary ids_to_copy acc tyr xr ~has_lazy_storage:hr
        >|=? fun (ctxt, xr, ids_to_copy, acc) ->
        (ctxt, (xl, xr), ids_to_copy, acc)
    | (Union_f (has_lazy_storage, _), Union_t ((ty, _), (_, _), _), L x) ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, L x, ids_to_copy, acc)
    | (Union_f (_, has_lazy_storage), Union_t ((_, _), (ty, _), _), R x) ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, R x, ids_to_copy, acc)
    | (Option_f has_lazy_storage, Option_t (ty, _), Some x) ->
        aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
        >|=? fun (ctxt, x, ids_to_copy, acc) -> (ctxt, Some x, ids_to_copy, acc)
    | (List_f has_lazy_storage, List_t (ty, _), l) ->
        fold_left_s
          (fun (ctxt, l, ids_to_copy, acc) x ->
            aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
            >|=? fun (ctxt, x, ids_to_copy, acc) ->
            (ctxt, list_cons x l, ids_to_copy, acc))
          (ctxt, list_empty, ids_to_copy, acc)
          l.elements
        >|=? fun (ctxt, l, ids_to_copy, acc) ->
        let reversed = {length = l.length; elements = List.rev l.elements} in
        (ctxt, reversed, ids_to_copy, acc)
    | (Map_f has_lazy_storage, Map_t (_, ty, _), (module M)) ->
        fold_left_s
          (fun (ctxt, m, ids_to_copy, acc) (k, x) ->
            aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
            >|=? fun (ctxt, x, ids_to_copy, acc) ->
            (ctxt, M.OPS.add k x m, ids_to_copy, acc))
          (ctxt, M.OPS.empty, ids_to_copy, acc)
          (M.OPS.bindings (fst M.boxed))
        >|=? fun (ctxt, m, ids_to_copy, acc) ->
        let module M = struct
          module OPS = M.OPS

          type key = M.key

          type value = M.value

          let key_ty = M.key_ty

          let boxed = (m, snd M.boxed)
        end in
        ( ctxt,
          (module M : Boxed_map with type key = M.key and type value = M.value),
          ids_to_copy,
          acc )
    | (_, Option_t (_, _), None) ->
        return (ctxt, None, ids_to_copy, acc)
    | _ ->
        assert false
   (* TODO: fix injectivity of types *)
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
    type a error.
    f:('acc, error) Fold_lazy_storage.result Lazy_storage.IdSet.fold_f ->
    init:'acc ->
    context ->
    a ty ->
    a ->
    has_lazy_storage:a has_lazy_storage ->
    (('acc, error) Fold_lazy_storage.result * context) tzresult =
 fun ~f ~init ctxt ty x ~has_lazy_storage ->
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle
  >>? fun ctxt ->
  match (has_lazy_storage, ty, x) with
  | (_, Big_map_t (_, _, _), {id = Some id}) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle
      >>? fun ctxt -> ok (f.f Big_map id (Fold_lazy_storage.Ok init), ctxt)
  | (_, Sapling_state_t _, {id = Some id}) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle
      >>? fun ctxt ->
      ok (f.f Sapling_state id (Fold_lazy_storage.Ok init), ctxt)
  | (False_f, _, _) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (_, Big_map_t (_, _, _), {id = None}) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (_, Sapling_state_t _, {id = None}) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (Pair_f (hl, hr), Pair_t ((tyl, _, _), (tyr, _, _), _), (xl, xr)) -> (
      fold_lazy_storage ~f ~init ctxt tyl xl ~has_lazy_storage:hl
      >>? fun (init, ctxt) ->
      match init with
      | Fold_lazy_storage.Ok init ->
          fold_lazy_storage ~f ~init ctxt tyr xr ~has_lazy_storage:hr
      | Fold_lazy_storage.Error ->
          ok (init, ctxt) )
  | (Union_f (has_lazy_storage, _), Union_t ((ty, _), (_, _), _), L x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (Union_f (_, has_lazy_storage), Union_t ((_, _), (ty, _), _), R x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (_, Option_t (_, _), None) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (Option_f has_lazy_storage, Option_t (ty, _), Some x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (List_f has_lazy_storage, List_t (ty, _), l) ->
      List.fold_left
        (fun (acc :
               (('acc, error) Fold_lazy_storage.result * context) tzresult)
             x ->
          acc
          >>? fun (init, ctxt) ->
          match init with
          | Fold_lazy_storage.Ok init ->
              fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
          | Fold_lazy_storage.Error ->
              ok (init, ctxt))
        (ok (Fold_lazy_storage.Ok init, ctxt))
        l.elements
  | (Map_f has_lazy_storage, Map_t (_, ty, _), m) ->
      map_fold
        (fun _
             v
             (acc :
               (('acc, error) Fold_lazy_storage.result * context) tzresult) ->
          acc
          >>? fun (init, ctxt) ->
          match init with
          | Fold_lazy_storage.Ok init ->
              fold_lazy_storage ~f ~init ctxt ty v ~has_lazy_storage
          | Fold_lazy_storage.Error ->
              ok (init, ctxt))
        m
        (ok (Fold_lazy_storage.Ok init, ctxt))
  | _ ->
      (* TODO: fix injectivity of types *) assert false

let collect_lazy_storage ctxt ty x =
  let has_lazy_storage = has_lazy_storage ty in
  let f kind id (acc : (_, never) Fold_lazy_storage.result) =
    let acc = match acc with Fold_lazy_storage.Ok acc -> acc in
    Fold_lazy_storage.Ok (Lazy_storage.IdSet.add kind id acc)
  in
  fold_lazy_storage ~f:{f} ~init:no_lazy_storage_id ctxt ty x ~has_lazy_storage
  >>? fun (ids, ctxt) ->
  match ids with Fold_lazy_storage.Ok ids -> ok (ids, ctxt)

let extract_lazy_storage_diff ctxt mode ~temporary ~to_duplicate ~to_update ty
    v =
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
  | [] ->
      (v, None, ctxt)
  | diffs ->
      (v, Some diffs (* do not reverse *), ctxt)

let list_of_big_map_ids ids =
  Lazy_storage.IdSet.fold Big_map (fun id acc -> id :: acc) ids []

let parse_data = parse_data ~stack_depth:0

let parse_instr :
    type a s.
    ?type_logger:type_logger ->
    tc_context ->
    context ->
    legacy:bool ->
    Script.node ->
    (a, s) stack_ty ->
    ((a, s) judgement * context) tzresult Lwt.t =
 fun ?type_logger tc_context ctxt ~legacy script_instr stack_ty ->
  parse_instr
    ~stack_depth:0
    ?type_logger
    tc_context
    ctxt
    ~legacy
    script_instr
    stack_ty

let unparse_data = unparse_data ~stack_depth:0

let unparse_code = unparse_code ~stack_depth:0

let parse_contract ~legacy context loc arg_ty contract ~entrypoint =
  parse_contract ~stack_depth:0 ~legacy context loc arg_ty contract ~entrypoint

let parse_comparable_ty = parse_comparable_ty ~stack_depth:0

let parse_big_map_value_ty = parse_big_map_value_ty ~stack_depth:0

let parse_packable_ty = parse_packable_ty ~stack_depth:0

let parse_parameter_ty = parse_parameter_ty ~stack_depth:0

let parse_any_ty = parse_any_ty ~stack_depth:0

let parse_ty = parse_ty ~stack_depth:0

let get_single_sapling_state ctxt ty x =
  let has_lazy_storage = has_lazy_storage ty in
  let f (type i a u) (kind : (i, a, u) Lazy_storage.Kind.t) (id : i)
      single_id_opt : (Sapling.Id.t option, unit) Fold_lazy_storage.result =
    match kind with
    | Lazy_storage.Kind.Sapling_state -> (
      match single_id_opt with
      | Fold_lazy_storage.Ok None ->
          Fold_lazy_storage.Ok (Some id)
      | Fold_lazy_storage.Ok (Some _) ->
          Fold_lazy_storage.Error (* more than one *)
      | Fold_lazy_storage.Error ->
          single_id_opt )
    | _ ->
        single_id_opt
  in
  fold_lazy_storage ~f:{f} ~init:None ctxt ty x ~has_lazy_storage
  >>? fun (id, ctxt) ->
  match id with
  | Fold_lazy_storage.Ok (Some id) ->
      ok (Some id, ctxt)
  | Fold_lazy_storage.Ok None | Fold_lazy_storage.Error ->
      ok (None, ctxt)
