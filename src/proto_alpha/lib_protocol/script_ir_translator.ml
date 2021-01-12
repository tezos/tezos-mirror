(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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
module Tc_context = Script_tc_context

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

type tc_context = Tc_context.t

type unparsing_mode = Optimized | Readable | Optimized_legacy

type type_logger =
  Script.location -> Script.expr list -> Script.expr list -> unit

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
      | (Type_namespace, Type_namespace)
      | (Instr_namespace, Instr_namespace)
      | (Constant_namespace, Constant_namespace) ->
          Invalid_primitive (loc, exp_prims, name)
      | (ns, _) -> Invalid_namespace (loc, name, exp_ns, ns))

let check_kind kinds expr =
  let kind = kind expr in
  if List.exists (kind_equal kind) kinds then Result.return_unit
  else
    let loc = location expr in
    error (Invalid_kind (loc, kinds, kind))

(* ---- Unparsing (Typed IR -> Untyped expressions) of types -----------------*)

(* This part contains the unparsing that does not depend on parsing
   (everything that cannot contain a lambda). The rest is located at
   the end of the file. *)

let rec ty_of_comparable_ty : type a. a comparable_ty -> a ty = function
  | Unit_key tname -> Unit_t tname
  | Never_key tname -> Never_t tname
  | Int_key tname -> Int_t tname
  | Nat_key tname -> Nat_t tname
  | Signature_key tname -> Signature_t tname
  | String_key tname -> String_t tname
  | Bytes_key tname -> Bytes_t tname
  | Mutez_key tname -> Mutez_t tname
  | Bool_key tname -> Bool_t tname
  | Key_hash_key tname -> Key_hash_t tname
  | Key_key tname -> Key_t tname
  | Timestamp_key tname -> Timestamp_t tname
  | Address_key tname -> Address_t tname
  | Chain_id_key tname -> Chain_id_t tname
  | Pair_key (l, r, tname) ->
      Pair_t (ty_of_comparable_ty l, ty_of_comparable_ty r, tname)
  | Union_key (l, r, tname) ->
      Union_t
        ((ty_of_comparable_ty l, None), (ty_of_comparable_ty r, None), tname)
  | Option_key (t, tname) -> Option_t (ty_of_comparable_ty t, tname)

let add_field_annot a = function
  | Prim (loc, prim, args, annots) ->
      Prim (loc, prim, args, annots @ unparse_field_annot a)
  | expr -> expr

let add_entrypoint_annot entrypoint expr =
  match (entrypoint, expr) with
  | (Some name, Prim (loc, prim, args, annots)) ->
      Prim (loc, prim, args, annots @ [Entrypoint.unparse_as_field_annot name])
  | (_, expr) -> expr

let rec unparse_comparable_ty_uncarbonated :
    type a loc. loc:loc -> a comparable_ty -> loc Script.michelson_node =
 fun ~loc -> function
  | Unit_key _meta -> Prim (loc, T_unit, [], [])
  | Never_key _meta -> Prim (loc, T_never, [], [])
  | Int_key _meta -> Prim (loc, T_int, [], [])
  | Nat_key _meta -> Prim (loc, T_nat, [], [])
  | Signature_key _meta -> Prim (loc, T_signature, [], [])
  | String_key _meta -> Prim (loc, T_string, [], [])
  | Bytes_key _meta -> Prim (loc, T_bytes, [], [])
  | Mutez_key _meta -> Prim (loc, T_mutez, [], [])
  | Bool_key _meta -> Prim (loc, T_bool, [], [])
  | Key_hash_key _meta -> Prim (loc, T_key_hash, [], [])
  | Key_key _meta -> Prim (loc, T_key, [], [])
  | Timestamp_key _meta -> Prim (loc, T_timestamp, [], [])
  | Address_key _meta -> Prim (loc, T_address, [], [])
  | Chain_id_key _meta -> Prim (loc, T_chain_id, [], [])
  | Pair_key (l, r, _meta) -> (
      let tl = unparse_comparable_ty_uncarbonated ~loc l in
      let tr = unparse_comparable_ty_uncarbonated ~loc r in
      (* Fold [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an] *)
      (* Note that the folding does not happen if the pair on the right has a
         field annotation because this annotation would be lost *)
      match tr with
      | Prim (_, T_pair, ts, []) -> Prim (loc, T_pair, tl :: ts, [])
      | _ -> Prim (loc, T_pair, [tl; tr], []))
  | Union_key (l, r, _meta) ->
      let tl = unparse_comparable_ty_uncarbonated ~loc l in
      let tr = unparse_comparable_ty_uncarbonated ~loc r in
      Prim (loc, T_or, [tl; tr], [])
  | Option_key (t, _meta) ->
      Prim (loc, T_option, [unparse_comparable_ty_uncarbonated ~loc t], [])

let unparse_memo_size ~loc memo_size =
  let z = Sapling.Memo_size.unparse_to_z memo_size in
  Int (loc, z)

let rec unparse_ty_uncarbonated :
    type a loc. loc:loc -> a ty -> loc Script.michelson_node =
 fun ~loc ty ->
  let (name, args) =
    match ty with
    | Unit_t _meta -> (T_unit, [])
    | Int_t _meta -> (T_int, [])
    | Nat_t _meta -> (T_nat, [])
    | Signature_t _meta -> (T_signature, [])
    | String_t _meta -> (T_string, [])
    | Bytes_t _meta -> (T_bytes, [])
    | Mutez_t _meta -> (T_mutez, [])
    | Bool_t _meta -> (T_bool, [])
    | Key_hash_t _meta -> (T_key_hash, [])
    | Key_t _meta -> (T_key, [])
    | Timestamp_t _meta -> (T_timestamp, [])
    | Address_t _meta -> (T_address, [])
    | Operation_t _meta -> (T_operation, [])
    | Chain_id_t _meta -> (T_chain_id, [])
    | Never_t _meta -> (T_never, [])
    | Bls12_381_g1_t _meta -> (T_bls12_381_g1, [])
    | Bls12_381_g2_t _meta -> (T_bls12_381_g2, [])
    | Bls12_381_fr_t _meta -> (T_bls12_381_fr, [])
    | Contract_t (ut, _meta) ->
        let t = unparse_ty_uncarbonated ~loc ut in
        (T_contract, [t])
    | Pair_t (utl, utr, _meta) -> (
        let tl = unparse_ty_uncarbonated ~loc utl in
        let tr = unparse_ty_uncarbonated ~loc utr in
        (* Fold [pair a1 (pair ... (pair an-1 an))] into [pair a1 ... an] *)
        (* Note that the folding does not happen if the pair on the right has an
           annotation because this annotation would be lost *)
        match tr with
        | Prim (_, T_pair, ts, []) -> (T_pair, tl :: ts)
        | _ -> (T_pair, [tl; tr]))
    | Union_t ((utl, l_field), (utr, r_field), _meta) ->
        let utl = unparse_ty_uncarbonated ~loc utl in
        let tl = add_field_annot l_field utl in
        let utr = unparse_ty_uncarbonated ~loc utr in
        let tr = add_field_annot r_field utr in
        (T_or, [tl; tr])
    | Lambda_t (uta, utr, _meta) ->
        let ta = unparse_ty_uncarbonated ~loc uta in
        let tr = unparse_ty_uncarbonated ~loc utr in
        (T_lambda, [ta; tr])
    | Option_t (ut, _meta) ->
        let ut = unparse_ty_uncarbonated ~loc ut in
        (T_option, [ut])
    | List_t (ut, _meta) ->
        let t = unparse_ty_uncarbonated ~loc ut in
        (T_list, [t])
    | Ticket_t (ut, _meta) ->
        let t = unparse_comparable_ty_uncarbonated ~loc ut in
        (T_ticket, [t])
    | Set_t (ut, _meta) ->
        let t = unparse_comparable_ty_uncarbonated ~loc ut in
        (T_set, [t])
    | Map_t (uta, utr, _meta) ->
        let ta = unparse_comparable_ty_uncarbonated ~loc uta in
        let tr = unparse_ty_uncarbonated ~loc utr in
        (T_map, [ta; tr])
    | Big_map_t (uta, utr, _meta) ->
        let ta = unparse_comparable_ty_uncarbonated ~loc uta in
        let tr = unparse_ty_uncarbonated ~loc utr in
        (T_big_map, [ta; tr])
    | Sapling_transaction_t (memo_size, _meta) ->
        (T_sapling_transaction, [unparse_memo_size ~loc memo_size])
    | Sapling_state_t (memo_size, _meta) ->
        (T_sapling_state, [unparse_memo_size ~loc memo_size])
    | Chest_key_t _meta -> (T_chest_key, [])
    | Chest_t _meta -> (T_chest, [])
  in
  Prim (loc, name, args, [])

let unparse_ty ~loc ctxt ty =
  Gas.consume ctxt (Unparse_costs.unparse_type ty) >|? fun ctxt ->
  (unparse_ty_uncarbonated ~loc ty, ctxt)

let unparse_comparable_ty ~loc ctxt comp_ty =
  Gas.consume ctxt (Unparse_costs.unparse_comparable_type comp_ty)
  >|? fun ctxt -> (unparse_comparable_ty_uncarbonated ~loc comp_ty, ctxt)

let unparse_parameter_ty ~loc ctxt ~root_name ty =
  unparse_ty ~loc ctxt ty >|? fun (unparsed, ctxt) ->
  (add_entrypoint_annot root_name unparsed, ctxt)

let[@coq_struct "function_parameter"] rec strip_var_annots = function
  | (Int _ | String _ | Bytes _) as atom -> atom
  | Seq (loc, args) -> Seq (loc, List.map strip_var_annots args)
  | Prim (loc, name, args, annots) ->
      let not_var_annot s = Compare.Char.(s.[0] <> '@') in
      let annots = List.filter not_var_annot annots in
      Prim (loc, name, List.map strip_var_annots args, annots)

let serialize_ty_for_error ty =
  (*
    Types are bounded by [Constants.michelson_maximum_type_size], so
    [unparse_ty_uncarbonated], [strip_var_annots], and [strip_locations] are
    bounded in time.

    It is hence OK to use them in errors that are not caught in the validation
    (only once in apply).
  *)
  let ty = unparse_ty_uncarbonated ~loc:() ty in
  Micheline.strip_locations (strip_var_annots ty)

let[@coq_axiom_with_reason "gadt"] rec comparable_ty_of_ty :
    type a.
    context -> Script.location -> a ty -> (a comparable_ty * context) tzresult =
 fun ctxt loc ty ->
  Gas.consume ctxt Typecheck_costs.comparable_ty_of_ty_cycle >>? fun ctxt ->
  match ty with
  | Unit_t tname -> ok ((Unit_key tname : a comparable_ty), ctxt)
  | Never_t tname -> ok (Never_key tname, ctxt)
  | Int_t tname -> ok (Int_key tname, ctxt)
  | Nat_t tname -> ok (Nat_key tname, ctxt)
  | Signature_t tname -> ok (Signature_key tname, ctxt)
  | String_t tname -> ok (String_key tname, ctxt)
  | Bytes_t tname -> ok (Bytes_key tname, ctxt)
  | Mutez_t tname -> ok (Mutez_key tname, ctxt)
  | Bool_t tname -> ok (Bool_key tname, ctxt)
  | Key_hash_t tname -> ok (Key_hash_key tname, ctxt)
  | Key_t tname -> ok (Key_key tname, ctxt)
  | Timestamp_t tname -> ok (Timestamp_key tname, ctxt)
  | Address_t tname -> ok (Address_key tname, ctxt)
  | Chain_id_t tname -> ok (Chain_id_key tname, ctxt)
  | Pair_t (l, r, pname) ->
      comparable_ty_of_ty ctxt loc l >>? fun (lty, ctxt) ->
      comparable_ty_of_ty ctxt loc r >|? fun (rty, ctxt) ->
      (Pair_key (lty, rty, pname), ctxt)
  | Union_t ((l, _al), (r, _ar), tname) ->
      comparable_ty_of_ty ctxt loc l >>? fun (lty, ctxt) ->
      comparable_ty_of_ty ctxt loc r >|? fun (rty, ctxt) ->
      (Union_key (lty, rty, tname), ctxt)
  | Option_t (tt, tname) ->
      comparable_ty_of_ty ctxt loc tt >|? fun (ty, ctxt) ->
      (Option_key (ty, tname), ctxt)
  | Lambda_t _ | List_t _ | Ticket_t _ | Set_t _ | Map_t _ | Big_map_t _
  | Contract_t _ | Operation_t _ | Bls12_381_fr_t _ | Bls12_381_g1_t _
  | Bls12_381_g2_t _ | Sapling_state_t _ | Sapling_transaction_t _
  | Chest_key_t _ | Chest_t _ ->
      let t = serialize_ty_for_error ty in
      error (Comparable_type_expected (loc, t))

let rec unparse_stack_uncarbonated :
    type a s. (a, s) stack_ty -> Script.expr list = function
  | Bot_t -> []
  | Item_t (ty, rest) ->
      let uty = unparse_ty_uncarbonated ~loc:() ty in
      let urest = unparse_stack_uncarbonated rest in
      strip_locations uty :: urest

let serialize_stack_for_error ctxt stack_ty =
  match Gas.level ctxt with
  | Unaccounted -> unparse_stack_uncarbonated stack_ty
  | Limited _ -> []

let unparse_unit ~loc ctxt () = ok (Prim (loc, D_Unit, [], []), ctxt)

let unparse_int ~loc ctxt v = ok (Int (loc, Script_int.to_zint v), ctxt)

let unparse_nat ~loc ctxt v = ok (Int (loc, Script_int.to_zint v), ctxt)

let unparse_string ~loc ctxt s =
  ok (String (loc, Script_string.to_string s), ctxt)

let unparse_bytes ~loc ctxt s = ok (Bytes (loc, s), ctxt)

let unparse_bool ~loc ctxt b =
  ok (Prim (loc, (if b then D_True else D_False), [], []), ctxt)

let unparse_timestamp ~loc ctxt mode t =
  match mode with
  | Optimized | Optimized_legacy ->
      ok (Int (loc, Script_timestamp.to_zint t), ctxt)
  | Readable -> (
      Gas.consume ctxt Unparse_costs.timestamp_readable >>? fun ctxt ->
      match Script_timestamp.to_notation t with
      | None -> ok (Int (loc, Script_timestamp.to_zint t), ctxt)
      | Some s -> ok (String (loc, s), ctxt))

let unparse_address ~loc ctxt mode {destination; entrypoint} =
  Gas.consume ctxt Unparse_costs.contract >|? fun ctxt ->
  match mode with
  | Optimized | Optimized_legacy ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn
          Data_encoding.(tup2 Destination.encoding Entrypoint.value_encoding)
          (destination, entrypoint)
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      let notation =
        Destination.to_b58check destination
        ^ Entrypoint.to_address_suffix entrypoint
      in
      (String (loc, notation), ctxt)

let unparse_contract ~loc ctxt mode {arg_ty = _; address} =
  unparse_address ~loc ctxt mode address

let unparse_signature ~loc ctxt mode s =
  let s = Script_signature.get s in
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.signature_optimized >|? fun ctxt ->
      let bytes = Data_encoding.Binary.to_bytes_exn Signature.encoding s in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.signature_readable >|? fun ctxt ->
      (String (loc, Signature.to_b58check s), ctxt)

let unparse_mutez ~loc ctxt v = ok (Int (loc, Z.of_int64 (Tez.to_mutez v)), ctxt)

let unparse_key ~loc ctxt mode k =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.public_key_optimized >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding k
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.public_key_readable >|? fun ctxt ->
      (String (loc, Signature.Public_key.to_b58check k), ctxt)

let unparse_key_hash ~loc ctxt mode k =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.key_hash_optimized >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Signature.Public_key_hash.encoding k
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.key_hash_readable >|? fun ctxt ->
      (String (loc, Signature.Public_key_hash.to_b58check k), ctxt)

let unparse_operation ~loc ctxt {piop; lazy_storage_diff = _} =
  let bytes =
    Data_encoding.Binary.to_bytes_exn Operation.internal_operation_encoding piop
  in
  Gas.consume ctxt (Unparse_costs.operation bytes) >|? fun ctxt ->
  (Bytes (loc, bytes), ctxt)

let unparse_chain_id ~loc ctxt mode chain_id =
  match mode with
  | Optimized | Optimized_legacy ->
      Gas.consume ctxt Unparse_costs.chain_id_optimized >|? fun ctxt ->
      let bytes =
        Data_encoding.Binary.to_bytes_exn Script_chain_id.encoding chain_id
      in
      (Bytes (loc, bytes), ctxt)
  | Readable ->
      Gas.consume ctxt Unparse_costs.chain_id_readable >|? fun ctxt ->
      (String (loc, Script_chain_id.to_b58check chain_id), ctxt)

let unparse_bls12_381_g1 ~loc ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_g1 >|? fun ctxt ->
  let bytes = Script_bls.G1.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_bls12_381_g2 ~loc ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_g2 >|? fun ctxt ->
  let bytes = Script_bls.G2.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_bls12_381_fr ~loc ctxt x =
  Gas.consume ctxt Unparse_costs.bls12_381_fr >|? fun ctxt ->
  let bytes = Script_bls.Fr.to_bytes x in
  (Bytes (loc, bytes), ctxt)

let unparse_with_data_encoding ~loc ctxt s unparse_cost encoding =
  Lwt.return
    ( Gas.consume ctxt unparse_cost >|? fun ctxt ->
      let bytes = Data_encoding.Binary.to_bytes_exn encoding s in
      (Bytes (loc, bytes), ctxt) )

(* -- Unparsing data of complex types -- *)

type ('ty, 'depth) comb_witness =
  | Comb_Pair : ('t, 'd) comb_witness -> (_ * 't, unit -> 'd) comb_witness
  | Comb_Any : (_, _) comb_witness

let unparse_pair (type r) ~loc unparse_l unparse_r ctxt mode
    (r_comb_witness : (r, unit -> unit -> _) comb_witness) (l, (r : r)) =
  unparse_l ctxt l >>=? fun (l, ctxt) ->
  unparse_r ctxt r >|=? fun (r, ctxt) ->
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
        Micheline.Seq (loc, l :: r)
    | ( Optimized,
        Comb_Pair (Comb_Pair _),
        Prim (_, D_Pair, [x2; Prim (_, D_Pair, [x3; x4], [])], []) ) ->
        (* Optimized case n = 4 *)
        Micheline.Seq (loc, [l; x2; x3; x4])
    | (Readable, Comb_Pair _, Prim (_, D_Pair, xs, [])) ->
        (* Readable case n > 2 *)
        Prim (loc, D_Pair, l :: xs, [])
    | _ ->
        (* The remaining cases are:
            - Optimized n = 2,
            - Optimized n = 3, and
            - Readable n = 2,
            - Optimized_legacy, any n *)
        Prim (loc, D_Pair, [l; r], [])
  in
  (res, ctxt)

let unparse_union ~loc unparse_l unparse_r ctxt = function
  | L l ->
      unparse_l ctxt l >|=? fun (l, ctxt) -> (Prim (loc, D_Left, [l], []), ctxt)
  | R r ->
      unparse_r ctxt r >|=? fun (r, ctxt) -> (Prim (loc, D_Right, [r], []), ctxt)

let unparse_option ~loc unparse_v ctxt = function
  | Some v ->
      unparse_v ctxt v >|=? fun (v, ctxt) -> (Prim (loc, D_Some, [v], []), ctxt)
  | None -> return (Prim (loc, D_None, [], []), ctxt)

(* -- Unparsing data of comparable types -- *)

let comparable_comb_witness2 :
    type t. t comparable_ty -> (t, unit -> unit -> unit) comb_witness = function
  | Pair_key (_, Pair_key _, _) -> Comb_Pair (Comb_Pair Comb_Any)
  | Pair_key _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

let[@coq_axiom_with_reason "gadt"] rec unparse_comparable_data :
    type a loc.
    loc:loc ->
    context ->
    unparsing_mode ->
    a comparable_ty ->
    a ->
    (loc Script.michelson_node * context) tzresult Lwt.t =
 fun ~loc ctxt mode ty a ->
  (* No need for stack_depth here. Unlike [unparse_data],
     [unparse_comparable_data] doesn't call [unparse_code].
     The stack depth is bounded by the type depth, currently bounded
     by 1000 (michelson_maximum_type_size). *)
  Gas.consume ctxt Unparse_costs.unparse_data_cycle
  (* We could have a smaller cost but let's keep it consistent with
     [unparse_data] for now. *)
  >>?=
  fun ctxt ->
  match (ty, a) with
  | (Unit_key _, v) -> Lwt.return @@ unparse_unit ~loc ctxt v
  | (Int_key _, v) -> Lwt.return @@ unparse_int ~loc ctxt v
  | (Nat_key _, v) -> Lwt.return @@ unparse_nat ~loc ctxt v
  | (String_key _, s) -> Lwt.return @@ unparse_string ~loc ctxt s
  | (Bytes_key _, s) -> Lwt.return @@ unparse_bytes ~loc ctxt s
  | (Bool_key _, b) -> Lwt.return @@ unparse_bool ~loc ctxt b
  | (Timestamp_key _, t) -> Lwt.return @@ unparse_timestamp ~loc ctxt mode t
  | (Address_key _, address) ->
      Lwt.return @@ unparse_address ~loc ctxt mode address
  | (Signature_key _, s) -> Lwt.return @@ unparse_signature ~loc ctxt mode s
  | (Mutez_key _, v) -> Lwt.return @@ unparse_mutez ~loc ctxt v
  | (Key_key _, k) -> Lwt.return @@ unparse_key ~loc ctxt mode k
  | (Key_hash_key _, k) -> Lwt.return @@ unparse_key_hash ~loc ctxt mode k
  | (Chain_id_key _, chain_id) ->
      Lwt.return @@ unparse_chain_id ~loc ctxt mode chain_id
  | (Pair_key (tl, tr, _), pair) ->
      let r_witness = comparable_comb_witness2 tr in
      let unparse_l ctxt v = unparse_comparable_data ~loc ctxt mode tl v in
      let unparse_r ctxt v = unparse_comparable_data ~loc ctxt mode tr v in
      unparse_pair ~loc unparse_l unparse_r ctxt mode r_witness pair
  | (Union_key (tl, tr, _), v) ->
      let unparse_l ctxt v = unparse_comparable_data ~loc ctxt mode tl v in
      let unparse_r ctxt v = unparse_comparable_data ~loc ctxt mode tr v in
      unparse_union ~loc unparse_l unparse_r ctxt v
  | (Option_key (t, _), v) ->
      let unparse_v ctxt v = unparse_comparable_data ~loc ctxt mode t v in
      unparse_option ~loc unparse_v ctxt v
  | (Never_key _, _) -> .

let pack_node unparsed ctxt =
  Gas.consume ctxt (Script.strip_locations_cost unparsed) >>? fun ctxt ->
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      expr_encoding
      (Micheline.strip_locations unparsed)
  in
  Gas.consume ctxt (Script.serialized_cost bytes) >|? fun ctxt ->
  let bytes = Bytes.cat (Bytes.of_string "\005") bytes in
  (bytes, ctxt)

let pack_comparable_data ctxt typ data ~mode =
  unparse_comparable_data ~loc:() ctxt mode typ data
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
  | Unit_key _ | Never_key _ | Int_key _ | Nat_key _ | Signature_key _
  | String_key _ | Bytes_key _ | Mutez_key _ | Bool_key _ | Key_hash_key _
  | Key_key _ | Timestamp_key _ | Chain_id_key _ | Address_key _ | Pair_key _
  | Union_key _ | Option_key _ ->
      ()

let check_dupable_ty ctxt loc ty =
  let rec aux : type a. location -> a ty -> (unit, error trace) Gas_monad.t =
   fun loc ty ->
    let open Gas_monad in
    consume_gas Typecheck_costs.check_dupable_cycle >>$ fun () ->
    match ty with
    | Unit_t _ -> return_unit
    | Int_t _ -> return_unit
    | Nat_t _ -> return_unit
    | Signature_t _ -> return_unit
    | String_t _ -> return_unit
    | Bytes_t _ -> return_unit
    | Mutez_t _ -> return_unit
    | Key_hash_t _ -> return_unit
    | Key_t _ -> return_unit
    | Timestamp_t _ -> return_unit
    | Address_t _ -> return_unit
    | Bool_t _ -> return_unit
    | Contract_t (_, _) -> return_unit
    | Operation_t _ -> return_unit
    | Chain_id_t _ -> return_unit
    | Never_t _ -> return_unit
    | Bls12_381_g1_t _ -> return_unit
    | Bls12_381_g2_t _ -> return_unit
    | Bls12_381_fr_t _ -> return_unit
    | Sapling_state_t _ -> return_unit
    | Sapling_transaction_t _ -> return_unit
    | Chest_t _ -> return_unit
    | Chest_key_t _ -> return_unit
    | Ticket_t _ -> of_result (error (Unexpected_ticket loc))
    | Pair_t (ty_a, ty_b, _) -> aux loc ty_a >>$ fun () -> aux loc ty_b
    | Union_t ((ty_a, _), (ty_b, _), _) ->
        aux loc ty_a >>$ fun () -> aux loc ty_b
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
    | Option_t (ty, _) -> aux loc ty
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
  res >|? fun () -> ctxt

(* ---- Equality witnesses --------------------------------------------------*)

type ('ta, 'tb) eq = Eq : ('same, 'same) eq

let merge_type_metadata :
    type error_trace.
    error_details:error_trace error_details ->
    'a ty_metadata ->
    'b ty_metadata ->
    ('a ty_metadata, error_trace) result =
 fun ~error_details {size = size_a} {size = size_b} ->
  Type_size.merge ~error_details size_a size_b >|? fun size -> {size}

let default_merge_type_error ty1 ty2 =
  let ty1 = serialize_ty_for_error ty1 in
  let ty2 = serialize_ty_for_error ty2 in
  Inconsistent_types (None, ty1, ty2)

(* Takes two comparable types and simultaneously merge their annotations and
   check that they represent the same type.

   The result contains:
   - an equality witness between the types of the two inputs
   - the merged type
   - an updated context (for gas consumption)

   The tzresult monad is used at two levels: the inner tzresult
   is used for tracking merge errors (types of different shapes
   or annotation mismatches), the outer tzresult is used only
   for gas consumption. Separating these two error cases like
   this allows to recover from a type comparison error without
   reverting the gas consumption.
 *)
let rec merge_comparable_types :
    type ta tb error_trace.
    legacy:bool ->
    error_details:error_trace error_details ->
    ta comparable_ty ->
    tb comparable_ty ->
    ( (ta comparable_ty, tb comparable_ty) eq * ta comparable_ty,
      error_trace )
    Gas_monad.t =
  let open Gas_monad in
  fun ~legacy ~error_details ta tb ->
    consume_gas Typecheck_costs.merge_cycle >>$ fun () ->
    let merge_type_metadata meta_a meta_b =
      of_result @@ merge_type_metadata ~error_details meta_a meta_b
    in
    let return f eq annot_a annot_b :
        ( (ta comparable_ty, tb comparable_ty) eq * ta comparable_ty,
          error_trace )
        gas_monad =
      merge_type_metadata annot_a annot_b >>$ fun annot -> return (eq, f annot)
    in
    match (ta, tb) with
    | (Unit_key annot_a, Unit_key annot_b) ->
        return (fun annot -> Unit_key annot) Eq annot_a annot_b
    | (Never_key annot_a, Never_key annot_b) ->
        return (fun annot -> Never_key annot) Eq annot_a annot_b
    | (Int_key annot_a, Int_key annot_b) ->
        return (fun annot -> Int_key annot) Eq annot_a annot_b
    | (Nat_key annot_a, Nat_key annot_b) ->
        return (fun annot -> Nat_key annot) Eq annot_a annot_b
    | (Signature_key annot_a, Signature_key annot_b) ->
        return (fun annot -> Signature_key annot) Eq annot_a annot_b
    | (String_key annot_a, String_key annot_b) ->
        return (fun annot -> String_key annot) Eq annot_a annot_b
    | (Bytes_key annot_a, Bytes_key annot_b) ->
        return (fun annot -> Bytes_key annot) Eq annot_a annot_b
    | (Mutez_key annot_a, Mutez_key annot_b) ->
        return (fun annot -> Mutez_key annot) Eq annot_a annot_b
    | (Bool_key annot_a, Bool_key annot_b) ->
        return (fun annot -> Bool_key annot) Eq annot_a annot_b
    | (Key_hash_key annot_a, Key_hash_key annot_b) ->
        return (fun annot -> Key_hash_key annot) Eq annot_a annot_b
    | (Key_key annot_a, Key_key annot_b) ->
        return (fun annot -> Key_key annot) Eq annot_a annot_b
    | (Timestamp_key annot_a, Timestamp_key annot_b) ->
        return (fun annot -> Timestamp_key annot) Eq annot_a annot_b
    | (Chain_id_key annot_a, Chain_id_key annot_b) ->
        return (fun annot -> Chain_id_key annot) Eq annot_a annot_b
    | (Address_key annot_a, Address_key annot_b) ->
        return (fun annot -> Address_key annot) Eq annot_a annot_b
    | (Pair_key (left_a, right_a, annot_a), Pair_key (left_b, right_b, annot_b))
      ->
        merge_type_metadata annot_a annot_b >>$ fun annot ->
        merge_comparable_types ~legacy ~error_details left_a left_b
        >>$ fun (Eq, left) ->
        merge_comparable_types ~legacy ~error_details right_a right_b
        >|$ fun (Eq, right) ->
        ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
          Pair_key (left, right, annot) )
    | ( Union_key (left_a, right_a, annot_a),
        Union_key (left_b, right_b, annot_b) ) ->
        merge_type_metadata annot_a annot_b >>$ fun annot ->
        merge_comparable_types ~legacy ~error_details left_a left_b
        >>$ fun (Eq, left) ->
        merge_comparable_types ~legacy ~error_details right_a right_b
        >|$ fun (Eq, right) ->
        ( (Eq : (ta comparable_ty, tb comparable_ty) eq),
          Union_key (left, right, annot) )
    | (Option_key (ta, annot_a), Option_key (tb, annot_b)) ->
        merge_type_metadata annot_a annot_b >>$ fun annot ->
        merge_comparable_types ~legacy ~error_details ta tb >|$ fun (Eq, t) ->
        ((Eq : (ta comparable_ty, tb comparable_ty) eq), Option_key (t, annot))
    | (_, _) ->
        of_result
        @@ Error
             (match error_details with
             | Fast -> (Inconsistent_types_fast : error_trace)
             | Informative ->
                 trace_of_error
                 @@ default_merge_type_error
                      (ty_of_comparable_ty ta)
                      (ty_of_comparable_ty tb))

(* This function does not distinguish gas errors from merge errors. If you need
   to recover from a type mismatch and consume the exact gas for the failed
   comparison, use [merge_comparable_types] instead.
*)
let comparable_ty_eq :
    type ta tb.
    context ->
    ta comparable_ty ->
    tb comparable_ty ->
    ((ta comparable_ty, tb comparable_ty) eq * context) tzresult =
 fun ctxt ta tb ->
  Gas_monad.run
    ctxt
    (merge_comparable_types ~legacy:true ~error_details:Informative ta tb)
  >>? fun (eq_ty, ctxt) ->
  eq_ty >|? fun (eq, _ty) -> (eq, ctxt)

let merge_memo_sizes :
    type error_trace.
    error_details:error_trace error_details ->
    Sapling.Memo_size.t ->
    Sapling.Memo_size.t ->
    (Sapling.Memo_size.t, error_trace) result =
 fun ~error_details ms1 ms2 ->
  if Sapling.Memo_size.equal ms1 ms2 then ok ms1
  else
    Error
      (match error_details with
      | Fast -> Inconsistent_types_fast
      | Informative -> trace_of_error @@ Inconsistent_memo_sizes (ms1, ms2))

(* Same as merge_comparable_types but for any types *)
let merge_types :
    type a b error_trace.
    legacy:bool ->
    error_details:error_trace error_details ->
    Script.location ->
    a ty ->
    b ty ->
    ((a ty, b ty) eq * a ty, error_trace) Gas_monad.t =
  let open Gas_monad in
  fun ~legacy ~error_details loc ty1 ty2 ->
    let merge_type_metadata tn1 tn2 =
      of_result @@ merge_type_metadata ~error_details tn1 tn2
      |> Gas_monad.record_trace_eval ~error_details (fun () ->
             let ty1 = serialize_ty_for_error ty1 in
             let ty2 = serialize_ty_for_error ty2 in
             Inconsistent_types (Some loc, ty1, ty2))
    in
    let merge_field_annot ~legacy tn1 tn2 =
      of_result (merge_field_annot ~legacy ~error_details tn1 tn2)
    in
    let merge_memo_sizes ms1 ms2 =
      of_result (merge_memo_sizes ~error_details ms1 ms2)
    in
    let rec help :
        type ta tb.
        ta ty -> tb ty -> ((ta ty, tb ty) eq * ta ty, error_trace) gas_monad =
     fun ty1 ty2 ->
      help0 ty1 ty2
      |> Gas_monad.record_trace_eval ~error_details (fun () ->
             default_merge_type_error ty1 ty2)
    and help0 :
        type ta tb.
        ta ty -> tb ty -> ((ta ty, tb ty) eq * ta ty, error_trace) gas_monad =
     fun ty1 ty2 ->
      consume_gas Typecheck_costs.merge_cycle >>$ fun () ->
      let return f eq annot_a annot_b :
          ((ta ty, tb ty) eq * ta ty, error_trace) gas_monad =
        merge_type_metadata annot_a annot_b >>$ fun annot -> return (eq, f annot)
      in
      match (ty1, ty2) with
      | (Unit_t tn1, Unit_t tn2) ->
          return (fun tname -> Unit_t tname) Eq tn1 tn2
      | (Int_t tn1, Int_t tn2) -> return (fun tname -> Int_t tname) Eq tn1 tn2
      | (Nat_t tn1, Nat_t tn2) -> return (fun tname -> Nat_t tname) Eq tn1 tn2
      | (Key_t tn1, Key_t tn2) -> return (fun tname -> Key_t tname) Eq tn1 tn2
      | (Key_hash_t tn1, Key_hash_t tn2) ->
          return (fun tname -> Key_hash_t tname) Eq tn1 tn2
      | (String_t tn1, String_t tn2) ->
          return (fun tname -> String_t tname) Eq tn1 tn2
      | (Bytes_t tn1, Bytes_t tn2) ->
          return (fun tname -> Bytes_t tname) Eq tn1 tn2
      | (Signature_t tn1, Signature_t tn2) ->
          return (fun tname -> Signature_t tname) Eq tn1 tn2
      | (Mutez_t tn1, Mutez_t tn2) ->
          return (fun tname -> Mutez_t tname) Eq tn1 tn2
      | (Timestamp_t tn1, Timestamp_t tn2) ->
          return (fun tname -> Timestamp_t tname) Eq tn1 tn2
      | (Address_t tn1, Address_t tn2) ->
          return (fun tname -> Address_t tname) Eq tn1 tn2
      | (Bool_t tn1, Bool_t tn2) ->
          return (fun tname -> Bool_t tname) Eq tn1 tn2
      | (Chain_id_t tn1, Chain_id_t tn2) ->
          return (fun tname -> Chain_id_t tname) Eq tn1 tn2
      | (Never_t tn1, Never_t tn2) ->
          return (fun tname -> Never_t tname) Eq tn1 tn2
      | (Operation_t tn1, Operation_t tn2) ->
          return (fun tname -> Operation_t tname) Eq tn1 tn2
      | (Bls12_381_g1_t tn1, Bls12_381_g1_t tn2) ->
          return (fun tname -> Bls12_381_g1_t tname) Eq tn1 tn2
      | (Bls12_381_g2_t tn1, Bls12_381_g2_t tn2) ->
          return (fun tname -> Bls12_381_g2_t tname) Eq tn1 tn2
      | (Bls12_381_fr_t tn1, Bls12_381_fr_t tn2) ->
          return (fun tname -> Bls12_381_fr_t tname) Eq tn1 tn2
      | (Map_t (tal, tar, tn1), Map_t (tbl, tbr, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tar tbr >>$ fun (Eq, value) ->
          merge_comparable_types ~legacy ~error_details tal tbl
          >|$ fun (Eq, tk) ->
          ((Eq : (ta ty, tb ty) eq), Map_t (tk, value, tname))
      | (Big_map_t (tal, tar, tn1), Big_map_t (tbl, tbr, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tar tbr >>$ fun (Eq, value) ->
          merge_comparable_types ~legacy ~error_details tal tbl
          >|$ fun (Eq, tk) ->
          ((Eq : (ta ty, tb ty) eq), Big_map_t (tk, value, tname))
      | (Set_t (ea, tn1), Set_t (eb, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          merge_comparable_types ~legacy ~error_details ea eb >|$ fun (Eq, e) ->
          ((Eq : (ta ty, tb ty) eq), Set_t (e, tname))
      | (Ticket_t (ea, tn1), Ticket_t (eb, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          merge_comparable_types ~legacy ~error_details ea eb >|$ fun (Eq, e) ->
          ((Eq : (ta ty, tb ty) eq), Ticket_t (e, tname))
      | (Pair_t (tal, tar, tn1), Pair_t (tbl, tbr, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tal tbl >>$ fun (Eq, left_ty) ->
          help tar tbr >|$ fun (Eq, right_ty) ->
          ((Eq : (ta ty, tb ty) eq), Pair_t (left_ty, right_ty, tname))
      | ( Union_t ((tal, tal_annot), (tar, tar_annot), tn1),
          Union_t ((tbl, tbl_annot), (tbr, tbr_annot), tn2) ) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          merge_field_annot ~legacy tal_annot tbl_annot >>$ fun left_annot ->
          merge_field_annot ~legacy tar_annot tbr_annot >>$ fun right_annot ->
          help tal tbl >>$ fun (Eq, left_ty) ->
          help tar tbr >|$ fun (Eq, right_ty) ->
          ( (Eq : (ta ty, tb ty) eq),
            Union_t ((left_ty, left_annot), (right_ty, right_annot), tname) )
      | (Lambda_t (tal, tar, tn1), Lambda_t (tbl, tbr, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tal tbl >>$ fun (Eq, left_ty) ->
          help tar tbr >|$ fun (Eq, right_ty) ->
          ((Eq : (ta ty, tb ty) eq), Lambda_t (left_ty, right_ty, tname))
      | (Contract_t (tal, tn1), Contract_t (tbl, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tal tbl >|$ fun (Eq, arg_ty) ->
          ((Eq : (ta ty, tb ty) eq), Contract_t (arg_ty, tname))
      | (Option_t (tva, tn1), Option_t (tvb, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tva tvb >|$ fun (Eq, ty) ->
          ((Eq : (ta ty, tb ty) eq), Option_t (ty, tname))
      | (List_t (tva, tn1), List_t (tvb, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          help tva tvb >|$ fun (Eq, ty) ->
          ((Eq : (ta ty, tb ty) eq), List_t (ty, tname))
      | (Sapling_state_t (ms1, tn1), Sapling_state_t (ms2, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          merge_memo_sizes ms1 ms2 >|$ fun ms ->
          (Eq, Sapling_state_t (ms, tname))
      | (Sapling_transaction_t (ms1, tn1), Sapling_transaction_t (ms2, tn2)) ->
          merge_type_metadata tn1 tn2 >>$ fun tname ->
          merge_memo_sizes ms1 ms2 >|$ fun ms ->
          (Eq, Sapling_transaction_t (ms, tname))
      | (Chest_t tn1, Chest_t tn2) ->
          return (fun tname -> Chest_t tname) Eq tn1 tn2
      | (Chest_key_t tn1, Chest_key_t tn2) ->
          return (fun tname -> Chest_key_t tname) Eq tn1 tn2
      | (_, _) ->
          of_result
          @@ Error
               (match error_details with
               | Fast -> (Inconsistent_types_fast : error_trace)
               | Informative ->
                   trace_of_error @@ default_merge_type_error ty1 ty2)
    in
    help ty1 ty2
  [@@coq_axiom_with_reason "non-top-level mutual recursion"]

(* This function does not distinguish gas errors from merge errors. If you need
   to recover from a type mismatch and consume the exact gas for the failed
   comparison, use [merge_types] instead.
*)
let ty_eq :
    type ta tb.
    legacy:bool ->
    context ->
    Script.location ->
    ta ty ->
    tb ty ->
    ((ta ty, tb ty) eq * context) tzresult =
 fun ~legacy ctxt loc ta tb ->
  Gas_monad.run ctxt @@ merge_types ~error_details:Informative ~legacy loc ta tb
  >>? fun (eq_ty, ctxt) ->
  eq_ty >|? fun (eq, _ty) -> (eq, ctxt)

(* Same as merge_comparable_types and merge_types but for stacks.
   A single error monad is used here because there is no need to
   recover from stack merging errors.  *)
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
    | (Bot_t, Bot_t) -> ok (Eq, Bot_t, ctxt)
    | (Item_t (ty1, rest1), Item_t (ty2, rest2)) ->
        Gas_monad.run ctxt
        @@ merge_types ~error_details:Informative ~legacy loc ty1 ty2
        |> record_trace (Bad_stack_item lvl)
        >>? fun (eq_ty, ctxt) ->
        eq_ty >>? fun (Eq, ty) ->
        help ctxt (lvl + 1) rest1 rest2 >|? fun (Eq, rest, ctxt) ->
        ( (Eq : ((ta, ts) stack_ty, (tb, tu) stack_ty) eq),
          Item_t (ty, rest),
          ctxt )
    | (_, _) -> error Bad_stack_length
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
    'r 'f.
    ('a, 's, 'r, 'f) descr -> ('b, 'u, 'r, 'f) descr -> ('c, 'v, 'r, 'f) descr;
}
[@@unboxed]

let merge_branches :
    type a s b u c v.
    legacy:bool ->
    context ->
    Script.location ->
    (a, s) judgement ->
    (b, u) judgement ->
    (a, s, b, u, c, v) branch ->
    ((c, v) judgement * context) tzresult =
 fun ~legacy ctxt loc btr bfr {branch} ->
  match (btr, bfr) with
  | (Typed ({aft = aftbt; _} as dbt), Typed ({aft = aftbf; _} as dbf)) ->
      let unmatched_branches () =
        let aftbt = serialize_stack_for_error ctxt aftbt in
        let aftbf = serialize_stack_for_error ctxt aftbf in
        Unmatched_branches (loc, aftbt, aftbf)
      in
      record_trace_eval
        unmatched_branches
        ( merge_stacks ~legacy loc ctxt 1 aftbt aftbf
        >|? fun (Eq, merged_stack, ctxt) ->
          ( Typed
              (branch
                 {dbt with aft = merged_stack}
                 {dbf with aft = merged_stack}),
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
      | Ok _ as ok_memo_size -> ok_memo_size [@coq_cast]
      | Error msg ->
          error
          @@ Invalid_syntactic_constant (location n, strip_locations n, msg))
  | _ -> error @@ Invalid_kind (location n, [Int_kind], kind n)

type ex_comparable_ty =
  | Ex_comparable_ty : 'a comparable_ty -> ex_comparable_ty

let[@coq_struct "ty"] rec parse_comparable_ty :
    stack_depth:int ->
    context ->
    Script.node ->
    (ex_comparable_ty * context) tzresult =
 fun ~stack_depth ctxt ty ->
  Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
  if Compare.Int.(stack_depth > 10000) then
    error Typechecking_too_many_recursive_calls
  else
    match ty with
    | Prim (loc, T_unit, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty unit_key, ctxt)
    | Prim (loc, T_never, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty never_key, ctxt)
    | Prim (loc, T_int, [], annot) ->
        check_type_annot loc annot >|? fun () -> (Ex_comparable_ty int_key, ctxt)
    | Prim (loc, T_nat, [], annot) ->
        check_type_annot loc annot >|? fun () -> (Ex_comparable_ty nat_key, ctxt)
    | Prim (loc, T_signature, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty signature_key, ctxt)
    | Prim (loc, T_string, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty string_key, ctxt)
    | Prim (loc, T_bytes, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty bytes_key, ctxt)
    | Prim (loc, T_mutez, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty mutez_key, ctxt)
    | Prim (loc, T_bool, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty bool_key, ctxt)
    | Prim (loc, T_key_hash, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty key_hash_key, ctxt)
    | Prim (loc, T_key, [], annot) ->
        check_type_annot loc annot >|? fun () -> (Ex_comparable_ty key_key, ctxt)
    | Prim (loc, T_timestamp, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty timestamp_key, ctxt)
    | Prim (loc, T_chain_id, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty chain_id_key, ctxt)
    | Prim (loc, T_address, [], annot) ->
        check_type_annot loc annot >|? fun () ->
        (Ex_comparable_ty address_key, ctxt)
    | Prim
        ( loc,
          (( T_unit | T_never | T_int | T_nat | T_string | T_bytes | T_mutez
           | T_bool | T_key_hash | T_timestamp | T_address | T_chain_id
           | T_signature | T_key ) as prim),
          l,
          _ ) ->
        error (Invalid_arity (loc, prim, 0, List.length l))
    | Prim (loc, T_pair, left :: right, annot) ->
        check_type_annot loc annot >>? fun () ->
        extract_field_annot left >>? fun (left, _left_annot) ->
        (match right with
        | [right] -> extract_field_annot right
        | right ->
            (* Unfold [pair t1 ... tn] as [pair t1 (... (pair tn-1 tn))] *)
            ok (Prim (loc, T_pair, right, []), None))
        >>? fun (right, _right_annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt right
        >>? fun (Ex_comparable_ty right, ctxt) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt left
        >>? fun (Ex_comparable_ty left, ctxt) ->
        pair_key loc left right >|? fun ty -> (Ex_comparable_ty ty, ctxt)
    | Prim (loc, T_or, [left; right], annot) ->
        check_type_annot loc annot >>? fun () ->
        extract_field_annot left >>? fun (left, _left_annot) ->
        extract_field_annot right >>? fun (right, _right_annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt right
        >>? fun (Ex_comparable_ty right, ctxt) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt left
        >>? fun (Ex_comparable_ty left, ctxt) ->
        union_key loc left right >|? fun ty -> (Ex_comparable_ty ty, ctxt)
    | Prim (loc, ((T_pair | T_or) as prim), l, _) ->
        error (Invalid_arity (loc, prim, 2, List.length l))
    | Prim (loc, T_option, [t], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt t
        >>? fun (Ex_comparable_ty t, ctxt) ->
        option_key loc t >|? fun ty -> (Ex_comparable_ty ty, ctxt)
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
             [
               T_unit;
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
               T_key;
             ]

type ex_ty = Ex_ty : 'a ty -> ex_ty

type ex_parameter_ty_and_entrypoints =
  | Ex_parameter_ty_and_entrypoints : {
      arg_type : 'a ty;
      root_name : Entrypoint.t option;
    }
      -> ex_parameter_ty_and_entrypoints

let[@coq_axiom_with_reason "complex mutually recursive definition"] rec parse_passable_ty
    :
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

and parse_view_input_ty :
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
    ~allow_contract:true
    ~allow_ticket:false

and parse_view_output_ty :
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
    ~allow_contract:true
    ~allow_ticket:false

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_normal_storage_ty
    :
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

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_any_ty
    :
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

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_ty :
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
  Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
  if Compare.Int.(stack_depth > 10000) then
    error Typechecking_too_many_recursive_calls
  else
    match node with
    | Prim (loc, T_unit, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty unit_t, ctxt)
    | Prim (loc, T_int, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty int_t, ctxt)
    | Prim (loc, T_nat, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty nat_t, ctxt)
    | Prim (loc, T_string, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty string_t, ctxt)
    | Prim (loc, T_bytes, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty bytes_t, ctxt)
    | Prim (loc, T_mutez, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty mutez_t, ctxt)
    | Prim (loc, T_bool, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty bool_t, ctxt)
    | Prim (loc, T_key, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty key_t, ctxt)
    | Prim (loc, T_key_hash, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty key_hash_t, ctxt)
    | Prim (loc, T_chest_key, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty chest_key_t, ctxt)
    | Prim (loc, T_chest, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty chest_t, ctxt)
    | Prim (loc, T_timestamp, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty timestamp_t, ctxt)
    | Prim (loc, T_address, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty address_t, ctxt)
    | Prim (loc, T_signature, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty signature_t, ctxt)
    | Prim (loc, T_operation, [], annot) ->
        if allow_operation then
          check_type_annot loc annot >>? fun () -> ok (Ex_ty operation_t, ctxt)
        else error (Unexpected_operation loc)
    | Prim (loc, T_chain_id, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty chain_id_t, ctxt)
    | Prim (loc, T_never, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty never_t, ctxt)
    | Prim (loc, T_bls12_381_g1, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty bls12_381_g1_t, ctxt)
    | Prim (loc, T_bls12_381_g2, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty bls12_381_g2_t, ctxt)
    | Prim (loc, T_bls12_381_fr, [], annot) ->
        check_type_annot loc annot >>? fun () -> ok (Ex_ty bls12_381_fr_t, ctxt)
    | Prim (loc, T_contract, [utl], annot) ->
        if allow_contract then
          parse_passable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy utl
          >>? fun (Ex_ty tl, ctxt) ->
          check_type_annot loc annot >>? fun () ->
          contract_t loc tl >|? fun ty -> (Ex_ty ty, ctxt)
        else error (Unexpected_contract loc)
    | Prim (loc, T_pair, utl :: utr, annot) ->
        extract_field_annot utl >>? fun (utl, _left_field) ->
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
        (match utr with
        | [utr] -> extract_field_annot utr
        | utr ->
            (* Unfold [pair t1 ... tn] as [pair t1 (... (pair tn-1 tn))] *)
            ok (Prim (loc, T_pair, utr, []), None))
        >>? fun (utr, _right_field) ->
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
        check_type_annot loc annot >>? fun () ->
        pair_t loc tl tr >|? fun ty -> (Ex_ty ty, ctxt)
    | Prim (loc, T_or, [utl; utr], annot) ->
        extract_field_annot utl >>? fun (utl, left_constr) ->
        extract_field_annot utr >>? fun (utr, right_constr) ->
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
        check_type_annot loc annot >>? fun () ->
        union_t loc (tl, left_constr) (tr, right_constr) >|? fun ty ->
        (Ex_ty ty, ctxt)
    | Prim (loc, T_lambda, [uta; utr], annot) ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy uta
        >>? fun (Ex_ty ta, ctxt) ->
        parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy utr
        >>? fun (Ex_ty tr, ctxt) ->
        check_type_annot loc annot >>? fun () ->
        lambda_t loc ta tr >|? fun ty -> (Ex_ty ty, ctxt)
    | Prim (loc, T_option, [ut], annot) ->
        (if legacy then
         (* legacy semantics with (broken) field annotations *)
         extract_field_annot ut >>? fun (ut, _some_constr) ->
         check_composed_type_annot loc annot >>? fun () -> ok ut
        else check_type_annot loc annot >>? fun () -> ok ut)
        >>? fun ut ->
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
        option_t loc t >|? fun ty -> (Ex_ty ty, ctxt)
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
        check_type_annot loc annot >>? fun () ->
        list_t loc t >|? fun ty -> (Ex_ty ty, ctxt)
    | Prim (loc, T_ticket, [ut], annot) ->
        if allow_ticket then
          parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
          >>? fun (Ex_comparable_ty t, ctxt) ->
          check_type_annot loc annot >>? fun () ->
          ticket_t loc t >|? fun ty -> (Ex_ty ty, ctxt)
        else error (Unexpected_ticket loc)
    | Prim (loc, T_set, [ut], annot) ->
        parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt ut
        >>? fun (Ex_comparable_ty t, ctxt) ->
        check_type_annot loc annot >>? fun () ->
        set_t loc t >|? fun ty -> (Ex_ty ty, ctxt)
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
        check_type_annot loc annot >>? fun () ->
        map_t loc ta tr >|? fun ty -> (Ex_ty ty, ctxt)
    | Prim (loc, T_sapling_transaction, [memo_size], annot) ->
        check_type_annot loc annot >>? fun () ->
        parse_memo_size memo_size >|? fun memo_size ->
        (Ex_ty (sapling_transaction_t ~memo_size), ctxt)
    (*
    /!\ When adding new lazy storage kinds, be careful to use
    [when allow_lazy_storage] /!\
    Lazy storage should not be packable to avoid stealing a lazy storage
    from another contract with `PUSH t id` or `UNPACK`.
  *)
    | Prim (loc, T_big_map, args, annot) when allow_lazy_storage ->
        (parse_big_map_ty [@tailcall]) ctxt ~stack_depth ~legacy loc args annot
    | Prim (loc, T_sapling_state, [memo_size], annot) when allow_lazy_storage ->
        check_type_annot loc annot >>? fun () ->
        parse_memo_size memo_size >|? fun memo_size ->
        (Ex_ty (sapling_state_t ~memo_size), ctxt)
    | Prim (loc, (T_big_map | T_sapling_state), _, _) ->
        error (Unexpected_lazy_storage loc)
    | Prim
        ( loc,
          (( T_unit | T_signature | T_int | T_nat | T_string | T_bytes | T_mutez
           | T_bool | T_key | T_key_hash | T_timestamp | T_address | T_chain_id
           | T_operation | T_never ) as prim),
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
               T_pair;
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
               T_ticket;
             ]

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_big_map_ty
    ctxt ~stack_depth ~legacy big_map_loc args map_annot =
  Gas.consume ctxt Typecheck_costs.parse_type_cycle >>? fun ctxt ->
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
      check_type_annot big_map_loc map_annot >>? fun () ->
      big_map_t big_map_loc key_ty value_ty >|? fun big_map_ty ->
      (Ex_ty big_map_ty, ctxt)
  | args -> error @@ Invalid_arity (big_map_loc, T_big_map, 2, List.length args)

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_big_map_value_ty
    ctxt ~stack_depth ~legacy value_ty =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:legacy
    ~allow_ticket:true
    value_ty

let parse_packable_ty ctxt ~stack_depth ~legacy node =
  (parse_ty [@tailcall])
    ctxt
    ~stack_depth
    ~legacy
    ~allow_lazy_storage:false
    ~allow_operation:false
    ~allow_contract:
      legacy
      (* type contract is forbidden in UNPACK because of
         https://gitlab.com/tezos/tezos/-/issues/301 *)
    ~allow_ticket:false
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
          pair_t loc big_map_ty remaining_storage >|? fun ty -> (Ex_ty ty, ctxt)
      )
  | _ -> (parse_normal_storage_ty [@tailcall]) ctxt ~stack_depth ~legacy node

let check_packable ~legacy loc root =
  let rec check : type t. t ty -> unit tzresult = function
    (* /!\ When adding new lazy storage kinds, be sure to return an error. /!\
       Lazy storage should not be packable. *)
    | Big_map_t _ -> error (Unexpected_lazy_storage loc)
    | Sapling_state_t _ -> error (Unexpected_lazy_storage loc)
    | Operation_t _ -> error (Unexpected_operation loc)
    | Unit_t _ -> Result.return_unit
    | Int_t _ -> Result.return_unit
    | Nat_t _ -> Result.return_unit
    | Signature_t _ -> Result.return_unit
    | String_t _ -> Result.return_unit
    | Bytes_t _ -> Result.return_unit
    | Mutez_t _ -> Result.return_unit
    | Key_hash_t _ -> Result.return_unit
    | Key_t _ -> Result.return_unit
    | Timestamp_t _ -> Result.return_unit
    | Address_t _ -> Result.return_unit
    | Bool_t _ -> Result.return_unit
    | Chain_id_t _ -> Result.return_unit
    | Never_t _ -> Result.return_unit
    | Set_t (_, _) -> Result.return_unit
    | Ticket_t _ -> error (Unexpected_ticket loc)
    | Lambda_t (_, _, _) -> Result.return_unit
    | Bls12_381_g1_t _ -> Result.return_unit
    | Bls12_381_g2_t _ -> Result.return_unit
    | Bls12_381_fr_t _ -> Result.return_unit
    | Pair_t (l_ty, r_ty, _) -> check l_ty >>? fun () -> check r_ty
    | Union_t ((l_ty, _), (r_ty, _), _) -> check l_ty >>? fun () -> check r_ty
    | Option_t (v_ty, _) -> check v_ty
    | List_t (elt_ty, _) -> check elt_ty
    | Map_t (_, elt_ty, _) -> check elt_ty
    | Contract_t (_, _) when legacy -> Result.return_unit
    | Contract_t (_, _) -> error (Unexpected_contract loc)
    | Sapling_transaction_t _ -> ok ()
    | Chest_key_t _ -> Result.return_unit
    | Chest_t _ -> Result.return_unit
  in
  check root

type toplevel = {
  code_field : Script.node;
  arg_type : Script.node;
  storage_type : Script.node;
  views : view SMap.t;
  root_name : Entrypoint.t option;
}

type ('arg, 'storage) code = {
  code : (('arg, 'storage) pair, (operation boxed_list, 'storage) pair) lambda;
  arg_type : 'arg ty;
  storage_type : 'storage ty;
  views : view SMap.t;
  root_name : Entrypoint.t option;
  code_size : Cache_memory_helpers.sint;
}

type ex_script = Ex_script : ('a, 'c) script -> ex_script

type ex_code = Ex_code : ('a, 'c) code -> ex_code

type 'storage ex_view =
  | Ex_view :
      ('input * 'storage, 'output) Script_typed_ir.lambda
      -> 'storage ex_view

type (_, _) dig_proof_argument =
  | Dig_proof_argument :
      ('x, 'a * 's, 'a, 's, 'b, 't, 'c, 'u) stack_prefix_preservation_witness
      * 'x ty
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

let find_entrypoint (type full error_trace)
    ~(error_details : error_trace error_details) (full : full ty) ~root_name
    entrypoint : ((Script.node -> Script.node) * ex_ty, error_trace) Gas_monad.t
    =
  let open Gas_monad in
  let loc = Micheline.dummy_location in
  let rec find_entrypoint :
      type t.
      t ty ->
      Entrypoint.t ->
      ((Script.node -> Script.node) * ex_ty, unit) Gas_monad.t =
   fun t entrypoint ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) -> (
        consume_gas Typecheck_costs.find_entrypoint_cycle >>$ fun () ->
        if field_annot_opt_eq_entrypoint_lax al entrypoint then
          return ((fun e -> Prim (loc, D_Left, [e], [])), Ex_ty tl)
        else if field_annot_opt_eq_entrypoint_lax ar entrypoint then
          return ((fun e -> Prim (loc, D_Right, [e], [])), Ex_ty tr)
        else
          find_entrypoint tl entrypoint >??$ function
          | Ok (f, t) -> return ((fun e -> Prim (loc, D_Left, [f e], [])), t)
          | Error () ->
              find_entrypoint tr entrypoint >|$ fun (f, t) ->
              ((fun e -> Prim (loc, D_Right, [f e], [])), t))
    | _ -> of_result (Error ())
  in
  match root_name with
  | Some root_name
    when (* This comparison should be taken into account by the caller. *)
         Entrypoint.(root_name = entrypoint) ->
      return ((fun e -> e), Ex_ty full)
  | _ -> (
      find_entrypoint full entrypoint >??$ function
      | Ok result -> return result
      | Error () ->
          if Entrypoint.is_default entrypoint then
            return ((fun e -> e), Ex_ty full)
          else
            of_result
            @@ Error
                 (match error_details with
                 | Fast -> (Inconsistent_types_fast : error_trace)
                 | Informative ->
                     trace_of_error @@ No_such_entrypoint entrypoint))

let find_entrypoint_for_type (type full exp error_trace) ~legacy ~error_details
    ~(full : full ty) ~(expected : exp ty) ~root_name entrypoint loc :
    (Entrypoint.t * exp ty, error_trace) Gas_monad.t =
  let open Gas_monad in
  find_entrypoint ~error_details full ~root_name entrypoint >>$ function
  | (_, Ex_ty ty) -> (
      match root_name with
      | Some root_name
        when Entrypoint.is_root root_name && Entrypoint.is_default entrypoint
        -> (
          merge_types ~legacy ~error_details:Fast loc ty expected >??$ function
          | Ok (Eq, ty) -> return (Entrypoint.default, (ty : exp ty))
          | Error Inconsistent_types_fast ->
              merge_types ~legacy ~error_details loc full expected
              >?$ fun (Eq, full) -> ok (Entrypoint.root, (full : exp ty)))
      | _ ->
          merge_types ~legacy ~error_details loc ty expected >|$ fun (Eq, ty) ->
          (entrypoint, (ty : exp ty)))

let well_formed_entrypoints (type full) (full : full ty) ~root_name =
  let merge path annot (type t) (ty : t ty) reachable
      ((first_unreachable, all) as acc) =
    match annot with
    | None ->
        ok
          ( (if reachable then acc
            else
              match ty with
              | Union_t _ -> acc
              | _ -> (
                  match first_unreachable with
                  | None -> (Some (List.rev path), all)
                  | Some _ -> acc)),
            reachable )
    | Some (Field_annot name) ->
        Entrypoint.of_annot_lax name >>? fun name ->
        if Entrypoint.Set.mem name all then error (Duplicate_entrypoint name)
        else ok ((first_unreachable, Entrypoint.Set.add name all), true)
  in
  let rec check :
      type t.
      t ty ->
      prim list ->
      bool ->
      prim list option * Entrypoint.Set.t ->
      (prim list option * Entrypoint.Set.t) tzresult =
   fun t path reachable acc ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) ->
        merge (D_Left :: path) al tl reachable acc >>? fun (acc, l_reachable) ->
        merge (D_Right :: path) ar tr reachable acc
        >>? fun (acc, r_reachable) ->
        check tl (D_Left :: path) l_reachable acc >>? fun acc ->
        check tr (D_Right :: path) r_reachable acc
    | _ -> ok acc
  in
  let (init, reachable) =
    match root_name with
    | None -> (Entrypoint.Set.empty, false)
    | Some name -> (Entrypoint.Set.singleton name, true)
  in
  check full [] reachable (None, init) >>? fun (first_unreachable, all) ->
  if not (Entrypoint.Set.mem Entrypoint.default all) then Result.return_unit
  else
    match first_unreachable with
    | None -> Result.return_unit
    | Some path -> error (Unreachable_entrypoint path)

let parse_parameter_ty_and_entrypoints :
    context ->
    stack_depth:int ->
    legacy:bool ->
    root_name:Entrypoint.t option ->
    Script.node ->
    (ex_parameter_ty_and_entrypoints * context) tzresult =
 fun ctxt ~stack_depth ~legacy ~root_name node ->
  parse_passable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy node
  >>? fun (Ex_ty arg_type, ctxt) ->
  (if legacy then Result.return_unit
  else well_formed_entrypoints ~root_name arg_type)
  >|? fun () -> (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, ctxt)

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
let opened_ticket_type loc ty = pair_3_key loc address_key ty nat_key

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
      Gas.consume ctxt Typecheck_costs.timestamp_readable >>? fun ctxt ->
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

let parse_address ctxt : Script.node -> (address * context) tzresult = function
  | Bytes (loc, bytes) as expr (* As unparsed with [Optimized]. *) -> (
      Gas.consume ctxt Typecheck_costs.contract >>? fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt
          Data_encoding.(tup2 Destination.encoding Entrypoint.value_encoding)
          bytes
      with
      | Some (destination, entrypoint) -> Ok ({destination; entrypoint}, ctxt)
      | None ->
          error
          @@ Invalid_syntactic_constant
               (loc, strip_locations expr, "a valid address"))
  | String (loc, s) (* As unparsed with [Readable]. *) ->
      Gas.consume ctxt Typecheck_costs.contract >>? fun ctxt ->
      (match String.index_opt s '%' with
      | None -> ok (s, Entrypoint.default)
      | Some pos ->
          let len = String.length s - pos - 1 in
          let name = String.sub s (pos + 1) len in
          Entrypoint.of_string_strict ~loc name >|? fun entrypoint ->
          (String.sub s 0 pos, entrypoint))
      >>? fun (addr, entrypoint) ->
      Destination.of_b58check addr >|? fun destination ->
      ({destination; entrypoint}, ctxt)
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
    | ([r], _) -> ok r
    | ([], _) -> error @@ Invalid_arity (loc, D_Pair, 2, 1)
    | (_ :: _, Comb_Pair _) ->
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
      fail @@ Invalid_arity (loc, D_Pair, 2, List.length l)
  (* Unfold [{x1; ...; xn}] as [Pair x1 x2 ... xn-1 xn] for n >= 2 *)
  | Seq (loc, l :: (_ :: _ as rs)) -> parse_comb loc l rs
  | Seq (loc, l) -> fail @@ Invalid_seq_arity (loc, 2, List.length l)
  | expr -> fail @@ unexpected expr [] Constant_namespace [D_Pair]

let parse_union parse_l parse_r ctxt ~legacy = function
  | Prim (loc, D_Left, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_l ctxt v >|=? fun (v, ctxt) -> (L v, ctxt)
  | Prim (loc, D_Left, l, _) ->
      fail @@ Invalid_arity (loc, D_Left, 1, List.length l)
  | Prim (loc, D_Right, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_r ctxt v >|=? fun (v, ctxt) -> (R v, ctxt)
  | Prim (loc, D_Right, l, _) ->
      fail @@ Invalid_arity (loc, D_Right, 1, List.length l)
  | expr -> fail @@ unexpected expr [] Constant_namespace [D_Left; D_Right]

let parse_option parse_v ctxt ~legacy = function
  | Prim (loc, D_Some, [v], annot) ->
      (if legacy then Result.return_unit else error_unexpected_annot loc annot)
      >>?= fun () ->
      parse_v ctxt v >|=? fun (v, ctxt) -> (Some v, ctxt)
  | Prim (loc, D_Some, l, _) ->
      fail @@ Invalid_arity (loc, D_Some, 1, List.length l)
  | Prim (loc, D_None, [], annot) ->
      Lwt.return
        ( (if legacy then Result.return_unit
          else error_unexpected_annot loc annot)
        >|? fun () -> (None, ctxt) )
  | Prim (loc, D_None, l, _) ->
      fail @@ Invalid_arity (loc, D_None, 0, List.length l)
  | expr -> fail @@ unexpected expr [] Constant_namespace [D_Some; D_None]

(* -- parse data of comparable types -- *)

let comparable_comb_witness1 :
    type t. t comparable_ty -> (t, unit -> unit) comb_witness = function
  | Pair_key _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

let[@coq_axiom_with_reason "gadt"] rec parse_comparable_data :
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
    let ty = serialize_ty_for_error (ty_of_comparable_ty ty) in
    Invalid_constant (location script_data, strip_locations script_data, ty)
  in
  let traced_no_lwt body = record_trace_eval parse_data_error body in
  let traced body = trace_eval parse_data_error body in
  Gas.consume ctxt Typecheck_costs.parse_data_cycle
  (* We could have a smaller cost but let's keep it consistent with
     [parse_data] for now. *)
  >>?=
  fun ctxt ->
  let legacy = false in
  match (ty, script_data) with
  | (Unit_key _, expr) ->
      Lwt.return @@ traced_no_lwt
      @@ (parse_unit ctxt ~legacy expr : (a * context) tzresult)
  | (Bool_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_bool ctxt ~legacy expr
  | (String_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_string ctxt expr
  | (Bytes_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_bytes ctxt expr
  | (Int_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_int ctxt expr
  | (Nat_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_nat ctxt expr
  | (Mutez_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_mutez ctxt expr
  | (Timestamp_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_timestamp ctxt expr
  | (Key_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_key ctxt expr
  | (Key_hash_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_key_hash ctxt expr
  | (Signature_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_signature ctxt expr
  | (Chain_id_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_chain_id ctxt expr
  | (Address_key _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_address ctxt expr
  | (Pair_key (tl, tr, _), expr) ->
      let r_witness = comparable_comb_witness1 tr in
      let parse_l ctxt v = parse_comparable_data ?type_logger ctxt tl v in
      let parse_r ctxt v = parse_comparable_data ?type_logger ctxt tr v in
      traced @@ parse_pair parse_l parse_r ctxt ~legacy r_witness expr
  | (Union_key (tl, tr, _), expr) ->
      let parse_l ctxt v = parse_comparable_data ?type_logger ctxt tl v in
      let parse_r ctxt v = parse_comparable_data ?type_logger ctxt tr v in
      traced @@ parse_union parse_l parse_r ctxt ~legacy expr
  | (Option_key (t, _), expr) ->
      let parse_v ctxt v = parse_comparable_data ?type_logger ctxt t v in
      traced @@ parse_option parse_v ctxt ~legacy expr
  | (Never_key _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_never expr

(* -- parse data of any type -- *)

let comb_witness1 : type t. t ty -> (t, unit -> unit) comb_witness = function
  | Pair_t _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

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

let[@coq_axiom_with_reason "gadt"] rec parse_data :
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
  Gas.consume ctxt Typecheck_costs.parse_data_cycle >>?= fun ctxt ->
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
    let ty = serialize_ty_for_error ty in
    Invalid_constant (location script_data, strip_locations script_data, ty)
  in
  let fail_parse_data () = fail (parse_data_error ()) in
  let traced_no_lwt body = record_trace_eval parse_data_error body in
  let traced body = trace_eval parse_data_error body in
  let traced_fail err = Lwt.return @@ traced_no_lwt (error err) in
  let parse_items ?type_logger ctxt expr key_type value_type items item_wrapper
      =
    List.fold_left_es
      (fun (last_value, map, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if legacy then Result.return_unit
            else error_unexpected_annot loc annot)
            >>?= fun () ->
            parse_comparable_data ?type_logger ctxt key_type k
            >>=? fun (k, ctxt) ->
            non_terminal_recursion ?type_logger ctxt ~legacy value_type v
            >>=? fun (v, ctxt) ->
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
            fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            fail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ -> fail_parse_data ())
      (None, Script_map.empty key_type, ctxt)
      items
    |> traced
    >|=? fun (_, items, ctxt) -> (items, ctxt)
  in
  let parse_big_map_items (type t) ?type_logger ctxt expr
      (key_type : t comparable_ty) value_type items item_wrapper =
    List.fold_left_es
      (fun (last_key, {map; size}, ctxt) item ->
        match item with
        | Prim (loc, D_Elt, [k; v], annot) ->
            (if legacy then Result.return_unit
            else error_unexpected_annot loc annot)
            >>?= fun () ->
            parse_comparable_data ?type_logger ctxt key_type k
            >>=? fun (k, ctxt) ->
            hash_comparable_data ctxt key_type k >>=? fun (key_hash, ctxt) ->
            non_terminal_recursion ?type_logger ctxt ~legacy value_type v
            >>=? fun (v, ctxt) ->
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
            fail @@ Invalid_arity (loc, D_Elt, 2, List.length l)
        | Prim (loc, name, _, _) ->
            fail @@ Invalid_primitive (loc, [D_Elt], name)
        | Int _ | String _ | Bytes _ | Seq _ -> fail_parse_data ())
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
  | (String_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_string ctxt expr
  | (Bytes_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_bytes ctxt expr
  | (Int_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_int ctxt expr
  | (Nat_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_nat ctxt expr
  | (Mutez_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_mutez ctxt expr
  | (Timestamp_t _, expr) ->
      Lwt.return @@ traced_no_lwt @@ parse_timestamp ctxt expr
  | (Key_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_key ctxt expr
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
  | (Contract_t (arg_ty, _), expr) ->
      traced
        ( parse_address ctxt expr >>?= fun (address, ctxt) ->
          let loc = location expr in
          parse_contract
            ~stack_depth:(stack_depth + 1)
            ~legacy
            ctxt
            loc
            arg_ty
            address.destination
            ~entrypoint:address.entrypoint
          >|=? fun (ctxt, _) -> ({arg_ty; address}, ctxt) )
  (* Pairs *)
  | (Pair_t (tl, tr, _), expr) ->
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
           Tc_context.data
           ?type_logger
           ~stack_depth:(stack_depth + 1)
           ctxt
           ~legacy
           ta
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
      @@ List.fold_right_es
           (fun v (rest, ctxt) ->
             non_terminal_recursion ?type_logger ctxt ~legacy t v
             >|=? fun (v, ctxt) -> (Script_list.cons v rest, ctxt))
           items
           (Script_list.empty, ctxt)
  | (List_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Tickets *)
  | (Ticket_t (t, _ty_name), expr) ->
      if allow_forged then
        opened_ticket_type (location expr) t >>?= fun ty ->
        parse_comparable_data ?type_logger ctxt ty expr
        >|=? fun (({destination; entrypoint = _}, (contents, amount)), ctxt) ->
        match destination with
        | Contract ticketer -> ({ticketer; contents; amount}, ctxt)
      else traced_fail (Unexpected_forged_value (location expr))
  (* Sets *)
  | (Set_t (t, _ty_name), (Seq (loc, vs) as expr)) ->
      traced
      @@ List.fold_left_es
           (fun (last_value, set, ctxt) v ->
             parse_comparable_data ?type_logger ctxt t v >>=? fun (v, ctxt) ->
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
  | (Set_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  (* Maps *)
  | (Map_t (tk, tv, _ty_name), (Seq (_, vs) as expr)) ->
      parse_items ?type_logger ctxt expr tk tv vs (fun x -> x)
  | (Map_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Seq_kind], kind expr))
  | (Big_map_t (tk, tv, _ty_name), expr) ->
      (match expr with
      | Int (loc, id) ->
          return (Some (id, loc), {map = Big_map_overlay.empty; size = 0}, ctxt)
      | Seq (_, vs) ->
          parse_big_map_items ?type_logger ctxt expr tk tv vs (fun x -> Some x)
          >|=? fun (diff, ctxt) -> (None, diff, ctxt)
      | Prim (loc, D_Pair, [Int (loc_id, id); Seq (_, vs)], annot) ->
          error_unexpected_annot loc annot >>?= fun () ->
          option_t loc tv >>?= fun tv_opt ->
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
            (unexpected expr [Seq_kind; Int_kind] Constant_namespace [D_Pair]))
      >>=? fun (id_opt, diff, ctxt) ->
      (match id_opt with
      | None -> return @@ (None, ctxt)
      | Some (id, loc) ->
          if allow_forged then
            let id = Big_map.Id.parse_z id in
            Big_map.exists ctxt id >>=? function
            | (_, None) -> traced_fail (Invalid_big_map (loc, id))
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
                    comparable_ty_eq ctxt tk btk >>? fun (Eq, ctxt) ->
                    ty_eq ~legacy:true ctxt loc tv btv >>? fun (Eq, ctxt) ->
                    ok (Some id, ctxt) )
          else traced_fail (Unexpected_forged_value loc))
      >|=? fun (id, ctxt) -> ({id; diff; key_type = tk; value_type = tv}, ctxt)
  | (Never_t _, expr) -> Lwt.return @@ traced_no_lwt @@ parse_never expr
  (* Bls12_381 types *)
  | (Bls12_381_g1_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g1 >>?= fun ctxt ->
      match Script_bls.G1.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | (Bls12_381_g1_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Bls12_381_g2_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_g2 >>?= fun ctxt ->
      match Script_bls.G2.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | (Bls12_381_g2_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Bls12_381_fr_t _, Bytes (_, bs)) -> (
      Gas.consume ctxt Typecheck_costs.bls12_381_fr >>?= fun ctxt ->
      match Script_bls.Fr.of_bytes_opt bs with
      | Some pt -> return (pt, ctxt)
      | None -> fail_parse_data ())
  | (Bls12_381_fr_t _, Int (_, v)) ->
      Gas.consume ctxt Typecheck_costs.bls12_381_fr >>?= fun ctxt ->
      return (Script_bls.Fr.of_z v, ctxt)
  | (Bls12_381_fr_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  (*
    /!\ When adding new lazy storage kinds, you may want to guard the parsing
    of identifiers with [allow_forged].
  *)
  (* Sapling *)
  | (Sapling_transaction_t (memo_size, _), Bytes (_, bytes)) -> (
      match
        Data_encoding.Binary.of_bytes_opt Sapling.transaction_encoding bytes
      with
      | Some transaction -> (
          match Sapling.transaction_get_memo_size transaction with
          | None -> return (transaction, ctxt)
          | Some transac_memo_size ->
              Lwt.return
                ( merge_memo_sizes
                    ~error_details:Informative
                    memo_size
                    transac_memo_size
                >|? fun _ms -> (transaction, ctxt) ))
      | None -> fail_parse_data ())
  | (Sapling_transaction_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Sapling_state_t (memo_size, _), Int (loc, id)) ->
      if allow_forged then
        let id = Sapling.Id.parse_z id in
        Sapling.state_from_id ctxt id >>=? fun (state, ctxt) ->
        Lwt.return
          ( traced_no_lwt
          @@ merge_memo_sizes
               ~error_details:Informative
               memo_size
               state.Sapling.memo_size
          >|? fun _memo_size -> (state, ctxt) )
      else traced_fail (Unexpected_forged_value loc)
  | (Sapling_state_t (memo_size, _), Seq (_, [])) ->
      return (Sapling.empty_state ~memo_size (), ctxt)
  | (Sapling_state_t _, expr) ->
      (* Do not allow to input diffs as they are untrusted and may not be the
         result of a verify_update. *)
      traced_fail
        (Invalid_kind (location expr, [Int_kind; Seq_kind], kind expr))
  (* Time lock*)
  | (Chest_key_t _, Bytes (_, bytes)) -> (
      Gas.consume ctxt Typecheck_costs.chest_key >>?= fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt
          Script_timelock.chest_key_encoding
          bytes
      with
      | Some chest_key -> return (chest_key, ctxt)
      | None -> fail_parse_data ())
  | (Chest_key_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))
  | (Chest_t _, Bytes (_, bytes)) -> (
      Gas.consume ctxt (Typecheck_costs.chest ~bytes:(Bytes.length bytes))
      >>?= fun ctxt ->
      match
        Data_encoding.Binary.of_bytes_opt Script_timelock.chest_encoding bytes
      with
      | Some chest -> return (chest, ctxt)
      | None -> fail_parse_data ())
  | (Chest_t _, expr) ->
      traced_fail (Invalid_kind (location expr, [Bytes_kind], kind expr))

and parse_view_returning :
    type storage.
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    storage ty ->
    view ->
    (storage ex_view * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy storage_type {input_ty; output_ty; view_code} ->
  let input_ty_loc = location input_ty in
  record_trace_eval
    (fun () ->
      Ill_formed_type
        (Some "arg of view", strip_locations input_ty, input_ty_loc))
    (parse_view_input_ty ctxt ~stack_depth:0 ~legacy input_ty)
  >>?= fun (Ex_ty input_ty', ctxt) ->
  let output_ty_loc = location output_ty in
  record_trace_eval
    (fun () ->
      Ill_formed_type
        (Some "return of view", strip_locations output_ty, output_ty_loc))
    (parse_view_output_ty ctxt ~stack_depth:0 ~legacy output_ty)
  >>?= fun (Ex_ty output_ty', ctxt) ->
  pair_t input_ty_loc input_ty' storage_type >>?= fun pair_ty ->
  parse_instr
    ?type_logger
    ~stack_depth:0
    Tc_context.view
    ctxt
    ~legacy
    view_code
    (Item_t (pair_ty, Bot_t))
  >>=? fun (judgement, ctxt) ->
  Lwt.return
  @@
  match judgement with
  | Failed {descr} ->
      let cur_view' =
        Ex_view
          (Lam (close_descr (descr (Item_t (output_ty', Bot_t))), view_code))
      in
      ok (cur_view', ctxt)
  | Typed ({loc; aft; _} as descr) -> (
      let ill_type_view loc stack_ty () =
        let actual = serialize_stack_for_error ctxt stack_ty in
        let expected_stack = Item_t (output_ty', Bot_t) in
        let expected = serialize_stack_for_error ctxt expected_stack in
        Ill_typed_view {loc; actual; expected}
      in
      match aft with
      | Item_t (ty, Bot_t) ->
          record_trace_eval
            (ill_type_view loc aft : unit -> _)
            ( ty_eq ~legacy ctxt loc ty output_ty' >|? fun (Eq, ctxt) ->
              let view' = Ex_view (Lam (close_descr descr, view_code)) in
              (view', ctxt) )
      | _ -> error (ill_type_view loc aft ()))

and typecheck_views :
    type storage.
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    storage ty ->
    view SMap.t ->
    context tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy storage_type views ->
  let aux _name cur_view ctxt =
    parse_view_returning ?type_logger ctxt ~legacy storage_type cur_view
    >|=? fun (_parsed_view, ctxt) -> ctxt
  in
  SMap.fold_es aux views ctxt

and[@coq_axiom_with_reason "gadt"] parse_returning :
    type arg ret.
    ?type_logger:type_logger ->
    stack_depth:int ->
    tc_context ->
    context ->
    legacy:bool ->
    arg ty ->
    ret ty ->
    Script.node ->
    ((arg, ret) lambda * context) tzresult Lwt.t =
 fun ?type_logger ~stack_depth tc_context ctxt ~legacy arg ret script_instr ->
  parse_instr
    ?type_logger
    tc_context
    ctxt
    ~legacy
    ~stack_depth:(stack_depth + 1)
    script_instr
    (Item_t (arg, Bot_t))
  >>=? function
  | (Typed ({loc; aft = Item_t (ty, Bot_t) as stack_ty; _} as descr), ctxt) ->
      Lwt.return
      @@ record_trace_eval
           (fun () ->
             let ret = serialize_ty_for_error ret in
             let stack_ty = serialize_stack_for_error ctxt stack_ty in
             Bad_return (loc, stack_ty, ret))
           ( ty_eq ~legacy ctxt loc ty ret >|? fun (Eq, ctxt) ->
             ((Lam (close_descr descr, script_instr) : (arg, ret) lambda), ctxt)
           )
  | (Typed {loc; aft = stack_ty; _}, ctxt) ->
      let ret = serialize_ty_for_error ret in
      let stack_ty = serialize_stack_for_error ctxt stack_ty in
      fail @@ Bad_return (loc, stack_ty, ret)
  | (Failed {descr}, ctxt) ->
      return
        ( (Lam (close_descr (descr (Item_t (ret, Bot_t))), script_instr)
            : (arg, ret) lambda),
          ctxt )

and[@coq_axiom_with_reason "gadt"] parse_instr :
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
        let stack_ty = serialize_stack_for_error ctxt stack_ty in
        Bad_stack (loc, name, m, stack_ty))
    @@ record_trace
         (Bad_stack_item n)
         ( Gas_monad.run ctxt
         @@ merge_types ~legacy ~error_details:Informative loc exp got
         >>? fun (eq_ty, ctxt) ->
           eq_ty >|? fun (Eq, ty) -> ((Eq : (a, b) eq), (ty : a ty), ctxt) )
  in
  let log_stack loc stack_ty aft =
    match (type_logger, script_instr) with
    | (None, _) | (Some _, (Int _ | String _ | Bytes _)) -> ()
    | (Some log, (Prim _ | Seq _)) ->
        (* Unparsing for logging is not carbonated as this
              is used only by the client and not the protocol *)
        let stack_ty = unparse_stack_uncarbonated stack_ty in
        let aft = unparse_stack_uncarbonated aft in
        log loc stack_ty aft
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
  | (Prim (loc, I_DROP, [], annot), Item_t (_, rest)) ->
      (error_unexpected_annot loc annot >>?= fun () ->
       typed ctxt loc {apply = (fun kinfo k -> IDrop (kinfo, k))} rest
        : ((a, s) judgement * context) tzresult Lwt.t)
  | (Prim (loc, I_DROP, [n], result_annot), whole_stack) ->
      parse_uint10 n >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n) >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dropn_proof_argument tzresult =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) -> ok @@ Dropn_proof_argument (KRest, rest)
        | (false, Item_t (_, rest)) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dropn_proof_argument (n', stack_after_drops)) ->
            let kinfo = {iloc = loc; kstack_ty = rest} in
            Dropn_proof_argument (KPrefix (kinfo, n'), stack_after_drops)
        | (_, _) ->
            let whole_stack = serialize_stack_for_error ctxt whole_stack in
            error (Bad_stack (loc, I_DROP, whole_n, whole_stack))
      in
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument whole_n whole_stack
      >>?= fun (Dropn_proof_argument (n', stack_after_drops)) ->
      let kdropn kinfo k = IDropn (kinfo, whole_n, n', k) in
      typed ctxt loc {apply = kdropn} stack_after_drops
  | (Prim (loc, I_DROP, (_ :: _ :: _ as l), _), _) ->
      (* Technically, the arities 0 and 1 are allowed but the error only mentions 1.
            However, DROP is equivalent to DROP 1 so hinting at an arity of 1 makes sense. *)
      fail (Invalid_arity (loc, I_DROP, 1, List.length l))
  | (Prim (loc, I_DUP, [], annot), Item_t (v, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      record_trace_eval
        (fun () ->
          let t = serialize_ty_for_error v in
          Non_dupable_type (loc, t))
        (check_dupable_ty ctxt loc v)
      >>?= fun ctxt ->
      let dup = {apply = (fun kinfo k -> IDup (kinfo, k))} in
      typed ctxt loc dup (Item_t (v, Item_t (v, rest)))
  | (Prim (loc, I_DUP, [n], v_annot), stack_ty) ->
      check_var_annot loc v_annot >>?= fun () ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) dup_n_proof_argument tzresult =
       fun n (stack_ty : (a, s) stack_ty) ->
        match (n, stack_ty) with
        | (1, Item_t (hd_ty, _)) ->
            ok @@ Dup_n_proof_argument (Dup_n_zero, hd_ty)
        | (n, Item_t (_, tl_ty)) ->
            make_proof_argument (n - 1) tl_ty
            >|? fun (Dup_n_proof_argument (dup_n_witness, b_ty)) ->
            Dup_n_proof_argument (Dup_n_succ dup_n_witness, b_ty)
        | _ ->
            let whole_stack = serialize_stack_for_error ctxt stack_ty in
            error (Bad_stack (loc, I_DUP, 1, whole_stack))
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
      let dupn = {apply = (fun kinfo k -> IDup_n (kinfo, n, witness, k))} in
      typed ctxt loc dupn (Item_t (after_ty, stack_ty))
  | (Prim (loc, I_DIG, [n], result_annot), stack) ->
      let rec make_proof_argument :
          type a s. int -> (a, s) stack_ty -> (a, s) dig_proof_argument tzresult
          =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, Item_t (v, rest)) -> ok @@ Dig_proof_argument (KRest, v, rest)
        | (false, Item_t (v, rest)) ->
            make_proof_argument (n - 1) rest
            >|? fun (Dig_proof_argument (n', x, aft')) ->
            let kinfo = {iloc = loc; kstack_ty = aft'} in
            Dig_proof_argument (KPrefix (kinfo, n'), x, Item_t (v, aft'))
        | (_, _) ->
            let whole_stack = serialize_stack_for_error ctxt stack in
            error (Bad_stack (loc, I_DIG, 3, whole_stack))
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument n stack >>?= fun (Dig_proof_argument (n', x, aft)) ->
      let dig = {apply = (fun kinfo k -> IDig (kinfo, n, n', k))} in
      typed ctxt loc dig (Item_t (x, aft))
  | (Prim (loc, I_DIG, (([] | _ :: _ :: _) as l), _), _) ->
      fail (Invalid_arity (loc, I_DIG, 1, List.length l))
  | (Prim (loc, I_DUG, [n], result_annot), Item_t (x, whole_stack)) ->
      parse_uint10 n >>?= fun whole_n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument whole_n) >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s x.
          int ->
          x ty ->
          (a, s) stack_ty ->
          (a, s, x) dug_proof_argument tzresult =
       fun n x stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) -> ok @@ Dug_proof_argument (KRest, Item_t (x, rest))
        | (false, Item_t (v, rest)) ->
            make_proof_argument (n - 1) x rest
            >|? fun (Dug_proof_argument (n', aft')) ->
            let kinfo = {iloc = loc; kstack_ty = aft'} in
            Dug_proof_argument (KPrefix (kinfo, n'), Item_t (v, aft'))
        | (_, _) ->
            let whole_stack = serialize_stack_for_error ctxt whole_stack in
            error (Bad_stack (loc, I_DUG, whole_n, whole_stack))
      in
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument whole_n x whole_stack
      >>?= fun (Dug_proof_argument (n', aft)) ->
      let dug = {apply = (fun kinfo k -> IDug (kinfo, whole_n, n', k))} in
      typed ctxt loc dug aft
  | (Prim (loc, I_DUG, [_], result_annot), stack) ->
      Lwt.return
        ( error_unexpected_annot loc result_annot >>? fun () ->
          let stack = serialize_stack_for_error ctxt stack in
          error (Bad_stack (loc, I_DUG, 1, stack)) )
  | (Prim (loc, I_DUG, (([] | _ :: _ :: _) as l), _), _) ->
      fail (Invalid_arity (loc, I_DUG, 1, List.length l))
  | (Prim (loc, I_SWAP, [], annot), Item_t (v, Item_t (w, rest))) ->
      error_unexpected_annot loc annot >>?= fun () ->
      let swap = {apply = (fun kinfo k -> ISwap (kinfo, k))} in
      let stack_ty = Item_t (w, Item_t (v, rest)) in
      typed ctxt loc swap stack_ty
  | (Prim (loc, I_PUSH, [t; d], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
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
      typed ctxt loc const (Item_t (t, stack))
  | (Prim (loc, I_UNIT, [], annot), stack) ->
      check_var_type_annot loc annot >>?= fun () ->
      let const = {apply = (fun kinfo k -> IConst (kinfo, (), k))} in
      typed ctxt loc const (Item_t (unit_t, stack))
  (* options *)
  | (Prim (loc, I_SOME, [], annot), Item_t (t, rest)) ->
      check_var_type_annot loc annot >>?= fun () ->
      let cons_some = {apply = (fun kinfo k -> ICons_some (kinfo, k))} in
      option_t loc t >>?= fun ty -> typed ctxt loc cons_some (Item_t (ty, rest))
  | (Prim (loc, I_NONE, [t], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let cons_none = {apply = (fun kinfo k -> ICons_none (kinfo, k))} in
      option_t loc t >>?= fun ty ->
      let stack_ty = Item_t (ty, stack) in
      typed ctxt loc cons_none stack_ty
  | (Prim (loc, I_MAP, [body], annot), Item_t (Option_t (t, _), rest)) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        ~legacy
        tc_context
        ctxt
        body
        (Item_t (t, rest))
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
            ( merge_stacks ~legacy loc ctxt 1 aft_rest rest
            >>? fun (Eq, rest, ctxt) ->
              option_t loc ret >>? fun opt_ty ->
              let final_stack = Item_t (opt_ty, rest) in
              let hinfo = {iloc = loc; kstack_ty = Item_t (ret, aft_rest)} in
              let cinfo = kinfo_of_descr kibody in
              let body = kibody.instr.apply cinfo (IHalt hinfo) in
              let apply kinfo k = IOpt_map {kinfo; body; k} in
              typed_no_lwt ctxt loc {apply} final_stack )
      | Typed {aft = Bot_t; _} ->
          let aft = serialize_stack_for_error ctxt Bot_t in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | ( Prim (loc, I_IF_NONE, [bt; bf], annot),
      (Item_t (Option_t (t, _), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bt rest
      >>=? fun (btr, ctxt) ->
      let stack_ty = Item_t (t, rest) in
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bf stack_ty
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let ifnone =
          {
            apply =
              (fun kinfo k ->
                let hinfo = kinfo_of_kinstr k in
                let btinfo = kinfo_of_descr ibt
                and bfinfo = kinfo_of_descr ibf in
                let branch_if_none = ibt.instr.apply btinfo (IHalt hinfo)
                and branch_if_some = ibf.instr.apply bfinfo (IHalt hinfo) in
                IIf_none {kinfo; branch_if_none; branch_if_some; k});
          }
        in
        {loc; instr = ifnone; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ~legacy ctxt loc btr bfr {branch}
  (* pairs *)
  | (Prim (loc, I_PAIR, [], annot), Item_t (a, Item_t (b, rest))) ->
      parse_constr_annot loc annot >>?= fun (_l_field, _r_field) ->
      pair_t loc a b >>?= fun ty ->
      let stack_ty = Item_t (ty, rest) in
      let cons_pair = {apply = (fun kinfo k -> ICons_pair (kinfo, k))} in
      typed ctxt loc cons_pair stack_ty
  | (Prim (loc, I_PAIR, [n], annot), stack_ty) ->
      check_var_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) comb_proof_argument tzresult =
       fun n stack_ty ->
        match (n, stack_ty) with
        | (1, Item_t (a_ty, tl_ty)) ->
            ok (Comb_proof_argument (Comb_one, Item_t (a_ty, tl_ty)))
        | (n, Item_t (a_ty, tl_ty)) ->
            make_proof_argument (n - 1) tl_ty
            >>? fun (Comb_proof_argument (comb_witness, Item_t (b_ty, tl_ty')))
              ->
            pair_t loc a_ty b_ty >|? fun pair_t ->
            Comb_proof_argument (Comb_succ comb_witness, Item_t (pair_t, tl_ty'))
        | _ ->
            let whole_stack = serialize_stack_for_error ctxt stack_ty in
            error (Bad_stack (loc, I_PAIR, 1, whole_stack))
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Pair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Comb_proof_argument (witness, after_ty)) ->
      let comb = {apply = (fun kinfo k -> IComb (kinfo, n, witness, k))} in
      typed ctxt loc comb after_ty
  | (Prim (loc, I_UNPAIR, [n], annot), stack_ty) ->
      error_unexpected_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a * s) uncomb_proof_argument tzresult =
       fun n stack_ty ->
        match (n, stack_ty) with
        | (1, Item_t (a_ty, tl_ty)) ->
            ok @@ Uncomb_proof_argument (Uncomb_one, Item_t (a_ty, tl_ty))
        | (n, Item_t (Pair_t (a_ty, b_ty, _), tl_ty)) ->
            make_proof_argument (n - 1) (Item_t (b_ty, tl_ty))
            >|? fun (Uncomb_proof_argument (uncomb_witness, after_ty)) ->
            Uncomb_proof_argument
              (Uncomb_succ uncomb_witness, Item_t (a_ty, after_ty))
        | _ ->
            let whole_stack = serialize_stack_for_error ctxt stack_ty in
            error (Bad_stack (loc, I_UNPAIR, 1, whole_stack))
      in
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      error_unless (Compare.Int.( > ) n 1) (Unpair_bad_argument loc)
      >>?= fun () ->
      make_proof_argument n stack_ty
      >>?= fun (Uncomb_proof_argument (witness, after_ty)) ->
      let uncomb = {apply = (fun kinfo k -> IUncomb (kinfo, n, witness, k))} in
      typed ctxt loc uncomb after_ty
  | (Prim (loc, I_GET, [n], annot), Item_t (comb_ty, rest_ty)) ->
      check_var_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type b. int -> b ty -> b comb_get_proof_argument tzresult =
       fun n ty ->
        match (n, ty) with
        | (0, value_ty) ->
            ok @@ Comb_get_proof_argument (Comb_get_zero, value_ty)
        | (1, Pair_t (hd_ty, _, _annot)) ->
            ok @@ Comb_get_proof_argument (Comb_get_one, hd_ty)
        | (n, Pair_t (_, tl_ty, _annot)) ->
            make_proof_argument (n - 2) tl_ty
            >|? fun (Comb_get_proof_argument (comb_get_left_witness, ty')) ->
            Comb_get_proof_argument
              (Comb_get_plus_two comb_get_left_witness, ty')
        | _ ->
            let whole_stack = serialize_stack_for_error ctxt stack_ty in
            error (Bad_stack (loc, I_GET, 1, whole_stack))
      in
      parse_uint11 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      make_proof_argument n comb_ty
      >>?= fun (Comb_get_proof_argument (witness, ty')) ->
      let after_stack_ty = Item_t (ty', rest_ty) in
      let comb_get =
        {apply = (fun kinfo k -> IComb_get (kinfo, n, witness, k))}
      in
      typed ctxt loc comb_get after_stack_ty
  | ( Prim (loc, I_UPDATE, [n], annot),
      Item_t (value_ty, Item_t (comb_ty, rest_ty)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let rec make_proof_argument :
          type value before.
          int ->
          value ty ->
          before ty ->
          (value, before) comb_set_proof_argument tzresult =
       fun n value_ty ty ->
        match (n, ty) with
        | (0, _) -> ok @@ Comb_set_proof_argument (Comb_set_zero, value_ty)
        | (1, Pair_t (_hd_ty, tl_ty, _)) ->
            pair_t loc value_ty tl_ty >|? fun after_ty ->
            Comb_set_proof_argument (Comb_set_one, after_ty)
        | (n, Pair_t (hd_ty, tl_ty, _)) ->
            make_proof_argument (n - 2) value_ty tl_ty
            >>? fun (Comb_set_proof_argument (comb_set_left_witness, tl_ty')) ->
            pair_t loc hd_ty tl_ty' >|? fun after_ty ->
            Comb_set_proof_argument
              (Comb_set_plus_two comb_set_left_witness, after_ty)
        | _ ->
            let whole_stack = serialize_stack_for_error ctxt stack_ty in
            error (Bad_stack (loc, I_UPDATE, 2, whole_stack))
      in
      parse_uint11 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      make_proof_argument n value_ty comb_ty
      >>?= fun (Comb_set_proof_argument (witness, after_ty)) ->
      let after_stack_ty = Item_t (after_ty, rest_ty) in
      let comb_set =
        {apply = (fun kinfo k -> IComb_set (kinfo, n, witness, k))}
      in
      typed ctxt loc comb_set after_stack_ty
  | (Prim (loc, I_UNPAIR, [], annot), Item_t (Pair_t (a, b, _), rest)) ->
      check_unpair_annot loc annot >>?= fun () ->
      let unpair = {apply = (fun kinfo k -> IUnpair (kinfo, k))} in
      typed ctxt loc unpair (Item_t (a, Item_t (b, rest)))
  | (Prim (loc, I_CAR, [], annot), Item_t (Pair_t (a, _, _), rest)) ->
      check_destr_annot loc annot >>?= fun () ->
      let car = {apply = (fun kinfo k -> ICar (kinfo, k))} in
      typed ctxt loc car (Item_t (a, rest))
  | (Prim (loc, I_CDR, [], annot), Item_t (Pair_t (_, b, _), rest)) ->
      check_destr_annot loc annot >>?= fun () ->
      let cdr = {apply = (fun kinfo k -> ICdr (kinfo, k))} in
      typed ctxt loc cdr (Item_t (b, rest))
  (* unions *)
  | (Prim (loc, I_LEFT, [tr], annot), Item_t (tl, rest)) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tr
      >>?= fun (Ex_ty tr, ctxt) ->
      parse_constr_annot loc annot >>?= fun (l_field, r_field) ->
      let cons_left = {apply = (fun kinfo k -> ICons_left (kinfo, k))} in
      union_t loc (tl, l_field) (tr, r_field) >>?= fun ty ->
      let stack_ty = Item_t (ty, rest) in
      typed ctxt loc cons_left stack_ty
  | (Prim (loc, I_RIGHT, [tl], annot), Item_t (tr, rest)) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tl
      >>?= fun (Ex_ty tl, ctxt) ->
      parse_constr_annot loc annot >>?= fun (l_field, r_field) ->
      let cons_right = {apply = (fun kinfo k -> ICons_right (kinfo, k))} in
      union_t loc (tl, l_field) (tr, r_field) >>?= fun ty ->
      let stack_ty = Item_t (ty, rest) in
      typed ctxt loc cons_right stack_ty
  | ( Prim (loc, I_IF_LEFT, [bt; bf], annot),
      (Item_t (Union_t ((tl, _l_field), (tr, _r_field), _), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bt
        (Item_t (tl, rest))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bf
        (Item_t (tr, rest))
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let infobt = kinfo_of_descr ibt and infobf = kinfo_of_descr ibf in
        let instr =
          {
            apply =
              (fun kinfo k ->
                let hinfo = kinfo_of_kinstr k in
                let branch_if_left = ibt.instr.apply infobt (IHalt hinfo)
                and branch_if_right = ibf.instr.apply infobf (IHalt hinfo) in
                IIf_left {kinfo; branch_if_left; branch_if_right; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ~legacy ctxt loc btr bfr {branch}
  (* lists *)
  | (Prim (loc, I_NIL, [t], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy t
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let nil = {apply = (fun kinfo k -> INil (kinfo, k))} in
      list_t loc t >>?= fun ty -> typed ctxt loc nil (Item_t (ty, stack))
  | ( Prim (loc, I_CONS, [], annot),
      Item_t (tv, Item_t (List_t (t, ty_name), rest)) ) ->
      check_item_ty ctxt tv t loc I_CONS 1 2 >>?= fun (Eq, t, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let cons_list = {apply = (fun kinfo k -> ICons_list (kinfo, k))} in
      (typed ctxt loc cons_list (Item_t (List_t (t, ty_name), rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_IF_CONS, [bt; bf], annot),
      (Item_t (List_t (t, ty_name), rest) as bef) ) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        bt
        (Item_t (t, Item_t (List_t (t, ty_name), rest)))
      >>=? fun (btr, ctxt) ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy bf rest
      >>=? fun (bfr, ctxt) ->
      let branch ibt ibf =
        let infobt = kinfo_of_descr ibt and infobf = kinfo_of_descr ibf in
        let instr =
          {
            apply =
              (fun kinfo k ->
                let hinfo = kinfo_of_kinstr k in
                let branch_if_cons = ibt.instr.apply infobt (IHalt hinfo)
                and branch_if_nil = ibf.instr.apply infobf (IHalt hinfo) in
                IIf_cons {kinfo; branch_if_nil; branch_if_cons; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ~legacy ctxt loc btr bfr {branch}
  | (Prim (loc, I_SIZE, [], annot), Item_t (List_t _, rest)) ->
      check_var_type_annot loc annot >>?= fun () ->
      let list_size = {apply = (fun kinfo k -> IList_size (kinfo, k))} in
      typed ctxt loc list_size (Item_t (nat_t, rest))
  | (Prim (loc, I_MAP, [body], annot), Item_t (List_t (elt, _), starting_rest))
    -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, starting_rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ({aft = Item_t (ret, rest); _} as kibody) ->
          let invalid_map_body () =
            let aft = serialize_stack_for_error ctxt kibody.aft in
            Invalid_map_body (loc, aft)
          in
          record_trace_eval
            invalid_map_body
            ( merge_stacks ~legacy loc ctxt 1 rest starting_rest
            >>? fun (Eq, rest, ctxt) ->
              let binfo = kinfo_of_descr kibody in
              let hinfo = {iloc = loc; kstack_ty = Item_t (ret, rest)} in
              let ibody = kibody.instr.apply binfo (IHalt hinfo) in
              let list_map =
                {apply = (fun kinfo k -> IList_map (kinfo, ibody, k))}
              in
              list_t loc ret >>? fun ty ->
              let stack = Item_t (ty, rest) in
              typed_no_lwt ctxt loc list_map stack )
      | Typed {aft; _} ->
          let aft = serialize_stack_for_error ctxt aft in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | (Prim (loc, I_ITER, [body], annot), Item_t (List_t (elt, _), rest)) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, rest))
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
            ( merge_stacks ~legacy loc ctxt 1 aft rest
            >>? fun (Eq, rest, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (mk_list_iter ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (mk_list_iter (descr rest)) rest
      )
  (* sets *)
  | (Prim (loc, I_EMPTY_SET, [t], annot), rest) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt t
      >>?= fun (Ex_comparable_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEmpty_set (kinfo, t, k))} in
      set_t loc t >>?= fun ty -> typed ctxt loc instr (Item_t (ty, rest))
  | (Prim (loc, I_ITER, [body], annot), Item_t (Set_t (comp_elt, _), rest)) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      let elt = ty_of_comparable_ty comp_elt in
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (elt, rest))
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
            ( merge_stacks ~legacy loc ctxt 1 aft rest
            >>? fun (Eq, rest, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (mk_iset_iter ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (mk_iset_iter (descr rest)) rest
      )
  | (Prim (loc, I_MEM, [], annot), Item_t (v, Item_t (Set_t (elt, _), rest))) ->
      let elt = ty_of_comparable_ty elt in
      check_var_type_annot loc annot >>?= fun () ->
      check_item_ty ctxt elt v loc I_MEM 1 2 >>?= fun (Eq, _, ctxt) ->
      let instr = {apply = (fun kinfo k -> ISet_mem (kinfo, k))} in
      (typed ctxt loc instr (Item_t (bool_t, rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t (v, Item_t (Bool_t _, Item_t (Set_t (elt, tname), rest))) ) ->
      check_item_ty ctxt (ty_of_comparable_ty elt) v loc I_UPDATE 1 3
      >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISet_update (kinfo, k))} in
      (typed ctxt loc instr (Item_t (Set_t (elt, tname), rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | (Prim (loc, I_SIZE, [], annot), Item_t (Set_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISet_size (kinfo, k))} in
      typed ctxt loc instr (Item_t (nat_t, rest))
  (* maps *)
  | (Prim (loc, I_EMPTY_MAP, [tk; tv], annot), stack) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEmpty_map (kinfo, tk, k))} in
      map_t loc tk tv >>?= fun ty -> typed ctxt loc instr (Item_t (ty, stack))
  | ( Prim (loc, I_MAP, [body], annot),
      Item_t (Map_t (ck, elt, _), starting_rest) ) -> (
      let k = ty_of_comparable_ty ck in
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_type_annot loc annot >>?= fun () ->
      pair_t loc k elt >>?= fun ty ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (ty, starting_rest))
      >>=? fun (judgement, ctxt) ->
      Lwt.return
      @@
      match judgement with
      | Typed ({aft = Item_t (ret, rest); _} as ibody) ->
          let invalid_map_body () =
            let aft = serialize_stack_for_error ctxt ibody.aft in
            Invalid_map_body (loc, aft)
          in
          record_trace_eval
            invalid_map_body
            ( merge_stacks ~legacy loc ctxt 1 rest starting_rest
            >>? fun (Eq, rest, ctxt) ->
              let instr =
                {
                  apply =
                    (fun kinfo k ->
                      let binfo = kinfo_of_descr ibody in
                      let hinfo =
                        {iloc = loc; kstack_ty = Item_t (ret, rest)}
                      in
                      let ibody = ibody.instr.apply binfo (IHalt hinfo) in
                      IMap_map (kinfo, ibody, k));
                }
              in
              map_t loc ck ret >>? fun ty ->
              let stack = Item_t (ty, rest) in
              typed_no_lwt ctxt loc instr stack )
      | Typed {aft; _} ->
          let aft = serialize_stack_for_error ctxt aft in
          error (Invalid_map_body (loc, aft))
      | Failed _ -> error (Invalid_map_block_fail loc))
  | ( Prim (loc, I_ITER, [body], annot),
      Item_t (Map_t (comp_elt, element_ty, _), rest) ) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      let key = ty_of_comparable_ty comp_elt in
      pair_t loc key element_ty >>?= fun ty ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (ty, rest))
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
            ( merge_stacks ~legacy loc ctxt 1 aft rest
            >>? fun (Eq, rest, ctxt) : ((a, s) judgement * context) tzresult ->
              typed_no_lwt ctxt loc (make_instr ibody) rest )
      | Failed {descr} -> typed_no_lwt ctxt loc (make_instr (descr rest)) rest)
  | (Prim (loc, I_MEM, [], annot), Item_t (vk, Item_t (Map_t (ck, _, _), rest)))
    ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_MEM 1 2 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMap_mem (kinfo, k))} in
      (typed ctxt loc instr (Item_t (bool_t, rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Map_t (ck, elt, _), rest)) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET 1 2 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMap_get (kinfo, k))} in
      option_t loc elt
      >>?= fun ty : ((a, s) judgement * context) tzresult Lwt.t ->
      typed ctxt loc instr (Item_t (ty, rest))
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        (vk, Item_t (Option_t (vv, _), Item_t (Map_t (ck, v, map_name), rest)))
    ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_UPDATE 1 3 >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_UPDATE 2 3 >>?= fun (Eq, v, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMap_update (kinfo, k))} in
      (typed ctxt loc instr (Item_t (Map_t (ck, v, map_name), rest))
        : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t (Option_t (vv, vname), Item_t (Map_t (ck, v, map_name), rest))
        ) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3 >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3 >>?= fun (Eq, v, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMap_get_and_update (kinfo, k))} in
      let stack =
        Item_t (Option_t (vv, vname), Item_t (Map_t (ck, v, map_name), rest))
      in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | (Prim (loc, I_SIZE, [], annot), Item_t (Map_t (_, _, _), rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMap_size (kinfo, k))} in
      typed ctxt loc instr (Item_t (nat_t, rest))
  (* big_map *)
  | (Prim (loc, I_EMPTY_BIG_MAP, [tk; tv], annot), stack) ->
      parse_comparable_ty ~stack_depth:(stack_depth + 1) ctxt tk
      >>?= fun (Ex_comparable_ty tk, ctxt) ->
      parse_big_map_value_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy tv
      >>?= fun (Ex_ty tv, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> IEmpty_big_map (kinfo, tk, tv, k))}
      in
      big_map_t loc tk tv >>?= fun ty ->
      let stack = Item_t (ty, stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MEM, [], annot),
      Item_t (set_key, Item_t (Big_map_t (map_key, _, _), rest)) ) ->
      let k = ty_of_comparable_ty map_key in
      check_item_ty ctxt set_key k loc I_MEM 1 2 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBig_map_mem (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET, [], annot),
      Item_t (vk, Item_t (Big_map_t (ck, elt, _), rest)) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET 1 2 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBig_map_get (kinfo, k))} in
      option_t loc elt >>?= fun ty ->
      let stack = Item_t (ty, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_UPDATE, [], annot),
      Item_t
        ( set_key,
          Item_t
            ( Option_t (set_value, _),
              Item_t (Big_map_t (map_key, map_value, map_name), rest) ) ) ) ->
      let k = ty_of_comparable_ty map_key in
      check_item_ty ctxt set_key k loc I_UPDATE 1 3 >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt set_value map_value loc I_UPDATE 2 3
      >>?= fun (Eq, map_value, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBig_map_update (kinfo, k))} in
      let stack = Item_t (Big_map_t (map_key, map_value, map_name), rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_GET_AND_UPDATE, [], annot),
      Item_t
        ( vk,
          Item_t
            (Option_t (vv, vname), Item_t (Big_map_t (ck, v, map_name), rest))
        ) ) ->
      let k = ty_of_comparable_ty ck in
      check_item_ty ctxt vk k loc I_GET_AND_UPDATE 1 3 >>?= fun (Eq, _, ctxt) ->
      check_item_ty ctxt vv v loc I_GET_AND_UPDATE 2 3 >>?= fun (Eq, v, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> IBig_map_get_and_update (kinfo, k))}
      in
      let stack =
        Item_t (Option_t (vv, vname), Item_t (Big_map_t (ck, v, map_name), rest))
      in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  (* Sapling *)
  | (Prim (loc, I_SAPLING_EMPTY_STATE, [memo_size], annot), rest) ->
      parse_memo_size memo_size >>?= fun memo_size ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> ISapling_empty_state (kinfo, memo_size, k))}
      in
      let stack = Item_t (sapling_state_t ~memo_size, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SAPLING_VERIFY_UPDATE, [], _),
      Item_t
        ( Sapling_transaction_t (transaction_memo_size, _),
          Item_t ((Sapling_state_t (state_memo_size, _) as state_ty), rest) ) )
    ->
      merge_memo_sizes
        ~error_details:Informative
        state_memo_size
        transaction_memo_size
      >>?= fun _memo_size ->
      let instr =
        {apply = (fun kinfo k -> ISapling_verify_update (kinfo, k))}
      in
      pair_t loc int_t state_ty >>?= fun pair_ty ->
      option_t loc pair_ty >>?= fun ty ->
      let stack = Item_t (ty, rest) in
      typed ctxt loc instr stack
  (* control *)
  | (Seq (loc, []), stack) ->
      let instr = {apply = (fun _kinfo k -> k)} in
      typed ctxt loc instr stack
  | (Seq (_, [single]), stack) ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy single stack
  | (Seq (loc, hd :: tl), stack) -> (
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy hd stack
      >>=? fun (judgement, ctxt) ->
      match judgement with
      | Failed _ -> fail (Fail_not_in_tail_position (Micheline.location hd))
      | Typed ({aft = middle; _} as ihd) ->
          non_terminal_recursion
            ?type_logger
            tc_context
            ctxt
            ~legacy
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
  | (Prim (loc, I_IF, [bt; bf], annot), (Item_t (Bool_t _, rest) as bef)) ->
      check_kind [Seq_kind] bt >>?= fun () ->
      check_kind [Seq_kind] bf >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
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
                let hinfo = kinfo_of_kinstr k in
                let branch_if_true = ibt.instr.apply infobt (IHalt hinfo)
                and branch_if_false = ibf.instr.apply infobf (IHalt hinfo) in
                IIf {kinfo; branch_if_true; branch_if_false; k});
          }
        in
        {loc; instr; bef; aft = ibt.aft}
      in
      Lwt.return @@ merge_branches ~legacy ctxt loc btr bfr {branch}
  | (Prim (loc, I_LOOP, [body], annot), (Item_t (Bool_t _, rest) as stack)) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      error_unexpected_annot loc annot >>?= fun () ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy body rest
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
              typed_no_lwt ctxt loc instr rest )
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
          typed_no_lwt ctxt loc instr rest)
  | ( Prim (loc, I_LOOP_LEFT, [body], annot),
      (Item_t (Union_t ((tl, _l_field), (tr, _), _), rest) as stack) ) -> (
      check_kind [Seq_kind] body >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      non_terminal_recursion
        ?type_logger
        tc_context
        ctxt
        ~legacy
        body
        (Item_t (tl, rest))
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
              let stack = Item_t (tr, rest) in
              typed_no_lwt ctxt loc instr stack )
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
          let stack = Item_t (tr, rest) in
          typed_no_lwt ctxt loc instr stack)
  | (Prim (loc, I_LAMBDA, [arg; ret; code], annot), stack) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy arg
      >>?= fun (Ex_ty arg, ctxt) ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ret
      >>?= fun (Ex_ty ret, ctxt) ->
      check_kind [Seq_kind] code >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      parse_returning
        (Tc_context.add_lambda tc_context)
        ?type_logger
        ~stack_depth:(stack_depth + 1)
        ctxt
        ~legacy
        arg
        ret
        code
      >>=? fun (lambda, ctxt) ->
      let instr = {apply = (fun kinfo k -> ILambda (kinfo, lambda, k))} in
      lambda_t loc arg ret >>?= fun ty ->
      let stack = Item_t (ty, stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_EXEC, [], annot),
      Item_t (arg, Item_t (Lambda_t (param, ret, _), rest)) ) ->
      check_item_ty ctxt arg param loc I_EXEC 1 2 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IExec (kinfo, k))} in
      let stack = Item_t (ret, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, I_APPLY, [], annot),
      Item_t
        ( capture,
          Item_t (Lambda_t (Pair_t (capture_ty, arg_ty, _), ret, _), rest) ) )
    ->
      check_packable ~legacy:false loc capture_ty >>?= fun () ->
      check_item_ty ctxt capture capture_ty loc I_APPLY 1 2
      >>?= fun (Eq, capture_ty, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IApply (kinfo, capture_ty, k))} in
      lambda_t loc arg_ty ret
      (* This cannot fail because the type [lambda 'arg 'ret] is always smaller than
         the input type [lambda (pair 'arg 'capture) 'ret]. In an ideal world, there
         would be a smart deconstructor to ensure this statically. *)
      >>?=
      fun res_ty ->
      let stack = Item_t (res_ty, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | (Prim (loc, I_DIP, [code], annot), Item_t (v, rest)) -> (
      error_unexpected_annot loc annot >>?= fun () ->
      check_kind [Seq_kind] code >>?= fun () ->
      non_terminal_recursion ?type_logger tc_context ctxt ~legacy code rest
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
          let stack = Item_t (v, descr.aft) in
          typed ctxt loc instr stack
      | Failed _ -> fail (Fail_not_in_tail_position loc))
  | (Prim (loc, I_DIP, [n; code], result_annot), stack) ->
      parse_uint10 n >>?= fun n ->
      Gas.consume ctxt (Typecheck_costs.proof_argument n) >>?= fun ctxt ->
      let rec make_proof_argument :
          type a s.
          int -> (a, s) stack_ty -> (a, s) dipn_proof_argument tzresult Lwt.t =
       fun n stk ->
        match (Compare.Int.(n = 0), stk) with
        | (true, rest) -> (
            non_terminal_recursion
              ?type_logger
              tc_context
              ctxt
              ~legacy
              code
              rest
            >>=? fun (judgement, ctxt) ->
            Lwt.return
            @@
            match judgement with
            | Typed descr ->
                ok
                  (Dipn_proof_argument (KRest, ctxt, descr, descr.aft)
                    : (a, s) dipn_proof_argument)
            | Failed _ -> error (Fail_not_in_tail_position loc))
        | (false, Item_t (v, rest)) ->
            make_proof_argument (n - 1) rest
            >|=? fun (Dipn_proof_argument (n', ctxt, descr, aft')) ->
            let kinfo' = {iloc = loc; kstack_ty = aft'} in
            let w = KPrefix (kinfo', n') in
            Dipn_proof_argument (w, ctxt, descr, Item_t (v, aft'))
        | (_, _) ->
            Lwt.return
              (let whole_stack = serialize_stack_for_error ctxt stack in
               error (Bad_stack (loc, I_DIP, 1, whole_stack)))
      in
      error_unexpected_annot loc result_annot >>?= fun () ->
      make_proof_argument n stack
      >>=? fun (Dipn_proof_argument (n', ctxt, descr, aft)) ->
      let kinfo = {iloc = descr.loc; kstack_ty = descr.bef} in
      let kinfoh = {iloc = descr.loc; kstack_ty = descr.aft} in
      let b = descr.instr.apply kinfo (IHalt kinfoh) in
      let res = {apply = (fun kinfo k -> IDipn (kinfo, n, n', b, k))} in
      typed ctxt loc res aft
  | (Prim (loc, I_DIP, (([] | _ :: _ :: _ :: _) as l), _), _) ->
      (* Technically, the arities 1 and 2 are allowed but the error only mentions 2.
            However, DIP {code} is equivalent to DIP 1 {code} so hinting at an arity of 2 makes sense. *)
      fail (Invalid_arity (loc, I_DIP, 2, List.length l))
  | (Prim (loc, I_FAILWITH, [], annot), Item_t (v, _rest)) ->
      Lwt.return
        ( error_unexpected_annot loc annot >>? fun () ->
          (if legacy then Result.return_unit
          else check_packable ~legacy:false loc v)
          >|? fun () ->
          let instr = {apply = (fun kinfo _k -> IFailwith (kinfo, loc, v))} in
          let descr aft = {loc; instr; bef = stack_ty; aft} in
          log_stack loc stack_ty Bot_t ;
          (Failed {descr}, ctxt) )
  | (Prim (loc, I_NEVER, [], annot), Item_t (Never_t _, _rest)) ->
      Lwt.return
        ( error_unexpected_annot loc annot >|? fun () ->
          let instr = {apply = (fun kinfo _k -> INever kinfo)} in
          let descr aft = {loc; instr; bef = stack_ty; aft} in
          log_stack loc stack_ty Bot_t ;
          (Failed {descr}, ctxt) )
  (* timestamp operations *)
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> IAdd_timestamp_to_seconds (kinfo, k))}
      in
      typed ctxt loc instr (Item_t (Timestamp_t tname, rest))
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Int_t _, Item_t (Timestamp_t tname, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> IAdd_seconds_to_timestamp (kinfo, k))}
      in
      typed ctxt loc instr (Item_t (Timestamp_t tname, rest))
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t tname, Item_t (Int_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> ISub_timestamp_seconds (kinfo, k))}
      in
      let stack = Item_t (Timestamp_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Timestamp_t _, Item_t (Timestamp_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IDiff_timestamps (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  (* string operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (String_t tn1, Item_t (String_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IConcat_string_pair (kinfo, k))} in
      typed ctxt loc instr (Item_t (String_t tname, rest))
  | (Prim (loc, I_CONCAT, [], annot), Item_t (List_t (String_t tname, _), rest))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IConcat_string (kinfo, k))} in
      typed ctxt loc instr (Item_t (String_t tname, rest))
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t _, Item_t (Nat_t _, Item_t (String_t _, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISlice_string (kinfo, k))} in
      let stack = Item_t (option_string_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SIZE, [], annot), Item_t (String_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IString_size (kinfo, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  (* bytes operations *)
  | ( Prim (loc, I_CONCAT, [], annot),
      Item_t (Bytes_t tn1, Item_t (Bytes_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IConcat_bytes_pair (kinfo, k))} in
      let stack = Item_t (Bytes_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_CONCAT, [], annot), Item_t (List_t (Bytes_t tname, _), rest))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IConcat_bytes (kinfo, k))} in
      let stack = Item_t (Bytes_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SLICE, [], annot),
      Item_t (Nat_t _, Item_t (Nat_t _, Item_t (Bytes_t _, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISlice_bytes (kinfo, k))} in
      let stack = Item_t (option_bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SIZE, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBytes_size (kinfo, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  (* currency operations *)
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_tez (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SUB, [], annot),
      Item_t (Mutez_t tn1, Item_t (Mutez_t tn2, rest)) ) ->
      if legacy then
        check_var_annot loc annot >>?= fun () ->
        merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
        let instr = {apply = (fun kinfo k -> ISub_tez_legacy (kinfo, k))} in
        let stack = Item_t (Mutez_t tname, rest) in
        typed ctxt loc instr stack
      else fail (Deprecated_instruction I_SUB)
  | ( Prim (loc, I_SUB_MUTEZ, [], annot),
      Item_t (Mutez_t _, Item_t (Mutez_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISub_tez (kinfo, k))} in
      let stack = Item_t (option_mutez_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Mutez_t tname, Item_t (Nat_t _, rest)) ) ->
      (* no type name check *)
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_teznat (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Mutez_t tname, rest)) ) ->
      (* no type name check *)
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_nattez (kinfo, k))} in
      let stack = Item_t (Mutez_t tname, rest) in
      typed ctxt loc instr stack
  (* boolean operations *)
  | (Prim (loc, I_OR, [], annot), Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IOr (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_AND, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAnd (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_XOR, [], annot),
      Item_t (Bool_t tn1, Item_t (Bool_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IXor (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Bool_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INot (kinfo, k))} in
      let stack = Item_t (Bool_t tname, rest) in
      typed ctxt loc instr stack
  (* integer operations *)
  | (Prim (loc, I_ABS, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAbs_int (kinfo, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_ISNAT, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IIs_nat (kinfo, k))} in
      let stack = Item_t (option_nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_INT, [], annot), Item_t (Nat_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IInt_nat (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Int_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeg (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Nat_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeg (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_ADD, [], annot), Item_t (Int_t tn1, Item_t (Int_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_ADD, [], annot), Item_t (Int_t tname, Item_t (Nat_t _, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAdd_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_ADD, [], annot), Item_t (Nat_t _, Item_t (Int_t tname, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAdd_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_ADD, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SUB, [], annot), Item_t (Int_t tn1, Item_t (Int_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SUB, [], annot), Item_t (Int_t tname, Item_t (Nat_t _, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SUB, [], annot), Item_t (Nat_t _, Item_t (Int_t tname, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SUB, [], annot), Item_t (Nat_t _, Item_t (Nat_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISub_int (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_MUL, [], annot), Item_t (Int_t tn1, Item_t (Int_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IMul_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_MUL, [], annot), Item_t (Int_t tname, Item_t (Nat_t _, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_MUL, [], annot), Item_t (Nat_t _, Item_t (Int_t tname, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_nat (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_MUL, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IMul_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Mutez_t _, Item_t (Nat_t _, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_teznat (kinfo, k))} in
      let stack = Item_t (option_pair_mutez_mutez_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Mutez_t _, Item_t (Mutez_t _, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_tez (kinfo, k))} in
      let stack = Item_t (option_pair_nat_mutez_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Int_t _, Item_t (Int_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_int (kinfo, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Int_t _, Item_t (Nat_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_int (kinfo, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Nat_t _, Item_t (Int_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_nat (kinfo, k))} in
      let stack = Item_t (option_pair_int_nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_EDIV, [], annot), Item_t (Nat_t _, Item_t (Nat_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEdiv_nat (kinfo, k))} in
      let stack = Item_t (option_pair_nat_nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_LSL, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ILsl_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_LSR, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> ILsr_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_OR, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IOr_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_AND, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAnd_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_AND, [], annot), Item_t (Int_t _, Item_t (Nat_t tname, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAnd_int_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_XOR, [], annot), Item_t (Nat_t tn1, Item_t (Nat_t tn2, rest)))
    ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IXor_nat (kinfo, k))} in
      let stack = Item_t (Nat_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Int_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INot_int (kinfo, k))} in
      let stack = Item_t (Int_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NOT, [], annot), Item_t (Nat_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INot_int (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  (* comparison *)
  | (Prim (loc, I_COMPARE, [], annot), Item_t (t1, Item_t (t2, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      check_item_ty ctxt t1 t2 loc I_COMPARE 1 2 >>?= fun (Eq, t, ctxt) ->
      comparable_ty_of_ty ctxt loc t >>?= fun (key, ctxt) ->
      let instr = {apply = (fun kinfo k -> ICompare (kinfo, key, k))} in
      let stack = Item_t (int_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  (* comparators *)
  | (Prim (loc, I_EQ, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IEq (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEQ, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeq (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_LT, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ILt (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_GT, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IGt (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_LE, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ILe (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_GE, [], annot), Item_t (Int_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IGe (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  (* annotations *)
  | (Prim (loc, I_CAST, [cast_t], annot), Item_t (t, stack)) ->
      check_var_annot loc annot >>?= fun () ->
      parse_any_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy cast_t
      >>?= fun (Ex_ty cast_t, ctxt) ->
      ty_eq ~legacy ctxt loc cast_t t >>?= fun (Eq, ctxt) ->
      let instr = {apply = (fun _ k -> k)} in
      let stack = Item_t (cast_t, stack) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | (Prim (loc, I_RENAME, [], annot), Item_t (t, stack)) ->
      check_var_annot loc annot >>?= fun () ->
      (* can erase annot *)
      let instr = {apply = (fun _ k -> k)} in
      let stack = Item_t (t, stack) in
      typed ctxt loc instr stack
  (* packing *)
  | (Prim (loc, I_PACK, [], annot), Item_t (t, rest)) ->
      check_packable
        ~legacy:true
        (* allow to pack contracts for hash/signature checks *) loc
        t
      >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IPack (kinfo, t, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_UNPACK, [ty], annot), Item_t (Bytes_t _, rest)) ->
      parse_packable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      check_var_type_annot loc annot >>?= fun () ->
      option_t loc t >>?= fun res_ty ->
      let instr = {apply = (fun kinfo k -> IUnpack (kinfo, t, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  (* protocol *)
  | (Prim (loc, I_ADDRESS, [], annot), Item_t (Contract_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAddress (kinfo, k))} in
      let stack = Item_t (address_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_CONTRACT, [ty], annot), Item_t (Address_t _, rest)) ->
      parse_passable_ty ctxt ~stack_depth:(stack_depth + 1) ~legacy ty
      >>?= fun (Ex_ty t, ctxt) ->
      contract_t loc t >>?= fun contract_ty ->
      option_t loc contract_ty >>?= fun res_ty ->
      parse_entrypoint_annot loc annot >>?= fun entrypoint ->
      Script_ir_annot.field_annot_opt_to_entrypoint_strict ~loc entrypoint
      >>?= fun entrypoint ->
      let instr =
        {apply = (fun kinfo k -> IContract (kinfo, t, entrypoint, k))}
      in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_VIEW, [name; output_ty], annot),
      Item_t (input_ty, Item_t (Address_t _, rest)) ) ->
      let output_ty_loc = location output_ty in
      parse_view_name ctxt name >>?= fun (name, ctxt) ->
      parse_view_output_ty ctxt ~stack_depth:0 ~legacy output_ty
      >>?= fun (Ex_ty output_ty, ctxt) ->
      option_t output_ty_loc output_ty >>?= fun res_ty ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {
          apply =
            (fun kinfo k ->
              IView (kinfo, View_signature {name; input_ty; output_ty}, k));
        }
      in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, (I_TRANSFER_TOKENS as prim), [], annot),
      Item_t (p, Item_t (Mutez_t _, Item_t (Contract_t (cp, _), rest))) ) ->
      Tc_context.check_not_in_view loc ~legacy tc_context prim >>?= fun () ->
      check_item_ty ctxt p cp loc prim 1 4 >>?= fun (Eq, _, ctxt) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ITransfer_tokens (kinfo, k))} in
      let stack = Item_t (operation_t, rest) in
      (typed ctxt loc instr stack : ((a, s) judgement * context) tzresult Lwt.t)
  | ( Prim (loc, (I_SET_DELEGATE as prim), [], annot),
      Item_t (Option_t (Key_hash_t _, _), rest) ) ->
      Tc_context.check_not_in_view loc ~legacy tc_context prim >>?= fun () ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISet_delegate (kinfo, k))} in
      let stack = Item_t (operation_t, rest) in
      typed ctxt loc instr stack
  | (Prim (_, I_CREATE_ACCOUNT, _, _), _) ->
      fail (Deprecated_instruction I_CREATE_ACCOUNT)
  | (Prim (loc, I_IMPLICIT_ACCOUNT, [], annot), Item_t (Key_hash_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IImplicit_account (kinfo, k))} in
      let stack = Item_t (contract_unit_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, (I_CREATE_CONTRACT as prim), [(Seq _ as code)], annot),
      Item_t
        (Option_t (Key_hash_t _, _), Item_t (Mutez_t _, Item_t (ginit, rest)))
    ) ->
      Tc_context.check_not_in_view ~legacy loc tc_context prim >>?= fun () ->
      check_two_var_annot loc annot >>?= fun () ->
      let canonical_code = Micheline.strip_locations code in
      parse_toplevel ctxt ~legacy canonical_code
      >>?= fun ({arg_type; storage_type; code_field; views; root_name}, ctxt) ->
      record_trace
        (Ill_formed_type (Some "parameter", canonical_code, location arg_type))
        (parse_parameter_ty_and_entrypoints
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           ~root_name
           arg_type)
      >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, ctxt) ->
      record_trace
        (Ill_formed_type (Some "storage", canonical_code, location storage_type))
        (parse_storage_ty
           ctxt
           ~stack_depth:(stack_depth + 1)
           ~legacy
           storage_type)
      >>?= fun (Ex_ty storage_type, ctxt) ->
      pair_t loc arg_type storage_type >>?= fun arg_type_full ->
      pair_t loc list_operation_t storage_type >>?= fun ret_type_full ->
      trace
        (Ill_typed_contract (canonical_code, []))
        (parse_returning
           (Tc_context.toplevel ~storage_type ~param_type:arg_type root_name)
           ctxt
           ~legacy
           ?type_logger
           ~stack_depth:(stack_depth + 1)
           arg_type_full
           ret_type_full
           code_field)
      >>=? fun ( (Lam
                    ( {kbef = Item_t (arg, Bot_t); kaft = Item_t (ret, Bot_t); _},
                      _ ) as lambda),
                 ctxt ) ->
      let views_result =
        typecheck_views ctxt ?type_logger ~legacy storage_type views
      in
      trace (Ill_typed_contract (canonical_code, [])) views_result
      >>=? fun ctxt ->
      ty_eq ~legacy ctxt loc arg arg_type_full >>?= fun (Eq, ctxt) ->
      ty_eq ~legacy ctxt loc ret ret_type_full >>?= fun (Eq, ctxt) ->
      ty_eq ~legacy ctxt loc storage_type ginit >>?= fun (Eq, ctxt) ->
      let instr =
        {
          apply =
            (fun kinfo k ->
              ICreate_contract
                {kinfo; storage_type; arg_type; lambda; views; root_name; k});
        }
      in
      let stack = Item_t (operation_t, Item_t (address_t, rest)) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NOW, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INow (kinfo, k))} in
      let stack = Item_t (timestamp_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_AMOUNT, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IAmount (kinfo, k))} in
      let stack = Item_t (mutez_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_CHAIN_ID, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IChainId (kinfo, k))} in
      let stack = Item_t (chain_id_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_BALANCE, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBalance (kinfo, k))} in
      let stack = Item_t (mutez_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_LEVEL, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ILevel (kinfo, k))} in
      let stack = Item_t (nat_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_VOTING_POWER, [], annot), Item_t (Key_hash_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IVoting_power (kinfo, k))} in
      let stack = Item_t (nat_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_TOTAL_VOTING_POWER, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ITotal_voting_power (kinfo, k))} in
      let stack = Item_t (nat_t, stack) in
      typed ctxt loc instr stack
  | (Prim (_, I_STEPS_TO_QUOTA, _, _), _) ->
      fail (Deprecated_instruction I_STEPS_TO_QUOTA)
  | (Prim (loc, I_SOURCE, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISource (kinfo, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SENDER, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISender (kinfo, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  | (Prim (loc, (I_SELF as prim), [], annot), stack) ->
      Lwt.return
        ( parse_entrypoint_annot loc annot >>? fun entrypoint ->
          (match entrypoint with
          | None -> Ok Entrypoint.default
          | Some (Field_annot annot) -> Entrypoint.of_annot_lax annot)
          >>? fun entrypoint ->
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
          | Toplevel {param_type; root_name; storage_type = _} ->
              Gas_monad.run ctxt
              @@ find_entrypoint
                   ~error_details:Informative
                   param_type
                   ~root_name
                   entrypoint
              >>? fun (r, ctxt) ->
              r >>? fun (_, Ex_ty param_type) ->
              contract_t loc param_type >>? fun res_ty ->
              let instr =
                {
                  apply =
                    (fun kinfo k -> ISelf (kinfo, param_type, entrypoint, k));
                }
              in
              let stack = Item_t (res_ty, stack) in
              typed_no_lwt ctxt loc instr stack )
  | (Prim (loc, I_SELF_ADDRESS, [], annot), stack) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISelf_address (kinfo, k))} in
      let stack = Item_t (address_t, stack) in
      typed ctxt loc instr stack
  (* cryptography *)
  | (Prim (loc, I_HASH_KEY, [], annot), Item_t (Key_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IHash_key (kinfo, k))} in
      let stack = Item_t (key_hash_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_CHECK_SIGNATURE, [], annot),
      Item_t (Key_t _, Item_t (Signature_t _, Item_t (Bytes_t _, rest))) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ICheck_signature (kinfo, k))} in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_BLAKE2B, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IBlake2b (kinfo, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SHA256, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISha256 (kinfo, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SHA512, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISha512 (kinfo, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_KECCAK, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IKeccak (kinfo, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_SHA3, [], annot), Item_t (Bytes_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> ISha3 (kinfo, k))} in
      let stack = Item_t (bytes_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g1_t tn1, Item_t (Bls12_381_g1_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_g2_t tn1, Item_t (Bls12_381_g2_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_ADD, [], annot),
      Item_t (Bls12_381_fr_t tn1, Item_t (Bls12_381_fr_t tn2, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      merge_type_metadata ~error_details:Informative tn1 tn2 >>?= fun tname ->
      let instr = {apply = (fun kinfo k -> IAdd_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g1_t tname, Item_t (Bls12_381_fr_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_g2_t tname, Item_t (Bls12_381_fr_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Bls12_381_fr_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Nat_t _, Item_t (Bls12_381_fr_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr_z (kinfo, k))} in
      let stack = Item_t (bls12_381_fr_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Int_t _, Item_t (Bls12_381_fr_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_fr_z (kinfo, k))} in
      let stack = Item_t (bls12_381_fr_t, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Int_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_z_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_MUL, [], annot),
      Item_t (Bls12_381_fr_t tname, Item_t (Nat_t _, rest)) ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IMul_bls12_381_z_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_INT, [], annot), Item_t (Bls12_381_fr_t _, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> IInt_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (int_t, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_g1_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_g1 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g1_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_g2_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_g2 (kinfo, k))} in
      let stack = Item_t (Bls12_381_g2_t tname, rest) in
      typed ctxt loc instr stack
  | (Prim (loc, I_NEG, [], annot), Item_t (Bls12_381_fr_t tname, rest)) ->
      check_var_annot loc annot >>?= fun () ->
      let instr = {apply = (fun kinfo k -> INeg_bls12_381_fr (kinfo, k))} in
      let stack = Item_t (Bls12_381_fr_t tname, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_PAIRING_CHECK, [], annot),
      Item_t (List_t (Pair_t (Bls12_381_g1_t _, Bls12_381_g2_t _, _), _), rest)
    ) ->
      check_var_annot loc annot >>?= fun () ->
      let instr =
        {apply = (fun kinfo k -> IPairing_check_bls12_381 (kinfo, k))}
      in
      let stack = Item_t (bool_t, rest) in
      typed ctxt loc instr stack
  (* Tickets *)
  | (Prim (loc, I_TICKET, [], annot), Item_t (t, Item_t (Nat_t _, rest))) ->
      check_var_annot loc annot >>?= fun () ->
      comparable_ty_of_ty ctxt loc t >>?= fun (ty, ctxt) ->
      ticket_t loc ty >>?= fun res_ty ->
      let instr = {apply = (fun kinfo k -> ITicket (kinfo, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_READ_TICKET, [], annot),
      (Item_t (Ticket_t (t, _), _) as full_stack) ) ->
      check_var_annot loc annot >>?= fun () ->
      let () = check_dupable_comparable_ty t in
      opened_ticket_type loc t >>?= fun opened_ticket_ty ->
      let result = ty_of_comparable_ty opened_ticket_ty in
      let instr = {apply = (fun kinfo k -> IRead_ticket (kinfo, k))} in
      let stack = Item_t (result, full_stack) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_SPLIT_TICKET, [], annot),
      Item_t
        ( (Ticket_t (t, _) as ticket_t),
          Item_t (Pair_t (Nat_t _, Nat_t _, _), rest) ) ) ->
      check_var_annot loc annot >>?= fun () ->
      let () = check_dupable_comparable_ty t in
      pair_t loc ticket_t ticket_t >>?= fun pair_tickets_ty ->
      option_t loc pair_tickets_ty >>?= fun res_ty ->
      let instr = {apply = (fun kinfo k -> ISplit_ticket (kinfo, k))} in
      let stack = Item_t (res_ty, rest) in
      typed ctxt loc instr stack
  | ( Prim (loc, I_JOIN_TICKETS, [], annot),
      Item_t (Pair_t ((Ticket_t _ as ty_a), (Ticket_t _ as ty_b), _), rest) )
    -> (
      check_var_annot loc annot >>?= fun () ->
      Gas_monad.run ctxt
      @@ merge_types ~legacy ~error_details:Informative loc ty_a ty_b
      >>?= fun (eq_ty, ctxt) ->
      eq_ty >>?= fun (Eq, ty) ->
      match ty with
      | Ticket_t (contents_ty, _) ->
          option_t loc ty >>?= fun res_ty ->
          let instr =
            {apply = (fun kinfo k -> IJoin_tickets (kinfo, contents_ty, k))}
          in
          let stack = Item_t (res_ty, rest) in
          typed ctxt loc instr stack)
  (* Timelocks *)
  | ( Prim (loc, I_OPEN_CHEST, [], _),
      Item_t (Chest_key_t _, Item_t (Chest_t _, Item_t (Nat_t _, rest))) ) ->
      let instr = {apply = (fun kinfo k -> IOpen_chest (kinfo, k))} in
      typed ctxt loc instr (Item_t (union_bytes_bool_t, rest))
  (* Primitive parsing errors *)
  | ( Prim
        ( loc,
          (( I_DUP | I_SWAP | I_SOME | I_UNIT | I_PAIR | I_UNPAIR | I_CAR
           | I_CDR | I_CONS | I_CONCAT | I_SLICE | I_MEM | I_UPDATE | I_GET
           | I_EXEC | I_FAILWITH | I_SIZE | I_ADD | I_SUB | I_SUB_MUTEZ | I_MUL
           | I_EDIV | I_OR | I_AND | I_XOR | I_NOT | I_ABS | I_NEG | I_LSL
           | I_LSR | I_COMPARE | I_EQ | I_NEQ | I_LT | I_GT | I_LE | I_GE
           | I_TRANSFER_TOKENS | I_SET_DELEGATE | I_NOW | I_IMPLICIT_ACCOUNT
           | I_AMOUNT | I_BALANCE | I_LEVEL | I_CHECK_SIGNATURE | I_HASH_KEY
           | I_SOURCE | I_SENDER | I_BLAKE2B | I_SHA256 | I_SHA512 | I_ADDRESS
           | I_RENAME | I_PACK | I_ISNAT | I_INT | I_SELF | I_CHAIN_ID | I_NEVER
           | I_VOTING_POWER | I_TOTAL_VOTING_POWER | I_KECCAK | I_SHA3
           | I_PAIRING_CHECK | I_TICKET | I_READ_TICKET | I_SPLIT_TICKET
           | I_JOIN_TICKETS | I_OPEN_CHEST ) as name),
          (_ :: _ as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 0, List.length l))
  | ( Prim
        ( loc,
          (( I_NONE | I_LEFT | I_RIGHT | I_NIL | I_MAP | I_ITER | I_EMPTY_SET
           | I_LOOP | I_LOOP_LEFT | I_CONTRACT | I_CAST | I_UNPACK
           | I_CREATE_CONTRACT ) as name),
          (([] | _ :: _ :: _) as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 1, List.length l))
  | ( Prim
        ( loc,
          (( I_PUSH | I_VIEW | I_IF_NONE | I_IF_LEFT | I_IF_CONS | I_EMPTY_MAP
           | I_EMPTY_BIG_MAP | I_IF ) as name),
          (([] | [_] | _ :: _ :: _ :: _) as l),
          _ ),
      _ ) ->
      fail (Invalid_arity (loc, name, 2, List.length l))
  | ( Prim (loc, I_LAMBDA, (([] | [_] | [_; _] | _ :: _ :: _ :: _ :: _) as l), _),
      _ ) ->
      fail (Invalid_arity (loc, I_LAMBDA, 3, List.length l))
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
      fail (Undefined_binop (loc, name, ta, tb))
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
      fail (Undefined_unop (loc, name, t))
  | (Prim (loc, ((I_UPDATE | I_SLICE | I_OPEN_CHEST) as name), [], _), stack) ->
      Lwt.return
        (let stack = serialize_stack_for_error ctxt stack in
         error (Bad_stack (loc, name, 3, stack)))
  | (Prim (loc, I_CREATE_CONTRACT, _, _), stack) ->
      let stack = serialize_stack_for_error ctxt stack in
      fail (Bad_stack (loc, I_CREATE_CONTRACT, 7, stack))
  | (Prim (loc, I_TRANSFER_TOKENS, [], _), stack) ->
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
  | (expr, _) ->
      fail
      @@ unexpected
           expr
           [Seq_kind]
           Instr_namespace
           [
             I_DROP;
             I_DUP;
             I_DIG;
             I_DUG;
             I_VIEW;
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
             I_SUB_MUTEZ;
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
             I_JOIN_TICKETS;
             I_OPEN_CHEST;
           ]

and[@coq_axiom_with_reason "complex mutually recursive definition"] parse_contract :
    type arg.
    stack_depth:int ->
    legacy:bool ->
    context ->
    Script.location ->
    arg ty ->
    Destination.t ->
    entrypoint:Entrypoint.t ->
    (context * arg typed_contract) tzresult Lwt.t =
 fun ~stack_depth ~legacy ctxt loc arg contract ~entrypoint ->
  match contract with
  | Contract contract -> (
      match Contract.is_implicit contract with
      | Some _ ->
          if Entrypoint.is_default entrypoint then
            (* An implicit account on the "default" entrypoint always exists and has type unit. *)
            Lwt.return
              ( ty_eq ~legacy:true ctxt loc arg unit_t >|? fun (Eq, ctxt) ->
                let destination : Destination.t = Contract contract in
                (ctxt, {arg_ty = arg; address = {destination; entrypoint}}) )
          else fail (No_such_entrypoint entrypoint)
      | None -> (
          (* Originated account *)
          trace (Invalid_contract (loc, contract))
          @@ Contract.get_script_code ctxt contract
          >>=? fun (ctxt, code) ->
          match code with
          | None -> fail (Invalid_contract (loc, contract))
          | Some code ->
              Lwt.return
                ( Script.force_decode_in_context
                    ~consume_deserialization_gas:When_needed
                    ctxt
                    code
                >>? fun (code, ctxt) ->
                  (* can only fail because of gas *)
                  parse_toplevel ctxt ~legacy:true code
                  >>? fun ({arg_type; root_name; _}, ctxt) ->
                  parse_parameter_ty_and_entrypoints
                    ctxt
                    ~stack_depth:(stack_depth + 1)
                    ~legacy:true
                    ~root_name
                    arg_type
                  >>? fun ( Ex_parameter_ty_and_entrypoints
                              {arg_type = targ; root_name},
                            ctxt ) ->
                  (* we don't check targ size here because it's a legacy contract code *)
                  Gas_monad.run ctxt
                  @@ find_entrypoint_for_type
                       ~legacy
                       ~error_details:Informative
                       ~full:targ
                       ~expected:arg
                       ~root_name
                       entrypoint
                       loc
                  >>? fun (entrypoint_arg, ctxt) ->
                  entrypoint_arg >|? fun (entrypoint, arg_ty) ->
                  let destination : Destination.t = Contract contract in
                  (ctxt, {arg_ty; address = {destination; entrypoint}}) )))

and parse_view_name ctxt : Script.node -> (Script_string.t * context) tzresult =
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

and parse_toplevel :
    context -> legacy:bool -> Script.expr -> (toplevel * context) tzresult =
 fun ctxt ~legacy toplevel ->
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
            if SMap.mem str views then error (Duplicated_view_name loc)
            else
              let views' =
                SMap.add str {input_ty; output_ty; view_code} views
              in
              find_fields ctxt p s c views' rest
        | Prim (loc, K_view, args, _) :: _ ->
            error (Invalid_arity (loc, K_view, 4, List.length args))
        | Prim (loc, name, _, _) :: _ ->
            let allowed = [K_parameter; K_storage; K_code; K_view] in
            error (Invalid_primitive (loc, allowed, name))
      in
      find_fields ctxt None None None SMap.empty fields
      >>? fun (ctxt, toplevel) ->
      match toplevel with
      | (None, _, _, _) -> error (Missing_field K_parameter)
      | (Some _, None, _, _) -> error (Missing_field K_storage)
      | (Some _, Some _, None, _) -> error (Missing_field K_code)
      | ( Some (p, ploc, pannot),
          Some (s, sloc, sannot),
          Some (c, cloc, carrot),
          views ) ->
          let maybe_root_name =
            (* root name can be attached to either the parameter
               primitive or the toplevel constructor (legacy only) *)
            Script_ir_annot.extract_field_annot p >>? fun (p, root_name) ->
            match root_name with
            | Some (Field_annot root_name) ->
                let root_name = Entrypoint.of_annot_lax_opt root_name in
                ok (p, pannot, root_name)
            | None -> (
                match pannot with
                | [single]
                  when legacy
                       && Compare.Int.(String.length single > 0)
                       && Compare.Char.(single.[0] = '%') -> (
                    parse_field_annot ploc [single] >|? function
                    | None -> (p, [], None)
                    | Some (Field_annot pannot) ->
                        let root_name = Entrypoint.of_annot_lax_opt pannot in
                        (p, [], root_name))
                | _ -> ok (p, pannot, None))
          in
          (* only one field annot is allowed to set the root entrypoint name *)
          maybe_root_name >>? fun (arg_type, pannot, root_name) ->
          Script_ir_annot.error_unexpected_annot ploc pannot >>? fun () ->
          Script_ir_annot.error_unexpected_annot cloc carrot >>? fun () ->
          Script_ir_annot.error_unexpected_annot sloc sannot >|? fun () ->
          ({code_field = c; arg_type; root_name; views; storage_type = s}, ctxt)
      )

(* Same as [parse_contract], but does not fail when the contact is missing or
   if the expected type doesn't match the actual one. In that case None is
   returned and some overapproximation of the typechecking gas is consumed.
   This can still fail on gas exhaustion. *)
let parse_contract_for_script :
    type arg.
    context ->
    Script.location ->
    arg ty ->
    Destination.t ->
    entrypoint:Entrypoint.t ->
    (context * arg typed_contract option) tzresult Lwt.t =
 fun ctxt loc arg contract ~entrypoint ->
  match contract with
  | Contract contract -> (
      match Contract.is_implicit contract with
      | Some _ ->
          if Entrypoint.is_default entrypoint then
            (* An implicit account on the "default" entrypoint always exists and has type unit. *)
            Lwt.return
              ( Gas_monad.run ctxt
              @@ merge_types ~legacy:true ~error_details:Fast loc arg unit_t
              >|? fun (eq_ty, ctxt) ->
                match eq_ty with
                | Ok (Eq, _ty) ->
                    let destination : Destination.t = Contract contract in
                    let contract =
                      {arg_ty = arg; address = {destination; entrypoint}}
                    in
                    (ctxt, Some contract)
                | Error Inconsistent_types_fast -> (ctxt, None) )
          else
            Lwt.return
              ( Gas.consume ctxt Typecheck_costs.parse_instr_cycle
              >|? fun ctxt ->
                (* An implicit account on any other entrypoint is not a valid contract. *)
                (ctxt, None) )
      | None -> (
          (* Originated account *)
          trace (Invalid_contract (loc, contract))
          @@ Contract.get_script_code ctxt contract
          >>=? fun (ctxt, code) ->
          match code with
          | None -> return (ctxt, None)
          | Some code ->
              Lwt.return
                ( Script.force_decode_in_context
                    ~consume_deserialization_gas:When_needed
                    ctxt
                    code
                >>? fun (code, ctxt) ->
                  (* can only fail because of gas *)
                  match parse_toplevel ctxt ~legacy:true code with
                  | Error _ -> error (Invalid_contract (loc, contract))
                  | Ok ({arg_type; root_name; _}, ctxt) -> (
                      match
                        parse_parameter_ty_and_entrypoints
                          ctxt
                          ~stack_depth:0
                          ~legacy:true
                          ~root_name
                          arg_type
                      with
                      | Error _ -> error (Invalid_contract (loc, contract))
                      | Ok
                          ( Ex_parameter_ty_and_entrypoints
                              {arg_type = targ; root_name},
                            ctxt ) -> (
                          (* we don't check targ size here because it's a legacy contract code *)
                          Gas_monad.run ctxt
                          @@ find_entrypoint_for_type
                               ~legacy:false
                               ~error_details:Fast
                               ~full:targ
                               ~expected:arg
                               ~root_name
                               entrypoint
                               loc
                          >|? fun (entrypoint_arg, ctxt) ->
                          match entrypoint_arg with
                          | Ok (entrypoint, arg_ty) ->
                              let destination = Destination.Contract contract in
                              let contract =
                                {arg_ty; address = {destination; entrypoint}}
                              in
                              (ctxt, Some contract)
                          | Error Inconsistent_types_fast -> (ctxt, None))) )))

let parse_code :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    code:lazy_expr ->
    (ex_code * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~code ->
  Script.force_decode_in_context
    ~consume_deserialization_gas:When_needed
    ctxt
    code
  >>?= fun (code, ctxt) ->
  Global_constants_storage.expand ctxt code >>=? fun (ctxt, code) ->
  parse_toplevel ctxt ~legacy code
  >>?= fun ({arg_type; storage_type; code_field; views; root_name}, ctxt) ->
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty_and_entrypoints
       ctxt
       ~stack_depth:0
       ~legacy
       ~root_name
       arg_type)
  >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, ctxt) ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (Ex_ty storage_type, ctxt) ->
  pair_t storage_type_loc arg_type storage_type >>?= fun arg_type_full ->
  pair_t storage_type_loc list_operation_t storage_type
  >>?= fun ret_type_full ->
  trace
    (Ill_typed_contract (code, []))
    (parse_returning
       Tc_context.(toplevel ~storage_type ~param_type:arg_type root_name)
       ctxt
       ~legacy
       ~stack_depth:0
       ?type_logger
       arg_type_full
       ret_type_full
       code_field)
  >>=? fun (code, ctxt) ->
  Lwt.return
    (let open Script_typed_ir_size in
    let view_size view =
      node_size view.view_code ++ node_size view.input_ty
      ++ node_size view.output_ty
    in
    let views_size = SMap.fold (fun _ v s -> view_size v ++ s) views zero in
    (* The size of the storage_type and the arg_type is counted by
       [lambda_size]. *)
    let ir_size = lambda_size code in
    let (nodes, code_size) = views_size ++ ir_size in
    (* We consume gas after the fact in order to not have to instrument
       [node_size] (for efficiency).
       This is safe, as we already pay gas proportional to [views_size]
       and [ir_size] during their typechecking. *)
    Gas.consume ctxt (Script_typed_ir_size_costs.nodes_cost ~nodes)
    >>? fun ctxt ->
    ok
      (Ex_code {code; arg_type; storage_type; views; root_name; code_size}, ctxt))

let parse_storage :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    allow_forged:bool ->
    'storage ty ->
    storage:lazy_expr ->
    ('storage * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~allow_forged storage_type ~storage ->
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
       ?type_logger
       ~stack_depth:0
       ctxt
       ~legacy
       ~allow_forged
       storage_type
       (root storage))

let[@coq_axiom_with_reason "gadt"] parse_script :
    ?type_logger:type_logger ->
    context ->
    legacy:bool ->
    allow_forged_in_storage:bool ->
    Script.t ->
    (ex_script * context) tzresult Lwt.t =
 fun ?type_logger ctxt ~legacy ~allow_forged_in_storage {code; storage} ->
  parse_code ~legacy ctxt ?type_logger ~code
  >>=? fun ( Ex_code {code; arg_type; storage_type; views; root_name; code_size},
             ctxt ) ->
  parse_storage
    ?type_logger
    ctxt
    ~legacy
    ~allow_forged:allow_forged_in_storage
    storage_type
    ~storage
  >|=? fun (storage, ctxt) ->
  ( Ex_script
      {code_size; code; arg_type; storage; storage_type; views; root_name},
    ctxt )

let typecheck_code :
    legacy:bool ->
    show_types:bool ->
    context ->
    Script.expr ->
    (type_map * context) tzresult Lwt.t =
 fun ~legacy ~show_types ctxt code ->
  (* Constants need to be expanded or [parse_toplevel] may fail. *)
  Global_constants_storage.expand ctxt code >>=? fun (ctxt, code) ->
  parse_toplevel ctxt ~legacy code
  >>?= fun ({arg_type; storage_type; code_field; views; root_name}, ctxt) ->
  let type_map = ref [] in
  let arg_type_loc = location arg_type in
  record_trace
    (Ill_formed_type (Some "parameter", code, arg_type_loc))
    (parse_parameter_ty_and_entrypoints
       ctxt
       ~stack_depth:0
       ~legacy
       ~root_name
       arg_type)
  >>?= fun (Ex_parameter_ty_and_entrypoints {arg_type; root_name}, ctxt) ->
  let storage_type_loc = location storage_type in
  record_trace
    (Ill_formed_type (Some "storage", code, storage_type_loc))
    (parse_storage_ty ctxt ~stack_depth:0 ~legacy storage_type)
  >>?= fun (Ex_ty storage_type, ctxt) ->
  pair_t storage_type_loc arg_type storage_type >>?= fun arg_type_full ->
  pair_t storage_type_loc list_operation_t storage_type
  >>?= fun ret_type_full ->
  let type_logger loc bef aft = type_map := (loc, (bef, aft)) :: !type_map in
  let type_logger = if show_types then Some type_logger else None in
  let result =
    parse_returning
      (Tc_context.toplevel ~storage_type ~param_type:arg_type root_name)
      ctxt
      ~legacy
      ~stack_depth:0
      ?type_logger
      arg_type_full
      ret_type_full
      code_field
  in
  trace (Ill_typed_contract (code, !type_map)) result >>=? fun (Lam _, ctxt) ->
  let views_result =
    typecheck_views
      ctxt
      ~type_logger:(fun loc bef aft ->
        type_map := (loc, (bef, aft)) :: !type_map)
      ~legacy
      storage_type
      views
  in
  trace (Ill_typed_contract (code, !type_map)) views_result >|=? fun ctxt ->
  (!type_map, ctxt)

let list_entrypoints (type full) (full : full ty) ctxt ~root_name =
  let merge path annot (type t) (ty : t ty) reachable
      ((unreachables, all) as acc) =
    match annot with
    | None ->
        ok
          ( (if reachable then acc
            else
              match ty with
              | Union_t _ -> acc
              | _ -> (List.rev path :: unreachables, all)),
            reachable )
    | Some (Field_annot name) ->
        (match Entrypoint.of_annot_lax_opt name with
        | None -> ok (List.rev path :: unreachables, all)
        | Some name ->
            if Entrypoint.Map.mem name all then
              ok (List.rev path :: unreachables, all)
            else
              unparse_ty ~loc:() ctxt ty >|? fun (unparsed_ty, _) ->
              ( unreachables,
                Entrypoint.Map.add name (List.rev path, unparsed_ty) all ))
        >|? fun unreachable_all -> (unreachable_all, true)
  in
  let rec fold_tree :
      type t.
      t ty ->
      prim list ->
      bool ->
      prim list list
      * (prim list * Script.unlocated_michelson_node) Entrypoint.Map.t ->
      (prim list list
      * (prim list * Script.unlocated_michelson_node) Entrypoint.Map.t)
      tzresult =
   fun t path reachable acc ->
    match t with
    | Union_t ((tl, al), (tr, ar), _) ->
        merge (D_Left :: path) al tl reachable acc >>? fun (acc, l_reachable) ->
        merge (D_Right :: path) ar tr reachable acc
        >>? fun (acc, r_reachable) ->
        fold_tree tl (D_Left :: path) l_reachable acc >>? fun acc ->
        fold_tree tr (D_Right :: path) r_reachable acc
    | _ -> ok acc
  in
  unparse_ty ~loc:() ctxt full >>? fun (unparsed_full, _) ->
  let (init, reachable) =
    match root_name with
    | None -> (Entrypoint.Map.empty, false)
    | Some name -> (Entrypoint.Map.singleton name ([], unparsed_full), true)
  in
  fold_tree full [] reachable ([], init)
  [@@coq_axiom_with_reason "unsupported syntax"]

(* ---- Unparsing (Typed IR -> Untyped expressions) --------------------------*)

(* -- Unparsing data of any type -- *)

let comb_witness2 : type t. t ty -> (t, unit -> unit -> unit) comb_witness =
  function
  | Pair_t (_, Pair_t _, _) -> Comb_Pair (Comb_Pair Comb_Any)
  | Pair_t _ -> Comb_Pair Comb_Any
  | _ -> Comb_Any

let[@coq_axiom_with_reason "gadt"] rec unparse_data :
    type a.
    context ->
    stack_depth:int ->
    unparsing_mode ->
    a ty ->
    a ->
    (Script.node * context) tzresult Lwt.t =
 fun ctxt ~stack_depth mode ty a ->
  Gas.consume ctxt Unparse_costs.unparse_data_cycle >>?= fun ctxt ->
  let non_terminal_recursion ctxt mode ty a =
    if Compare.Int.(stack_depth > 10_000) then
      fail Unparsing_too_many_recursive_calls
    else unparse_data ctxt ~stack_depth:(stack_depth + 1) mode ty a
  in
  let loc = Micheline.dummy_location in
  match (ty, a) with
  | (Unit_t _, v) -> Lwt.return @@ unparse_unit ~loc ctxt v
  | (Int_t _, v) -> Lwt.return @@ unparse_int ~loc ctxt v
  | (Nat_t _, v) -> Lwt.return @@ unparse_nat ~loc ctxt v
  | (String_t _, s) -> Lwt.return @@ unparse_string ~loc ctxt s
  | (Bytes_t _, s) -> Lwt.return @@ unparse_bytes ~loc ctxt s
  | (Bool_t _, b) -> Lwt.return @@ unparse_bool ~loc ctxt b
  | (Timestamp_t _, t) -> Lwt.return @@ unparse_timestamp ~loc ctxt mode t
  | (Address_t _, address) ->
      Lwt.return @@ unparse_address ~loc ctxt mode address
  | (Contract_t _, contract) ->
      Lwt.return @@ unparse_contract ~loc ctxt mode contract
  | (Signature_t _, s) -> Lwt.return @@ unparse_signature ~loc ctxt mode s
  | (Mutez_t _, v) -> Lwt.return @@ unparse_mutez ~loc ctxt v
  | (Key_t _, k) -> Lwt.return @@ unparse_key ~loc ctxt mode k
  | (Key_hash_t _, k) -> Lwt.return @@ unparse_key_hash ~loc ctxt mode k
  | (Operation_t _, operation) ->
      Lwt.return @@ unparse_operation ~loc ctxt operation
  | (Chain_id_t _, chain_id) ->
      Lwt.return @@ unparse_chain_id ~loc ctxt mode chain_id
  | (Bls12_381_g1_t _, x) -> Lwt.return @@ unparse_bls12_381_g1 ~loc ctxt x
  | (Bls12_381_g2_t _, x) -> Lwt.return @@ unparse_bls12_381_g2 ~loc ctxt x
  | (Bls12_381_fr_t _, x) -> Lwt.return @@ unparse_bls12_381_fr ~loc ctxt x
  | (Pair_t (tl, tr, _), pair) ->
      let r_witness = comb_witness2 tr in
      let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
      let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
      unparse_pair ~loc unparse_l unparse_r ctxt mode r_witness pair
  | (Union_t ((tl, _), (tr, _), _), v) ->
      let unparse_l ctxt v = non_terminal_recursion ctxt mode tl v in
      let unparse_r ctxt v = non_terminal_recursion ctxt mode tr v in
      unparse_union ~loc unparse_l unparse_r ctxt v
  | (Option_t (t, _), v) ->
      let unparse_v ctxt v = non_terminal_recursion ctxt mode t v in
      unparse_option ~loc unparse_v ctxt v
  | (List_t (t, _), items) ->
      List.fold_left_es
        (fun (l, ctxt) element ->
          non_terminal_recursion ctxt mode t element
          >|=? fun (unparsed, ctxt) -> (unparsed :: l, ctxt))
        ([], ctxt)
        items.elements
      >|=? fun (items, ctxt) -> (Micheline.Seq (loc, List.rev items), ctxt)
  | (Ticket_t (t, _), {ticketer; contents; amount}) ->
      (* ideally we would like to allow a little overhead here because it is only used for unparsing *)
      opened_ticket_type loc t >>?= fun opened_ticket_ty ->
      let t = ty_of_comparable_ty opened_ticket_ty in
      let destination : Destination.t = Contract ticketer in
      let addr = {destination; entrypoint = Entrypoint.default} in
      (unparse_data [@tailcall])
        ctxt
        ~stack_depth
        mode
        t
        (addr, (contents, amount))
  | (Set_t (t, _), set) ->
      List.fold_left_es
        (fun (l, ctxt) item ->
          unparse_comparable_data ~loc ctxt mode t item >|=? fun (item, ctxt) ->
          (item :: l, ctxt))
        ([], ctxt)
        (Script_set.fold (fun e acc -> e :: acc) set [])
      >|=? fun (items, ctxt) -> (Micheline.Seq (loc, items), ctxt)
  | (Map_t (kt, vt, _), map) ->
      let items = Script_map.fold (fun k v acc -> (k, v) :: acc) map [] in
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) -> (Micheline.Seq (loc, items), ctxt)
  | (Big_map_t (_kt, _vt, _), {id = Some id; diff = {size; _}; _})
    when Compare.Int.( = ) size 0 ->
      return (Micheline.Int (loc, Big_map.Id.unparse_to_z id), ctxt)
  | (Big_map_t (kt, vt, _), {id = Some id; diff = {map; _}; _}) ->
      let items =
        Big_map_overlay.fold (fun _ (k, v) acc -> (k, v) :: acc) map []
      in
      let items =
        (* Sort the items in Michelson comparison order and not in key
           hash order. This code path is only exercised for tracing,
           so we don't bother carbonating this sort operation
           precisely. Also, the sort uses a reverse compare because
           [unparse_items] will reverse the result. *)
        List.sort
          (fun (a, _) (b, _) -> Script_comparable.compare_comparable kt b a)
          items
      in
      (* this can't fail if the original type is well-formed
         because [option vt] is always strictly smaller than [big_map kt vt] *)
      option_t loc vt >>?= fun vt ->
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) ->
      ( Micheline.Prim
          ( loc,
            D_Pair,
            [Int (loc, Big_map.Id.unparse_to_z id); Seq (loc, items)],
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
        List.sort
          (fun (a, _) (b, _) -> Script_comparable.compare_comparable kt b a)
          items
      in
      unparse_items ctxt ~stack_depth:(stack_depth + 1) mode kt vt items
      >|=? fun (items, ctxt) -> (Micheline.Seq (loc, items), ctxt)
  | (Lambda_t _, Lam (_, original_code)) ->
      unparse_code ctxt ~stack_depth:(stack_depth + 1) mode original_code
  | (Never_t _, _) -> .
  | (Sapling_transaction_t _, s) ->
      Lwt.return
        ( Gas.consume ctxt (Unparse_costs.sapling_transaction s) >|? fun ctxt ->
          let bytes =
            Data_encoding.Binary.to_bytes_exn Sapling.transaction_encoding s
          in
          (Bytes (loc, bytes), ctxt) )
  | (Sapling_state_t _, {id; diff; _}) ->
      Lwt.return
        ( Gas.consume ctxt (Unparse_costs.sapling_diff diff) >|? fun ctxt ->
          ( (match diff with
            | {commitments_and_ciphertexts = []; nullifiers = []} -> (
                match id with
                | None -> Micheline.Seq (loc, [])
                | Some id ->
                    let id = Sapling.Id.unparse_to_z id in
                    Micheline.Int (loc, id))
            | diff -> (
                let diff_bytes =
                  Data_encoding.Binary.to_bytes_exn Sapling.diff_encoding diff
                in
                let unparsed_diff = Bytes (loc, diff_bytes) in
                match id with
                | None -> unparsed_diff
                | Some id ->
                    let id = Sapling.Id.unparse_to_z id in
                    Micheline.Prim
                      (loc, D_Pair, [Int (loc, id); unparsed_diff], []))),
            ctxt ) )
  | (Chest_key_t _, s) ->
      unparse_with_data_encoding
        ~loc
        ctxt
        s
        Unparse_costs.chest_key
        Script_timelock.chest_key_encoding
  | (Chest_t _, s) ->
      unparse_with_data_encoding
        ~loc
        ctxt
        s
        (Unparse_costs.chest
           ~plaintext_size:(Script_timelock.get_plaintext_size s))
        Script_timelock.chest_encoding

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
  List.fold_left_es
    (fun (l, ctxt) (k, v) ->
      let loc = Micheline.dummy_location in
      unparse_comparable_data ~loc ctxt mode kt k >>=? fun (key, ctxt) ->
      unparse_data ctxt ~stack_depth:(stack_depth + 1) mode vt v
      >|=? fun (value, ctxt) -> (Prim (loc, D_Elt, [key; value], []) :: l, ctxt))
    ([], ctxt)
    items

and[@coq_axiom_with_reason "gadt"] unparse_code ctxt ~stack_depth mode code =
  let legacy = true in
  Gas.consume ctxt Unparse_costs.unparse_instr_cycle >>?= fun ctxt ->
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
      List.fold_left_es
        (fun (l, ctxt) item ->
          non_terminal_recursion ctxt mode item >|=? fun (item, ctxt) ->
          (item :: l, ctxt))
        ([], ctxt)
        items
      >>=? fun (items, ctxt) ->
      return (Micheline.Seq (loc, List.rev items), ctxt)
  | Prim (loc, prim, items, annot) ->
      List.fold_left_es
        (fun (l, ctxt) item ->
          non_terminal_recursion ctxt mode item >|=? fun (item, ctxt) ->
          (item :: l, ctxt))
        ([], ctxt)
        items
      >>=? fun (items, ctxt) ->
      return (Prim (loc, prim, List.rev items, annot), ctxt)
  | (Int _ | String _ | Bytes _) as atom -> return (atom, ctxt)

(* Gas accounting may not be perfect in this function, as it is only called by RPCs. *)
(* TODO: https://gitlab.com/tezos/tezos/-/issues/1688
   Refactor the sharing part of unparse_script and create_contract *)
let unparse_script ctxt mode
    {code; arg_type; storage; storage_type; root_name; views; _} =
  let (Lam (_, original_code)) = code in
  unparse_code ctxt ~stack_depth:0 mode original_code >>=? fun (code, ctxt) ->
  unparse_data ctxt ~stack_depth:0 mode storage_type storage
  >>=? fun (storage, ctxt) ->
  Lwt.return
    (let loc = Micheline.dummy_location in
     unparse_parameter_ty ~loc ctxt arg_type ~root_name
     >>? fun (arg_type, ctxt) ->
     unparse_ty ~loc ctxt storage_type >>? fun (storage_type, ctxt) ->
     let open Micheline in
     let view name {input_ty; output_ty; view_code} views =
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
     let views = SMap.fold view views [] |> List.rev in
     let code =
       Seq
         ( loc,
           [
             Prim (loc, K_parameter, [arg_type], []);
             Prim (loc, K_storage, [storage_type], []);
             Prim (loc, K_code, [code], []);
           ]
           @ views )
     in
     Gas.consume ctxt Unparse_costs.unparse_instr_cycle >>? fun ctxt ->
     Gas.consume ctxt Unparse_costs.unparse_instr_cycle >>? fun ctxt ->
     Gas.consume ctxt Unparse_costs.unparse_instr_cycle >>? fun ctxt ->
     Gas.consume ctxt Unparse_costs.unparse_instr_cycle >>? fun ctxt ->
     Gas.consume ctxt (Script.strip_locations_cost code) >>? fun ctxt ->
     Gas.consume ctxt (Script.strip_locations_cost storage) >|? fun ctxt ->
     ( {
         code = lazy_expr (strip_locations code);
         storage = lazy_expr (strip_locations storage);
       },
       ctxt ))

let pack_data_with_mode ctxt typ data ~mode =
  unparse_data ~stack_depth:0 ctxt mode typ data >>=? fun (unparsed, ctxt) ->
  Lwt.return @@ pack_node unparsed ctxt

let hash_data ctxt typ data =
  pack_data_with_mode ctxt typ data ~mode:Optimized_legacy
  >>=? fun (bytes, ctxt) -> Lwt.return @@ hash_bytes ctxt bytes

let pack_data ctxt typ data =
  pack_data_with_mode ctxt typ data ~mode:Optimized_legacy

(* ---------------- Big map -------------------------------------------------*)

let empty_big_map key_type value_type =
  {
    id = None;
    diff = {map = Big_map_overlay.empty; size = 0};
    key_type;
    value_type;
  }

let big_map_mem ctxt key {id; diff; key_type; _} =
  hash_comparable_data ctxt key_type key >>=? fun (key, ctxt) ->
  match (Big_map_overlay.find key diff.map, id) with
  | (None, None) -> return (false, ctxt)
  | (None, Some id) ->
      Alpha_context.Big_map.mem ctxt id key >|=? fun (ctxt, res) -> (res, ctxt)
  | (Some (_, None), _) -> return (false, ctxt)
  | (Some (_, Some _), _) -> return (true, ctxt)

let big_map_get_by_hash ctxt key {id; diff; value_type; _} =
  match (Big_map_overlay.find key diff.map, id) with
  | (Some (_, x), _) -> return (x, ctxt)
  | (None, None) -> return (None, ctxt)
  | (None, Some id) -> (
      Alpha_context.Big_map.get_opt ctxt id key >>=? function
      | (ctxt, None) -> return (None, ctxt)
      | (ctxt, Some value) ->
          parse_data
            ~stack_depth:0
            ctxt
            ~legacy:true
            ~allow_forged:true
            value_type
            (Micheline.root value)
          >|=? fun (x, ctxt) -> (Some x, ctxt))

let big_map_get ctxt key map =
  hash_comparable_data ctxt map.key_type key >>=? fun (key_hash, ctxt) ->
  big_map_get_by_hash ctxt key_hash map

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
  hash_comparable_data ctxt map.key_type key >>=? fun (key_hash, ctxt) ->
  big_map_update_by_hash ctxt key_hash key value map

let big_map_get_and_update ctxt key value map =
  hash_comparable_data ctxt map.key_type key >>=? fun (key_hash, ctxt) ->
  big_map_update_by_hash ctxt key_hash key value map >>=? fun (map', ctxt) ->
  big_map_get_by_hash ctxt key_hash map >>=? fun (old_value, ctxt) ->
  return ((old_value, map'), ctxt)

(* ---------------- Lazy storage---------------------------------------------*)

type lazy_storage_ids = Lazy_storage.IdSet.t

let no_lazy_storage_id = Lazy_storage.IdSet.empty

let diff_of_big_map ctxt mode ~temporary ~ids_to_copy
    {id; key_type; value_type; diff} =
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
      unparse_comparable_data ~loc:() ctxt mode key_type key
      >>=? fun (key_node, ctxt) ->
      Gas.consume ctxt (Script.strip_locations_cost key_node) >>?= fun ctxt ->
      let key = Micheline.strip_locations key_node in
      (match value with
      | None -> return (None, ctxt)
      | Some x ->
          unparse_data ~stack_depth:0 ctxt mode value_type x
          >>=? fun (node, ctxt) ->
          Lwt.return
            ( Gas.consume ctxt (Script.strip_locations_cost node) >|? fun ctxt ->
              (Some (Micheline.strip_locations node), ctxt) ))
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
 fun ty ->
  let aux1 cons t =
    match has_lazy_storage t with False_f -> False_f | h -> cons h
  in
  let aux2 cons t1 t2 =
    match (has_lazy_storage t1, has_lazy_storage t2) with
    | (False_f, False_f) -> False_f
    | (h1, h2) -> cons h1 h2
  in
  match ty with
  | Big_map_t (_, _, _) -> Big_map_f
  | Sapling_state_t _ -> Sapling_state_f
  | Unit_t _ -> False_f
  | Int_t _ -> False_f
  | Nat_t _ -> False_f
  | Signature_t _ -> False_f
  | String_t _ -> False_f
  | Bytes_t _ -> False_f
  | Mutez_t _ -> False_f
  | Key_hash_t _ -> False_f
  | Key_t _ -> False_f
  | Timestamp_t _ -> False_f
  | Address_t _ -> False_f
  | Bool_t _ -> False_f
  | Lambda_t (_, _, _) -> False_f
  | Set_t (_, _) -> False_f
  | Contract_t (_, _) -> False_f
  | Operation_t _ -> False_f
  | Chain_id_t _ -> False_f
  | Never_t _ -> False_f
  | Bls12_381_g1_t _ -> False_f
  | Bls12_381_g2_t _ -> False_f
  | Bls12_381_fr_t _ -> False_f
  | Sapling_transaction_t _ -> False_f
  | Ticket_t _ -> False_f
  | Chest_key_t _ -> False_f
  | Chest_t _ -> False_f
  | Pair_t (l, r, _) -> aux2 (fun l r -> Pair_f (l, r)) l r
  | Union_t ((l, _), (r, _), _) -> aux2 (fun l r -> Union_f (l, r)) l r
  | Option_t (t, _) -> aux1 (fun h -> Option_f h) t
  | List_t (t, _) -> aux1 (fun h -> List_f h) t
  | Map_t (_, t, _) -> aux1 (fun h -> Map_f h) t

(**
  Transforms a value potentially containing lazy storage in an intermediary
  state to a value containing lazy storage only represented by identifiers.

  Returns the updated value, the updated set of ids to copy, and the lazy
  storage diff to show on the receipt and apply on the storage.

*)
let[@coq_axiom_with_reason "gadt"] extract_lazy_storage_updates ctxt mode
    ~temporary ids_to_copy acc ty x =
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
      (context * a * Lazy_storage.IdSet.t * Lazy_storage.diffs) tzresult Lwt.t =
   fun ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage ->
    Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>?= fun ctxt ->
    match (has_lazy_storage, ty, x) with
    | (False_f, _, _) -> return (ctxt, x, ids_to_copy, acc)
    | (Big_map_f, Big_map_t (_, _, _), map) ->
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
    | (Sapling_state_f, Sapling_state_t _, sapling_state) ->
        diff_of_sapling_state ctxt ~temporary ~ids_to_copy sapling_state
        >|=? fun (diff, id, ctxt) ->
        let sapling_state =
          Sapling.empty_state ~id ~memo_size:sapling_state.memo_size ()
        in
        let diff = Lazy_storage.make Sapling_state id diff in
        let ids_to_copy = Lazy_storage.IdSet.add Sapling_state id ids_to_copy in
        (ctxt, sapling_state, ids_to_copy, diff :: acc)
    | (Pair_f (hl, hr), Pair_t (tyl, tyr, _), (xl, xr)) ->
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
        List.fold_left_es
          (fun (ctxt, l, ids_to_copy, acc) x ->
            aux ctxt mode ~temporary ids_to_copy acc ty x ~has_lazy_storage
            >|=? fun (ctxt, x, ids_to_copy, acc) ->
            (ctxt, Script_list.cons x l, ids_to_copy, acc))
          (ctxt, Script_list.empty, ids_to_copy, acc)
          l.elements
        >|=? fun (ctxt, l, ids_to_copy, acc) ->
        let reversed = {length = l.length; elements = List.rev l.elements} in
        (ctxt, reversed, ids_to_copy, acc)
    | (Map_f has_lazy_storage, Map_t (_, ty, _), map) ->
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

          let key_ty = M.key_ty

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
    | (_, Option_t (_, _), None) -> return (ctxt, None, ids_to_copy, acc)
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
let[@coq_axiom_with_reason "gadt"] rec fold_lazy_storage :
    type a error.
    f:('acc, error) Fold_lazy_storage.result Lazy_storage.IdSet.fold_f ->
    init:'acc ->
    context ->
    a ty ->
    a ->
    has_lazy_storage:a has_lazy_storage ->
    (('acc, error) Fold_lazy_storage.result * context) tzresult =
 fun ~f ~init ctxt ty x ~has_lazy_storage ->
  Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
  match (has_lazy_storage, ty, x) with
  | (Big_map_f, Big_map_t (_, _, _), {id = Some id; _}) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
      ok (f.f Big_map id (Fold_lazy_storage.Ok init), ctxt)
  | (Sapling_state_f, Sapling_state_t _, {id = Some id; _}) ->
      Gas.consume ctxt Typecheck_costs.parse_instr_cycle >>? fun ctxt ->
      ok (f.f Sapling_state id (Fold_lazy_storage.Ok init), ctxt)
  | (False_f, _, _) -> ok (Fold_lazy_storage.Ok init, ctxt)
  | (Big_map_f, Big_map_t (_, _, _), {id = None; _}) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (Sapling_state_f, Sapling_state_t _, {id = None; _}) ->
      ok (Fold_lazy_storage.Ok init, ctxt)
  | (Pair_f (hl, hr), Pair_t (tyl, tyr, _), (xl, xr)) -> (
      fold_lazy_storage ~f ~init ctxt tyl xl ~has_lazy_storage:hl
      >>? fun (init, ctxt) ->
      match init with
      | Fold_lazy_storage.Ok init ->
          fold_lazy_storage ~f ~init ctxt tyr xr ~has_lazy_storage:hr
      | Fold_lazy_storage.Error -> ok (init, ctxt))
  | (Union_f (has_lazy_storage, _), Union_t ((ty, _), (_, _), _), L x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (Union_f (_, has_lazy_storage), Union_t ((_, _), (ty, _), _), R x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (_, Option_t (_, _), None) -> ok (Fold_lazy_storage.Ok init, ctxt)
  | (Option_f has_lazy_storage, Option_t (ty, _), Some x) ->
      fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
  | (List_f has_lazy_storage, List_t (ty, _), l) ->
      List.fold_left_e
        (fun ((init, ctxt) : ('acc, error) Fold_lazy_storage.result * context) x ->
          match init with
          | Fold_lazy_storage.Ok init ->
              fold_lazy_storage ~f ~init ctxt ty x ~has_lazy_storage
          | Fold_lazy_storage.Error -> ok (init, ctxt))
        (Fold_lazy_storage.Ok init, ctxt)
        l.elements
  | (Map_f has_lazy_storage, Map_t (_, ty, _), m) ->
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

let[@coq_axiom_with_reason "gadt"] collect_lazy_storage ctxt ty x =
  let has_lazy_storage = has_lazy_storage ty in
  let f kind id (acc : (_, never) Fold_lazy_storage.result) =
    let acc = match acc with Fold_lazy_storage.Ok acc -> acc in
    Fold_lazy_storage.Ok (Lazy_storage.IdSet.add kind id acc)
  in
  fold_lazy_storage ~f:{f} ~init:no_lazy_storage_id ctxt ty x ~has_lazy_storage
  >>? fun (ids, ctxt) ->
  match ids with Fold_lazy_storage.Ok ids -> ok (ids, ctxt)

let[@coq_axiom_with_reason "gadt"] extract_lazy_storage_diff ctxt mode
    ~temporary ~to_duplicate ~to_update ty v =
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

let unparse_code ctxt mode code =
  (* Constants need to be expanded or [unparse_code] may fail. *)
  Global_constants_storage.expand ctxt (strip_locations code)
  >>=? fun (ctxt, code) -> unparse_code ~stack_depth:0 ctxt mode (root code)

let parse_contract ~legacy context loc arg_ty contract ~entrypoint =
  parse_contract ~stack_depth:0 ~legacy context loc arg_ty contract ~entrypoint

let parse_toplevel ctxt ~legacy toplevel =
  Global_constants_storage.expand ctxt toplevel >>=? fun (ctxt, toplevel) ->
  Lwt.return @@ parse_toplevel ctxt ~legacy toplevel

let parse_comparable_ty = parse_comparable_ty ~stack_depth:0

let parse_big_map_value_ty = parse_big_map_value_ty ~stack_depth:0

let parse_packable_ty = parse_packable_ty ~stack_depth:0

let parse_passable_ty = parse_passable_ty ~stack_depth:0

let parse_any_ty = parse_any_ty ~stack_depth:0

let parse_ty = parse_ty ~stack_depth:0

let parse_parameter_ty_and_entrypoints =
  parse_parameter_ty_and_entrypoints ~stack_depth:0

let ty_eq ctxt = ty_eq ~legacy:true ctxt

let[@coq_axiom_with_reason "gadt"] get_single_sapling_state ctxt ty x =
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
      {
        code_size;
        code = _;
        arg_type = _;
        storage;
        storage_type;
        root_name = _;
        views = _;
      }) =
  let (nodes, storage_size) =
    Script_typed_ir_size.value_size storage_type storage
  in
  let cost = Script_typed_ir_size_costs.nodes_cost ~nodes in
  (Saturation_repr.(add code_size storage_size |> to_int), cost)
