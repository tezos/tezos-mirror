(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Script_typed_ir
include Cache_memory_helpers

let script_string_size s = Script_string.to_string s |> string_size

(* The model assumes that annotations' sizes are counted once in the
   Micheline representation and that the strings are always
   shared. (One can check that they are never copied.) Besides, the
   following types are unboxed so that they have no tags. *)
let type_annot_size (Type_annot _) = zero

let field_annot_size (Field_annot _) = zero

let var_annot_size (Var_annot _) = zero

(* Memo-sizes are 16-bit integers *)
let sapling_memo_size_size = zero

let metadata_size {annot; size = _} =
  (word_size *? 2) +! header_size +! option_size type_annot_size annot

let (comparable_ty_size, ty_size) =
  let base metadata = header_size +! word_size +! metadata_size metadata in
  let apply_comparable : type a. sint -> a comparable_ty -> sint =
   fun accu cty ->
    let self_size =
      match cty with
      | Unit_key a -> base a
      | Int_key a -> base a
      | Nat_key a -> base a
      | Signature_key a -> base a
      | String_key a -> base a
      | Bytes_key a -> base a
      | Mutez_key a -> base a
      | Key_hash_key a -> base a
      | Key_key a -> base a
      | Timestamp_key a -> base a
      | Address_key a -> base a
      | Bool_key a -> base a
      | Chain_id_key a -> base a
      | Never_key a -> base a
      | Pair_key ((_ty1, fa1), (_ty2, fa2), a) ->
          base a
          +! ((word_size *? 6) +! (header_size *? 2))
          +! option_size field_annot_size fa1
          +! option_size field_annot_size fa2
      | Union_key ((_ty1, fa1), (_ty2, fa2), a) ->
          base a
          +! ((word_size *? 6) +! (header_size *? 2))
          +! option_size field_annot_size fa1
          +! option_size field_annot_size fa2
      | Option_key (_ty, a) -> base a +! word_size
    in
    accu +! self_size
  and apply : type a. sint -> a ty -> sint =
   fun accu ty ->
    let self_size =
      match ty with
      | Unit_t a -> base a
      | Int_t a -> base a
      | Nat_t a -> base a
      | Signature_t a -> base a
      | String_t a -> base a
      | Bytes_t a -> base a
      | Mutez_t a -> base a
      | Key_hash_t a -> base a
      | Key_t a -> base a
      | Timestamp_t a -> base a
      | Address_t a -> base a
      | Bool_t a -> base a
      | Operation_t a -> base a
      | Chain_id_t a -> base a
      | Never_t a -> base a
      | Bls12_381_g1_t a -> base a
      | Bls12_381_g2_t a -> base a
      | Bls12_381_fr_t a -> base a
      | Chest_key_t a -> base a
      | Chest_t a -> base a
      | Pair_t ((_ty1, fa1, va1), (_ty2, fa2, va2), a) ->
          base a
          +! ((word_size *? 8) +! (header_size *? 2))
          +! option_size field_annot_size fa1
          +! option_size var_annot_size va1
          +! option_size field_annot_size fa2
          +! option_size var_annot_size va2
      | Union_t ((_ty1, fa1), (_ty2, fa2), a) ->
          base a
          +! ((word_size *? 6) +! (header_size *? 2))
          +! option_size field_annot_size fa1
          +! option_size field_annot_size fa2
      | Lambda_t (_ty1, _ty2, a) -> base a +! (word_size *? 2)
      | Option_t (_ty, a) -> base a +! word_size
      | List_t (_ty, a) -> base a +! word_size
      | Set_t (_cty, a) -> base a +! word_size
      | Map_t (_cty, _ty, a) -> base a +! (word_size *? 2)
      | Big_map_t (_cty, _ty, a) -> base a +! (word_size *? 2)
      | Contract_t (_ty, a) -> base a +! word_size
      | Sapling_transaction_t (_m, a) ->
          base a +! sapling_memo_size_size +! word_size
      | Sapling_state_t (_m, a) -> base a +! sapling_memo_size_size +! word_size
      | Ticket_t (_cty, a) -> base a +! word_size
    in
    accu +! self_size
  in
  let f = ({apply; apply_comparable} : sint ty_traverse) in
  ( (fun cty -> comparable_ty_traverse cty zero f),
    fun ty -> ty_traverse ty zero f )

let stack_ty_size s =
  let apply : type a s. sint -> (a, s) stack_ty -> sint =
   fun accu s ->
    let self_size =
      match s with
      | Bot_t -> zero
      | Item_t (ty, _, annot) ->
          ty_size ty +! header_size +! (word_size *? 3)
          +! option_size var_annot_size annot
    in
    accu +! self_size
  in
  stack_ty_traverse s zero {apply}

let script_nat_size n = Script_int.to_zint n |> z_size

let script_int_size n = Script_int.to_zint n |> z_size

let signature_size = header_size +! (word_size *? 3) +? Signature.size

let key_hash_size (x : Signature.public_key_hash) =
  header_size +! word_size
  +? Signature.(
       match x with
       | Ed25519 _ -> Ed25519.Public_key_hash.size
       | Secp256k1 _ -> Secp256k1.Public_key_hash.size
       | P256 _ -> P256.Public_key_hash.size)

let public_key_size (x : public_key) =
  let ks = Signature.Public_key.size x in
  header_size +! word_size +? ks

let mutez_size = header_size +! (word_size *? 2)

let timestamp_size x = Script_timestamp.to_zint x |> z_size

let contract_size = Contract.in_memory_size

let address_size ((c, s) : address) =
  header_size +! (word_size *? 2) +! contract_size c +! string_size s

let view_signature_size (View_signature {name; input_ty; output_ty}) =
  header_size +! (word_size *? 3) +! script_string_size name +! ty_size input_ty
  +! ty_size output_ty

let script_expr_hash_size = Script_expr_hash.size

let peano_shape_proof k = (header_size +! (header_size +! word_size)) *? k

let stack_prefix_preservation_witness_size k =
  let kinfo_size = header_size +! (word_size *? 2) in
  (header_size +! (header_size +! (word_size *? 2) +! kinfo_size)) *? k

let comb_gadt_witness_size = peano_shape_proof

let uncomb_gadt_witness_size = peano_shape_proof

let comb_get_gadt_witness_size = peano_shape_proof

let comb_set_gadt_witness_size = peano_shape_proof

let dup_n_gadt_witness_size = peano_shape_proof

let contract_size (arg_ty, address) =
  header_size +! (word_size *? 2) +! ty_size arg_ty +! address_size address

let sapling_state_size {Sapling.id; diff; memo_size = _} =
  header_size +! (word_size *? 3)
  +! option_size (fun x -> z_size (Sapling.Id.unparse_to_z x)) id
  +! Sapling.diff_in_memory_size diff
  +! sapling_memo_size_size

let operation_size
    (operation :
      packed_internal_operation * Lazy_storage.diffs_item list option) =
  let (poi, diffs) = operation in
  header_size +! (word_size *? 2)
  +! Operation.packed_internal_operation_in_memory_size poi
  +! option_size Lazy_storage.diffs_in_memory_size diffs

let chain_id_size = header_size +! word_size +? Chain_id.size

(* [contents] is handle by the recursion scheme in [value_size] *)
let ticket_size {ticketer; contents = _; amount} =
  header_size +! (word_size *? 3) +! address_size ticketer
  +! script_nat_size amount

let chest_size chest =
  (*
     type chest = {
       locked_value : locked_value;
       rsa_public : rsa_public;
       ciphertext : ciphertext;
     }
  *)
  let locked_value_size = 256 in
  let rsa_public_size = 256 in
  let ciphertext_size = Timelock.get_plaintext_size chest in
  header_size +! (word_size *? 3) +? locked_value_size +? rsa_public_size
  +? ciphertext_size

let chest_key_size _ =
  (*
     type chest_key = {
       unlocked_value : unlocked_value;
       proof : time_lock_proof
     }
  *)
  let unlocked_value_size = 256 in
  let proof_size = 256 in
  header_size +! (word_size *? 2) +? unlocked_value_size +? proof_size

let view_size {input_ty; output_ty; view_code} =
  header_size +! (word_size *? 3) +! node_size input_ty +! node_size output_ty
  +! node_size view_code

let views_size views =
  SMap.fold
    (fun k view accu ->
      accu +! script_string_size k +! view_size view
      +! (header_size +! (word_size *? 4)))
    views
    zero

let kinfo_size {iloc = _; kstack_ty = _} = header_size +! (word_size *? 2)

(* The following mutually recursive functions are mostly
   tail-recursive and the only recursive call that is not a tailcall
   cannot be nested. (See [big_map_size].) For this reason, these
   functions should not trigger stack overflows. *)
let rec value_size :
    type a.
    count_lambda_nodes:bool ->
    sint ->
    (a ty, a comparable_ty) union ->
    a ->
    sint =
 fun ~count_lambda_nodes accu ty x ->
  let apply : type a. sint -> a ty -> a -> sint =
   fun accu ty x ->
    match ty with
    | Unit_t _ -> accu
    | Int_t _ -> accu +! script_int_size x
    | Nat_t _ -> accu +! script_nat_size x
    | Signature_t _ -> accu +! signature_size
    | String_t _ -> accu +! script_string_size x
    | Bytes_t _ -> accu +! bytes_size x
    | Mutez_t _ -> accu +! mutez_size
    | Key_hash_t _ -> accu +! key_hash_size x
    | Key_t _ -> accu +! public_key_size x
    | Timestamp_t _ -> accu +! timestamp_size x
    | Address_t _ -> accu +! address_size x
    | Bool_t _ -> accu
    | Pair_t (_, _, _) -> accu +! header_size +! (word_size *? 2)
    | Union_t (_, _, _) -> accu +! header_size +! word_size
    | Lambda_t (_, _, _) ->
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu x
    | Option_t (_, _) -> accu +! option_size (fun _ -> zero) x
    | List_t (_, _) ->
        accu +! header_size +! (word_size *? 2)
        +! ((header_size +! (word_size *? 2)) *? x.length)
    | Set_t (_, _) ->
        let module M = (val x) in
        let boxing_space = !!300 in
        accu +! boxing_space +! ((header_size +! (word_size *? 4)) *? M.size)
    | Map_t (_, _, _) ->
        let module M = (val x) in
        let size = snd M.boxed in
        let boxing_space = !!300 in
        accu +! boxing_space +! ((header_size +! (word_size *? 5)) *? size)
    | Big_map_t (cty, ty', _) ->
        (big_map_size [@ocaml.tailcall]) ~count_lambda_nodes accu cty ty' x
    | Contract_t (_, _) -> accu +! contract_size x
    | Sapling_transaction_t (_, _) ->
        accu +! Sapling.transaction_in_memory_size x
    | Sapling_state_t (_, _) -> accu +! sapling_state_size x
    | Operation_t _ -> accu +! operation_size x
    | Chain_id_t _ -> accu +! chain_id_size
    | Never_t _ -> ( match x with _ -> .)
    | Bls12_381_g1_t _ -> accu +? Bls12_381.G1.size_in_bytes
    | Bls12_381_g2_t _ -> accu +? Bls12_381.G2.size_in_bytes
    | Bls12_381_fr_t _ -> accu +? Bls12_381.Fr.size_in_bytes
    | Ticket_t (_, _) -> accu +! ticket_size x
    | Chest_key_t _ -> accu +! chest_key_size x
    | Chest_t _ -> accu +! chest_size x
  in
  let apply_comparable : type a. sint -> a comparable_ty -> a -> sint =
   fun accu ty x ->
    match ty with
    | Unit_key _ -> accu
    | Int_key _ -> accu +! script_int_size x
    | Nat_key _ -> accu +! script_nat_size x
    | Signature_key _ -> accu +! signature_size
    | String_key _ -> accu +! script_string_size x
    | Bytes_key _ -> accu +! bytes_size x
    | Mutez_key _ -> accu +! mutez_size
    | Key_hash_key _ -> accu +! key_hash_size x
    | Key_key _ -> accu +! public_key_size x
    | Timestamp_key _ -> accu +! timestamp_size x
    | Address_key _ -> accu +! address_size x
    | Bool_key _ -> accu
    | Pair_key (_, _, _) -> accu +! header_size +! (word_size *? 2)
    | Union_key (_, _, _) -> accu +! header_size +! word_size
    | Option_key (_, _) -> accu +! option_size (fun _ -> zero) x
    | Chain_id_key _ -> accu +! chain_id_size
    | Never_key _ -> ( match x with _ -> .)
  in
  value_traverse ty x accu {apply; apply_comparable}

and big_map_size :
    type a b.
    count_lambda_nodes:bool ->
    sint ->
    a comparable_ty ->
    b ty ->
    (a, b) big_map ->
    sint =
 fun ~count_lambda_nodes accu cty ty' {id; diff; key_type; value_type} ->
  (* [Map.bindings] cannot overflow and only consumes a
     logarithmic amount of stack. *)
  let diff_size =
    let map_size =
      Big_map_overlay.fold
        (fun _key_hash (key, value) accu ->
          let accu = accu +? script_expr_hash_size in
          (* The following recursive call cannot introduce a stack
             overflow between this would require a key of type
             big_map while big_map is not comparable. *)
          let accu = value_size ~count_lambda_nodes accu (R cty) key in
          match value with
          | None -> accu
          | Some value ->
              (value_size [@ocaml.tailcall])
                ~count_lambda_nodes
                accu
                (L ty')
                value)
        diff.map
        accu
    in

    map_size +! header_size +! (word_size *? 2)
  in
  let big_map_id_size s = z_size (Big_map.Id.unparse_to_z s) in
  let id_size = option_size big_map_id_size id in
  header_size +! (word_size *? 4)
  +! comparable_ty_size key_type
  +! ty_size value_type +! id_size +! diff_size

and lambda_size :
    type i o. count_lambda_nodes:bool -> sint -> (i, o) lambda -> sint =
 fun ~count_lambda_nodes accu (Lam (kdescr, node)) ->
  (* We assume that the nodes' size have already been counted if the
     lambda is not a toplevel lambda. *)
  let accu =
    accu +! header_size +! (word_size *? 2)
    +! if count_lambda_nodes then node_size node else zero
  in
  (kdescr_size [@ocaml.tailcall]) ~count_lambda_nodes:false accu kdescr

and kdescr_size :
    type a s r f. count_lambda_nodes:bool -> sint -> (a, s, r, f) kdescr -> sint
    =
 fun ~count_lambda_nodes accu {kloc = _; kbef; kaft; kinstr} ->
  let accu =
    accu +! header_size +! (word_size *? 4) +! stack_ty_size kbef
    +! stack_ty_size kaft
  in
  (kinstr_size [@ocaml.tailcall]) ~count_lambda_nodes accu kinstr

and kinstr_size :
    type a s r f. count_lambda_nodes:bool -> sint -> (a, s, r, f) kinstr -> sint
    =
 fun ~count_lambda_nodes accu t ->
  let base kinfo = header_size +! (word_size *? 2) +! kinfo_size kinfo in
  let apply : type a s r f. sint -> (a, s, r, f) kinstr -> sint =
   fun accu t ->
    match t with
    | IDrop (kinfo, _) -> accu +! base kinfo
    | IDup (kinfo, _) -> accu +! base kinfo
    | ISwap (kinfo, _) -> accu +! base kinfo
    | IConst (kinfo, x, k) ->
        let accu = accu +! base kinfo +! word_size in
        (value_size [@ocaml.tailcall])
          ~count_lambda_nodes
          accu
          (L (stack_top_ty (kinfo_of_kinstr k).kstack_ty))
          x
    | ICons_pair (kinfo, _) -> accu +! base kinfo
    | ICar (kinfo, _) -> accu +! base kinfo
    | ICdr (kinfo, _) -> accu +! base kinfo
    | IUnpair (kinfo, _) -> accu +! base kinfo
    | ICons_some (kinfo, _) -> accu +! base kinfo
    | ICons_none (kinfo, _) -> accu +! base kinfo
    | IIf_none {kinfo; _} -> accu +! base kinfo
    | ICons_left (kinfo, _) -> accu +! base kinfo
    | ICons_right (kinfo, _) -> accu +! base kinfo
    | IIf_left {kinfo; _} -> accu +! base kinfo
    | ICons_list (kinfo, _) -> accu +! base kinfo
    | INil (kinfo, _) -> accu +! base kinfo
    | IIf_cons {kinfo; _} -> accu +! base kinfo
    | IList_map (kinfo, _, _) -> accu +! base kinfo
    | IList_iter (kinfo, _, _) -> accu +! base kinfo
    | IList_size (kinfo, _) -> accu +! base kinfo
    | IEmpty_set (kinfo, cty, _) ->
        accu +! base kinfo +! word_size +! comparable_ty_size cty
    | ISet_iter (kinfo, _, _) -> accu +! base kinfo
    | ISet_mem (kinfo, _) -> accu +! base kinfo
    | ISet_update (kinfo, _) -> accu +! base kinfo
    | ISet_size (kinfo, _) -> accu +! base kinfo
    | IEmpty_map (kinfo, cty, _) ->
        accu +! base kinfo +! word_size +! comparable_ty_size cty
    | IMap_map (kinfo, _, _) -> accu +! base kinfo +! word_size
    | IMap_iter (kinfo, _, _) -> accu +! base kinfo +! word_size
    | IMap_mem (kinfo, _) -> accu +! base kinfo
    | IMap_get (kinfo, _) -> accu +! base kinfo
    | IMap_update (kinfo, _) -> accu +! base kinfo
    | IMap_get_and_update (kinfo, _) -> accu +! base kinfo
    | IMap_size (kinfo, _) -> accu +! base kinfo
    | IEmpty_big_map (kinfo, cty, ty, _) ->
        accu +! base kinfo +! (word_size *? 2) +! ty_size ty
        +! comparable_ty_size cty
    | IBig_map_mem (kinfo, _) -> accu +! base kinfo
    | IBig_map_get (kinfo, _) -> accu +! base kinfo
    | IBig_map_update (kinfo, _) -> accu +! base kinfo
    | IBig_map_get_and_update (kinfo, _) -> accu +! base kinfo
    | IConcat_string (kinfo, _) -> accu +! base kinfo
    | IConcat_string_pair (kinfo, _) -> accu +! base kinfo
    | ISlice_string (kinfo, _) -> accu +! base kinfo
    | IString_size (kinfo, _) -> accu +! base kinfo
    | IConcat_bytes (kinfo, _) -> accu +! base kinfo
    | IConcat_bytes_pair (kinfo, _) -> accu +! base kinfo
    | ISlice_bytes (kinfo, _) -> accu +! base kinfo
    | IBytes_size (kinfo, _) -> accu +! base kinfo
    | IAdd_seconds_to_timestamp (kinfo, _) -> accu +! base kinfo
    | IAdd_timestamp_to_seconds (kinfo, _) -> accu +! base kinfo
    | ISub_timestamp_seconds (kinfo, _) -> accu +! base kinfo
    | IDiff_timestamps (kinfo, _) -> accu +! base kinfo
    | IAdd_tez (kinfo, _) -> accu +! base kinfo
    | ISub_tez (kinfo, _) -> accu +! base kinfo
    | IMul_teznat (kinfo, _) -> accu +! base kinfo
    | IMul_nattez (kinfo, _) -> accu +! base kinfo
    | IEdiv_teznat (kinfo, _) -> accu +! base kinfo
    | IEdiv_tez (kinfo, _) -> accu +! base kinfo
    | IOr (kinfo, _) -> accu +! base kinfo
    | IAnd (kinfo, _) -> accu +! base kinfo
    | IXor (kinfo, _) -> accu +! base kinfo
    | INot (kinfo, _) -> accu +! base kinfo
    | IIs_nat (kinfo, _) -> accu +! base kinfo
    | INeg_nat (kinfo, _) -> accu +! base kinfo
    | INeg_int (kinfo, _) -> accu +! base kinfo
    | IAbs_int (kinfo, _) -> accu +! base kinfo
    | IInt_nat (kinfo, _) -> accu +! base kinfo
    | IAdd_intint (kinfo, _) -> accu +! base kinfo
    | IAdd_intnat (kinfo, _) -> accu +! base kinfo
    | IAdd_natint (kinfo, _) -> accu +! base kinfo
    | IAdd_natnat (kinfo, _) -> accu +! base kinfo
    | ISub_int (kinfo, _) -> accu +! base kinfo
    | IMul_intint (kinfo, _) -> accu +! base kinfo
    | IMul_intnat (kinfo, _) -> accu +! base kinfo
    | IMul_natint (kinfo, _) -> accu +! base kinfo
    | IMul_natnat (kinfo, _) -> accu +! base kinfo
    | IEdiv_intint (kinfo, _) -> accu +! base kinfo
    | IEdiv_intnat (kinfo, _) -> accu +! base kinfo
    | IEdiv_natint (kinfo, _) -> accu +! base kinfo
    | IEdiv_natnat (kinfo, _) -> accu +! base kinfo
    | ILsl_nat (kinfo, _) -> accu +! base kinfo
    | ILsr_nat (kinfo, _) -> accu +! base kinfo
    | IOr_nat (kinfo, _) -> accu +! base kinfo
    | IAnd_nat (kinfo, _) -> accu +! base kinfo
    | IAnd_int_nat (kinfo, _) -> accu +! base kinfo
    | IXor_nat (kinfo, _) -> accu +! base kinfo
    | INot_nat (kinfo, _) -> accu +! base kinfo
    | INot_int (kinfo, _) -> accu +! base kinfo
    | IIf {kinfo; _} -> accu +! base kinfo
    | ILoop (kinfo, _, _) -> accu +! base kinfo
    | ILoop_left (kinfo, _, _) -> accu +! base kinfo +! word_size
    | IDip (kinfo, _, _) -> accu +! base kinfo +! word_size
    | IExec (kinfo, _) -> accu +! base kinfo
    | IApply (kinfo, ty, _) -> accu +! base kinfo +! word_size +! ty_size ty
    | ILambda (kinfo, lambda, _) ->
        let accu = accu +! base kinfo +! word_size in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | IFailwith (kinfo, _, ty) -> accu +! base kinfo +! ty_size ty +! word_size
    | ICompare (kinfo, cty, _) ->
        accu +! base kinfo +! comparable_ty_size cty +! word_size
    | IEq (kinfo, _) -> accu +! base kinfo
    | INeq (kinfo, _) -> accu +! base kinfo
    | ILt (kinfo, _) -> accu +! base kinfo
    | IGt (kinfo, _) -> accu +! base kinfo
    | ILe (kinfo, _) -> accu +! base kinfo
    | IGe (kinfo, _) -> accu +! base kinfo
    | IAddress (kinfo, _) -> accu +! base kinfo
    | IContract (kinfo, ty, s, _) ->
        accu +! base kinfo +! ty_size ty +! string_size s +! (word_size *? 2)
    | IView (kinfo, s, _) ->
        accu +! base kinfo +! view_signature_size s +! word_size
    | ITransfer_tokens (kinfo, _) -> accu +! base kinfo
    | IImplicit_account (kinfo, _) -> accu +! base kinfo
    | ICreate_contract
        {kinfo; storage_type; arg_type; lambda; root_name; views; k = _} ->
        let accu =
          accu +! base kinfo +! (word_size *? 4) +! ty_size storage_type
          +! ty_size arg_type
          +! option_size field_annot_size root_name
          +! views_size views
        in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | ISet_delegate (kinfo, _) -> accu +! base kinfo
    | INow (kinfo, _) -> accu +! base kinfo
    | IBalance (kinfo, _) -> accu +! base kinfo
    | ILevel (kinfo, _) -> accu +! base kinfo
    | ICheck_signature (kinfo, _) -> accu +! base kinfo
    | IHash_key (kinfo, _) -> accu +! base kinfo
    | IPack (kinfo, ty, _) -> accu +! base kinfo +! ty_size ty +! word_size
    | IUnpack (kinfo, ty, _) -> accu +! base kinfo +! ty_size ty +! word_size
    | IBlake2b (kinfo, _) -> accu +! base kinfo
    | ISha256 (kinfo, _) -> accu +! base kinfo
    | ISha512 (kinfo, _) -> accu +! base kinfo
    | ISource (kinfo, _) -> accu +! base kinfo
    | ISender (kinfo, _) -> accu +! base kinfo
    | ISelf (kinfo, ty, s, _) ->
        accu +! base kinfo +! (word_size *? 2) +! ty_size ty +! string_size s
    | ISelf_address (kinfo, _) -> accu +! base kinfo
    | IAmount (kinfo, _) -> accu +! base kinfo
    | ISapling_empty_state (kinfo, _m, _) ->
        accu +! base kinfo +! word_size +! sapling_memo_size_size
    | ISapling_verify_update (kinfo, _) -> accu +! base kinfo
    | IDig (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2)
        +! stack_prefix_preservation_witness_size n
    | IDug (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2)
        +! stack_prefix_preservation_witness_size n
    | IDipn (kinfo, n, _, _, _) ->
        accu +! base kinfo +! (word_size *? 2)
        +! stack_prefix_preservation_witness_size n
    | IDropn (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2)
        +! stack_prefix_preservation_witness_size n
    | IChainId (kinfo, _) -> accu +! base kinfo
    | INever kinfo -> accu +! kinfo_size kinfo
    | IVoting_power (kinfo, _) -> accu +! base kinfo
    | ITotal_voting_power (kinfo, _) -> accu +! base kinfo
    | IKeccak (kinfo, _) -> accu +! base kinfo
    | ISha3 (kinfo, _) -> accu +! base kinfo
    | IAdd_bls12_381_g1 (kinfo, _) -> accu +! base kinfo
    | IAdd_bls12_381_g2 (kinfo, _) -> accu +! base kinfo
    | IAdd_bls12_381_fr (kinfo, _) -> accu +! base kinfo
    | IMul_bls12_381_g1 (kinfo, _) -> accu +! base kinfo
    | IMul_bls12_381_g2 (kinfo, _) -> accu +! base kinfo
    | IMul_bls12_381_fr (kinfo, _) -> accu +! base kinfo
    | IMul_bls12_381_z_fr (kinfo, _) -> accu +! base kinfo
    | IMul_bls12_381_fr_z (kinfo, _) -> accu +! base kinfo
    | IInt_bls12_381_fr (kinfo, _) -> accu +! base kinfo
    | INeg_bls12_381_g1 (kinfo, _) -> accu +! base kinfo
    | INeg_bls12_381_g2 (kinfo, _) -> accu +! base kinfo
    | INeg_bls12_381_fr (kinfo, _) -> accu +! base kinfo
    | IPairing_check_bls12_381 (kinfo, _) -> accu +! base kinfo
    | IComb (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2) +! comb_gadt_witness_size n
    | IUncomb (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2) +! uncomb_gadt_witness_size n
    | IComb_get (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2) +! comb_get_gadt_witness_size n
    | IComb_set (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2) +! comb_set_gadt_witness_size n
    | IDup_n (kinfo, n, _, _) ->
        accu +! base kinfo +! (word_size *? 2) +! dup_n_gadt_witness_size n
    | ITicket (kinfo, _) -> accu +! base kinfo
    | IRead_ticket (kinfo, _) -> accu +! base kinfo
    | ISplit_ticket (kinfo, _) -> accu +! base kinfo
    | IJoin_tickets (kinfo, cty, _) ->
        accu +! base kinfo +! word_size +! comparable_ty_size cty
    | IOpen_chest (kinfo, _) -> accu +! base kinfo
    | IHalt kinfo -> accu +! header_size +! word_size +! kinfo_size kinfo
    | ILog (_, _, _, _) ->
        (* This instruction is ignored because it is only used for testing. *)
        accu
  in
  kinstr_traverse t accu {apply}

let rec kinstr_extra_size : type a s r f. (a, s, r, f) kinstr -> sint =
 fun t ->
  let apply : type a s r f. sint -> (a, s, r, f) kinstr -> sint =
   fun accu t ->
    let stack_prefix_preservation_witness_size n = !!24 *? n in
    let dup_n_gadt_witness_size n = !!16 *? n in
    let comb n = !!16 *? n in
    let if_join k =
      let kinfo = Script_typed_ir.kinfo_of_kinstr k in
      stack_ty_size kinfo.kstack_ty
    in
    let self_size : sint =
      match t with
      (* Op n *)
      | IDig (_, n, _, _) -> stack_prefix_preservation_witness_size n
      | IDug (_, n, _, _) -> stack_prefix_preservation_witness_size n
      | IDipn (_, n, _, _, _) -> stack_prefix_preservation_witness_size n
      | IDropn (_, n, _, _) -> stack_prefix_preservation_witness_size n
      | IComb (_, n, _, _) -> comb n
      | IUncomb (_, n, _, _) -> comb n
      | IComb_get (_, n, _, _) -> comb (n / 2)
      | IComb_set (_, n, _, _) -> comb (n / 2)
      | IDup_n (_, n, _, _) -> dup_n_gadt_witness_size n
      (* Whole stack types after conditionals and loops. *)
      | IIf {k; _} -> if_join k
      | IIf_cons {k; _} -> if_join k
      | IIf_none {k; _} -> if_join k
      | IIf_left {k; _} -> if_join k
      (* Every instruction whose elaboration uses [merge_types],
         [check_item_ty], [comparable_of_ty], or [ty_of_comparable_ty]
         to create a type that is embedded in the IR. *)
      | IJoin_tickets (_, _, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | ITicket (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IRead_ticket (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | ICons_list (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IMap_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IMap_get_and_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IBig_map_get_and_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IApply (_, ty, _) -> ty_size ty
      | ICompare (_, ty, _) -> comparable_ty_size ty
      | IList_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IList_map (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | ISet_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IMap_map (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | IMap_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _, _) -> ty_size ty)
      | ILambda (_, lambda, _) -> lambda_extra_size lambda
      | ICreate_contract {lambda; _} -> lambda_extra_size lambda
      | _ -> zero
    in
    accu +! self_size
  in
  kinstr_traverse t zero {apply}

and lambda_extra_size : type i o. (i, o) lambda -> sint =
 fun (Lam ({kinstr; _}, _)) -> kinstr_extra_size kinstr

let lambda_size lam =
  (*

      The following formula has been obtained through a regression
      over the corpus of mainnet contract in Granada.

  *)
  (lambda_size ~count_lambda_nodes:true zero lam *? 157 /? 100)
  +! (lambda_extra_size lam *? 18 /? 100)

let kinstr_size kinstr =
  (kinstr_size ~count_lambda_nodes:true zero kinstr *? 157 /? 100)
  +! (kinstr_extra_size kinstr *? 18 /? 100)

let value_size ty x = value_size ~count_lambda_nodes:true zero (L ty) x
