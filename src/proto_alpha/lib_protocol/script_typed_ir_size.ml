(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
open Script_typed_ir
include Cache_memory_helpers

let script_string_size s = Script_string.to_string s |> string_size

(* Memo-sizes are 16-bit integers *)
let sapling_memo_size_size = !!0

let ty_traverse_f =
  let base_basic =
    !!0
    (* Basic types count for 0 because they are all static values, hence shared
       and not counted by `reachable_words`.
       On the other hand compound types are functions, hence not shared. *)
  in
  let base_compound_no_meta = header_size in
  let base_compound _meta = h1w in
  let apply_comparable :
      type a. nodes_and_size -> a comparable_ty -> nodes_and_size =
   fun accu cty ->
    match cty with
    | Unit_key -> ret_succ_adding accu base_basic
    | Int_key -> ret_succ_adding accu base_basic
    | Nat_key -> ret_succ_adding accu base_basic
    | Signature_key -> ret_succ_adding accu base_basic
    | String_key -> ret_succ_adding accu base_basic
    | Bytes_key -> ret_succ_adding accu base_basic
    | Mutez_key -> ret_succ_adding accu base_basic
    | Key_hash_key -> ret_succ_adding accu base_basic
    | Key_key -> ret_succ_adding accu base_basic
    | Timestamp_key -> ret_succ_adding accu base_basic
    | Address_key -> ret_succ_adding accu base_basic
    | Bool_key -> ret_succ_adding accu base_basic
    | Chain_id_key -> ret_succ_adding accu base_basic
    | Never_key -> ret_succ_adding accu base_basic
    | Pair_key (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Union_key (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Option_key (_ty, a) ->
        ret_succ_adding accu @@ (base_compound a +! word_size)
  and apply : type a. nodes_and_size -> a ty -> nodes_and_size =
   fun accu ty ->
    match ty with
    | Unit_t -> ret_succ_adding accu base_basic
    | Int_t -> ret_succ_adding accu base_basic
    | Nat_t -> ret_succ_adding accu base_basic
    | Signature_t -> ret_succ_adding accu base_basic
    | String_t -> ret_succ_adding accu base_basic
    | Bytes_t -> ret_succ_adding accu base_basic
    | Mutez_t -> ret_succ_adding accu base_basic
    | Key_hash_t -> ret_succ_adding accu base_basic
    | Key_t -> ret_succ_adding accu base_basic
    | Timestamp_t -> ret_succ_adding accu base_basic
    | Address_t -> ret_succ_adding accu base_basic
    | Bool_t -> ret_succ_adding accu base_basic
    | Operation_t -> ret_succ_adding accu base_basic
    | Chain_id_t -> ret_succ_adding accu base_basic
    | Never_t -> ret_succ_adding accu base_basic
    | Bls12_381_g1_t -> ret_succ_adding accu base_basic
    | Bls12_381_g2_t -> ret_succ_adding accu base_basic
    | Bls12_381_fr_t -> ret_succ_adding accu base_basic
    | Chest_key_t -> ret_succ_adding accu base_basic
    | Chest_t -> ret_succ_adding accu base_basic
    | Pair_t (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Union_t (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Lambda_t (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Option_t (_ty, a) -> ret_succ_adding accu @@ (base_compound a +! word_size)
    | List_t (_ty, a) -> ret_succ_adding accu @@ (base_compound a +! word_size)
    | Set_t (_cty, a) -> ret_succ_adding accu @@ (base_compound a +! word_size)
    | Map_t (_cty, _ty, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Big_map_t (_cty, _ty, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Contract_t (_ty, a) ->
        ret_succ_adding accu @@ (base_compound a +! word_size)
    | Sapling_transaction_t _m ->
        ret_succ_adding accu
        @@ (base_compound_no_meta +! sapling_memo_size_size +! word_size)
    | Sapling_state_t _m ->
        ret_succ_adding accu
        @@ (base_compound_no_meta +! sapling_memo_size_size +! word_size)
    | Ticket_t (_cty, a) ->
        ret_succ_adding accu @@ (base_compound a +! word_size)
  in
  ({apply; apply_comparable} : nodes_and_size ty_traverse)

let comparable_ty_size : type a. a comparable_ty -> nodes_and_size =
 fun cty -> comparable_ty_traverse cty zero ty_traverse_f

let ty_size : type a. a ty -> nodes_and_size =
 fun ty -> ty_traverse ty zero ty_traverse_f

let stack_ty_size s =
  let apply : type a s. nodes_and_size -> (a, s) stack_ty -> nodes_and_size =
   fun accu s ->
    match s with
    | Bot_t -> ret_succ accu
    | Item_t (ty, _) -> ret_succ_adding (accu ++ ty_size ty) h2w
  in
  stack_ty_traverse s zero {apply}

let script_nat_size n = Script_int.to_zint n |> z_size

let script_int_size n = Script_int.to_zint n |> z_size

let signature_size = h3w +? Signature.size

let key_hash_size (x : Signature.public_key_hash) =
  h1w
  +? Signature.(
       match x with
       | Ed25519 _ -> Ed25519.Public_key_hash.size
       | Secp256k1 _ -> Secp256k1.Public_key_hash.size
       | P256 _ -> P256.Public_key_hash.size)

let public_key_size (x : public_key) =
  let ks = Signature.Public_key.size x in
  h1w +? ks

let mutez_size = h2w

let timestamp_size x = Script_timestamp.to_zint x |> z_size

let destination_size = Destination.in_memory_size

let address_size addr =
  h2w
  +! destination_size addr.destination
  +! Entrypoint.in_memory_size addr.entrypoint

let view_signature_size (View_signature {name; input_ty; output_ty}) =
  ret_adding
    (ty_size input_ty ++ ty_size output_ty)
    (h3w +! script_string_size name)

let script_expr_hash_size = Script_expr_hash.size

let peano_shape_proof =
  let scale = header_size +! h1w in
  fun k -> scale *? k

let stack_prefix_preservation_witness_size =
  let kinfo_size = h2w in
  let scale = header_size +! (h2w +! kinfo_size) in
  fun k -> scale *? k

let comb_gadt_witness_size = peano_shape_proof

let uncomb_gadt_witness_size = peano_shape_proof

let comb_get_gadt_witness_size = peano_shape_proof

let comb_set_gadt_witness_size = peano_shape_proof

let dup_n_gadt_witness_size = peano_shape_proof

let contract_size {arg_ty; address} =
  ret_adding (ty_size arg_ty) (h2w +! address_size address)

let sapling_state_size {Sapling.id; diff; memo_size = _} =
  h3w
  +! option_size (fun x -> z_size (Sapling.Id.unparse_to_z x)) id
  +! Sapling.diff_in_memory_size diff
  +! sapling_memo_size_size

let operation_size {piop; lazy_storage_diff} =
  ret_adding
    (Operation.packed_internal_operation_in_memory_size piop
    ++ option_size_vec Lazy_storage.diffs_in_memory_size lazy_storage_diff)
    h2w

let chain_id_size = h1w +? Chain_id.size

(* [contents] is handle by the recursion scheme in [value_size] *)
let ticket_size {ticketer; contents = _; amount} =
  h3w +! Contract.in_memory_size ticketer +! script_nat_size amount

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
  let ciphertext_size = Script_timelock.get_plaintext_size chest in
  h3w +? (locked_value_size + rsa_public_size + ciphertext_size)

let chest_key_size _ =
  (*
     type chest_key = {
       unlocked_value : unlocked_value;
       proof : time_lock_proof
     }
  *)
  let unlocked_value_size = 256 in
  let proof_size = 256 in
  h2w +? (unlocked_value_size + proof_size)

let view_size {input_ty; output_ty; view_code} =
  ret_adding
    (node_size input_ty ++ node_size output_ty ++ node_size view_code)
    h3w

let views_size views =
  SMap.fold
    (fun k view accu ->
      ret_adding (accu ++ view_size view) (script_string_size k +! h4w))
    views
    zero

let rec entrypoints_size : type arg. arg entrypoints -> nodes_and_size =
 fun {name; nested} ->
  let name_size = option_size Entrypoint.in_memory_size name in
  let nested_size =
    match nested with
    | Entrypoints_None -> zero
    | Entrypoints_Union {left; right} ->
        ret_adding (entrypoints_size left ++ entrypoints_size right) h2w
  in
  ret_succ_adding nested_size name_size

let kinfo_size {iloc = _; kstack_ty = _} = h2w

(* The following mutually recursive functions are mostly
   tail-recursive and the only recursive call that is not a tailcall
   cannot be nested. (See [big_map_size].) For this reason, these
   functions should not trigger stack overflows. *)
let rec value_size :
    type a.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a ty, a comparable_ty) union ->
    a ->
    nodes_and_size =
 fun ~count_lambda_nodes accu ty x ->
  let apply : type a. nodes_and_size -> a ty -> a -> nodes_and_size =
   fun accu ty x ->
    match ty with
    | Unit_t -> ret_succ accu
    | Int_t -> ret_succ_adding accu (script_int_size x)
    | Nat_t -> ret_succ_adding accu (script_nat_size x)
    | Signature_t -> ret_succ_adding accu signature_size
    | String_t -> ret_succ_adding accu (script_string_size x)
    | Bytes_t -> ret_succ_adding accu (bytes_size x)
    | Mutez_t -> ret_succ_adding accu mutez_size
    | Key_hash_t -> ret_succ_adding accu (key_hash_size x)
    | Key_t -> ret_succ_adding accu (public_key_size x)
    | Timestamp_t -> ret_succ_adding accu (timestamp_size x)
    | Address_t -> ret_succ_adding accu (address_size x)
    | Bool_t -> ret_succ accu
    | Pair_t (_, _, _) -> ret_succ_adding accu h2w
    | Union_t (_, _, _) -> ret_succ_adding accu h1w
    | Lambda_t (_, _, _) ->
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes (ret_succ accu) x
    | Option_t (_, _) -> ret_succ_adding accu (option_size (fun _ -> !!0) x)
    | List_t (_, _) -> ret_succ_adding accu (h2w +! (h2w *? x.length))
    | Set_t (_, _) ->
        let module M = (val Script_set.get x) in
        let boxing_space = !!300 in
        ret_succ_adding accu (boxing_space +! (h4w *? M.size))
    | Map_t (_, _, _) ->
        let module M = (val Script_map.get_module x) in
        let boxing_space = !!300 in
        ret_succ_adding accu (boxing_space +! (h5w *? M.size))
    | Big_map_t (cty, ty', _) ->
        (big_map_size [@ocaml.tailcall])
          ~count_lambda_nodes
          (ret_succ accu)
          cty
          ty'
          x
    | Contract_t (_, _) -> ret_succ (accu ++ contract_size x)
    | Sapling_transaction_t _ ->
        ret_succ_adding accu (Sapling.transaction_in_memory_size x)
    | Sapling_state_t _ -> ret_succ_adding accu (sapling_state_size x)
    | Operation_t -> ret_succ (accu ++ operation_size x)
    | Chain_id_t -> ret_succ_adding accu chain_id_size
    | Never_t -> ( match x with _ -> .)
    (* Related to https://gitlab.com/dannywillems/ocaml-bls12-381/-/issues/56.
       Since the update to blst as a backend for bls12-381, size_in_bytes is not
       the correct value for the allocated memory.
       There is 1 word for the OCaml block header, 1 word for the C pointer and
       a certain number of words for the actual value of the algebraic object
       whose size is fixed and defined by the object itself.
       For G1, it allocates 3 C values of type blst_fp which is 48 bytes.
       For G2, it allocates 3 C values of type blst_fp2 which is 48 * 2 bytes.
       For Fr, it allocates 1 C value of type blst_fr which is 32 bytes.
    *)
    | Bls12_381_g1_t -> ret_succ_adding accu !!((2 * 8) + (3 * 48))
    | Bls12_381_g2_t -> ret_succ_adding accu !!((2 * 8) + (3 * 48 * 2))
    | Bls12_381_fr_t -> ret_succ_adding accu !!((2 * 8) + 32)
    | Ticket_t (_, _) -> ret_succ_adding accu (ticket_size x)
    | Chest_key_t -> ret_succ_adding accu (chest_key_size x)
    | Chest_t -> ret_succ_adding accu (chest_size x)
  in
  let apply_comparable :
      type a. nodes_and_size -> a comparable_ty -> a -> nodes_and_size =
   fun accu ty x ->
    match ty with
    | Unit_key -> ret_succ accu
    | Int_key -> ret_succ_adding accu (script_int_size x)
    | Nat_key -> ret_succ_adding accu (script_nat_size x)
    | Signature_key -> ret_succ_adding accu signature_size
    | String_key -> ret_succ_adding accu (script_string_size x)
    | Bytes_key -> ret_succ_adding accu (bytes_size x)
    | Mutez_key -> ret_succ_adding accu mutez_size
    | Key_hash_key -> ret_succ_adding accu (key_hash_size x)
    | Key_key -> ret_succ_adding accu (public_key_size x)
    | Timestamp_key -> ret_succ_adding accu (timestamp_size x)
    | Address_key -> ret_succ_adding accu (address_size x)
    | Bool_key -> ret_succ accu
    | Pair_key (_, _, _) -> ret_succ_adding accu h2w
    | Union_key (_, _, _) -> ret_succ_adding accu h1w
    | Option_key (_, _) -> ret_succ_adding accu (option_size (fun _ -> !!0) x)
    | Chain_id_key -> ret_succ_adding accu chain_id_size
    | Never_key -> ( match x with _ -> .)
  in
  value_traverse ty x accu {apply; apply_comparable}
 [@@coq_axiom_with_reason "unreachable expressions '.' not handled for now"]

and big_map_size :
    type a b.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    a comparable_ty ->
    b ty ->
    (a, b) big_map ->
    nodes_and_size =
 fun ~count_lambda_nodes accu cty ty' {id; diff; key_type; value_type} ->
  (* [Map.bindings] cannot overflow and only consumes a
     logarithmic amount of stack. *)
  let diff_size =
    let map_size =
      Big_map_overlay.fold
        (fun _key_hash (key, value) accu ->
          let accu = ret_succ_adding accu !!script_expr_hash_size in
          (* The following recursive call cannot introduce a stack
             overflow because this would require a key of type
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

    ret_adding map_size h2w
  in
  let big_map_id_size s = z_size (Big_map.Id.unparse_to_z s) in
  let id_size = option_size big_map_id_size id in
  ret_adding
    (comparable_ty_size key_type ++ ty_size value_type ++ diff_size)
    (h4w +! id_size)

and lambda_size :
    type i o.
    count_lambda_nodes:bool -> nodes_and_size -> (i, o) lambda -> nodes_and_size
    =
 fun ~count_lambda_nodes accu (Lam (kdescr, node)) ->
  (* We assume that the nodes' size have already been counted if the
     lambda is not a toplevel lambda. *)
  let accu =
    ret_adding (accu ++ if count_lambda_nodes then node_size node else zero) h2w
  in
  (kdescr_size [@ocaml.tailcall]) ~count_lambda_nodes:false accu kdescr

and kdescr_size :
    type a s r f.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a, s, r, f) kdescr ->
    nodes_and_size =
 fun ~count_lambda_nodes accu {kloc = _; kbef; kaft; kinstr} ->
  let accu =
    ret_adding (accu ++ stack_ty_size kbef ++ stack_ty_size kaft) h4w
  in
  (kinstr_size [@ocaml.tailcall]) ~count_lambda_nodes accu kinstr

and kinstr_size :
    type a s r f.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a, s, r, f) kinstr ->
    nodes_and_size =
 fun ~count_lambda_nodes accu t ->
  let base kinfo = h2w +! kinfo_size kinfo in
  let apply :
      type a s r f. nodes_and_size -> (a, s, r, f) kinstr -> nodes_and_size =
   fun accu t ->
    match t with
    | IDrop (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IDup (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISwap (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IConst (kinfo, x, k) ->
        let accu = ret_succ_adding accu (base kinfo +! word_size) in
        (value_size [@ocaml.tailcall])
          ~count_lambda_nodes
          accu
          (L (stack_top_ty (kinfo_of_kinstr k).kstack_ty))
          x
    | ICons_pair (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICar (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICdr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IUnpair (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICons_some (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICons_none (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IIf_none {kinfo; _} -> ret_succ_adding accu (base kinfo)
    | IOpt_map {kinfo; _} -> ret_succ_adding accu (base kinfo)
    | ICons_left (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICons_right (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IIf_left {kinfo; _} -> ret_succ_adding accu (base kinfo)
    | ICons_list (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INil (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IIf_cons {kinfo; _} -> ret_succ_adding accu (base kinfo)
    | IList_map (kinfo, _, _) -> ret_succ_adding accu (base kinfo)
    | IList_iter (kinfo, _, _) -> ret_succ_adding accu (base kinfo)
    | IList_size (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEmpty_set (kinfo, cty, _) ->
        ret_succ_adding
          (accu ++ comparable_ty_size cty)
          (base kinfo +! word_size)
    | ISet_iter (kinfo, _, _) -> ret_succ_adding accu (base kinfo)
    | ISet_mem (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISet_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISet_size (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEmpty_map (kinfo, cty, _) ->
        ret_succ_adding
          (accu ++ comparable_ty_size cty)
          (base kinfo +! word_size)
    | IMap_map (kinfo, _, _) -> ret_succ_adding accu (base kinfo +! word_size)
    | IMap_iter (kinfo, _, _) -> ret_succ_adding accu (base kinfo +! word_size)
    | IMap_mem (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMap_get (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMap_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMap_get_and_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMap_size (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEmpty_big_map (kinfo, cty, ty, _) ->
        ret_succ_adding
          (accu ++ comparable_ty_size cty ++ ty_size ty)
          (base kinfo +! (word_size *? 2))
    | IBig_map_mem (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IBig_map_get (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IBig_map_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IBig_map_get_and_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IConcat_string (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IConcat_string_pair (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISlice_string (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IString_size (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IConcat_bytes (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IConcat_bytes_pair (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISlice_bytes (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IBytes_size (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_seconds_to_timestamp (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_timestamp_to_seconds (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISub_timestamp_seconds (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IDiff_timestamps (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_tez (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISub_tez (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISub_tez_legacy (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_teznat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_nattez (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEdiv_teznat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEdiv_tez (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IOr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAnd (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IXor (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INot (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IIs_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INeg (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAbs_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IInt_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISub_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEdiv_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IEdiv_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ILsl_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ILsr_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IOr_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAnd_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAnd_int_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IXor_nat (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INot_int (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IIf {kinfo; _} -> ret_succ_adding accu (base kinfo)
    | ILoop (kinfo, _, _) -> ret_succ_adding accu (base kinfo)
    | ILoop_left (kinfo, _, _) -> ret_succ_adding accu (base kinfo +! word_size)
    | IDip (kinfo, _, _) -> ret_succ_adding accu (base kinfo +! word_size)
    | IExec (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IApply (kinfo, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base kinfo +! word_size)
    | ILambda (kinfo, lambda, _) ->
        let accu = ret_succ_adding accu (base kinfo +! word_size) in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | IFailwith (kinfo, _, ty) ->
        ret_succ_adding (accu ++ ty_size ty) (base kinfo +! word_size)
    | ICompare (kinfo, cty, _) ->
        ret_succ_adding
          (accu ++ comparable_ty_size cty)
          (base kinfo +! word_size)
    | IEq (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INeq (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ILt (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IGt (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ILe (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IGe (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAddress (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IContract (kinfo, ty, s, _) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base kinfo +! Entrypoint.in_memory_size s +! (word_size *? 2))
    | IView (kinfo, s, _) ->
        ret_succ_adding (accu ++ view_signature_size s) (base kinfo +! word_size)
    | ITransfer_tokens (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IImplicit_account (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICreate_contract
        {kinfo; storage_type; arg_type; lambda; entrypoints; views; k = _} ->
        let accu =
          ret_succ_adding
            (accu ++ ty_size storage_type ++ ty_size arg_type
           ++ views_size views
            ++ entrypoints_size entrypoints)
            (base kinfo +! (word_size *? 4))
        in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | ISet_delegate (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INow (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IBalance (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ILevel (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ICheck_signature (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IHash_key (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IPack (kinfo, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base kinfo +! word_size)
    | IUnpack (kinfo, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base kinfo +! word_size)
    | IBlake2b (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISha256 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISha512 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISource (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISender (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISelf (kinfo, ty, s, _) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base kinfo +! (word_size *? 2) +! Entrypoint.in_memory_size s)
    | ISelf_address (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAmount (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISapling_empty_state (kinfo, _m, _) ->
        ret_succ_adding accu (base kinfo +! word_size +! sapling_memo_size_size)
    | ISapling_verify_update (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IDig (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2)
          +! stack_prefix_preservation_witness_size n)
    | IDug (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2)
          +! stack_prefix_preservation_witness_size n)
    | IDipn (kinfo, n, _, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2)
          +! stack_prefix_preservation_witness_size n)
    | IDropn (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2)
          +! stack_prefix_preservation_witness_size n)
    | IChainId (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INever kinfo -> ret_succ_adding accu (kinfo_size kinfo)
    | IVoting_power (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ITotal_voting_power (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IKeccak (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISha3 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_bls12_381_g1 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_bls12_381_g2 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IAdd_bls12_381_fr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_bls12_381_g1 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_bls12_381_g2 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_bls12_381_fr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_bls12_381_z_fr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IMul_bls12_381_fr_z (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IInt_bls12_381_fr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INeg_bls12_381_g1 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INeg_bls12_381_g2 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | INeg_bls12_381_fr (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IPairing_check_bls12_381 (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IComb (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2) +! comb_gadt_witness_size n)
    | IUncomb (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2) +! uncomb_gadt_witness_size n)
    | IComb_get (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2) +! comb_get_gadt_witness_size n)
    | IComb_set (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2) +! comb_set_gadt_witness_size n)
    | IDup_n (kinfo, n, _, _) ->
        ret_succ_adding
          accu
          (base kinfo +! (word_size *? 2) +! dup_n_gadt_witness_size n)
    | ITicket (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IRead_ticket (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | ISplit_ticket (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IJoin_tickets (kinfo, cty, _) ->
        ret_succ_adding
          (accu ++ comparable_ty_size cty)
          (base kinfo +! word_size)
    | IOpen_chest (kinfo, _) -> ret_succ_adding accu (base kinfo)
    | IHalt kinfo -> ret_succ_adding accu (h1w +! kinfo_size kinfo)
    | ILog (_, _, _, _) ->
        (* This instruction is ignored because it is only used for testing. *)
        accu
  in
  kinstr_traverse t accu {apply}

let rec kinstr_extra_size : type a s r f. (a, s, r, f) kinstr -> nodes_and_size
    =
 fun t ->
  let ret_zero x = (Nodes.zero, x) in
  let apply :
      type a s r f. nodes_and_size -> (a, s, r, f) kinstr -> nodes_and_size =
   fun accu t ->
    let stack_prefix_preservation_witness_size n = ret_zero (!!24 *? n) in
    let dup_n_gadt_witness_size n = ret_zero (!!16 *? n) in
    let comb n = ret_zero (!!16 *? n) in
    let if_join k =
      let kinfo = Script_typed_ir.kinfo_of_kinstr k in
      stack_ty_size kinfo.kstack_ty
    in
    let self_size =
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
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | ITicket (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IRead_ticket (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | ICons_list (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IMap_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IMap_get_and_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IBig_map_get_and_update (_, k) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr k in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IApply (_, ty, _) -> ty_size ty
      | ICompare (_, ty, _) -> comparable_ty_size ty
      | IList_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IList_map (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | ISet_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IMap_map (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | IMap_iter (_, body, _) -> (
          let kinfo = Script_typed_ir.kinfo_of_kinstr body in
          match kinfo.kstack_ty with Item_t (ty, _) -> ty_size ty)
      | ILambda (_, lambda, _) -> lambda_extra_size lambda
      | ICreate_contract {lambda; _} -> lambda_extra_size lambda
      | _ -> zero
    in
    ret_succ (accu ++ self_size)
  in
  kinstr_traverse t zero {apply}

and lambda_extra_size : type i o. (i, o) lambda -> nodes_and_size =
 fun (Lam ({kinstr; _}, _)) -> kinstr_extra_size kinstr

let lambda_size lam =
  (*

      The following formula has been obtained through a regression
      over the corpus of mainnet contracts in Granada.

  *)
  let (lambda_nodes, lambda_size) =
    lambda_size ~count_lambda_nodes:true zero lam
  in
  let (lambda_extra_size_nodes, lambda_extra_size) = lambda_extra_size lam in
  let size = (lambda_size *? 157 /? 100) +! (lambda_extra_size *? 18 /? 100) in
  (Nodes.add lambda_nodes lambda_extra_size_nodes, size)

let kinstr_size kinstr =
  let (kinstr_extra_size_nodes, kinstr_extra_size) = kinstr_extra_size kinstr in
  let (kinstr_nodes, kinstr_size) =
    kinstr_size ~count_lambda_nodes:true zero kinstr
  in
  let size = (kinstr_size *? 157 /? 100) +! (kinstr_extra_size *? 18 /? 100) in
  (Nodes.add kinstr_nodes kinstr_extra_size_nodes, size)

let value_size ty x = value_size ~count_lambda_nodes:true zero (L ty) x

module Internal_for_tests = struct
  let ty_size = ty_size

  let comparable_ty_size = comparable_ty_size

  let kinstr_size = kinstr_size
end
