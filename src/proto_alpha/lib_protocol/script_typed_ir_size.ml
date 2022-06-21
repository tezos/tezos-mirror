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
  let apply : type a ac. nodes_and_size -> (a, ac) ty -> nodes_and_size =
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
    | Tx_rollup_l2_address_t -> ret_succ_adding accu base_basic
    | Bool_t -> ret_succ_adding accu base_basic
    | Operation_t -> ret_succ_adding accu base_basic
    | Chain_id_t -> ret_succ_adding accu base_basic
    | Never_t -> ret_succ_adding accu base_basic
    | Bls12_381_g1_t -> ret_succ_adding accu base_basic
    | Bls12_381_g2_t -> ret_succ_adding accu base_basic
    | Bls12_381_fr_t -> ret_succ_adding accu base_basic
    | Chest_key_t -> ret_succ_adding accu base_basic
    | Chest_t -> ret_succ_adding accu base_basic
    | Pair_t (_ty1, _ty2, a, _) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 3))
    | Union_t (_ty1, _ty2, a, _) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 3))
    | Lambda_t (_ty1, _ty2, a) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
    | Option_t (_ty, a, _) ->
        ret_succ_adding accu @@ (base_compound a +! (word_size *? 2))
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
    | Sapling_transaction_deprecated_t _m ->
        ret_succ_adding accu
        @@ (base_compound_no_meta +! sapling_memo_size_size +! word_size)
    | Sapling_state_t _m ->
        ret_succ_adding accu
        @@ (base_compound_no_meta +! sapling_memo_size_size +! word_size)
    | Ticket_t (_cty, a) ->
        ret_succ_adding accu @@ (base_compound a +! word_size)
  in
  ({apply} : nodes_and_size ty_traverse)

let ty_size : type a ac. (a, ac) ty -> nodes_and_size =
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

let signature_size = !!96 (* By Obj.reachable_words. *)

let key_hash_size (_x : Signature.public_key_hash) = !!64
(* By Obj.reachable_words. *)

let public_key_size (x : public_key) =
  h1w +? match x with Ed25519 _ -> 64 | Secp256k1 _ -> 72 | P256 _ -> 96

let mutez_size = h2w

let timestamp_size x = Script_timestamp.to_zint x |> z_size

let destination_size = Destination.in_memory_size

let address_size addr =
  h2w
  +! destination_size addr.destination
  +! Entrypoint.in_memory_size addr.entrypoint

let tx_rollup_l2_address_size (tx : tx_rollup_l2_address) =
  Tx_rollup_l2_address.Indexable.in_memory_size @@ Indexable.forget tx

let view_signature_size (View_signature {name; input_ty; output_ty}) =
  ret_adding
    (ty_size input_ty ++ ty_size output_ty)
    (h3w +! script_string_size name)

let script_expr_hash_size = !!64

let peano_shape_proof =
  let scale = header_size +! h1w in
  fun k -> scale *? k

(* Note: this function is NOT tail-recursive, but that's okay, since
   the recursion is bound by the size of the witness, which is an
   11-bit unsigned integer, i.e. at most 2048. This is enough to
   guarantee there will be no stack overflow. *)
let rec stack_prefix_preservation_witness_size :
    type a b c d e f g h.
    (a, b, c, d, e, f, g, h) stack_prefix_preservation_witness -> nodes_and_size
    = function
  | KPrefix (_loc, ty, w) ->
      ret_succ_adding
        (ty_size ty ++ stack_prefix_preservation_witness_size w)
        h3w
  | KRest -> zero

let comb_gadt_witness_size = peano_shape_proof

let uncomb_gadt_witness_size = peano_shape_proof

let comb_get_gadt_witness_size = peano_shape_proof

let comb_set_gadt_witness_size = peano_shape_proof

let dup_n_gadt_witness_size = peano_shape_proof

let contract_size (Typed_contract {arg_ty; address}) =
  ret_adding (ty_size arg_ty) (h2w +! address_size address)

let sapling_state_size {Sapling.id; diff; memo_size = _} =
  h3w
  +! option_size (fun x -> z_size (Sapling.Id.unparse_to_z x)) id
  +! Sapling.diff_in_memory_size diff
  +! sapling_memo_size_size

let chain_id_size = !!16 (* by Obj.reachable_words. *)

(* [contents] is handled by the recursion scheme in [value_size]. *)
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

(* The following mutually recursive functions are mostly
   tail-recursive and the only recursive call that is not a tailcall
   cannot be nested. (See [big_map_size].) For this reason, these
   functions should not trigger stack overflows. *)
let rec value_size :
    type a ac.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a, ac) ty ->
    a ->
    nodes_and_size =
 fun ~count_lambda_nodes accu ty x ->
  let apply : type a ac. nodes_and_size -> (a, ac) ty -> a -> nodes_and_size =
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
    | Tx_rollup_l2_address_t ->
        ret_succ_adding accu (tx_rollup_l2_address_size x)
    | Bool_t -> ret_succ accu
    | Pair_t (_, _, _, _) -> ret_succ_adding accu h2w
    | Union_t (_, _, _, _) -> ret_succ_adding accu h1w
    | Lambda_t (_, _, _) ->
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes (ret_succ accu) x
    | Option_t (_, _, _) -> ret_succ_adding accu (option_size (fun _ -> !!0) x)
    | List_t (_, _) -> ret_succ_adding accu (h2w +! (h2w *? x.length))
    | Set_t (_, _) ->
        let module M = (val Script_set.get x) in
        let boxing_space = !!536 (* By Obj.reachable_words. *) in
        ret_succ_adding accu (boxing_space +! (h4w *? M.size))
    | Map_t (_, _, _) ->
        let module M = (val Script_map.get_module x) in
        let boxing_space = !!696 (* By Obj.reachable_words. *) in
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
    | Sapling_transaction_deprecated_t _ ->
        ret_succ_adding accu (Sapling.Legacy.transaction_in_memory_size x)
    | Sapling_state_t _ -> ret_succ_adding accu (sapling_state_size x)
    (* Operations are neither storable nor pushable, so they can appear neither
       in the storage nor in the script. Hence they cannot appear in the cache
       and we never need to measure their size. *)
    | Operation_t -> assert false
    | Chain_id_t -> ret_succ_adding accu chain_id_size
    | Never_t -> ( match x with _ -> .)
    | Bls12_381_g1_t -> ret_succ_adding accu !!Bls12_381.G1.size_in_memory
    | Bls12_381_g2_t -> ret_succ_adding accu !!Bls12_381.G2.size_in_memory
    | Bls12_381_fr_t -> ret_succ_adding accu !!Bls12_381.Fr.size_in_memory
    | Ticket_t (_, _) -> ret_succ_adding accu (ticket_size x)
    | Chest_key_t -> ret_succ_adding accu (chest_key_size x)
    | Chest_t -> ret_succ_adding accu (chest_size x)
  in
  value_traverse ty x accu {apply}
 [@@coq_axiom_with_reason "unreachable expressions '.' not handled for now"]

and big_map_size :
    type a b bc.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    a comparable_ty ->
    (b, bc) ty ->
    (a, b) big_map ->
    nodes_and_size =
 fun ~count_lambda_nodes accu cty ty' (Big_map {id; diff; key_type; value_type}) ->
  (* [Map.bindings] cannot overflow and only consumes a
     logarithmic amount of stack. *)
  let diff_size =
    let map_size =
      Big_map_overlay.fold
        (fun _key_hash (key, value) accu ->
          let base = h5w +! (word_size *? 3) +! script_expr_hash_size in
          let accu = ret_succ_adding accu base in
          (* The following recursive call cannot introduce a stack
             overflow because this would require a key of type
             big_map while big_map is not comparable. *)
          let accu = value_size ~count_lambda_nodes accu cty key in
          match value with
          | None -> accu
          | Some value ->
              let accu = ret_succ_adding accu h1w in
              (value_size [@ocaml.tailcall]) ~count_lambda_nodes accu ty' value)
        diff.map
        accu
    in
    ret_adding map_size h2w
  in
  let big_map_id_size s = z_size (Big_map.Id.unparse_to_z s) in
  let id_size = option_size big_map_id_size id in
  ret_adding
    (ty_size key_type ++ ty_size value_type ++ diff_size)
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
  let base = h2w in
  let apply :
      type a s r f. nodes_and_size -> (a, s, r, f) kinstr -> nodes_and_size =
   fun accu t ->
    match t with
    | IDrop (_, _) -> ret_succ_adding accu base
    | IDup (_, _) -> ret_succ_adding accu base
    | ISwap (_, _) -> ret_succ_adding accu base
    | IConst (_, ty, x, _) ->
        let accu = ret_succ_adding accu (base +! (word_size *? 2)) in
        (value_size [@ocaml.tailcall])
          ~count_lambda_nodes
          (accu ++ ty_size ty)
          ty
          x
    | ICons_pair (_, _) -> ret_succ_adding accu base
    | ICar (_, _) -> ret_succ_adding accu base
    | ICdr (_, _) -> ret_succ_adding accu base
    | IUnpair (_, _) -> ret_succ_adding accu base
    | ICons_some (_, _) -> ret_succ_adding accu base
    | ICons_none (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | IIf_none {loc = _; branch_if_none = _; branch_if_some = _; k = _} ->
        ret_succ_adding accu (base +! (word_size *? 2))
    | IOpt_map {loc = _; body = _; k = _} ->
        ret_succ_adding accu (base +! word_size)
    | ICons_left (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | ICons_right (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | IIf_left {loc = _; branch_if_left = _; branch_if_right = _; k = _} ->
        ret_succ_adding accu (base +! (word_size *? 2))
    | ICons_list (_, _) -> ret_succ_adding accu base
    | INil (_, ty, _) -> ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | IIf_cons {loc = _; branch_if_nil = _; branch_if_cons = _; k = _} ->
        ret_succ_adding accu (base +! (word_size *? 2))
    | IList_map (_loc, _k1, ty, _k2) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! (word_size *? 2))
    | IList_iter (_, ty, _, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! (word_size *? 2))
    | IList_size (_, _) -> ret_succ_adding accu base
    | IEmpty_set (_, cty, _) ->
        ret_succ_adding (accu ++ ty_size cty) (base +! word_size)
    | ISet_iter (_, ty, _, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! (word_size *? 2))
    | ISet_mem (_, _) -> ret_succ_adding accu base
    | ISet_update (_, _) -> ret_succ_adding accu base
    | ISet_size (_, _) -> ret_succ_adding accu base
    | IEmpty_map (_, cty, vty, _) ->
        ret_succ_adding
          (accu ++ ty_size cty ++ ty_size vty)
          (base +! (word_size *? 2))
    | IMap_map (_, ty, _, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! (word_size *? 2))
    | IMap_iter (_, kvty, _, _) ->
        ret_succ_adding (accu ++ ty_size kvty) (base +! (word_size *? 2))
    | IMap_mem (_, _) -> ret_succ_adding accu base
    | IMap_get (_, _) -> ret_succ_adding accu base
    | IMap_update (_, _) -> ret_succ_adding accu base
    | IMap_get_and_update (_, _) -> ret_succ_adding accu base
    | IMap_size (_, _) -> ret_succ_adding accu base
    | IEmpty_big_map (_, cty, ty, _) ->
        ret_succ_adding
          (accu ++ ty_size cty ++ ty_size ty)
          (base +! (word_size *? 2))
    | IBig_map_mem (_, _) -> ret_succ_adding accu base
    | IBig_map_get (_, _) -> ret_succ_adding accu base
    | IBig_map_update (_, _) -> ret_succ_adding accu base
    | IBig_map_get_and_update (_, _) -> ret_succ_adding accu base
    | IConcat_string (_, _) -> ret_succ_adding accu base
    | IConcat_string_pair (_, _) -> ret_succ_adding accu base
    | ISlice_string (_, _) -> ret_succ_adding accu base
    | IString_size (_, _) -> ret_succ_adding accu base
    | IConcat_bytes (_, _) -> ret_succ_adding accu base
    | IConcat_bytes_pair (_, _) -> ret_succ_adding accu base
    | ISlice_bytes (_, _) -> ret_succ_adding accu base
    | IBytes_size (_, _) -> ret_succ_adding accu base
    | IAdd_seconds_to_timestamp (_, _) -> ret_succ_adding accu base
    | IAdd_timestamp_to_seconds (_, _) -> ret_succ_adding accu base
    | ISub_timestamp_seconds (_, _) -> ret_succ_adding accu base
    | IDiff_timestamps (_, _) -> ret_succ_adding accu base
    | IAdd_tez (_, _) -> ret_succ_adding accu base
    | ISub_tez (_, _) -> ret_succ_adding accu base
    | ISub_tez_legacy (_, _) -> ret_succ_adding accu base
    | IMul_teznat (_, _) -> ret_succ_adding accu base
    | IMul_nattez (_, _) -> ret_succ_adding accu base
    | IEdiv_teznat (_, _) -> ret_succ_adding accu base
    | IEdiv_tez (_, _) -> ret_succ_adding accu base
    | IOr (_, _) -> ret_succ_adding accu base
    | IAnd (_, _) -> ret_succ_adding accu base
    | IXor (_, _) -> ret_succ_adding accu base
    | INot (_, _) -> ret_succ_adding accu base
    | IIs_nat (_, _) -> ret_succ_adding accu base
    | INeg (_, _) -> ret_succ_adding accu base
    | IAbs_int (_, _) -> ret_succ_adding accu base
    | IInt_nat (_, _) -> ret_succ_adding accu base
    | IAdd_int (_, _) -> ret_succ_adding accu base
    | IAdd_nat (_, _) -> ret_succ_adding accu base
    | ISub_int (_, _) -> ret_succ_adding accu base
    | IMul_int (_, _) -> ret_succ_adding accu base
    | IMul_nat (_, _) -> ret_succ_adding accu base
    | IEdiv_int (_, _) -> ret_succ_adding accu base
    | IEdiv_nat (_, _) -> ret_succ_adding accu base
    | ILsl_nat (_, _) -> ret_succ_adding accu base
    | ILsr_nat (_, _) -> ret_succ_adding accu base
    | IOr_nat (_, _) -> ret_succ_adding accu base
    | IAnd_nat (_, _) -> ret_succ_adding accu base
    | IAnd_int_nat (_, _) -> ret_succ_adding accu base
    | IXor_nat (_, _) -> ret_succ_adding accu base
    | INot_int (_, _) -> ret_succ_adding accu base
    | IIf {loc = _; branch_if_true = _; branch_if_false = _; k = _} ->
        ret_succ_adding accu (base +! (word_size *? 2))
    | ILoop (_, _, _) -> ret_succ_adding accu (base +! word_size)
    | ILoop_left (_, _, _) -> ret_succ_adding accu (base +! word_size)
    | IDip (_, _, _, _) -> ret_succ_adding accu (base +! word_size)
    | IExec (_, sty, _) ->
        ret_succ_adding (accu ++ stack_ty_size sty) (base +! (word_size *? 2))
    | IApply (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | ILambda (_, lambda, _) ->
        let accu = ret_succ_adding accu (base +! word_size) in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | IFailwith (_, ty) -> ret_succ_adding (accu ++ ty_size ty) base
    | ICompare (_, cty, _) ->
        ret_succ_adding (accu ++ ty_size cty) (base +! word_size)
    | IEq (_, _) -> ret_succ_adding accu base
    | INeq (_, _) -> ret_succ_adding accu base
    | ILt (_, _) -> ret_succ_adding accu base
    | IGt (_, _) -> ret_succ_adding accu base
    | ILe (_, _) -> ret_succ_adding accu base
    | IGe (_, _) -> ret_succ_adding accu base
    | IAddress (_, _) -> ret_succ_adding accu base
    | IContract (_, ty, s, _) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base +! Entrypoint.in_memory_size s +! (word_size *? 2))
    | IView (_loc, s, sty, _k) ->
        ret_succ_adding
          (accu ++ view_signature_size s ++ stack_ty_size sty)
          (base +! (word_size *? 2))
    | ITransfer_tokens (_, _) -> ret_succ_adding accu base
    | IImplicit_account (_, _) -> ret_succ_adding accu base
    | ICreate_contract {loc = _; storage_type; code; k = _} ->
        ret_succ_adding
          (accu ++ ty_size storage_type ++ expr_size code)
          (word_size *? 2)
    | ISet_delegate (_, _) -> ret_succ_adding accu base
    | INow (_, _) -> ret_succ_adding accu base
    | IMin_block_time (_, _) -> ret_succ_adding accu base
    | IBalance (_, _) -> ret_succ_adding accu base
    | ILevel (_, _) -> ret_succ_adding accu base
    | ICheck_signature (_, _) -> ret_succ_adding accu base
    | IHash_key (_, _) -> ret_succ_adding accu base
    | IPack (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | IUnpack (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | IBlake2b (_, _) -> ret_succ_adding accu base
    | ISha256 (_, _) -> ret_succ_adding accu base
    | ISha512 (_, _) -> ret_succ_adding accu base
    | ISource (_, _) -> ret_succ_adding accu base
    | ISender (_, _) -> ret_succ_adding accu base
    | ISelf (_, ty, s, _) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base +! (word_size *? 2) +! Entrypoint.in_memory_size s)
    | ISelf_address (_, _) -> ret_succ_adding accu base
    | IAmount (_, _) -> ret_succ_adding accu base
    | ISapling_empty_state (_, _m, _) ->
        ret_succ_adding accu (base +! word_size +! sapling_memo_size_size)
    | ISapling_verify_update (_, _) -> ret_succ_adding accu base
    | ISapling_verify_update_deprecated (_, _) -> ret_succ_adding accu base
    | IDig (_loc, _n, w, _k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size w)
          (base +! (word_size *? 2))
    | IDug (_loc, _n, w, _k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size w)
          (base +! (word_size *? 2))
    | IDipn (_loc, _n, w, _k1, _k2) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size w)
          (base +! (word_size *? 3))
    | IDropn (_loc, _n, w, _k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size w)
          (base +! (word_size *? 2))
    | IChainId (_, _) -> ret_succ_adding accu base
    | INever _loc -> ret_succ_adding accu h1w
    | IVoting_power (_, _) -> ret_succ_adding accu base
    | ITotal_voting_power (_, _) -> ret_succ_adding accu base
    | IKeccak (_, _) -> ret_succ_adding accu base
    | ISha3 (_, _) -> ret_succ_adding accu base
    | IAdd_bls12_381_g1 (_, _) -> ret_succ_adding accu base
    | IAdd_bls12_381_g2 (_, _) -> ret_succ_adding accu base
    | IAdd_bls12_381_fr (_, _) -> ret_succ_adding accu base
    | IMul_bls12_381_g1 (_, _) -> ret_succ_adding accu base
    | IMul_bls12_381_g2 (_, _) -> ret_succ_adding accu base
    | IMul_bls12_381_fr (_, _) -> ret_succ_adding accu base
    | IMul_bls12_381_z_fr (_, _) -> ret_succ_adding accu base
    | IMul_bls12_381_fr_z (_, _) -> ret_succ_adding accu base
    | IInt_bls12_381_fr (_, _) -> ret_succ_adding accu base
    | INeg_bls12_381_g1 (_, _) -> ret_succ_adding accu base
    | INeg_bls12_381_g2 (_, _) -> ret_succ_adding accu base
    | INeg_bls12_381_fr (_, _) -> ret_succ_adding accu base
    | IPairing_check_bls12_381 (_, _) -> ret_succ_adding accu base
    | IComb (_, n, _, _) ->
        ret_succ_adding
          accu
          (base +! (word_size *? 2) +! comb_gadt_witness_size n)
    | IUncomb (_, n, _, _) ->
        ret_succ_adding
          accu
          (base +! (word_size *? 2) +! uncomb_gadt_witness_size n)
    | IComb_get (_, n, _, _) ->
        ret_succ_adding
          accu
          (base +! (word_size *? 2) +! comb_get_gadt_witness_size n)
    | IComb_set (_, n, _, _) ->
        ret_succ_adding
          accu
          (base +! (word_size *? 2) +! comb_set_gadt_witness_size n)
    | IDup_n (_, n, _, _) ->
        ret_succ_adding
          accu
          (base +! (word_size *? 2) +! dup_n_gadt_witness_size n)
    | ITicket (_, cty, _) ->
        ret_succ_adding (accu ++ ty_size cty) (base +! word_size)
    | IRead_ticket (_, ty, _) ->
        ret_succ_adding (accu ++ ty_size ty) (base +! word_size)
    | ISplit_ticket (_, _) -> ret_succ_adding accu base
    | IJoin_tickets (_, cty, _) ->
        ret_succ_adding (accu ++ ty_size cty) (base +! word_size)
    | IOpen_chest (_, _) -> ret_succ_adding accu base
    | IEmit {loc = _; tag; ty; addr = _; k = _} ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base
          +! Entrypoint.in_memory_size tag
          +! (word_size *? 3)
          +! Saturation_repr.safe_int Contract_event.Hash.size)
    | IHalt _ -> ret_succ_adding accu h1w
    | ILog _ ->
        (* This instruction is ignored because it is only used for testing. *)
        accu
  in
  kinstr_traverse t accu {apply}

let lambda_size lam = lambda_size ~count_lambda_nodes:true zero lam

let kinstr_size kinstr = kinstr_size ~count_lambda_nodes:true zero kinstr

let value_size ty x = value_size ~count_lambda_nodes:true zero ty x

module Internal_for_tests = struct
  let ty_size = ty_size

  let kinstr_size = kinstr_size
end
