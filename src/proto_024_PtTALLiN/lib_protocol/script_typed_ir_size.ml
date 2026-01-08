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
    | Or_t (_ty1, _ty2, a, _) ->
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
    | Sapling_transaction_t m ->
        ret_succ_adding accu
        @@ base_compound_no_meta
           +! Sapling.Memo_size.in_memory_size m
           +! word_size
    | Sapling_transaction_deprecated_t m ->
        ret_succ_adding accu
        @@ base_compound_no_meta
           +! Sapling.Memo_size.in_memory_size m
           +! word_size
    | Sapling_state_t m ->
        ret_succ_adding accu
        @@ base_compound_no_meta
           +! Sapling.Memo_size.in_memory_size m
           +! word_size
    | Ticket_t (_cty, a) ->
        ret_succ_adding accu @@ (base_compound a +! word_size)
  in
  ({apply} : nodes_and_size ty_traverse)

let ty_size : type a ac. (a, ac) ty -> nodes_and_size =
 fun ty -> ty_traverse ty zero ty_traverse_f

(* Types stored for logging are optional and never present in the cache. Therefore
   it's safe not to count them. *)
let ty_for_logging_size : type a ac. (a, ac) ty option -> sint = fun _ty -> !!0

let stack_ty_size s =
  let apply : type a s. nodes_and_size -> (a, s) stack_ty -> nodes_and_size =
   fun accu s ->
    match s with
    | Bot_t -> ret_succ accu
    | Item_t (ty, _) -> ret_succ_adding (accu ++ ty_size ty) h2w
  in
  stack_ty_traverse s zero {apply}

(* Stack types for logging are optional and never present in the cache. Therefore
   it's safe not to count them. One word taken by the [None] tag is already
   accounted for by the call-sites of this function. *)
let stack_ty_for_logging_size : type a s. (a, s) stack_ty option -> sint =
 fun _ -> !!0

let script_nat_size n = Script_int.to_zint n |> z_size

let script_int_size n = Script_int.to_zint n |> z_size

let signature_size (Script_signature.Signature_tag x) =
  match x with
  (* By Obj.reachable_words. *)
  | Ed25519 _ | Secp256k1 _ | P256 _ | Unknown _ -> !!96
  | Bls _ -> !!128

let key_hash_size (_x : Signature.public_key_hash) = !!64
(* By Obj.reachable_words. *)

let public_key_size (x : public_key) =
  h1w
  +?
  match x with
  | Ed25519 _ -> 64
  | Secp256k1 _ -> 72
  | P256 _ -> 96
  | Bls _ -> 64

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

let script_expr_hash_size = !!64

(* Note: this function is NOT tail-recursive, but that's okay, since
   the recursion is bound by the size of the witness, which is an
   11-bit unsigned integer, i.e. at most 2048. This is enough to
   guarantee there will be no stack overflow. *)
let rec stack_prefix_preservation_witness_size_internal : type a b c d e f g h.
    (a, b, c, d, e, f, g, h) stack_prefix_preservation_witness -> nodes_and_size
    = function
  | KPrefix (_loc, ty, w) ->
      ret_succ_adding
        (ty_size ty ++ stack_prefix_preservation_witness_size_internal w)
        h3w
  | KRest -> zero

let stack_prefix_preservation_witness_size (_n : int) w =
  stack_prefix_preservation_witness_size_internal w

let peano_shape_proof =
  let scale = header_size +! h1w in
  fun k -> scale *? k

let comb_gadt_witness_size n (_w : (_, _, _, _, _, _) comb_gadt_witness) =
  peano_shape_proof n

let uncomb_gadt_witness_size n (_w : (_, _, _, _, _, _) uncomb_gadt_witness) =
  peano_shape_proof n

let comb_get_gadt_witness_size n (_w : (_, _) comb_get_gadt_witness) =
  peano_shape_proof n

let comb_set_gadt_witness_size n (_w : (_, _, _) comb_set_gadt_witness) =
  peano_shape_proof n

let dup_n_gadt_witness_size n (_w : (_, _, _, _) dup_n_gadt_witness) =
  peano_shape_proof n

let contract_size : type t. t typed_contract -> nodes_and_size = function
  | Typed_implicit _ -> ret_adding zero (h1w +! public_key_hash_in_memory_size)
  | Typed_implicit_with_ticket {ticket_ty; destination = _} ->
      ret_adding (ty_size ticket_ty) (h2w +! public_key_hash_in_memory_size)
  | Typed_originated {arg_ty; contract_hash = _; entrypoint} ->
      ret_adding
        (ty_size arg_ty)
        (h3w +! blake2b_hash_size +! Entrypoint.in_memory_size entrypoint)
  | Typed_sc_rollup {arg_ty; sc_rollup; entrypoint} ->
      ret_adding
        (ty_size arg_ty)
        (h3w
        +! Sc_rollup.in_memory_size sc_rollup
        +! Entrypoint.in_memory_size entrypoint)
  | Typed_zk_rollup {arg_ty; zk_rollup} ->
      ret_adding (ty_size arg_ty) (h2w +! Zk_rollup.in_memory_size zk_rollup)

let sapling_state_size {Sapling.id; diff; memo_size} =
  h3w
  +! option_size (fun x -> z_size (Sapling.Id.unparse_to_z x)) id
  +! Sapling.diff_in_memory_size diff
  +! Sapling.Memo_size.in_memory_size memo_size

let chain_id_size = !!16 (* by Obj.reachable_words. *)

(* [contents] is handled by the recursion scheme in [value_size]. *)
let ticket_size {ticketer; contents = _; amount} =
  h3w
  +! Contract.in_memory_size ticketer
  +! script_nat_size (amount :> Script_int.n Script_int.num)

let chest_size chest =
  (*
     type chest = {
       locked_value : locked_value;
       ciphertext : ciphertext;
     }
  *)
  let locked_value_size = 256 in
  let ciphertext_size = Script_timelock.get_plaintext_size chest in
  h3w +? (locked_value_size + ciphertext_size)

let chest_key_size _ =
  (*
     type chest_key = {
       vdf_tuple : vdf_tuple; a record of 3 group elements, each of size 256 bytes
       nonce : Z.t;  RSA modulus size (256 bytes) + 128 bits
     }
  *)
  let vdf_tuple_size = 3 * 256 in
  let nonce_size = 256 + 16 in
  h2w +? (vdf_tuple_size + nonce_size)

(* The following mutually recursive functions are mostly
   tail-recursive and the only recursive call that is not a tailcall
   cannot be nested. (See [big_map_size].) For this reason, these
   functions should not trigger stack overflows. *)
let rec value_size : type a ac.
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
    | Signature_t -> ret_succ_adding accu (signature_size x)
    | String_t -> ret_succ_adding accu (script_string_size x)
    | Bytes_t -> ret_succ_adding accu (bytes_size x)
    | Mutez_t -> ret_succ_adding accu mutez_size
    | Key_hash_t -> ret_succ_adding accu (key_hash_size x)
    | Key_t -> ret_succ_adding accu (public_key_size x)
    | Timestamp_t -> ret_succ_adding accu (timestamp_size x)
    | Address_t -> ret_succ_adding accu (address_size x)
    | Bool_t -> ret_succ accu
    | Pair_t (_, _, _, _) -> ret_succ_adding accu h2w
    | Or_t (_, _, _, _) -> ret_succ_adding accu h1w
    | Lambda_t (_, _, _) ->
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes (ret_succ accu) x
    | Option_t (_, _, _) -> ret_succ_adding accu (option_size (fun _ -> !!0) x)
    | List_t (_, _) -> ret_succ_adding accu (h2w +! (h2w *? x.length))
    | Set_t (_, _) ->
        let module M = (val Script_set.get x) in
        let boxing_space =
          !!536
          (* By Obj.reachable_words. *)
        in
        ret_succ_adding accu (boxing_space +! (h4w *? M.size))
    | Map_t (_, _, _) ->
        let module M = (val Script_map.get_module x) in
        let boxing_space =
          !!696
          (* By Obj.reachable_words. *)
        in
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
    | Bls12_381_g1_t -> ret_succ_adding accu !!Bls.Primitive.G1.size_in_memory
    | Bls12_381_g2_t -> ret_succ_adding accu !!Bls.Primitive.G2.size_in_memory
    | Bls12_381_fr_t -> ret_succ_adding accu !!Bls.Primitive.Fr.size_in_memory
    | Ticket_t (_, _) -> ret_succ_adding accu (ticket_size x)
    | Chest_key_t -> ret_succ_adding accu (chest_key_size x)
    | Chest_t -> ret_succ_adding accu (chest_size x)
  in
  value_traverse ty x accu {apply}

and big_map_size : type a b bc.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    a comparable_ty ->
    (b, bc) ty ->
    (a, b) big_map ->
    nodes_and_size =
 fun ~count_lambda_nodes
     accu
     cty
     ty'
     (Big_map {id; diff; key_type; value_type}) ->
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

and lambda_size : type i o.
    count_lambda_nodes:bool -> nodes_and_size -> (i, o) lambda -> nodes_and_size
    =
 fun ~count_lambda_nodes accu lam ->
  let count_lambda_body kdescr node =
    (* We assume that the nodes' size have already been counted if the
       lambda is not a toplevel lambda. *)
    let accu =
      ret_adding
        (accu ++ if count_lambda_nodes then node_size node else zero)
        h2w
    in
    (kdescr_size [@ocaml.tailcall]) ~count_lambda_nodes:false accu kdescr
  in
  match lam with
  | Lam (kdescr, node) -> count_lambda_body kdescr node
  | LamRec (kdescr, node) -> count_lambda_body kdescr node

and kdescr_size : type a s r f.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a, s, r, f) kdescr ->
    nodes_and_size =
 fun ~count_lambda_nodes accu {kloc = _; kbef; kaft; kinstr} ->
  let accu =
    ret_adding (accu ++ stack_ty_size kbef ++ stack_ty_size kaft) h4w
  in
  (kinstr_size [@ocaml.tailcall]) ~count_lambda_nodes accu kinstr

and kinstr_size : type a s r f.
    count_lambda_nodes:bool ->
    nodes_and_size ->
    (a, s, r, f) kinstr ->
    nodes_and_size =
 fun ~count_lambda_nodes accu t ->
  (* To avoid forgetting counting things, the [apply] function below must ignore
     no values (can be checked by grepping \b_\w*\b), except for the [ILog] case.
     Use the [base] function depending on the number of continuations in the
     instruction and only count other fields.
     Location counts as zero because it's an immediate integer.
     Continuations are counted by the [kinstr_traverse] function.
  *)
  let base0 (_loc : Script.location) = h1w in
  let base1 (_loc : Script.location) (_k : (_, _, _, _) kinstr) = h2w in
  let base2 (_loc : Script.location) (_k1 : (_, _, _, _) kinstr)
      (_k2 : (_, _, _, _) kinstr) =
    h3w
  in
  let base3 (_loc : Script.location) (_k1 : (_, _, _, _) kinstr)
      (_k2 : (_, _, _, _) kinstr) (_k3 : (_, _, _, _) kinstr) =
    h4w
  in
  let apply : type a s r f.
      nodes_and_size -> (a, s, r, f) kinstr -> nodes_and_size =
   fun accu t ->
    match t with
    | IDrop (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IDup (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISwap (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IPush (loc, ty, x, k) ->
        let accu = ret_succ_adding accu (base1 loc k +! (word_size *? 2)) in
        (value_size [@ocaml.tailcall])
          ~count_lambda_nodes
          (accu ++ ty_size ty)
          ty
          x
    | IUnit (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICons_pair (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICar (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICdr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IUnpair (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICons_some (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICons_none (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | IIf_none {loc; branch_if_none = k1; branch_if_some = k2; k = k3} ->
        ret_succ_adding accu (base3 loc k1 k2 k3)
    | IOpt_map {loc; body = k1; k = k2} ->
        ret_succ_adding accu (base2 loc k1 k2)
    | ICons_left (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | ICons_right (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | IIf_left {loc; branch_if_left = k1; branch_if_right = k2; k = k3} ->
        ret_succ_adding accu (base3 loc k1 k2 k3)
    | ICons_list (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INil (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | IIf_cons {loc; branch_if_nil = k1; branch_if_cons = k2; k = k3} ->
        ret_succ_adding accu (base3 loc k1 k2 k3)
    | IList_map (loc, k1, ty, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size ty +! word_size)
    | IList_iter (loc, ty, k1, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size ty +! word_size)
    | IList_size (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEmpty_set (loc, cty, k) ->
        ret_succ_adding (accu ++ ty_size cty) (base1 loc k +! word_size)
    | ISet_iter (loc, ty, k1, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size ty +! word_size)
    | ISet_mem (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISet_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISet_size (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEmpty_map (loc, cty, vty, k) ->
        ret_succ_adding
          (accu ++ ty_size cty)
          (base1 loc k +! ty_for_logging_size vty +! (word_size *? 2))
    | IMap_map (loc, ty, k1, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size ty +! word_size)
    | IMap_iter (loc, kvty, k1, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size kvty +! word_size)
    | IMap_mem (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMap_get (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMap_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMap_get_and_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMap_size (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEmpty_big_map (loc, cty, ty, k) ->
        ret_succ_adding
          (accu ++ ty_size cty ++ ty_size ty)
          (base1 loc k +! (word_size *? 2))
    | IBig_map_mem (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBig_map_get (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBig_map_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBig_map_get_and_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IConcat_string (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IConcat_string_pair (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISlice_string (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IString_size (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IConcat_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IConcat_bytes_pair (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISlice_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBytes_size (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILsl_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILsr_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IOr_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAnd_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IXor_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INot_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBytes_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INat_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBytes_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IInt_bytes (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_seconds_to_timestamp (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_timestamp_to_seconds (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISub_timestamp_seconds (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IDiff_timestamps (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_tez (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISub_tez (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISub_tez_legacy (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_teznat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_nattez (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEdiv_teznat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEdiv_tez (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IOr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAnd (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IXor (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INot (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IIs_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INeg (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAbs_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IInt_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISub_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEdiv_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEdiv_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILsl_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILsr_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IOr_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAnd_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAnd_int_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IXor_nat (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INot_int (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IIf {loc; branch_if_true = k1; branch_if_false = k2; k = k3} ->
        ret_succ_adding accu (base3 loc k1 k2 k3)
    | ILoop (loc, k1, k2) -> ret_succ_adding accu (base2 loc k1 k2)
    | ILoop_left (loc, k1, k2) -> ret_succ_adding accu (base2 loc k1 k2)
    | IDip (loc, k1, ty, k2) ->
        ret_succ_adding
          accu
          (base2 loc k1 k2 +! ty_for_logging_size ty +! word_size)
    | IExec (loc, sty, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! stack_ty_for_logging_size sty +! word_size)
    | IApply (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | ILambda (loc, lambda, k) ->
        let accu = ret_succ_adding accu (base1 loc k +! word_size) in
        (lambda_size [@ocaml.tailcall]) ~count_lambda_nodes accu lambda
    | IFailwith (loc, ty) ->
        ret_succ_adding (accu ++ ty_size ty) (base0 loc +! word_size)
    | ICompare (loc, cty, k) ->
        ret_succ_adding (accu ++ ty_size cty) (base1 loc k +! word_size)
    | IEq (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INeq (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILt (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IGt (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILe (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IGe (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAddress (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IContract (loc, ty, s, k) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base1 loc k +! Entrypoint.in_memory_size s +! (word_size *? 2))
    | IView (loc, s, sty, k) ->
        ret_succ_adding
          (accu ++ view_signature_size s)
          (base1 loc k +! stack_ty_for_logging_size sty +! (word_size *? 2))
    | ITransfer_tokens (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IImplicit_account (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IIs_implicit_account (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IIndex_address (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IGet_address_index (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICreate_contract {loc; storage_type; code; k} ->
        ret_succ_adding
          (accu ++ ty_size storage_type ++ expr_size code)
          (base1 loc k +! (word_size *? 2))
    | ISet_delegate (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INow (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMin_block_time (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IBalance (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ILevel (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ICheck_signature (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IHash_key (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IPack (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | IUnpack (loc, ty, k) ->
        ret_succ_adding (accu ++ ty_size ty) (base1 loc k +! word_size)
    | IBlake2b (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISha256 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISha512 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISource (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISender (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISelf (loc, ty, s, k) ->
        ret_succ_adding
          (accu ++ ty_size ty)
          (base1 loc k +! (word_size *? 2) +! Entrypoint.in_memory_size s)
    | ISelf_address (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAmount (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISapling_empty_state (loc, m, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! word_size +! Sapling.Memo_size.in_memory_size m)
    | ISapling_verify_update (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISapling_verify_update_deprecated (loc, k) ->
        ret_succ_adding accu (base1 loc k)
    | IDig (loc, n, w, k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size n w)
          (base1 loc k +! (word_size *? 2))
    | IDug (loc, n, w, k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size n w)
          (base1 loc k +! (word_size *? 2))
    | IDipn (loc, n, w, k1, k2) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size n w)
          (base2 loc k1 k2 +! (word_size *? 2))
    | IDropn (loc, n, w, k) ->
        ret_succ_adding
          (accu ++ stack_prefix_preservation_witness_size n w)
          (base1 loc k +! (word_size *? 2))
    | IChainId (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INever loc -> ret_succ_adding accu (base0 loc)
    | IVoting_power (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ITotal_voting_power (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IKeccak (loc, k) -> ret_succ_adding accu (base1 loc k)
    | ISha3 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_bls12_381_g1 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_bls12_381_g2 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IAdd_bls12_381_fr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_bls12_381_g1 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_bls12_381_g2 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_bls12_381_fr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_bls12_381_z_fr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IMul_bls12_381_fr_z (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IInt_bls12_381_fr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INeg_bls12_381_g1 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INeg_bls12_381_g2 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | INeg_bls12_381_fr (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IPairing_check_bls12_381 (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IComb (loc, n, w, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! (word_size *? 2) +! comb_gadt_witness_size n w)
    | IUncomb (loc, n, w, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! (word_size *? 2) +! uncomb_gadt_witness_size n w)
    | IComb_get (loc, n, w, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! (word_size *? 2) +! comb_get_gadt_witness_size n w)
    | IComb_set (loc, n, w, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! (word_size *? 2) +! comb_set_gadt_witness_size n w)
    | IDup_n (loc, n, w, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! (word_size *? 2) +! dup_n_gadt_witness_size n w)
    | ITicket (loc, cty, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! ty_for_logging_size cty +! word_size)
    | ITicket_deprecated (loc, cty, k) ->
        ret_succ_adding
          accu
          (base1 loc k +! ty_for_logging_size cty +! word_size)
    | IRead_ticket (loc, ty, k) ->
        ret_succ_adding accu (base1 loc k +! ty_for_logging_size ty +! word_size)
    | ISplit_ticket (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IJoin_tickets (loc, cty, k) ->
        ret_succ_adding (accu ++ ty_size cty) (base1 loc k +! word_size)
    | IOpen_chest (loc, k) -> ret_succ_adding accu (base1 loc k)
    | IEmit {loc; tag; ty; unparsed_ty; k} ->
        ret_succ_adding
          (accu ++ ty_size ty ++ expr_size unparsed_ty)
          (base1 loc k +! Entrypoint.in_memory_size tag +! (word_size *? 3))
    | IHalt loc -> ret_succ_adding accu (base0 loc)
    | ILog _ ->
        (* This instruction is ignored because it is only used for testing.
           Keep this case at the end. *)
        accu
  in
  kinstr_traverse t accu {apply}

let lambda_size lam = lambda_size ~count_lambda_nodes:true zero lam

let kinstr_size kinstr = kinstr_size ~count_lambda_nodes:true zero kinstr

let value_size ty x = value_size ~count_lambda_nodes:true zero ty x

module Internal_for_tests = struct
  let ty_size = ty_size

  let kinstr_size = kinstr_size

  let stack_prefix_preservation_witness_size =
    stack_prefix_preservation_witness_size_internal
end
