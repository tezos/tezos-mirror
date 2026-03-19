(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

type transaction = Sapling.UTXO.transaction

let transaction_encoding = Sapling.UTXO.transaction_encoding

(* The two data structures in the state are all ordered by position, a diff
   contains the elements starting from an offset position up to the most recent
   position. A diff can be applied to a state stored in a context to obtain a
   new state.
   Diffs are used by the Michelson interpreter during the evaluation of smart
   contracts to keep a temporary state that may be discarded.
   Diffs are also returned by an RPC to allow a client to synchronize its own
   state with the chain.
*)
type diff = {
  commitments_and_ciphertexts :
    (Sapling.Commitment.t * Sapling.Ciphertext.t) list;
  nullifiers : Sapling.Nullifier.t list;
}

let diff_encoding =
  let open Data_encoding in
  conv
    (fun d -> (d.commitments_and_ciphertexts, d.nullifiers))
    (fun (commitments_and_ciphertexts, nullifiers) ->
      (match commitments_and_ciphertexts with
      | [] -> ()
      | (_cm_hd, ct_hd) :: rest ->
          let memo_size = Sapling.Ciphertext.get_memo_size ct_hd in
          List.iter
            (fun (_cm, ct) ->
              assert (
                Compare.Int.(Sapling.Ciphertext.get_memo_size ct = memo_size)))
            rest) ;
      {commitments_and_ciphertexts; nullifiers})
    (obj2
       (req
          "commitments_and_ciphertexts"
          (list (tup2 Sapling.Commitment.encoding Sapling.Ciphertext.encoding)))
       (req "nullifiers" (list Sapling.Nullifier.encoding)))

module Memo_size = struct
  type t = int

  let encoding = Data_encoding.uint16

  let equal = Compare.Int.( = )

  let max_uint16 = 0xffff

  let max_uint16_z = Z.of_int max_uint16

  let err =
    Error
      ("a positive 16-bit integer (between 0 and " ^ string_of_int max_uint16
     ^ ")")

  let parse_z z =
    if Compare.Z.(Z.zero <= z) && Compare.Z.(z <= max_uint16_z) then
      Ok (Z.to_int z)
    else err

  let unparse_to_z = Z.of_int

  let in_memory_size (_ : t) =
    let open Cache_memory_helpers in
    !!0
end

let transaction_get_memo_size (transaction : Sapling.UTXO.transaction) =
  match transaction.outputs with
  | [] -> None
  | {ciphertext; _} :: _ ->
      (* Encoding ensures all ciphertexts have the same memo size. *)
      Some (Sapling.Ciphertext.get_memo_size ciphertext)

open Cache_memory_helpers

(* This should be exported by [lib_sapling] rather than implemented here. *)
let input_in_memory_size =
  (* type input =
   *   Sapling.UTXO.input = {
   *   cv : Sapling.CV.t;
   *   nf : Sapling.Nullifier.t;
   *   rk : Sapling.UTXO.rk;
   *   proof_i : Sapling.UTXO.spend_proof;
   *   signature : Sapling.UTXO.spend_sig;
   * } *)
  let cv_size = string_size_gen 32 in
  let nf_size = string_size_gen 32 in
  let rk_size = string_size_gen 32 in
  let proof_i_size = string_size_gen @@ (48 + 96 + 48) in
  let signature_size = string_size_gen 64 in
  header_size +! (word_size *? 5) +! cv_size +! nf_size +! rk_size
  +! proof_i_size +! signature_size

let ciphertext_size =
  (* type t = {
   *   cv : CV.t;
   *   epk : DH.epk;
   *   payload_enc : Bytes.t;
   *   nonce_enc : Crypto_box.nonce;
   *   payload_out : Bytes.t;
   *   nonce_out : Crypto_box.nonce;
   * } *)
  let cv_size = string_size_gen 32 in
  let epk_size = string_size_gen 32 in
  let nonce_enc_size =
    string_size_gen 24
    (* from lib_hacl/hacl.ml:Nonce.size *)
  in
  let payload_out_size =
    string_size_gen (32 + 32 + 16)
    (* from lib_sapling/core.ml:Ciphertext.encoding *)
  in
  let nonce_out_size = string_size_gen 24 in
  let fixed_payload_data_size =
    11 + 8 + 32 + 16 + 4
    (* from lib_sapling/core.ml:Ciphertext.get_memo_size *)
  in

  fun memo_size ->
    let payload_size = string_size_gen (memo_size + fixed_payload_data_size) in
    header_size +! (word_size *? 6) +! cv_size +! epk_size +! payload_size
    +! nonce_enc_size +! payload_out_size +! nonce_out_size

let output_in_memory_size =
  (* type output = {
   *   cm : Commitment.t;
   *   proof_o : output_proof;
   *   ciphertext : Ciphertext.t;
   * } *)
  let cm_size = string_size_gen 32 in
  let proof_o_size = string_size_gen @@ (48 + 96 + 48) in
  let ciphertext_size = ciphertext_size in

  fun memo_size ->
    header_size +! (word_size *? 3) +! cm_size +! proof_o_size
    +! ciphertext_size memo_size

(** Returns an approximation of the in-memory size of a Sapling transaction.  *)
let transaction_in_memory_size (transaction : Sapling.UTXO.transaction) =
  (* type transaction =
   *   transaction = {
   *   inputs : Sapling.UTXO.input list;
   *   outputs : Sapling.UTXO.output list;
   *   binding_sig : Sapling.UTXO.binding_sig;
   *   balance : int64;
   *   root : Sapling.Hash.t;
   * } *)
  let binding_sig_size = string_size_gen 64 in
  let balance_size = int64_size in
  let root_size = string_size_gen 32 in
  let inputs = List.length transaction.inputs in
  let outputs = List.length transaction.outputs in
  let memo_size =
    Option.value ~default:0 (transaction_get_memo_size transaction)
  in
  let bound_data_size = string_size transaction.bound_data in
  header_size +! (word_size *? 5)
  +! (list_cell_size input_in_memory_size *? inputs)
  +! (list_cell_size (output_in_memory_size memo_size) *? outputs)
  +! binding_sig_size +! balance_size +! root_size +! bound_data_size

(** Returns an approximation of the in-memory size of a Sapling diff.  *)
let diff_in_memory_size ({commitments_and_ciphertexts; nullifiers} : diff) =
  let cms_and_cts = List.length commitments_and_ciphertexts in
  let nfs = List.length nullifiers in
  let cm_size = string_size_gen 32 in
  let nf_size = string_size_gen 32 in
  let memo_size =
    (* All memo_size in a diff should be equal (see invariant enforced by
       [diff] encoding above) *)
    match commitments_and_ciphertexts with
    | [] -> 0
    | (_, ct) :: _ -> Sapling.Ciphertext.get_memo_size ct
  in
  header_size +! (word_size *? 2)
  +! list_cell_size (boxed_tup2 cm_size (ciphertext_size memo_size))
     *? cms_and_cts
  +! (list_cell_size nf_size *? nfs)
