(* The MIT License (MIT)
 *
 * Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE. *)

(* Signature that hides the Bytes.t implementation *)
module type T = sig
  type ask (* 32 bytes *)

  type ak (* 32 *)

  type nsk (* 32 *)

  type nk (* 32 *)

  type ovk (* 32 *)

  type diversifier (* 11 *)

  type pkd (* 32 *)

  type nullifier (* 32 *)

  type commitment (* 32 *)

  type epk (* 32 *)

  type symkey (* 32 *)

  type sighash (* 32 *)

  type spend_sig (* 64 *)

  type hash (* 32 *)

  type cv (* 32 *)

  type rk (* 32 *)

  type spend_proof (* 48 + 48 + 96 *)

  type binding_sig (* 64 *)

  type output_proof (* 48 + 48 + 96 *)

  type ivk (* 32 *)

  type ar (* 32 *)

  type rcm (* 32 *)

  type esk (* 32 *)

  type diversifier_index (* 11 *)

  (*96 bytes*)
  type expanded_spending_key = {ask : ask; nsk : nsk; ovk : ovk}

  (* 169 bytes *)
  (* this is an extended_spending_key that can be used to derive more
       keys using zip32*)
  type zip32_expanded_spending_key = {
    depth : Bytes.t;
    (*  1 byte  *)
    parent_fvk_tag : Bytes.t;
    (*  4 bytes *)
    child_index : Bytes.t;
    (*  4 bytes *)
    chain_code : Bytes.t;
    (* 32 bytes *)
    expsk : expanded_spending_key;
    (* 96 bytes *)
    dk : Bytes.t; (* 32 bytes *)
  }

  (* 96 bytes*)
  type full_viewing_key = {ak : ak; nk : nk; ovk : ovk}

  (* 169 bytes *)
  (* this is an extended_full_viewing_key that can be used to derive more
     keys using zip32 *)
  type zip32_full_viewing_key = {
    depth : Bytes.t;
    (*  1 byte  *)
    parent_fvk_tag : Bytes.t;
    (*  4 bytes *)
    child_index : Bytes.t;
    (*  4 bytes *)
    chain_code : Bytes.t;
    (* 32 bytes *)
    fvk : full_viewing_key;
    (* 96 bytes *)
    dk : Bytes.t; (* 32 bytes *)
  }

  val to_nk : Bytes.t -> nk

  val to_ak : Bytes.t -> ak

  val to_ask : Bytes.t -> ask

  val to_nsk : Bytes.t -> nsk

  val to_pkd : Bytes.t -> pkd

  val to_ovk : Bytes.t -> ovk

  val to_nullifier : Bytes.t -> nullifier

  val to_commitment : Bytes.t -> commitment

  val to_symkey : Bytes.t -> symkey

  val to_epk : Bytes.t -> epk

  val to_spend_sig : Bytes.t -> spend_sig

  val to_hash : Bytes.t -> hash

  val to_cv : Bytes.t -> cv

  val to_rk : Bytes.t -> rk

  val to_spend_proof : Bytes.t -> spend_proof

  val to_output_proof : Bytes.t -> output_proof

  val to_sighash : Bytes.t -> sighash

  val to_binding_sig : Bytes.t -> binding_sig

  val to_diversifier : Bytes.t -> diversifier option

  val to_diversifier_index : Bytes.t -> diversifier_index

  val to_ar : Bytes.t -> ar

  val to_rcm : Bytes.t -> rcm

  val to_esk : Bytes.t -> esk

  val to_ivk : Bytes.t -> ivk

  val to_expanded_spending_key : Bytes.t -> expanded_spending_key

  val to_zip32_expanded_spending_key : Bytes.t -> zip32_expanded_spending_key

  val to_full_viewing_key : Bytes.t -> full_viewing_key

  val to_zip32_full_viewing_key : Bytes.t -> zip32_full_viewing_key

  val of_nk : nk -> Bytes.t

  val of_ak : ak -> Bytes.t

  val of_ask : ask -> Bytes.t

  val of_nsk : nsk -> Bytes.t

  val of_pkd : pkd -> Bytes.t

  val of_ovk : ovk -> Bytes.t

  val of_nullifier : nullifier -> Bytes.t

  val of_commitment : commitment -> Bytes.t

  val of_symkey : symkey -> Bytes.t

  val of_epk : epk -> Bytes.t

  val of_spend_sig : spend_sig -> Bytes.t

  val of_hash : hash -> Bytes.t

  val of_cv : cv -> Bytes.t

  val of_rk : rk -> Bytes.t

  val of_spend_proof : spend_proof -> Bytes.t

  val of_output_proof : output_proof -> Bytes.t

  val of_sighash : sighash -> Bytes.t

  val of_binding_sig : binding_sig -> Bytes.t

  val of_diversifier : diversifier -> Bytes.t

  val of_diversifier_index : diversifier_index -> Bytes.t

  val of_ar : ar -> Bytes.t

  val of_rcm : rcm -> Bytes.t

  val of_esk : esk -> Bytes.t

  val of_ivk : ivk -> Bytes.t

  val of_expanded_spending_key : expanded_spending_key -> Bytes.t

  val of_zip32_expanded_spending_key : zip32_expanded_spending_key -> Bytes.t

  val of_full_viewing_key : full_viewing_key -> Bytes.t

  val of_zip32_full_viewing_key : zip32_full_viewing_key -> Bytes.t

  val hash_compare : hash -> hash -> int

  val hash_of_commitment : commitment -> hash

  val commitment_of_hash : hash -> commitment
end
