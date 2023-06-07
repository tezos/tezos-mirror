(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2021 Danny Willems <be.danny.willems@gmail.com>             *)
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

module type SIGNATURE_SCHEME = sig
  type secret_key

  type public_key

  type signature

  val signature_to_bytes : signature -> Bytes.t

  val sign : secret_key -> Bytes.t -> signature

  val sign_deterministic : Bytes.t -> secret_key -> Bytes.t -> signature

  val verify : public_key -> Bytes.t -> signature -> bool
end

module MakeRedDSA
    (Ec : Ec_sig.AffineEdwardsT) (Param : sig
      val length : int

      val hash : Bytes.t -> Bytes.t

      val generator : Ec.t

      val to_compressed : Ec.t -> Bytes.t

      val of_compressed_opt : Bytes.t -> Ec.t option
    end) =
struct
  let () =
    if Param.length mod 8 <> 0 then failwith "Length must be a multiple of 8"

  let hash_star bytes =
    let limbs = Param.hash bytes in
    Ec.Scalar.of_bytes_exn limbs

  type secret_key = Ec.Scalar.t

  type public_key = Ec.t

  type signature = {r : Bytes.t; s : Bytes.t}

  let signature_to_bytes signature =
    Bytes.concat Bytes.empty [signature.r; signature.s]

  let sign_deterministic randomness sk message =
    (* length is given in bits *)
    let length = (Param.length + 128) / 8 in
    assert (Bytes.length randomness = length) ;
    (* IMPORTANT!!!
       r = H*(T || vk || M) --> This is the spec in the Sapling PDF. However,
       the reference implementation do not use the vk:
       - https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/redjubjub.rs#L80
       - https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/util.rs#L9
       - https://github.com/zcash/librustzcash/issues/179
    *)
    let r = hash_star (Bytes.concat Bytes.empty [randomness; message]) in
    (* R = r . P *)
    let p_r = Ec.mul Param.generator r in
    (* Get the little endian encoding of the point R *)
    let p_r_le = Param.to_compressed p_r in
    (* s = r + sk . H*(LE(P) || M) *)
    let s = hash_star (Bytes.concat Bytes.empty [p_r_le; message]) in
    let s = Ec.Scalar.(sk * s) in
    let s = Ec.Scalar.(r + s) in
    (* R || S *)
    {r = p_r_le; s = Ec.Scalar.to_bytes s}

  let sign sk message =
    (* Generate T, length + 128 bits *)
    let t =
      Bytes.init
        ((Param.length + 128) / 8)
        (fun _ -> char_of_int (Random.int 256))
    in
    sign_deterministic t sk message

  let verify vk message signature =
    (* we do not have to check the first condition as it is an invariant of the
       type signature
    *)
    let r = Param.of_compressed_opt signature.r in
    let s = Z.of_bits (Bytes.to_string signature.s) in
    (* The Sapling PDF does use vk in the hash, but not the reference
       implementation of zcash. Therefore, ignoring it
       - https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/redjubjub.rs#L80
       - https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/util.rs#L9
       - https://github.com/zcash/librustzcash/issues/179
    *)
    let c = hash_star (Bytes.concat Bytes.empty [signature.r; message]) in
    (* FIXME: not constant time!! Like everything... *)
    match r with
    | None -> false
    | Some r ->
        let s_leq_r = Z.(leq s Ec.Scalar.order) in
        (* [c] * vk *)
        let c_vk = Ec.(mul vk c) in
        (* [S] * P_G *)
        let s_p_g = Ec.(mul Param.generator (Scalar.of_z s)) in
        (* -[S] * P_G + R + [c] vk *)
        let res = Ec.(add (add (negate s_p_g) r) c_vk) in
        (* multiply by the cofactor -> [h_G] res *)
        let res = Ec.(mul res (Scalar.of_z cofactor)) in
        let res_is_null = Ec.is_zero res in
        s_leq_r && res_is_null
end
