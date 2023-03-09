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

(* Reference implementation:
   https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/sapling/group_hash.rs
*)

(* https://github.com/zcash/librustzcash/blob/da431a0eb207f69c9b0631d7d02136d819e1bfd9/zcash_primitives/src/constants.rs#L12 *)
let gh_first_block =
  "096b36a5804bfacef1691e173c366a47ff5ba84a44f26ddd7e8d9f79d5b42df0"

module Blake2s = Mec_digestif.Make_BLAKE2S (struct
  let digest_size = 32
end)

let group_hash message personalisation =
  let h = Blake2s.init ~personalisation () in
  let h = Blake2s.feed_string h gh_first_block in
  let h = Blake2s.feed_bytes h message in
  let hash_hex = `Hex Blake2s.(to_hex (get h)) in
  let hash_hex = Hex.to_bytes hash_hex in
  let p_opt = Jubjub.AffineEdwards.of_compressed_opt hash_hex in
  match p_opt with
  | None -> None
  | Some p ->
      let p = Jubjub.AffineEdwards.(mul p (Scalar.of_z cofactor)) in
      if Jubjub.AffineEdwards.is_zero p then None else Some p

let find_group_hash message personalisation =
  let rec aux i =
    let message =
      Bytes.concat Bytes.empty [message; Bytes.make 1 (char_of_int i)]
    in
    let p = group_hash message personalisation in
    match p with None -> aux (i + 1) | Some p -> p
  in
  aux 0
