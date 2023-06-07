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

module Make (Param : sig
  val generator : Jubjub.AffineEdwards.t
end) =
  Reddsa.MakeRedDSA
    (Jubjub.AffineEdwards)
    (struct
      let length = 512

      let generator = Param.generator

      module Blake2b = Mec_digestif.Make_BLAKE2B (struct
        let digest_size = 64
      end)

      let hash m =
        let ctx =
          Blake2b.init ~personalisation:(Bytes.of_string "Zcash_RedJubjubH") ()
        in
        let ctx = Blake2b.feed_bytes ctx m in
        let res_hexa = Blake2b.to_hex (Blake2b.get ctx) in
        let res_hexa_hex = `Hex res_hexa in
        let res_bytes = Hex.to_bytes res_hexa_hex in
        res_bytes

      let to_compressed = Jubjub.AffineEdwards.to_compressed

      let of_compressed_opt = Jubjub.AffineEdwards.of_compressed_opt
    end)
