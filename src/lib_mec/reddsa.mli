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
    end) :
  SIGNATURE_SCHEME with type secret_key = Ec.Scalar.t and type public_key = Ec.t
