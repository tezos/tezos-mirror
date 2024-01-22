(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Specification for Ed25519 is given in RFC 8032
   https://www.rfc-editor.org/rfc/rfc8032.txt *)
module Ed25519 = struct
  module Curve = Mec.Curve.Curve25519.AffineEdwards

  module P : sig
    type sk = Bytes.t

    type pk = Curve.t

    type signature = {r : Curve.t; s : bool list}

    type msg = Bytes.t

    val point_of_compressed_bytes_opt : Bytes.t -> Curve.t option

    val point_of_compressed_bytes_exn : Bytes.t -> Curve.t

    val scalar_of_bytes_exn : Bytes.t -> bool list

    val neuterize : sk -> pk

    val sign : sk -> msg -> signature

    val verify : msg -> pk -> signature -> bool
  end = struct
    module H = Hacl_star.Hacl.SHA2_512

    type sk = Bytes.t

    type pk = Curve.t

    type signature = {r : Curve.t; s : bool list}

    type msg = Bytes.t

    let recover_x y sign =
      let y2 = Curve.Base.mul y y in
      let x2 = Curve.Base.((y2 + negate one) / ((Curve.d * y2) + one)) in
      if Curve.Base.is_zero x2 then if sign = 0 then Some x2 else None
      else
        let x_opt = Curve.Base.sqrt_opt x2 in
        match x_opt with
        | None -> None
        | Some x ->
            let x_sign = Z.(Curve.Base.to_z x mod of_int 2) |> Z.to_int in
            let x = if x_sign <> sign then Curve.Base.negate x else x in
            Some x

    let point_of_compressed_bytes_opt bs =
      let bs = Bytes.copy bs in
      let len = Bytes.length bs in
      if len <> 32 then None
      else
        let last_byte = int_of_char @@ Bytes.get bs (len - 1) in
        let px_sign = last_byte lsr 7 in
        let last_byte_without_sign = last_byte land 0b01111111 in
        Bytes.set bs (len - 1) (char_of_int last_byte_without_sign) ;
        let yn = Z.of_bits (Bytes.to_string bs) in
        if yn >= Curve.Base.order then None
        else
          let py = Curve.Base.of_bytes_opt bs in
          match py with
          | None -> None
          | Some y -> (
              let px = recover_x y px_sign in
              match px with
              | None -> None
              | Some x ->
                  (* NOTE: Curve.from_coordinates_opt also checks
                     if a point is in the subgroup *)
                  Curve.from_coordinates_opt ~u:x ~v:y)

    let point_of_compressed_bytes_exn bs =
      match point_of_compressed_bytes_opt bs with
      | None ->
          raise
          @@ Failure
               (Format.sprintf
                  "point_of_compressed_bytes_exn: cannot recover a point from \
                   %s"
                  (Hex.show (Hex.of_bytes bs)))
      | Some p -> p

    (* nat_to_bytes_le 32 (pow2 255 * (x % 2) + y) *)
    let point_to_compressed_bytes p =
      let px = Curve.get_u_coordinate p |> Curve.Base.to_z in
      let py = Curve.get_v_coordinate p |> Curve.Base.to_z in
      let px_sign = Z.(px mod of_int 2) in
      let res = Z.(((one lsl 255) * px_sign) + py) in
      Bytes.of_string @@ Z.to_bits res

    let scalar_of_curve_scalar s =
      Curve.Scalar.to_z s
      |> Utils.bool_list_of_z ~nb_bits:(Z.numbits Curve.Scalar.order)

    let scalar_of_bytes_exn s =
      let sn = Z.of_bits (Bytes.to_string s) in
      if sn < Curve.Scalar.order then
        Curve.Scalar.of_bytes_exn s |> scalar_of_curve_scalar
      else
        raise
        @@ Failure
             (Format.sprintf
                "scalar_of_bytes_exn: scalar is not less than the order %s"
                (Hex.show (Hex.of_bytes s)))

    (* Compute the expanded keys for the EdDSA signature *)
    let expand_keys sk =
      assert (Bytes.length sk = 32) ;
      (* h <- (h_0, h_1, ..., h_{2b-1}) <- H (sk) *)
      let h = H.hash sk in
      let b = Bytes.length h / 2 in
      let h_low = Bytes.sub h 0 b in
      let h_high = Bytes.sub h b b in
      (* s <- 2^n + \sum_i h_i * 2^i for c <= i < n,
         where Curve.cofactor = 2^c and c <= n < b.
         For Ed25519, c = 3 and n = 254 *)
      let s =
        Bytes.set_uint8 h_low 0 (Int.logand (Bytes.get_uint8 h_low 0) 248) ;
        Bytes.set_uint8
          h_low
          31
          (Int.logor (Int.logand (Bytes.get_uint8 h_low 31) 127) 64) ;
        Curve.Scalar.of_bytes_exn h_low
      in
      (* pk <- [s]G *)
      let pk = Curve.mul Curve.one s in
      (s, pk, h_high)

    let neuterize sk =
      let _s, pk, _prefix = expand_keys sk in
      pk

    (* h <- H (compressed (R) || compressed (pk) || msg ) mod Curve.Scalar.order *)
    let compute_h msg pk r =
      let r = point_to_compressed_bytes r in
      let pk = point_to_compressed_bytes pk in
      H.hash (Bytes.concat Bytes.empty [r; pk; msg])
      |> Curve.Scalar.of_bytes_exn

    let sign sk msg =
      let s, pk, prefix = expand_keys sk in
      (* r <- H (prefix || msg) *)
      let r = H.hash (Bytes.cat prefix msg) |> Curve.Scalar.of_bytes_exn in
      (* R <- [r]G *)
      let sig_r = Curve.mul Curve.one r in
      (* h <- H (compressed (R) || compressed (pk) || msg ) *)
      let h = compute_h msg pk sig_r in
      (* s <- (r + h * s) mod Curve.Scalar.order *)
      let sig_s = Curve.Scalar.(r + (h * s)) |> scalar_of_curve_scalar in
      {r = sig_r; s = sig_s}

    (* the fact that pk & r are on curve is enforced by the type invariant of Curve.t *)
    let verify msg pk signature =
      (* h <- H (compressed (R) || compressed (pk) || msg ) *)
      let h = compute_h msg pk signature.r in
      let sig_s = Utils.bool_list_to_z signature.s in
      if sig_s < Curve.Scalar.order then
        (* [s]G =?= R + [h]pk *)
        Curve.(
          eq
            (mul Curve.one (Curve.Scalar.of_z sig_s))
            (add signature.r (mul pk h)))
      else false
  end

  open Lang_core
  open Lang_stdlib

  module V : functor (L : LIB) -> sig
    open L
    open Gadget_edwards25519.MakeEdwards25519(L)

    type pk = point

    type signature = {r : point repr; s : bool list repr}

    module Encoding : sig
      open L.Encodings

      val pk_encoding : (Curve.t, pk repr, pk) encoding

      val signature_encoding : (P.signature, signature, pk * bool list) encoding
    end

    val verify : Bytes.tl repr -> pk repr -> signature -> bool repr t
  end =
  functor
    (L : LIB)
    ->
    struct
      open L
      include Gadget_edwards25519.MakeEdwards25519 (L)
      module H = Gadget_sha2.SHA512 (L)

      type pk = point

      type signature = {r : point repr; s : bool list repr}

      module Encoding = struct
        open L.Encodings

        let pk_encoding = point_encoding

        let signature_encoding =
          conv
            (fun {r; s} -> (r, s))
            (fun (r, s) -> {r; s})
            (fun ({r; s} : P.signature) -> (r, s))
            (fun (r, s) -> {r; s})
            (obj2_encoding point_encoding (atomic_list_encoding bool_encoding))
      end

      (* h <- H (compressed (R) || compressed (pk) || msg ) *)
      let compute_h msg pk r =
        (* Ed25519 works with little-endian representation
           but SHA-512 with big-endian one *)
        let bytes_change_endianness b =
          ret @@ to_list (Utils.bool_list_change_endianness (of_list b))
        in
        with_label ~label:"Ed25519.compute_h"
        @@ let* r_bytes = to_compressed_bytes r in
           let* pk_bytes = to_compressed_bytes pk in
           let* r_pk_msg =
             bytes_change_endianness @@ Bytes.concat [|msg; pk_bytes; r_bytes|]
           in
           let* h = H.digest r_pk_msg in
           bytes_change_endianness h

      (* the fact that pk & r are on curve is enforced by point encodings *)
      let verify msg pk signature =
        let {r; s} = signature in
        (* range_check checks if x is in [0; 2^n), n = 253 *)
        (* s <= Curve.Scalar.order - 1 <==> 0 <= Curve.Scalar.order - 1 - s *)
        let* sb = Num.scalar_of_bytes s in
        let* order_minus_s =
          Num.add
            ~ql:S.(negate one)
            ~qr:S.zero
            ~qc:S.(of_z Curve.Scalar.order + negate one)
            sb
            sb
        in
        Num.range_check ~nb_bits:253 order_minus_s
        >* with_label ~label:"Ed25519.verify"
           @@ (* h <- H (compressed (R) || compressed (pk) || msg ) *)
           let* h = compute_h msg pk r in
           (* NOTE: we do not reduce a result of compute_h modulo Curve.Scalar.order *)
           with_label ~label:"Ed25519.scalar_mul"
           (* we can use multi_scalar_mul once h is reduced:
              [s]G =?= R + [h]pk <==> R =?= [s]G - [h]pk *)
           @@ let* base_point in
              let* sg = scalar_mul s base_point in
              let* hpk = scalar_mul h pk in
              let* rhpk = add r hpk in
              with_label ~label:"Ed25519.check" @@ equal sg rhpk
    end
end
