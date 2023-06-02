(*****************************************************************************)
(*                                                                           *)
(* MIT License                                                               *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

module Make
    (Curve : Mec.CurveSig.AffineEdwardsT) (H : sig
      module P : Hash_sig.P_HASH

      module V : Hash_sig.HASH
    end) =
struct
  module Curve = Curve
  open Lang_core

  (* vanilla implementation of Schnorr signature using ec-jubjub
     * general idea based on
     * https://github.com/dusk-network/schnorr/blob/main/src/key_variants/single_key.rs *)
  module P : sig
    type pk = Curve.t

    type signature = {
      sig_u_bytes : bool list;
      sig_r : Curve.t;
      c_bytes : bool list;
    }

    type sk = Curve.Scalar.t

    val neuterize : sk -> pk

    val sign : ?compressed:bool -> sk -> S.t -> Curve.Scalar.t -> signature

    val verify :
      ?compressed:bool ->
      msg:S.t ->
      pk:pk ->
      signature:signature ->
      unit ->
      bool
  end = struct
    module H = H.P

    (* S.t and Curve.t are the same but Curve.t is abstract *)
    let of_bls_scalar s = S.of_z (Curve.Base.to_z s)

    type sk = Curve.Scalar.t

    type pk = Curve.t

    type signature = {
      sig_u_bytes : bool list;
      sig_r : Curve.t;
      c_bytes : bool list;
    }

    let neuterize sk = Curve.mul Curve.one sk

    let bls_scalar_to_curve_scalar s = Curve.Scalar.of_z (S.to_z s)

    let hash_full a =
      let ctx = H.init () in
      let ctx = H.digest ctx a in
      H.get ctx

    let sign ?(compressed = false) sk msg rand =
      let r = Curve.mul Curve.one rand in
      let r_u = Curve.get_u_coordinate r |> of_bls_scalar in
      let r_v = Curve.get_v_coordinate r |> of_bls_scalar in
      let c =
        if compressed then H.direct ~input_length:2 [|r_u; msg|]
        else hash_full [|r_u; r_v; msg|]
      in

      let u =
        Curve.Scalar.sub
          rand
          (Curve.Scalar.mul (bls_scalar_to_curve_scalar c) sk)
      in
      let nb_bits_base = Z.numbits S.order in
      let sig_u_bytes =
        Utils.bool_list_of_z ~nb_bits:nb_bits_base (Curve.Scalar.to_z u)
      in
      let c_bytes = Utils.bool_list_of_z ~nb_bits:nb_bits_base (S.to_z c) in
      {sig_u_bytes; sig_r = r; c_bytes}

    let verify ?(compressed = false) ~msg ~pk ~signature () =
      let sig_u =
        Curve.Scalar.of_z @@ Utils.bool_list_to_z signature.sig_u_bytes
      in
      let sig_c = Utils.bool_list_to_scalar signature.c_bytes in
      let sig_r_u = Curve.get_u_coordinate signature.sig_r |> of_bls_scalar in
      let sig_r_v = Curve.get_v_coordinate signature.sig_r |> of_bls_scalar in
      let c =
        if compressed then H.direct ~input_length:2 [|sig_r_u; msg|]
        else hash_full [|sig_r_u; sig_r_v; msg|]
      in
      let c_check = S.eq c sig_c in
      let challenge_r =
        Curve.add
          (Curve.mul Curve.one sig_u)
          (Curve.mul
             pk
             (Curve.Scalar.of_z @@ Utils.bool_list_to_z signature.c_bytes))
      in
      c_check && signature.sig_r = challenge_r
  end

  open Lang_stdlib
  open Gadget_edwards

  module V : functor (L : LIB) -> sig
    open L
    open MakeAffine(Curve)(L)
    open Encodings

    (* TODO make abstract once compression is done with encodings *)
    type pk = point

    val pk_encoding : (P.pk, pk repr, pk) encoding

    type signature = {
      sig_u_bytes : bool list repr;
      sig_r : point repr;
      c_bytes : bool list repr;
    }

    val signature_encoding :
      (P.signature, signature, bool list * (pk * bool list)) encoding

    val verify :
      ?compressed:bool ->
      g:point repr ->
      msg:scalar repr ->
      pk:pk repr ->
      signature:signature ->
      unit ->
      bool repr t
  end =
  functor
    (L : LIB)
    ->
    struct
      open L

      open MakeAffine (Curve) (L)

      open H.V (L)

      type pk = point

      open Encodings

      let point_encoding : (Curve.t, pk repr, pk) encoding =
        let curve_base_to_s c = Lang_core.S.of_z @@ Curve.Base.to_z c in
        let curve_base_of_s c = Curve.Base.of_z @@ Lang_core.S.to_z c in
        with_implicit_bool_check is_on_curve
        @@ conv
             (fun r -> of_pair r)
             (fun (u, v) -> pair u v)
             (fun c ->
               ( curve_base_to_s @@ Curve.get_u_coordinate c,
                 curve_base_to_s @@ Curve.get_v_coordinate c ))
             (fun (u, v) ->
               Curve.from_coordinates_exn
                 ~u:(curve_base_of_s u)
                 ~v:(curve_base_of_s v))
             (obj2_encoding scalar_encoding scalar_encoding)

      let pk_encoding = point_encoding

      type signature = {
        sig_u_bytes : bool list repr;
        sig_r : point repr;
        c_bytes : bool list repr;
      }

      let signature_encoding =
        conv
          (fun {sig_u_bytes; sig_r; c_bytes} -> (sig_u_bytes, (sig_r, c_bytes)))
          (fun (sig_u_bytes, (sig_r, c_bytes)) -> {sig_u_bytes; sig_r; c_bytes})
          (fun ({sig_u_bytes; sig_r; c_bytes} : P.signature) ->
            (sig_u_bytes, (sig_r, c_bytes)))
          (fun (sig_u_bytes, (sig_r, c_bytes)) -> {sig_u_bytes; sig_r; c_bytes})
          (obj3_encoding
             (atomic_list_encoding bool_encoding)
             point_encoding
             (atomic_list_encoding bool_encoding))

      (* In the compressed variant, we drop sig_r_v of the challenge input as it
         can be represented with a single bit: its parity (because of Edwards curves
         are symmetric). We do so as we want to use a hash function with a fixed
         input length of 2 to minimize the number of constraints. We can still prove
         the security of this scheme using the Forking lemma and forking thrice.

         If we really want to include sig_r_v in the challenge input, we could use
         its parity bit instead of the whole scalar and compress it with the message
         if it is either small or the output of a hash. This however is expensive as
         we have to check the decomposition of sig_r_v with the parity bit.

         Netherless, if the msg input actually is the output of the hash,
         (msg = H(msg')) of a single scalar, we could fit sig_r_v in this inner hash
          while keeping the full security:
         c = H( g**r; msg) = H(g**r; H(msg'))
                           =  H( r_u, r_v, H(msg'))
                           ~  H( r_u, H(msg', r_v))  (equivalent in term of security)
         Doing this would not lead to an additional compression round in the hash,
         and as such the resulting overhead would be minimal (one constraint for
         Poseidon128).

         Assuming that max_account = max_counter = 2**32 (< 10**10) in the Transfer
         circuit (privacy-team/plompiler/test/benchmark.ml), and max_amount =
         max_fee = 2**64 (< 10**20), the signature message can be compressed in one
         scalar:
             msg = H(msg') = H(src || dst || fee || amount || counter)
             len(msg') = 192 < BLS12-381.Fr order ~ 2**255
         As such, we could use the above scheme to compute the challenge:
         c = H(r_u, H(src || dst || fee || amount || counter, r_v)) *)

      let hash ~compressed sig_r msg =
        with_label ~label:"Schnorr.hash"
        @@
        let sig_r_u = get_u_coordinate sig_r in
        if compressed then digest ~input_length:2 @@ to_list [sig_r_u; msg]
        else
          let sig_r_v = get_v_coordinate sig_r in
          digest @@ to_list [sig_r_u; sig_r_v; msg]

      (* Requires : [c_bytes] is computed correctly by the prover.
         Otherwise an assert is triggered. *)
      (* TODO: now msg is just one scalar, it will probably be a list of scalars *)
      let verify ?(compressed = false) ~g ~msg ~pk ~signature () =
        with_label ~label:"Schnorr.verify"
        @@
        (* assert bytes' length *)
        let {sig_u_bytes; sig_r; c_bytes} = signature in
        assert (
          List.compare_length_with (of_list sig_u_bytes) (Z.numbits base_order)
          = 0) ;
        assert (
          List.compare_length_with (of_list c_bytes) (Z.numbits base_order) = 0) ;

        (* generating challenge *)
        let* c = hash ~compressed sig_r msg in
        (* check challenge binary decomposition : of_bytes c_bytes = c *)
        let* c' = Num.scalar_of_bytes c_bytes in
        let* e = equal c' c in
        (* re-computing randomness challenge_r = u * G_E + c * pk *)
        let* challenge_r =
          let scalars = to_list [sig_u_bytes; c_bytes] in
          let points = to_list [g; pk] in
          multi_scalar_mul scalars points
        in
        (* check signature randomness sig_r equals challenge_r *)
        let* b = equal sig_r challenge_r in
        Bool.band e b
    end
end
