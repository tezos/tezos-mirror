(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Implementation of fast amortized Kate proofs
   https://github.com/khovratovich/Kate/blob/master/Kate_amortized.pdf). *)

module Kate_amortized = struct
  module Scalar = Bls12_381.Fr
  module G1 = Bls12_381.G1
  module G2 = Bls12_381.G2
  module GT = Bls12_381.GT
  module Pairing = Bls12_381.Pairing
  module Domain = Bls12_381_polynomial.Polynomial.Domain
  module Polynomial = Bls12_381_polynomial.Polynomial

  type proof = G1.t

  type srs = G1.t list * G2.t

  type commitment = G1.t

  let commit p srs =
    if p = [||] then G1.(copy zero)
    else if Array.(length p > length srs) then
      raise
        (Failure
           (Printf.sprintf
              "Kzg.compute_encoded_polynomial : Polynomial degree, %i, exceeds \
               srs’ length, %i."
              (Array.length p)
              (Array.length srs)))
    else G1.pippenger ~start:0 ~len:(Array.length p) srs p

  let inverse domain =
    let n = Array.length domain in
    Array.init n (fun i ->
        if i = 0 then Bls12_381.Fr.(copy one) else Array.get domain (n - i))

  (* First part of Toeplitz computing trick involving srs. *)
  let build_srs_part_h_list srs domain2m =
    let domain2m = inverse (Domain.inverse domain2m) in
    G1.fft ~domain:domain2m ~points:srs

  let build_h_list_with_precomputed_srs a_list (domain2m, precomputed_srs) =
    let y = precomputed_srs in
    let v = Scalar.fft ~domain:domain2m ~points:a_list in
    Array.map2 (fun yi vi -> G1.mul yi vi) y v

  (* Final ifft of Toeplitz computation. *)
  let build_h_list_final u domain2m =
    let res = G1.ifft ~domain:(inverse domain2m) ~points:u in
    Array.sub res 0 (Array.length domain2m / 2)

  (* part 3.2 *)

  let diff_next_power_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    if 1 lsl logx = x then 0 else (1 lsl (logx + 1)) - x

  let is_pow_of_two x =
    let logx = Z.log2 (Z.of_int x) in
    1 lsl logx = x

  (* Precompute first part of Toeplitz trick, which doesn't depends on the
     polynomial’s coefficients. *)
  let preprocess_multi_reveals ~chunk_len ~degree (srs1, _srs2) =
    let l = 1 lsl chunk_len in
    let k =
      let m_sur_l = degree / l in
      let log_inf = Z.log2 (Z.of_int m_sur_l) in
      if 1 lsl log_inf < m_sur_l then log_inf else log_inf + 1
    in
    let domain2m = Domain.build ~log:k in
    let precompute_srsj j =
      let quotient = (degree - j) / l in
      (*if quotient = 0 then None
        else*)
      let padding = diff_next_power_of_two (2 * quotient) in
      let srsj =
        Array.init
          ((2 * quotient) + padding)
          (fun i ->
            if i < quotient then srs1.(degree - j - ((i + 1) * l))
            else G1.(copy zero))
      in
      build_srs_part_h_list srsj domain2m
    in
    (domain2m, Array.init l precompute_srsj)

  (* Generate proofs of part 3.2. *)

  (** n, r are powers of two, m = 2^(log2(n)-1)
      coefs are f polynomial’s coefficients [f₀, f₁, f₂, …, fm-1]
      domain2m is the set of 2m-th roots of unity, used for Toeplitz computation
      (domain2m, precomputed_srs_part) = preprocess_multi_reveals r n m (srs1, _srs2)
      returns proofs of part 3.2. *)
  let multiple_multi_reveals ~chunk_len ~chunk_count ~degree
      ~preprocess:(domain2m, precomputed_srs_part) coefs =
    let n = chunk_len + chunk_count in
    assert (2 <= chunk_len) ;
    assert (chunk_len < n) ;
    assert (is_pow_of_two degree) ;
    assert (1 lsl chunk_len <= degree) ;
    assert (degree <= 1 lsl n) ;
    let l = 1 lsl chunk_len in
    (* Since we don’t need the first coefficient f₀, we remove it and add a zero
       as last coefficient to keep the size unchanged *)
    let coefs = List.tl (coefs @ [Scalar.(copy zero)]) in
    let coefs = Array.of_list coefs in
    let compute_h_j j =
      let rest = (degree - j) mod l in
      let quotient = (degree - j) / l in
      if quotient = 0 then None
      else
        (* Padding in case quotient is not a power of 2 to get proper fft in
           Toeplitz matrix part. *)
        let padding = diff_next_power_of_two (2 * quotient) in
        let a_list =
          (* fm, 0, …, 0, f₁, f₂, …, fm-1 *)
          let a_array =
            Array.init
              ((2 * quotient) + padding)
              (fun i ->
                if i <= quotient + (padding / 2) then Scalar.(copy zero)
                else coefs.(rest + ((i - (quotient + padding)) * l) - 1))
          in
          a_array.(0) <- coefs.(degree - j - 1) ;
          a_array
        in
        let res =
          Some
            (* Toeplitz stuff *)
            (build_h_list_with_precomputed_srs
               a_list
               (domain2m, precomputed_srs_part.(j)))
        in
        res
    in
    let hl =
      match compute_h_j 0 with
      | None -> failwith "Nothing to compute."
      | Some sum ->
          let rec sum_hj j =
            if j = l then ()
            else
              match compute_h_j j with
              | None -> ()
              | Some hj ->
                  (* sum.(i) <- sum.(i) + hj.(i) *)
                  Array.iteri (fun i hij -> sum.(i) <- G1.add sum.(i) hij) hj ;
                  sum_hj (j + 1)
          in
          sum_hj 1 ;
          build_h_list_final sum domain2m
    in
    let phidomain = Domain.build ~log:chunk_count in
    let phidomain = inverse (Domain.inverse phidomain) in
    G1.fft ~domain:phidomain ~points:hl

  (* h = polynomial such that h(y×domain[i]) = zi. *)
  let interpolation_h_poly y domain z_list =
    Scalar.ifft_inplace ~domain:(Domain.inverse domain) ~points:z_list ;
    let inv_y = Scalar.inverse_exn y in
    Array.fold_left_map
      (fun inv_yi h -> Scalar.(mul inv_yi inv_y, mul h inv_yi))
      Scalar.(copy one)
      z_list
    |> snd

  (* Part 3.2 verifier : verifies that f(w×domain.(i)) = evaluations.(i). *)
  let verify cm_f (srs1, srs2l) domain (w, evaluations) proof =
    let h = interpolation_h_poly w domain evaluations in
    let cm_h = commit h srs1 in
    let l = Domain.length domain in
    let sl_min_yl =
      G2.(add srs2l (negate (mul (copy one) (Scalar.pow w (Z.of_int l)))))
    in
    let diff_commits = G1.(add cm_h (negate cm_f)) in
    Pairing.pairing_check [(diff_commits, G2.(copy one)); (proof, sl_min_yl)]
end
