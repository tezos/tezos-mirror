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

open Plonk
open Utils

(* Our version of SnarkPack for PLONK *)

module type Aggregator = sig
  (* Public parameters *)
  type prover_public_parameters [@@deriving repr]

  type verifier_public_parameters [@@deriving repr]

  (* Data to be aggregated *)
  type data = Bls.G1.t

  (* Commitment to the data *)
  type commitment = {cmt_t : Bls.GT.t; cmt_len : int} [@@deriving repr]

  (* Randomness used to pack the data, usually derived from a commitment to it *)
  type randomness = Bls.Scalar.t

  (* Packed/aggregated data *)
  type packed = Bls.G1.t [@@deriving repr]

  (* Proof that the data was correctly aggregated *)
  type proof [@@deriving repr]

  type transcript = Bytes.t

  type setup_params

  val setup :
    int ->
    Bls12_381_polynomial.Srs.t ->
    prover_public_parameters * verifier_public_parameters

  val get_setup_params : prover_public_parameters -> setup_params

  val public_parameters_to_bytes : prover_public_parameters -> Bytes.t

  val commit : prover_public_parameters -> data array -> commitment

  val commitment_cardinal : commitment -> int

  val partial_commit :
    relevant_positions:int list ->
    prover_public_parameters ->
    data array ->
    commitment

  val empty_commitment : commitment

  val combine : commitment -> commitment -> commitment

  val prove_single :
    prover_public_parameters ->
    transcript ->
    randomness ->
    data array ->
    (packed * proof) * transcript

  val prove :
    prover_public_parameters ->
    transcript ->
    randomness ->
    data array list ->
    (packed list * proof) * transcript

  val verify_single :
    verifier_public_parameters ->
    transcript ->
    commitment ->
    randomness ->
    packed * proof ->
    bool * transcript

  val verify :
    verifier_public_parameters ->
    transcript ->
    commitment list ->
    randomness ->
    packed list * proof ->
    bool * transcript
end

module Pack_impl = struct
  open Bls

  type scalar = Scalar.t

  type g1 = G1.t

  type g2 = G2.t

  type gt = GT.t

  type prover_public_parameters = {
    length : int;
    srs2_t : G2.t array;
    g1_t : G1.t;
  }
  [@@deriving repr]

  type verifier_public_parameters = G1.t [@@deriving repr]

  type data = G1.t

  type commitment = {cmt_t : GT.t; cmt_len : int} [@@deriving repr]

  type randomness = Scalar.t

  type packed = G1.t [@@deriving repr]

  type ipa_proof = {
    t_Ls : GT.t array;
    t_Rs : GT.t array;
    r_Ls : G1.t array;
    r_Rs : G1.t array;
    a0 : G1.t;
    t0 : G2.t;
  }
  [@@deriving repr]

  let empty_ipa_proof len =
    {
      t_Ls = Array.init len (fun _i -> GT.zero);
      t_Rs = Array.init len (fun _i -> GT.zero);
      r_Ls = Array.init len (fun _i -> G1.zero);
      r_Rs = Array.init len (fun _i -> G1.zero);
      a0 = G1.zero;
      t0 = G2.zero;
    }

  type kzg_proof = G2.t [@@deriving repr]

  type proof = ipa_proof * kzg_proof [@@deriving repr]

  type transcript = Bytes.t

  type setup_params = int

  let hash ~transcript ~random ?(g1s = [[||]]) ?(g2s = [[||]]) ?(gts = [[||]])
      ?(scalars = [[||]]) () =
    let transcript =
      let open Utils.Hash in
      let st = init () in
      update st transcript ;
      List.iter (Array.iter (fun key -> update st (G1.to_bytes key))) g1s ;
      List.iter (Array.iter (fun key -> update st (G2.to_bytes key))) g2s ;
      List.iter (Array.iter (fun key -> update st (GT.to_bytes key))) gts ;
      List.iter
        (Array.iter (fun key -> update st (Scalar.to_bytes key)))
        scalars ;
      finish st
    in
    let seed, _ = Utils.Hash.bytes_to_seed transcript in
    let state = Some (Random.State.make seed) in
    (random ?state (), transcript)

  let ip_pairing array1 array2 =
    if Array.length array1 = 0 then GT.zero
    else
      let min_length = min (Array.length array1) (Array.length array2) in
      let list_combined =
        List.init min_length (fun i -> (array1.(i), array2.(i)))
      in
      Pairing.(miller_loop list_combined |> final_exponentiation_exn)

  let setup_verifier srs_g1_t = Srs_g1.get srs_g1_t 1

  let setup_prover d (srs_g1_t, srs_g2_t) =
    let srs2_t = Srs_g2.to_array ~len:d srs_g2_t in
    let g1_t = setup_verifier srs_g1_t in
    {length = d; srs2_t; g1_t}

  let setup d srs_t =
    let prv = setup_prover d srs_t in
    let vrf = setup_verifier (fst srs_t) in
    (prv, vrf)

  let get_setup_params public_parameters = public_parameters.length

  let public_parameters_to_bytes {srs2_t; g1_t; _} =
    hash
      ~transcript:Bytes.empty
      ~random:Scalar.random
      ~g1s:[[|g1_t|]]
      ~g2s:[srs2_t]
      ()
    |> fst |> Scalar.to_bytes

  let commit pp data =
    {cmt_t = ip_pairing data pp.srs2_t; cmt_len = Array.length data}

  let commitment_cardinal cmt = cmt.cmt_len

  let partial_commit ~relevant_positions pp data =
    let filter_srs : G2.t array -> G2.t array =
      let module ISet = Set.Make (Int) in
      let pos_set = ISet.of_list relevant_positions in
      fun srs ->
        List.filteri (fun i _proof -> ISet.mem i pos_set) (Array.to_list srs)
        |> Array.of_list
    in
    {
      cmt_t = ip_pairing data (filter_srs pp.srs2_t);
      cmt_len = Array.length data;
    }

  let bytes_of_commitment cmt =
    Bytes.cat
      (Bytes.of_string (string_of_int cmt.cmt_len))
      (GT.to_bytes cmt.cmt_t)

  let empty_commitment = {cmt_t = GT.zero; cmt_len = 0}

  let combine c0 c1 =
    let cmt_t = GT.add c0.cmt_t c1.cmt_t in
    let cmt_len = Int.add c0.cmt_len c1.cmt_len in
    {cmt_t; cmt_len}

  let pack rs data =
    if Array.length data = 0 then G1.zero
    else
      (* rs can be longer than needed *)
      let rs = Array.sub rs 0 (Array.length data) in
      let packed = G1.pippenger data rs in
      packed

  let array_split_in_half a =
    let len = Array.length a in
    let len2 = len / 2 in
    match len mod 2 with
    | 0 -> (Array.sub a 0 len2, Array.sub a len2 len2)
    | _ ->
        raise
          (Invalid_argument
             (Printf.sprintf "split_in_half: length %d not even." len))

  let array_padded_with_zero src dst_len zero =
    let src_len = Array.length src in
    assert (src_len <= dst_len) ;
    if src_len = dst_len then src
    else
      let dst = Array.init dst_len (fun _i -> zero) in
      Array.blit src 0 dst 0 src_len ;
      dst

  let prove_but_not_pack pp transcript r data packed =
    (* Assert that the data length is a power of 2 *)
    let data_length = Array.length data in
    if data_length = 0 then
      raise @@ Invalid_argument "[Array.length data] cannot be 0" ;
    let nb_iter = Z.(log2up @@ of_int data_length) in
    let next_2power = Int.shift_left 1 nb_iter in
    let diff_from_2power = next_2power - data_length in
    let data =
      if diff_from_2power = 0 then data
      else (
        Format.printf
          "\nWARNING: [Array.length data] is %d, not a power of 2, we pad it\n"
          data_length ;
        array_padded_with_zero data next_2power G1.zero)
    in
    let data_length = next_2power in
    let rs = Fr_generation.powers data_length r in
    let transcript = Bytes.cat transcript @@ G1.to_bytes packed in

    let rec loop transcript g_poly ipa_proof a b t i =
      if i = nb_iter then
        match (a, b, t) with
        | [|a0|], [|_|], [|t0|] -> (g_poly, {ipa_proof with a0; t0}, transcript)
        | _ -> raise @@ Invalid_argument "Aggregation: IPA loop"
      else
        let a_left, a_right = array_split_in_half a in
        let b_left, b_right = array_split_in_half b in
        let t_left, t_right = array_split_in_half t in

        let t_L = ip_pairing a_left t_right in
        let t_R = ip_pairing a_right t_left in

        let r_L = G1.pippenger a_left b_right in
        let r_R = G1.pippenger a_right b_left in

        let u, transcript =
          let g1s = [[|r_L; r_R|]] in
          let gts = [[|t_L; t_R|]] in
          Scalar.(hash ~transcript ~random ~g1s ~gts ())
        in
        let u_inv = Scalar.inverse_exn u in

        let merge ~add ~mul x y = add (mul x u) (mul y u_inv) in
        let a' = Array.map2 G1.(merge ~add ~mul) a_left a_right in
        let b' = Array.map2 Scalar.(merge ~add ~mul) b_right b_left in
        let t' = Array.map2 G2.(merge ~add ~mul) t_right t_left in

        ipa_proof.t_Ls.(i) <- t_L ;
        ipa_proof.t_Rs.(i) <- t_R ;
        ipa_proof.r_Ls.(i) <- r_L ;
        ipa_proof.r_Rs.(i) <- r_R ;

        let xn = Int.shift_left 1 (nb_iter - 1 - i) in
        let g'_poly = Poly.(g_poly * of_coefficients [(u_inv, 0); (u, xn)]) in

        loop transcript g'_poly ipa_proof a' b' t' (i + 1)
    in

    let srs2_t = Array.sub pp.srs2_t 0 data_length in
    let g, ipa_proof, transcript =
      loop transcript Poly.one (empty_ipa_proof nb_iter) data rs srs2_t 0
    in

    let gts = [ipa_proof.t_Ls; ipa_proof.t_Rs] in
    let g1s = [[|ipa_proof.a0|]; ipa_proof.r_Ls; ipa_proof.r_Rs] in
    let g2s = [[|ipa_proof.t0|]] in
    let rho, transcript = Scalar.(hash ~transcript ~random ~g1s ~g2s ~gts ()) in
    let h =
      fst
      @@ Poly.(
           division_xn (g - (constant @@ evaluate g rho)) 1 (Scalar.negate rho))
    in
    let h_coeffs = Poly.to_dense_coefficients h in
    let kzg_proof_t = G2.pippenger srs2_t h_coeffs in

    let proof = (ipa_proof, kzg_proof_t) in
    (proof, transcript)

  let prove_single pp transcript r data =
    let rs = Fr_generation.powers (Array.length data) r in
    let packed = pack rs data in
    let proof, transcript = prove_but_not_pack pp transcript r data packed in
    ((packed, proof), transcript)

  let prove pp transcript r data_list =
    let n = List.length data_list in
    if n = 0 then raise @@ Failure "data_list cannot be empty" ;

    let max_length_datas =
      List.fold_left max 0 @@ List.map Array.length data_list
    in

    (* Pad with zeros at the tail so that all datas have the same length *)
    let padded_datas =
      List.map
        (fun l -> array_padded_with_zero l max_length_datas G1.zero)
        data_list
    in
    let delta, transcript = Scalar.(hash ~transcript ~random ()) in
    let deltas = Fr_generation.powers n delta |> Array.to_list in
    let data =
      (* data = delta^0·padded_datas.(0) +...+ delta^(n-1)·padded_datas.(n-1) *)
      let safe_tl = function _ :: tl -> tl | _ -> [] in
      List.fold_left2
        (fun acc padded_data d ->
          Array.map2 (fun a b -> G1.(add a (mul b d))) acc padded_data)
        (List.hd padded_datas)
        (safe_tl padded_datas)
        (safe_tl deltas)
    in
    let rs = Fr_generation.powers max_length_datas r in
    let packed = pack rs data in
    let packed_list = List.map (pack rs) data_list in
    let proof, transcript = prove_but_not_pack pp transcript r data packed in
    ((packed_list, proof), transcript)

  let verify_single pp transcript cmt r (packed, (ipa_proof, kzg_proof)) =
    let transcript = Bytes.cat transcript @@ G1.to_bytes packed in
    (* FIXME: assert that the length of these six arrays (or at least one of them)
       equals the log2 of cmt.cmt_len *)
    let us, transcript =
      let len = Array.length ipa_proof.t_Ls in
      let us = Array.init len (fun _i -> Scalar.zero) in
      let transcript_i = ref transcript in
      for i = 0 to len - 1 do
        let u, transcript =
          let g1s = [[|ipa_proof.r_Ls.(i); ipa_proof.r_Rs.(i)|]] in
          let gts = [[|ipa_proof.t_Ls.(i); ipa_proof.t_Rs.(i)|]] in
          Scalar.(hash ~transcript:!transcript_i ~random ~g1s ~gts ())
        in
        us.(i) <- u ;
        transcript_i := transcript
      done ;
      (us, !transcript_i)
    in

    (* g(X) := (u₁⁻¹ + u₁ X^{2ᵏ⁻¹}) · (u₂⁻¹ + u₂ X^{2ᵏ⁻²}) ··· (uₖ⁻¹ + uₖ X) *)
    let eval_g x =
      let len = Array.length us in
      let acc = ref Scalar.one in
      let x_power = ref x in
      for i = 0 to len - 1 do
        let u = us.(len - 1 - i) in
        let term = Scalar.(inverse_exn u + (u * !x_power)) in
        acc := Scalar.mul !acc term ;
        x_power := Scalar.square !x_power
      done ;
      !acc
    in

    (* Verify the IPA proof *)
    let r0 = eval_g r in

    (* Computes [init + sum_j (u_j^2 L_j + u_j^{-2} R_j)] *)
    let rhs ~init ~add ~mul us gLs gRs =
      let len = Array.length us in
      let acc = ref init in
      for i = 0 to len - 1 do
        let u2 = Scalar.square us.(i) in
        let u2_inv = Scalar.inverse_exn u2 in
        acc := add !acc @@ add (mul gLs.(i) u2) (mul gRs.(i) u2_inv)
      done ;
      !acc
    in

    let lhs_t = Pairing.pairing ipa_proof.a0 ipa_proof.t0 in
    let rhs_t =
      GT.(rhs ~init:cmt.cmt_t ~add ~mul us ipa_proof.t_Ls ipa_proof.t_Rs)
    in

    let lhs_r = G1.mul ipa_proof.a0 r0 in
    let rhs_r =
      G1.(rhs ~init:packed ~add ~mul us ipa_proof.r_Ls ipa_proof.r_Rs)
    in

    let ipa_ok = GT.eq lhs_t rhs_t && G1.eq lhs_r rhs_r in

    (* Verify the KZG proof *)
    let gts = [ipa_proof.t_Ls; ipa_proof.t_Rs] in
    let g1s = [[|ipa_proof.a0|]; ipa_proof.r_Ls; ipa_proof.r_Rs] in
    let g2s = [[|ipa_proof.t0|]] in
    let rho, transcript = Scalar.(hash ~transcript ~random ~g1s ~g2s ~gts ()) in
    let m_v = eval_g rho |> Scalar.negate |> G2.(mul one) in
    let st0 = ipa_proof.t0 in
    let rho_g1 = G1.mul G1.one @@ Scalar.negate rho in

    let rhs =
      ip_pairing G1.[|negate one; add pp rho_g1|] G2.[|add st0 m_v; kzg_proof|]
    in
    let kzg_ok = GT.is_zero rhs in

    (ipa_ok && kzg_ok, transcript)

  let verify pp transcript cmt_list r (packed_list, proof) =
    let delta, transcript = Scalar.(hash ~transcript ~random ()) in

    let combine_cmt d c1 c2 =
      {
        cmt_t = GT.add c1.cmt_t (GT.mul c2.cmt_t d);
        cmt_len = max c1.cmt_len c2.cmt_len;
      }
    in

    let combine_packed d p1 p2 = G1.add p1 (G1.mul p2 d) in

    let cmt, packed, _ =
      List.fold_left2
        (fun (cmt, packed, d) c p ->
          (combine_cmt d cmt c, combine_packed d packed p, Scalar.mul d delta))
        ({cmt_t = GT.zero; cmt_len = 0}, G1.zero, Scalar.one)
        cmt_list
        packed_list
    in
    verify_single pp transcript cmt r (packed, proof)
end

include (Pack_impl : Aggregator)
