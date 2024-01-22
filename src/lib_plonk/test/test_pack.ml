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

module Internal = struct
  open Aggregation.Pack.Pack_impl

  let test_verifier_srs () =
    let n = 2 in
    let pp_prv, pp_vrf = setup n (snd Plonk_test.Helpers.srs) in
    assert (Kzg.Bls.G1.eq pp_prv.g1_t pp_vrf)
end

module External = struct
  open Aggregation.Pack
  open Kzg.Bls
  open Kzg.Utils

  let with_time f =
    let start_time = Sys.time () in
    let res = f () in
    let end_time = Sys.time () in
    (res, end_time -. start_time)

  let srs =
    let open Octez_bls12_381_polynomial.Srs in
    let x = Bls12_381.Fr.random () in
    (Srs_g1.generate_insecure 2 x, Srs_g2.generate_insecure 256 x)

  let test_prove_and_verify_single () =
    Random.init 31415 ;
    let n = 256 in
    let pp_prv, pp_vrf = setup n srs in
    let data = Array.init n (fun _i -> G1.random ()) in
    let cmt, t1 = with_time @@ fun () -> commit pp_prv data in
    let transcript = Transcript.expand commitment_t cmt Transcript.empty in
    let r, transcript = Fr_generation.random_fr transcript in
    let ((packed, proof), prover_transcript), t2 =
      with_time @@ fun () -> prove_single pp_prv transcript r data
    in
    let (b, verifier_transcript), t3 =
      with_time @@ fun () ->
      verify_single pp_vrf transcript cmt r (packed, proof)
    in
    assert (Transcript.equal prover_transcript verifier_transcript) ;
    assert b ;
    Format.printf "\nTime commit (single): %f s\n" t1 ;
    Format.printf "Time aggregate and prove (single): %f s\n" t2 ;
    Format.printf "Time verify (single): %f s\n" t3

  let test_prove_and_verify () =
    Random.init 31415 ;
    let max_n = 256 in
    let nb_instances = 10 in
    let pp_prv, pp_vrf = setup max_n srs in
    let generate_instance () =
      let data = Array.init (1 + Random.int max_n) (fun _i -> G1.random ()) in
      let cmt = commit pp_prv data in
      (data, cmt)
    in
    let instances = List.init nb_instances (fun _ -> generate_instance ()) in
    let datas, cmts = List.split instances in

    let transcript =
      Transcript.list_expand commitment_t cmts Transcript.empty
    in
    let r, transcript = Fr_generation.random_fr transcript in

    let ((packed_map, proof), prover_transcript), t1 =
      with_time @@ fun () -> prove pp_prv transcript r datas
    in
    let (b, verifier_transcript), t2 =
      with_time @@ fun () -> verify pp_vrf transcript cmts r (packed_map, proof)
    in
    Format.printf "Time aggregate and prove: %f s\n" t1 ;
    Format.printf "Time verify: %f s\n" t2 ;
    assert (Transcript.equal prover_transcript verifier_transcript) ;
    assert b
end

let tests =
  Alcotest.
    [
      test_case "SRS" `Quick Internal.test_verifier_srs;
      test_case
        "correctness (single)"
        `Quick
        External.test_prove_and_verify_single;
      test_case "correctness" `Quick External.test_prove_and_verify;
    ]
