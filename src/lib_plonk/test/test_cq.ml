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

module PC = Plonk.Polynomial_commitment

let ( !! ) = Plonk_test.Cases.( !! )

let srs = fst Plonk_test.Helpers.srs

let table1 = !![0; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20; 22; 24; 26; 28; 28]

let table2 = !![1; 3; 5; 7; 9; 11; 13; 15; 17; 19; 21; 21; 23; 25; 27; 29]

let table = [table1; table2]

let f00 = !![0; 2; 2; 20]

let f01 = !![1; 3; 3; 21]

let f10 = !![6; 8; 2; 26]

let f11 = !![7; 9; 3; 25]

let f_map =
  Plonk.SMap.
    [of_list [("w0", f00); ("w1", f01)]; of_list [("w0", f10); ("w1", f11)]]

let f_not_in_table = !![1; 3; 3; 1]

let f_map_not_in_table =
  Plonk.SMap.[of_list [("w0", f00); ("w1", f_not_in_table)]]

let wire_size = Array.length f00

let test_correctness () =
  let prv, vrf = Plonk.Cq.setup ~srs ~wire_size ~table in
  let transcript = Bytes.empty in
  let proof, prv_transcript = Plonk.Cq.prove prv transcript f_map in
  let vrf, vrf_transcript = Plonk.Cq.verify vrf transcript proof in
  assert (Bytes.equal prv_transcript vrf_transcript) ;
  assert vrf

let test_not_in_table () =
  let prv, _ = Plonk.Cq.setup ~srs ~wire_size ~table in
  let transcript = Bytes.empty in
  try
    let _ = Plonk.Cq.prove prv transcript f_map_not_in_table in
    failwith
      "Test_cq.test_not_in_table : proof generation was supposed to fail."
  with Plonk.Cq.Entry_not_in_table -> ()

(* Commented for now *)
(* let test_wrong_proof () =
   let module Cq = Plonk.Cq.Make (PC) in
   let prv, vrf = Cq.setup ~srs ~wire_size ~table in
   let transcript = Bytes.empty in
   let proof_f, _ = Cq.prove prv transcript [f; f2] in
   let wrong_proof =
     let f =
       Plonk.Bls.(
         Evaluations.(
           interpolation_fft
             (Domain.build wire_size)
             (of_array (wire_size - 1, f_not_in_table))))
     in
     let f2 =
       Plonk.Bls.(
         Evaluations.(
           interpolation_fft
             (Domain.build wire_size)
             (of_array (wire_size - 1, f2))))
     in
     let cm_f, _ =
       Plonk.SMap.of_list Cq.[("0~" ^ f_name, f); ("1~" ^ f_name, f2)]
       |> PC.Commitment.commit prv.pc
     in
     Cq.{proof_f with cm_f}
   in
   assert (not (fst @@ Cq.verify vrf transcript wrong_proof)) *)

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("Correctness", test_correctness);
      ("Not in table", test_not_in_table);
      (* ("Fake proof", test_wrong_proof); *)
    ]
