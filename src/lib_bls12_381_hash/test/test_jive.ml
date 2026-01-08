(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(* Testing
   -------
   Component:    Lib Bls12_381_hash
   Invocation:   dune exec src/lib_bls12_381_hash/test/main.exe \
                  -- --file test_jive.ml
   Subject:      Test Bls12_381_hash
*)

let test_fail_input_size_and_parameters_do_not_match () =
  (* TODO: add Poseidon state size 2 and Rescue state size 2 *)
  let args :
      ((module Bls12_381_hash.PERMUTATION with type parameters = 'p) * 'p) list
      =
    [
      ( (module Bls12_381_hash.Permutation.Anemoi : Bls12_381_hash.PERMUTATION
          with type parameters = 'p),
        Bls12_381_hash.Permutation.Anemoi.Parameters.security_128_state_size_2
      );
    ]
  in
  List.iter
    (fun ( (module P : Bls12_381_hash.PERMUTATION with type parameters = 'p),
           (security_param : 'p) )
       ->
      let input_size = 3 + Random.int 10 in
      let input = Array.init input_size (fun _ -> Bls12_381.Fr.random ()) in
      let msg =
        Printf.sprintf
          "The given array contains %d elements but the expected state size is \
           %d"
          input_size
          2
      in
      Alcotest.check_raises msg (Failure msg) (fun () ->
          ignore
          @@ Bls12_381_hash.Mode.Jive.digest (module P) security_param input))
    args

let test_fail_b_does_not_divide_input_size () =
  let args :
      ((module Bls12_381_hash.PERMUTATION with type parameters = 'p) * 'p) list
      =
    [
      ( (module Bls12_381_hash.Permutation.Anemoi : Bls12_381_hash.PERMUTATION
          with type parameters = 'p),
        Bls12_381_hash.Permutation.Anemoi.Parameters.security_128_state_size_2
      );
      ( (module Bls12_381_hash.Permutation.Anemoi : Bls12_381_hash.PERMUTATION
          with type parameters = 'p),
        Bls12_381_hash.Permutation.Anemoi.Parameters.security_128_state_size_6
      );
    ]
  in
  List.iter
    (fun ( (module P : Bls12_381_hash.PERMUTATION with type parameters = 'p),
           (security_param : 'p) )
       ->
      let input_size = 2 in
      let input = Array.init input_size (fun _ -> Bls12_381.Fr.random ()) in
      let msg = "b must divide the state size" in
      Alcotest.check_raises msg (Failure msg) (fun () ->
          ignore
          @@ Bls12_381_hash.Mode.Jive.digest_b (module P) security_param input 4))
    args

let test_anemoi_state_size_2 () =
  let module P = Bls12_381_hash.Permutation.Anemoi in
  let x = Bls12_381.Fr.random () in
  let y = Bls12_381.Fr.random () in
  let b = 2 in
  let output = P.jive128_1 x y in
  let digest x y =
    let exp_output_arr =
      Bls12_381_hash.Mode.Jive.digest_b
        (module P)
        P.Parameters.security_128_state_size_2
        [|x; y|]
        b
    in
    exp_output_arr.(0)
  in
  assert (Bls12_381.Fr.eq (digest x y) output)

let test_anemoi_state_size_4 () =
  let module P = Bls12_381_hash.Permutation.Anemoi in
  let x1 =
    Bls12_381.Fr.(
      of_string
        "39288125054603279103948425330560623804007631364900524838724476048252426591743")
  in
  let x2 =
    Bls12_381.Fr.of_string
      "28658687495010692659636508039246286087279814855063761962675596314362063193900"
  in
  let y1 =
    Bls12_381.Fr.of_string
      "30521808372563254163692862709213142878392090740378072614276234596069106349049"
  in
  let y2 =
    Bls12_381.Fr.of_string
      "51096746034954956708153164168845755025167527045502628092780406829455255679008"
  in
  let b = 2 in
  let exp_output =
    [|
      Bls12_381.Fr.of_string
        "4516709370008583832348307341992110967410468602387451593697260587093413732828";
      Bls12_381.Fr.of_string
        "14149364429884734929697042590398920659221729533171735081487592237091711387703";
    |]
  in
  let output =
    Bls12_381_hash.Mode.Jive.digest_b
      (module P)
      P.Parameters.security_128_state_size_4
      [|x1; x2; y1; y2|]
      b
  in
  assert (Array.for_all2 Bls12_381.Fr.eq exp_output output)

let test_anemoi_state_size_6 () =
  let module P = Bls12_381_hash.Permutation.Anemoi in
  let input =
    [|
      "2217853344865862743970224905327862424255928440839690896678198609345238715844";
      "5525791196634486842168255391147794242214926238565938472128432032027873773706";
      "44942772752843288052775354603866953297252575988652222073377984219849944553443";
      "47812858871832979537950563348730363522160236019100209662344664049338945674257";
      "47796750287196818117833119527878210275558352491668495251261943487827490944733";
      "18675497827065340973313153216602100036674811368886888073398973368357419355615";
    |]
    |> Array.map Bls12_381.Fr.of_string
  in
  let exp_output =
    [|
      "548157650485778415829789423515724969373531829417488095812227792093458460987";
      "24533101904433261837828489116977348961986591391494245235307669547135189177847";
      "19437683284737987027036084355301136646342868371278755462121209437095736336008";
    |]
    |> Array.map Bls12_381.Fr.of_string
  in
  let b = 2 in
  let output =
    Bls12_381_hash.Mode.Jive.digest_b
      (module P)
      P.Parameters.security_128_state_size_6
      input
      b
  in
  assert (Array.for_all2 Bls12_381.Fr.eq exp_output output)

let () =
  let open Alcotest in
  run
    ~__FILE__
    "The mode of operation Jive"
    [
      ( "Exceptions",
        [
          test_case
            "input size does not correspond to parameters"
            `Quick
            test_fail_input_size_and_parameters_do_not_match;
          test_case
            "b does not divide the state size"
            `Quick
            test_fail_b_does_not_divide_input_size;
        ] );
      ( "Anemoi",
        [
          test_case "b = 2, state_size = 2" `Quick test_anemoi_state_size_2;
          test_case "b = 2, state_size = 4" `Quick test_anemoi_state_size_4;
          test_case "b = 2, state_size = 6" `Quick test_anemoi_state_size_6;
        ] );
    ]
