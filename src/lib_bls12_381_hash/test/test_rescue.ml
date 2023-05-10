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
                  -- --file test_rescue.ml
   Subject:      Test Bls12_381_hash
*)

let test_state_getter_setter () =
  let open Bls12_381_hash.Permutation.Rescue.Parameters in
  let ctxt =
    Bls12_381_hash.Permutation.Rescue.(allocate_ctxt security_128_state_size_3)
  in
  let state =
    Array.init security_128_state_size_3.state_size (fun _ ->
        Bls12_381.Fr.random ())
  in
  let () = Bls12_381_hash.Permutation.Rescue.set_state ctxt state in
  assert (
    Array.for_all2
      Bls12_381.Fr.eq
      state
      (Bls12_381_hash.Permutation.Rescue.get_state ctxt))

let test_consistent_with_mec () =
  let test_vectors =
    [
      ( [|
          "0";
          "19540886853600136773806888540031779652697522926951761090609474934921975120659";
          "27368034540955591518185075247638312229509481411752400387472688330662143761856";
        |],
        [|
          "44673107086290821414141179286216522882469521449559636868455247005114020114445";
          "36631684854749147804429266824538272650916679166988362643455728142484809981118";
          "6299192490564547343493433393738313689693119470079691921424273466838440224855";
        |] );
      ( [|
          "0";
          "27079498335589470388559429012071885383086029562052523482197446658383111072774";
          "42808036164195249275280963312025828986508786508614910971333518929197538998773";
        |],
        [|
          "33984868849407543137734814872861866351541889998060667955612713996984910605397";
          "4641627185242898456867942643033909448689919023961721348259024082238188176596";
          "713519522119335567984877165818659706904142877478060072511766145209379073275";
        |] );
      ( [|
          "0";
          "2280773603130921897588628948561054337050485605305725986334019017364524534576";
          "49742584797721038216392695876666332237687899609664289853866233942594829409245";
        |],
        [|
          "20077542126429611153881019033799341995311428088869746915924850362129927371289";
          "38547305939988350836972846971113863192807010866171540518921476662284151835220";
          "8478277515116959072250029506006378039116363843044265570742187828912632979438";
        |] );
      ( [|
          "0";
          "47345255170739768354940339244069904962490289137071838723660628082786560244227";
          "12508134994244913485311518548620293355291296251148491744230743543606532994206";
        |],
        [|
          "50927739926931280620422144191479516292814380533564245832950972175783227490582";
          "47903072246991027484665412890156631636069614146568039741296638969334550407630";
          "47491331892735386854365197646017331478438648030708643559209673528669254013444";
        |] );
      ( [|
          "0";
          "22266149478348354835731057366322705807112053199389518651299197937563769914341";
          "46391019025260596444368872653036215340017923491388854958773789052412558961328";
        |],
        [|
          "37498820915193954318343590451968185938009803654297685764064124822117204060828";
          "16239130390180195445652573959561373168694718306164480810197800625237334891866";
          "21507804981211574030250333588537037287147041282903807447161895309722506987102";
        |] );
    ]
  in
  List.iter
    (fun (inputs, expected_output) ->
      let open Bls12_381_hash.Permutation.Rescue.Parameters in
      let inputs = Array.map Bls12_381.Fr.of_string inputs in
      let expected_output = Array.map Bls12_381.Fr.of_string expected_output in
      let ctxt =
        Bls12_381_hash.Permutation.Rescue.allocate_ctxt
          security_128_state_size_3
      in
      let () = Bls12_381_hash.Permutation.Rescue.set_state ctxt inputs in
      let () = Bls12_381_hash.Permutation.Rescue.apply_permutation ctxt in
      let output = Bls12_381_hash.Permutation.Rescue.get_state ctxt in
      Array.iter2
        (fun a b ->
          if not (Bls12_381.Fr.eq a b) then
            Alcotest.failf
              "Expected output is %s, computed %s\n"
              (Bls12_381.Fr.to_string a)
              (Bls12_381.Fr.to_string b))
        expected_output
        output)
    test_vectors

let () =
  let open Alcotest in
  run
    ~__FILE__
    "Rescue"
    [
      ( "Consistency with MEC",
        [test_case "vectors" `Quick test_consistent_with_mec] );
      ("State", [test_case "get and set" `Quick test_state_getter_setter]);
    ]
