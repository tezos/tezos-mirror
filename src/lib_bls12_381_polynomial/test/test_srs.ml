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

module Fr = Bls12_381.Fr
module G1 = Bls12_381.G1
module Poly = Octez_bls12_381_polynomial.Polynomial

module Srs :
  Octez_bls12_381_polynomial.Srs.S_unsafe
    with type elt = Bls12_381.G1.t
     and type polynomial = Poly.t =
  Octez_bls12_381_polynomial.Srs.Make (Octez_bls12_381_polynomial.Srs.Elt_g1)

let test_get () =
  let srs = Srs.generate_insecure 1 Fr.one in
  assert (G1.eq G1.one (Srs.get srs 0))

let test_pippenger () =
  let logn = 1 + Random.int 4 in
  let n = 1 lsl logn in
  let start = Random.int n in
  let len = 1 + Random.int (n - start) in
  let srs = Array.init n (fun _ -> G1.random ()) in
  let scalars = Array.init n (fun _ -> Fr.random ()) in
  let exp_output = Bls12_381.G1.pippenger ~start ~len srs scalars in
  let pippenger_ctxt = Srs.of_array srs in
  let poly = Poly.of_dense scalars in
  let output = Srs.pippenger ~offset:start ~len pippenger_ctxt poly in
  assert (Bls12_381.G1.eq output exp_output)

let test_add_and_extract_srs_from_pippenger_ctxt () =
  let logn = 1 + Random.int 3 in
  let n = 1 lsl logn in
  let srs = Array.init n (fun _ -> G1.random ()) in
  let pippenger_ctxt = Srs.of_array srs in
  let extracted_srs = Srs.to_array pippenger_ctxt in
  assert (Array.for_all2 Bls12_381.G1.eq srs extracted_srs)

let test_vector_pippenger () =
  let vectors =
    [
      ( [
          "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1";
          "0626cc1d12a23b7886a4f0b0ef67959b7741bf3cfb2c4a020c7d4cc363796a0a7c0f6ea5ac673865de74a88058e9326e0b96b667165e7320c6190a083999375619571ffa66ebc175565e827666d40fa56bbbb30dbaf0a729436545ac7cfd7ac1";
          "18932b935642ee2b2672c870314197639820562829b8957224ad4ace94ffd1b86523a7ba55c84997e3f528c69f40b42608fdce18c7373edb89594f25c3cb5b8ebf2addb430c713f54b65dca8a3e6f8814dd1cd6cb29ab5c159a10dd992dc9c95";
          "0016c8fe3880c4e18d15fd2401912cf56ea4439c98ff93a78b21d0e8533158b96a575252af3e2fc75f23f642bc04b42805444f638fbbe3cfb6cdb4c234a81858b572d8f68d6081d20e9097dc8dc7953b4333714639507e856920905ed76d21d2";
        ],
        [
          "01000040ffffff3fffc4fe3f023bcefe0362390706626b26f61d365f7e3df256";
          "000000000040000000c0809d00c0003b34c1809d337354230000000000000000";
          "000000c0ffffffbfff96ffbf0069ef54017668020276ce0c525f67cad469fb1c";
          "01000000ffbffffffe9b7d6202e4bc18d116216cd464e50f487d9d2953a7ed73";
        ] );
      ( [
          "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1";
          "0626cc1d12a23b7886a4f0b0ef67959b7741bf3cfb2c4a020c7d4cc363796a0a7c0f6ea5ac673865de74a88058e9326e0b96b667165e7320c6190a083999375619571ffa66ebc175565e827666d40fa56bbbb30dbaf0a729436545ac7cfd7ac1";
          "18932b935642ee2b2672c870314197639820562829b8957224ad4ace94ffd1b86523a7ba55c84997e3f528c69f40b42608fdce18c7373edb89594f25c3cb5b8ebf2addb430c713f54b65dca8a3e6f8814dd1cd6cb29ab5c159a10dd992dc9c95";
          "0016c8fe3880c4e18d15fd2401912cf56ea4439c98ff93a78b21d0e8533158b96a575252af3e2fc75f23f642bc04b42805444f638fbbe3cfb6cdb4c234a81858b572d8f68d6081d20e9097dc8dc7953b4333714639507e856920905ed76d21d2";
          "184b377e14ed55e31b35f445cbf468b705e9c71a946a55522a3c85be25b393a2e67265cef7df8edd619002e2a3ad065b0af36ec22b8926ff733c9b7b5478843b0e3dc84e8d94c1d0b2b923cba275e78d8acce1b8537cb242778600e5af19ad46";
          "0ed4874249807cc2a1f940e15fd4eb119786194a48a158135a951f58b4ca3b80f916c02a405b9c040338d4da3adbedfa047fae6023b2fd812933d114c1437c749aac6376d0a601229f5f825626974ee8e7ece8eacc4d6b6d75e7f3fd7201947d";
        ],
        [
          "18f30c353f952ad33eb17e03f56cc000e6cfa38c9181341b74ca3635c08be372";
          "f66f4007409ab02839855ebe1fc20893446e161d0409bebbdac47e83fbec855c";
          "430be8668e0567ba3ff8cba1afc74c5095016b47c0fe0d6a678c54f065152a5f";
          "10ee9a0e7c8ef77cdf20164a67bd0ad9afe9ec33972cb7473af8753b092b1954";
          "73cbbc6a1b984fbaabd6e75c316e476ce3a44eccf812529d8d89aea90329c147";
          "6de9f9ee3c5bd6fbd7def12f8b0534dd35a202a7b73eef348f1d6ec41be5c751";
        ] );
      ( [
          "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1";
          "0626cc1d12a23b7886a4f0b0ef67959b7741bf3cfb2c4a020c7d4cc363796a0a7c0f6ea5ac673865de74a88058e9326e0b96b667165e7320c6190a083999375619571ffa66ebc175565e827666d40fa56bbbb30dbaf0a729436545ac7cfd7ac1";
          "18932b935642ee2b2672c870314197639820562829b8957224ad4ace94ffd1b86523a7ba55c84997e3f528c69f40b42608fdce18c7373edb89594f25c3cb5b8ebf2addb430c713f54b65dca8a3e6f8814dd1cd6cb29ab5c159a10dd992dc9c95";
          "0016c8fe3880c4e18d15fd2401912cf56ea4439c98ff93a78b21d0e8533158b96a575252af3e2fc75f23f642bc04b42805444f638fbbe3cfb6cdb4c234a81858b572d8f68d6081d20e9097dc8dc7953b4333714639507e856920905ed76d21d2";
          "184b377e14ed55e31b35f445cbf468b705e9c71a946a55522a3c85be25b393a2e67265cef7df8edd619002e2a3ad065b0af36ec22b8926ff733c9b7b5478843b0e3dc84e8d94c1d0b2b923cba275e78d8acce1b8537cb242778600e5af19ad46";
          "0ed4874249807cc2a1f940e15fd4eb119786194a48a158135a951f58b4ca3b80f916c02a405b9c040338d4da3adbedfa047fae6023b2fd812933d114c1437c749aac6376d0a601229f5f825626974ee8e7ece8eacc4d6b6d75e7f3fd7201947d";
        ],
        [
          "ea900cee19d8ba557b68418d3bbc413542c3c92a48317cb6c6b975dbd04d2b23";
          "c693db96c8cbbea290d505e731ed16376fa998e68ab4203f7af599c0f78f983f";
          "83120bdf88b335f254baf192094c883858aaa785b0bb4255da24d871193fe64e";
          "a69a181630f9d27c9dd91e91aade10e310b38cd0f73ee33b08df594fe0e65f1c";
          "78b0bd6bc451bd7d6b65e65ce4e81c8f25a5c74c4748777007f35f5a5983da09";
          "1025092f62588e2cf536645b238ce88402cec77a5902ab455aa1d4131eaa6f2c";
        ] );
      ( [
          "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1";
          "0626cc1d12a23b7886a4f0b0ef67959b7741bf3cfb2c4a020c7d4cc363796a0a7c0f6ea5ac673865de74a88058e9326e0b96b667165e7320c6190a083999375619571ffa66ebc175565e827666d40fa56bbbb30dbaf0a729436545ac7cfd7ac1";
          "18932b935642ee2b2672c870314197639820562829b8957224ad4ace94ffd1b86523a7ba55c84997e3f528c69f40b42608fdce18c7373edb89594f25c3cb5b8ebf2addb430c713f54b65dca8a3e6f8814dd1cd6cb29ab5c159a10dd992dc9c95";
          "0016c8fe3880c4e18d15fd2401912cf56ea4439c98ff93a78b21d0e8533158b96a575252af3e2fc75f23f642bc04b42805444f638fbbe3cfb6cdb4c234a81858b572d8f68d6081d20e9097dc8dc7953b4333714639507e856920905ed76d21d2";
          "184b377e14ed55e31b35f445cbf468b705e9c71a946a55522a3c85be25b393a2e67265cef7df8edd619002e2a3ad065b0af36ec22b8926ff733c9b7b5478843b0e3dc84e8d94c1d0b2b923cba275e78d8acce1b8537cb242778600e5af19ad46";
          "0ed4874249807cc2a1f940e15fd4eb119786194a48a158135a951f58b4ca3b80f916c02a405b9c040338d4da3adbedfa047fae6023b2fd812933d114c1437c749aac6376d0a601229f5f825626974ee8e7ece8eacc4d6b6d75e7f3fd7201947d";
          "0037ae4b3f2191c9df9e179727b6739474e1bc4d502de668d4985a82404d5317ae79ecc63e6f8042ab520a74e897ad720406a2cfc76490fb0fd28cb8e13c706de396c37d613631818b58ea9ecc3db30d86217cd85c0da3c5998927b3789dcff4";
          "0eadc0859b8076866c8322add1566589767b15ea33ff225338df15bdb2c196a23e94baf9bc04f85f1f88070a4280adaf0db1f9d1016e4e90a0ae9b6cc47bb93028396d39ce24edfefef0713ca28c6d25b0a211b2dd918fe9d460d9692b8ae235";
          "13be3a770003d560b33744d5a0d89dc4a492c78e30f0921e68a5d863bb656693ea947409dc41141bc489ac98e05c3401017107261b06441911b8f980496025697b67c1ccae1b9b019bbd6620722b432991f9db5b86d08a3caeac0bfebb65d0f3";
          "0f7017817c17b571a9622905e433400f744a2f4f81c3a6b31d4973ea12fa3f0735c5a7486c699d7abc5eb33b0f1386f60667140c432c00669eccad648ce4b5ac49173f3af926333af85adf0526c17ad9c3987aeb99c8bde2c95a6f11594f2d6a";
          "16e397128d39fb23a2ece3b628af491c640768dcea09347039a976d8621c010ff321f9a96b57600f29379452152b0c6c02957a9b42a70224cca6f022d436746ab64a9def1e2da6a3899556ecf37e0d1ab3647838204fa9f37dede4e2bb580fb1";
          "00b2c446c12f14a5c5d9b85f3127062aa19865b66b33be62d696fcbe02fd14f8237b94a15cd0241d9ad1a4007942672417149de0f1606edfedb04bfc59bbc103ba7d7d2f6b8eb84a462e115369e9ce654539a1f5891ba704e1f3d95ea74bc75b";
          "0c27a162d50f8911af173175e64ec8f04b7cc38e6b96f206fae90e41b65887e61430e7eb5c65396a4c8727c3493f358402800d0ccbd9fd71bcbb0e1cd88f7894078e1b8f8c6a9a5101d190f25675211ff5dd3a1cc1ece5cab23ddf00dbd836e0";
        ],
        [
          "ca7b2410082015a7b7819d2cde1b46f2f15b2980af0cb0662439286a3e853b70";
          "34e57070ec49b5d3d43ce6771c21c478b9af5d66a27a385acc895bde459b575b";
          "5bd281d23682c4cd2df6f83ae6af7cfc5f2e67423ffca271a3a1d5c35a7feb3c";
          "9acdfd591f1de1433fc94c56239cfb3df27a3c05f7b6c27a5c4fd1d5be52f045";
          "48c2e23f9007c5fa3e6378ed195165fdb35a2eede808f47df6c8685e2982ad5b";
          "0066dfd993b3fd05afae8e515f13ad97db1a91093e3bce6c2c77e76f3dc2e01f";
        ] );
    ]
  in
  List.iter
    (fun (srs_bytes, scalars_bs) ->
      let srs =
        List.map
          (fun x -> Bls12_381.G1.of_bytes_exn (Hex.to_bytes (`Hex x)))
          srs_bytes
        |> Array.of_list
      in
      let scalars =
        List.map
          (fun x -> Bls12_381.Fr.of_bytes_exn (Hex.to_bytes (`Hex x)))
          scalars_bs
        |> Array.of_list
      in
      let start = 1 in
      let exp_output = Bls12_381.G1.pippenger ~start srs scalars in
      let pippenger_ctxt = Srs.of_array srs in
      let poly = Poly.of_dense scalars in
      let output = Srs.pippenger ~offset:start pippenger_ctxt poly in
      assert (Bls12_381.G1.eq output exp_output))
    vectors

let bigstring_of_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0o440 in
  Bigarray.array1_of_genarray
  @@ Unix.map_file
       fd
       Bigarray.char
       Bigarray.c_layout
       false
       [|(* [-1] means read the whole file *) -1|]

let path = project_root // Filename.dirname __FILE__

let test_load_from_file () =
  let bs = bigstring_of_file (path // "srs_zcash_g1_5") in
  let max_size = 1 lsl 5 in
  let srs = Srs.of_bigstring bs ~len:max_size |> Result.get_ok in
  assert (Srs.size srs = max_size) ;
  match Srs.of_bigstring bs ~len:(max_size + 1) with
  | Error (`End_of_file _) -> ()
  | _ -> assert false

let test_load_from_file_vector () =
  let bs = bigstring_of_file (path // "srs_zcash_g1_5") in
  let srs = Srs.of_bigstring bs ~len:(1 lsl 5) |> Result.get_ok in
  let v =
    [
      "17f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb08b3f481e3aaa0f1a09e30ed741d8ae4fcf5e095d5d00af600db18cb2c04b3edd03cc744a2888ae40caa232946c5e7e1";
      "0626cc1d12a23b7886a4f0b0ef67959b7741bf3cfb2c4a020c7d4cc363796a0a7c0f6ea5ac673865de74a88058e9326e0b96b667165e7320c6190a083999375619571ffa66ebc175565e827666d40fa56bbbb30dbaf0a729436545ac7cfd7ac1";
      "18932b935642ee2b2672c870314197639820562829b8957224ad4ace94ffd1b86523a7ba55c84997e3f528c69f40b42608fdce18c7373edb89594f25c3cb5b8ebf2addb430c713f54b65dca8a3e6f8814dd1cd6cb29ab5c159a10dd992dc9c95";
    ]
  in
  List.iteri
    (fun i expected ->
      let (`Hex e) = Bls12_381.G1.to_bytes (Srs.get srs i) |> Hex.of_bytes in
      assert (e = expected))
    v

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("get_set", test_get);
      ("pippenger", test_pippenger);
      ("pippenger test vectors", test_vector_pippenger);
      ( "add and extract srs from pippenger ctxt",
        test_add_and_extract_srs_from_pippenger_ctxt );
      ("load_from_file", test_load_from_file);
      ("load_from_file_vector", test_load_from_file_vector);
    ]
