(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
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

type t = {address : string; private_key : string}

let of_json json =
  let open JSON in
  let address = json |-> "address" |> as_string in
  let private_key = json |-> "privateKey" |> as_string in
  {address; private_key}

let bootstrap_accounts =
  [|
    {
      address = "0x6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c";
      private_key =
        "0x9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0";
    };
    {
      address = "0xB53dc01974176E5dFf2298C5a94343c2585E3c54";
      private_key =
        "0x3a6a6ca30c1ef1ce605a63a7a1a4ff4c689f8414ca0838bca29423f0ec280ff5";
    };
    {
      address = "0x9b49c988b5817Be31DfB00F7a5a4671772dCce2B";
      private_key =
        "0x0eb9bfa77d6cd145cdc0e3d6f902ee1464aeb5f62b02e38f111c9b60cd3adab5";
    };
    {
      address = "0xf39Fd6e51aad88F6F4ce6aB8827279cffFb92266";
      private_key =
        "0xac0974bec39a17e36ba4a6b4d238ff944bacb478cbed5efcae784d7bf4f2ff80";
    };
    {
      address = "0x70997970C51812dc3A010C7d01b50e0d17dc79C8";
      private_key =
        "0x59c6995e998f97a5a0044966f0945389dc9e86dae88c7a8412f4603b6b78690d";
    };
    {
      address = "0x3C44CdDdB6a900fa2b585dd299e03d12FA4293BC";
      private_key =
        "0x5de4111afa1a4b94908f83103eb1f1706367c2e68ca870fc3fb9a804cdab365a";
    };
    {
      address = "0x90F79bf6EB2c4f870365E785982E1f101E93b906";
      private_key =
        "0x7c852118294e51e653712a81e05800f419141751be58f605c371e15141b007a6";
    };
    {
      address = "0x15d34AAf54267DB7D7c367839AAf71A00a2C6A65";
      private_key =
        "0x47e179ec197488593b187f80a00eb0da91f1b9d0b13f8733639f19c30a34926a";
    };
  |]

let lots_of_address =
  [
    "301afc6e83c68958d06c2b8f463401bf1f7b972c";
    "213318651bd0b5293c6b466c31f3c76b5e83c9b0";
    "1c270c130ca45251e53ceb6093f290b7e06301d4";
    "d94f89c4bd3d0267d758ffd0cb5ea53063f90ef3";
    "8d8dfef57911ffca4c2cda0db5b77e75582772c1";
    "09fa10accd0a41845701fe7ad62743915bf64e4d";
    "229a124bb5d0874bf29cffcd84cfa473e77dd0a7";
    "1b110f473be3f9da0493ba8ed5c83cbd96e0cb5b";
    "2cc2cbef4607227ccd4250d94305fbc1c3f246ef";
    "f73d8e4c5d32c63c877be6cb0cbd3c35a1361638";
    "13d693a4688c597825ce7df0b010dc1bab2b4b54";
    "cec75bebf6221d8f11afd3d6c9f8c522bab715b2";
    "a7bc5da2f3bfabcec59a7e991c5500d55e5b99bd";
    "7f5065da45c62372a137f140a4162be05ff8b389";
    "e09d3a03059b5b5da66493a8d96f4a0f6d48b9a4";
    "5bcfb6baa795d473c8da521d0b8219d14fe3babe";
    "eb202b24d50efab6909f0b72819320539b81dd99";
    "47d5a8ed45293c5ed5bc7826d4b7512c74e85e47";
    "1682bc3e9118bf8506825036c5206281bdbaa178";
    "14abb293a07c3272badd1034c29bf0c9e003bc37";
    "9846d52d6607c35aa57944520c8df0c478cd8d79";
    "a44b60e164216740da722fdf3170bfd3de9ca1a5";
    "d3f6b468f3d3f4580e008f8b329823277feb4b5b";
    "f7f61f3cf229fd2fae6ed88dc89eef872c3a0993";
    "8faa26b274eea75be701915c928a3ed18727d242";
    "88bfdc773756d4bbcb3bf10b0898a73ebfa40c71";
    "f2a0da8dde2aa5658a4879184cdf9392434ce699";
    "99cd785d2c81d7c42b794d994f6551c11b9af221";
    "cc0113c46be7f15b0d100725dc48180f1b8f7238";
    "0475a951eebd9145bad9cb1d690300f2b55507b4";
    "5c760672af0de37354ccf0955c95636520e9f69a";
    "f8804a0c4028d8b50705a434265d5d9eb03c7079";
    "d8ec530a20439e6deb4c9eeaf2af3c5c6d042b80";
    "327ab84e83a6bca69345390d5b76d44e50bf43f7";
    "449de790f27ef46366344f444f32b9c491c232fb";
    "a208a8a82fb30ba10d7e72688e58608d5f82b8bb";
    "9bcd1d1d2f5b85786dfbb10371b7d650bdfde7ff";
    "f86e2d22a55a064ed85be99300fbdf8371c73364";
    "0fa7e3a6698c2ebb24f164c4f32b8efd660d4015";
    "305c6e74fff02dc274abc52fc95ef7056395b6e6";
    "c661da960de1dab4791343b74c345c81973c4c31";
    "21e8551c52324f522889e7e1b05f59e4f26a6d43";
    "00f5f99b30961c43d3095c5194ff5db66217645e";
    "3c4de268122ff617a86c736afb4be603ec4d0726";
    "4180a135014c4cb4119abca91f15bec2f21e7306";
    "0c35ff82a806dfc486e53f91511f1b4d5c6530db";
    "4f2093b4350493ab9f2c63ec3c4a50686f97f0c4";
    "f02324465e2d4822e35e523b7531d62ff459ed3f";
    "844206dd3f557a106b4bad0d1d0121d66d74dabe";
    "29758e1d906e329de26f7ef2f3838dd5a5e00531";
    "4320cbd2f35023aa7bb2feb68140428c50b213a4";
    "33eaf72a1c00b63885ea550be1c018141b63628e";
    "0340a36110f8dd87151689857c08b2d417620699";
    "fe68688766eb1234a93fdeefd6558206dabb10cd";
    "9045be24842b67b1b50cd8aafc9afd1596029266";
    "65587d29a141785eb02e5816c5d77d87be449e0f";
    "44b25896128722e16b8c858c169ad276b2c1c9af";
    "fa472372d8dfec026b496dc50eb7e5ef113b35f9";
    "2ca146d54b82f60a53d6f37eb9eb26ef556e0f7a";
    "542ba2e5b57d6cd21362d138583734b8a25d238f";
    "476ed16dbc0891ef7bd7389f969a741437c61ec1";
    "20b837ba21c115c15f3f144133f903e9ece97a4b";
    "7272a5214edff56a8379edfdb576dbdd2309374d";
    "8f0cb0b426b8b641419d05b0fb9bf884876990f7";
    "8f6f54ce7bb9c42540f51c825cfbdb85c7aa54f4";
    "6a7e0ec7fe1ff04326cf8b1d41863542e97ef30f";
    "0b729793937dbbe97df3c84ca32454228edeaa15";
    "00f83f3d387aa9b8f876e00a4fb3ab228d41b485";
    "d821e699092677578166040b2bc067b71fa7a31e";
    "dd5f9ce793b15606b4458b3caaba2de5951366f5";
    "401b2a577b3379e9ede0f7920ff40ba0671a98e5";
    "0b5f45167fc1d16d86f467d27dcd11f168bd5233";
    "1c9798a8ab619e8d4f5a8d04938449ddcce3467e";
    "29ac7d82f8ec936cc265f5a6386502468e6702dd";
    "422c4df728595d202a3e82062e5c306a7182118f";
    "7ddc1e9634d552e1f8bbecd3593036889580b0fd";
    "521711f20907fe56a879231309f1b1bf0264fda7";
    "ba61e88aba341555b2f22729d8e107b77d91735c";
    "498afd4b7c4eed77eb43bb1b1f03b1a04fb1fbf7";
    "6fb29a1a560eac931678574a998e5a963895dfc9";
    "af21fb8d6e68a1b6c8174bc367555288beb8ae0f";
    "b0d9765498e0297d6cd5247da7e80cd80a806bcb";
    "14dccd4abc3932802471edbb442e859e2faa216c";
    "fdc81da68c820ff7af3cc44a0fcfde2beacf183c";
    "89ee9f3c488c303a8c7115dfa3c54fd4ef5ddd19";
    "8cf3dbb64aac7c9f88f6c123b277296c703f0d95";
    "56f024aad2c446562ac3615243d0449646a308b7";
    "b68bdb767831abcdf2125a07ed7ef102d0668604";
    "6b8667efa1a49975c357a83a2780035fde1c00b2";
    "879f14ad56d6acc54fdc57db411ecc3ee8c02d54";
    "87b9362b651b335c5f88ecbc0c4051f59bcd1920";
    "01d407a9f9d3bf5fe0ffa0da5482778a0255c6e0";
    "e50898d9b50711635bedcd26db0bda21a2cf60b0";
    "419809db97ac07e19b42e6457e8903d154bc1e75";
    "ce6024258e3174cc65b0e7846cc320299e97948b";
    "793133c3310a95219259973c34bd5f763dedf23b";
    "a4ae88c92f3fc0149a500b0ecb05c2c34dfa2bc5";
    "915d1e68dbde5e74f05b9ea30d6b3fb685a09dcc";
    "e99ed5b90020575a9bda90e48f56c824a9bb079b";
    "c8c2d559b31e5022d1fe63cbc35361b6ed9acc52";
  ]

let accounts_seq =
  let cpt = ref 0 in
  Seq.memoize @@ Seq.of_dispenser
  @@ fun () ->
  incr cpt ;
  let open Tezos_crypto.Signature.Secp256k1 in
  let seed = Bytes.init 32 (fun _ -> '\000') in
  let si = string_of_int !cpt in
  Bytes.blit_string si 0 seed 0 (String.length si) ;
  let _pkh, pk, sk = generate_key ~seed () in
  let address = eth_address_of_public_key pk in
  let address = "0x" ^ Hex.(show @@ of_bytes address) in
  let private_key = Data_encoding.Binary.to_bytes_exn Secret_key.encoding sk in
  let private_key = "0x" ^ Hex.(show @@ of_bytes private_key) in
  Some {address; private_key}

let accounts nb = Seq.take nb accounts_seq |> Array.of_seq
