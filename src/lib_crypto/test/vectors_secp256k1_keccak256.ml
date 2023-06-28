(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
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

(* Context about how the values below were obtained.
   - `msgs` were arbitrarily taken from [src/lib_hacl/test/vectors_p256.ml], any
    message would work.
   - `keys` were generated via MetaMask.
   - `sigs` were obtained within the code base by calling:
     `Tezos_crypto.Signature.Secp256k1.sign_keccak256`
     (reference commit: 88ccae58e16e99fcba74b3d5f460af6b948ae19c) *)

let msgs =
  [
    "c9bd5b35d48f71b48656c39e395e4d138a45df54b7c03fad7330f5fa3f42a44d";
    "c2a49dc5141afdf02480dc2e9d7ec3f602ccc6cf322a0a9b481c80d37170713c";
    "72e8b038fd5cbe8a3f2bb8e9ffc8f48f39143279210e8bfa0131445da9d76b93";
    "6aeee423a44030de1632b7d4e42afc04473f9de218a11358016cd04a3dcc8593";
    "ba52a5eb037ceecd38bad63d2f004a46bd3ab7f6632d5aa72fe7ee1275e8a704";
    "bf49613ff391a12081cfca9ef682784aeeeb4c7774d8259627544e71b14ed15b";
    "582f18a35c36b2403ea78d8b78515dbb8aef4e666ae1ef68795f0969a84e3f28";
    "201b2e9fd3eac2e8c2ec737789551c052db59c9d8c90817c8af044a4de10c694";
    "20ec8437edc2d7b208b281997199da0362c3b619c77853f4330d00a366cbceff";
    "b404a9e26011d26f85e1ab3f327b90e582be990f664fc4af3924c9226b908828";
  ]

let nb_keys = 3

let keys =
  [
    "9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0";
    "3a6a6ca30c1ef1ce605a63a7a1a4ff4c689f8414ca0838bca29423f0ec280ff5";
    "0eb9bfa77d6cd145cdc0e3d6f902ee1464aeb5f62b02e38f111c9b60cd3adab5";
  ]

let sigs =
  [|
    [
      "a6c647a602f63e28cb7aab8b3a18a8c390c1613682582772892f6fdbaa25190f244c1b15f3d605e5bef95f91de65c5a57df8799b6dde02437770cd5edb7c0024";
      "f966a1628aab8ac02e8d8f785da72839587e052376d4a46b878ac6d8b8b243f75532d3a2c5a462267d5bd3e40b017d04abb2e7f9e91262e340e686c7c79052ca";
      "7f51d845d5bd61b731ce3a06e3665eaef513f18e4ff4a935c0047fc0baf1e64f627c443fdd5926058fde784062ccb4f4512f2be141020a37752f6c4a0001b9c6";
      "aace24ddc9469c3b4d66777884e274e22e6177935689980b3b838f04c3b5a8e751ec106b21119d086b0e98829f00e1d96220c9369dcf638b46e07be404eb2bec";
      "11ced8927adfc599de24efb0cc242d35049cf4fa30b441110a785f8b90525d4012eb8cef24cb5218a377030a176c05d1d48e2d9c10a1b17dfd77922bfc24f7b2";
      "250897cddd19a41cb8b2c6332c7b7f72f8ec97204fb81d01261e0c61970ff35d5b68d6ac08fab7ddac0dfdab9a0e9bd7b20b318304e50cc2ffe3ff5ab5752a11";
      "0403566fa111beb566184f731eee25864c443e5c87ad0cfd158772f6088bf68867aebe5fb6e7e55fe2dc9ced5d58e71462875cfddcbc8b28968bb7eb826c600e";
      "eb471dca75b5ff5e703d59a9b16ee1466e39f8c7f1f14add8ceca776153e915b5a9c0af16402fd4a72f640b6bdc111905944ae32740a05495c41e457d87c6175";
      "7c10447016a3e59285c7b2c73b4581f95b617c8e3f6eb8ca4baf05bc624c15e0563afcdec0bdcaea954342490b3cacff3a31cba6e84bf72f8fba8784c40d7b9f";
      "bea007b45f7a80edeafb33a97ba7d13f52a2103cf81de971e837b632825562a01935c78544fa5cd809fb4158822aec062afacde40e73270dd49514f717a34227";
    ];
    [
      "31a827cd493d38875e3381d5587b676222329669796ac4e84a266594700d6ff145896c91a4925a543b40af4aed063b0d1e5fe0333c24deba010514c91f290ca7";
      "c1eecb494e449dc6956e60128793002f0524bc378f53b926ad743516c30d85911cd99579ed04869bbbc4e89d3013b8909c83c12fd692ac24d1a6ffeae602153e";
      "0b82a8c15ea6f68df4b99b1336988f261dd9c5c60d9ff723761325c9d66ecd222af8019742d6101d14ff3c94d8d86e302d8437bb7d4e766440e8facb1155f107";
      "89ac758f58710892dc56f83fc21d5d7be12465ab2b48abe2c42584f7b0d360a02f7f91668638a7a6a8ad6a9f27a98324c3df278cc89ca8365275aa5d8864394d";
      "40b1387b348dd62151355305601c5a3cb6fee43c4011924aa3a89065247cf4ae29ff3780710359ab8c177aaf6276938d41a56a86083e02436ec6bb2fbc5ddcf8";
      "fd8b31d68497d80bf9a11fd14fa7284d237304d1361acf2a6f7ddb7ba762a4543394cc8e575653dd1f353e668063f633825bb767409f0b6e28c9f4c4a79633cd";
      "5aa040d0c022dcddac0c60757f3c572242e84b9d878ad44bbd921ed3e1bdab753a6a74e8c7254dfe05fccf8967912215b3d6c43992b720efab2004f90dbe0c0b";
      "83445715a408c812e22362925e6b87e581bf06af4a41f07af40a6ec75735ad077eeae2f6f08872264816c47b2f081c343dcd188a5dc28ac4aeb76760b7848646";
      "71be73259ba03164d111a8025cbadf75282fc7176149d5883bf2ed43a85b7aaa3a5c01e4e694c02ea6240b84ec0afdac31f02409316caeb6d62563d473296bbc";
      "ffb999978874c5719ca863674088cbf1de1e631ed69c1da1c4bdfc1c5d0bb1cc6f33372c57ea14ec50a5223a61ba02bcea6e95e9b15d2d4ae21f40cf73e004da";
    ];
    [
      "6d4690269cbfc304d6f2bad7c097bbc46def6791bed2ffd49ff452fe949e90fd3f29c3a252efc4e940da94e071fac9d5432df0a40751b2ddeb2f1d8aab9b3623";
      "f3ec810973e6a115698cace41ebbee9e1c1c2188195a74ea16086391747911b668a1bdd4585af33ff227cecdb415d5ebae5dab44121fc929161bfd8409982fca";
      "696d9895402d148e8e81aae3bcf772b4ca44c9cbb71af151e680c96af48e5fb023ffc2d1e1416b57768bd6fa1f51c2f65cd27bf273bd6060ddf9e3b13f4fc5f1";
      "ec1af9dc2926e809406226e7db81b21176d656291a26c5e9a4c448e30e7f99ee3d97ef800dfcc72f21ba99fcec9326eb2bedbe1d8d17a5f39b02152a8451a256";
      "030140301dcadfd74dbe3409599e0279b36ed0e6ee210eede86e4f1805ae11ca44be2978ac5c3aaa9891eae04e8331a435e6bbee5fc6683472622685d9ae6027";
      "dd2cad101279d52572b9ec73321953876478a5f9ca2ffb5da7ce66073294553b62272a95bff5bf80bb68a275ad204303e4c0e10d2e058d3a4d3a9eb33e8badf0";
      "724fee6ee2332bfaac5051e7ef12adad3b4105ad6cf184253dfa41d2fb204e656b835bb971a6780ddfdd00082c1ee903bf8ad70b20c61229505205b4a6e60441";
      "184f1ed3d42af526849f8599c9d67f9b4c24e3b70fb18e42a8f0573c2237133e44b7d19443b6b8c071ccdbe5ae824ed3c669c7db0dc994122279036c79a0c51a";
      "9586c7047d506ed0767ad7fe4472a077a13168392ce6945758fcb460ebeac2f82db0ccc06197a012dbd60a95d09e77e577722043b0edece349376d02a131b589";
      "98d6e188328f85824aaf655549bc40db004e7af6202119779d9290853a49e24b57f2835188dd6d96b126741797a05d58573655112e777e476cb4ed1f5a9bb104";
    ];
  |]

let vectors_assertion =
  nb_keys = List.length keys && nb_keys = Array.length sigs
