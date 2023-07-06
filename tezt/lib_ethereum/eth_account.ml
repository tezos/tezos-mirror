(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type t = {address : string; private_key : string; public_key : string}

let bootstrap_accounts =
  [|
    {
      address = "0x6ce4d79d4E77402e1ef3417Fdda433aA744C6e1c";
      private_key =
        "0x9722f6cc9ff938e63f8ccb74c3daa6b45837e5c5e3835ac08c44c50ab5f39dc0";
      public_key =
        "046edc43401193c9321730cdf73e454f68e8aa52e377d001499b0eaa431fa4763102e685fe33851f5f51bd31adb41582bbfb0ad85c1089c0a0b4adc049a271bc01";
    };
    {
      address = "0xB53dc01974176E5dFf2298C5a94343c2585E3c54";
      private_key =
        "0x3a6a6ca30c1ef1ce605a63a7a1a4ff4c689f8414ca0838bca29423f0ec280ff5";
      public_key =
        "0403450b94c1aa719ead59e231788ace63e3cb88e49e3cb2474ecd30a64755252190fe7b811ab42298f21440226496c890d6b0b7a623adcf12617b99afcf439419";
    };
    {
      address = "0x9b49c988b5817Be31DfB00F7a5a4671772dCce2B";
      private_key =
        "0x0eb9bfa77d6cd145cdc0e3d6f902ee1464aeb5f62b02e38f111c9b60cd3adab5";
      public_key =
        "04aace5a1bba77b9c91e507f830338dd655eacae345ad2729057705eb717a960e3e4d9bea19403a2246bdc4277491635e4a90e06494b1ff3d10eec20f20f3bf8d6";
    };
  |]
