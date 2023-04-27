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

module type VARIANT = sig
  val word_size : int

  val block_size : int

  val sum_constants : int array

  val sigma_constants : int array

  val round_constants : string array

  val init_hash : string array

  val loop_bound : int

  val digest_blocks : int
end

let word_size_224_256 = 32

let word_size_384_512 = 64

let block_size_224_256 = 512

let block_size_384_512 = 1024

let sum_constants_224_256 = [|2; 13; 22; 6; 11; 25|]

let sum_constants_384_512 = [|28; 34; 39; 14; 18; 41|]

let sigma_constants_224_256 = [|7; 18; 3; 17; 19; 10|]

let sigma_constants_384_512 = [|1; 8; 7; 19; 61; 6|]

let round_constants_224_256 =
  [|
    "428a2f98";
    "71374491";
    "b5c0fbcf";
    "e9b5dba5";
    "3956c25b";
    "59f111f1";
    "923f82a4";
    "ab1c5ed5";
    "d807aa98";
    "12835b01";
    "243185be";
    "550c7dc3";
    "72be5d74";
    "80deb1fe";
    "9bdc06a7";
    "c19bf174";
    "e49b69c1";
    "efbe4786";
    "0fc19dc6";
    "240ca1cc";
    "2de92c6f";
    "4a7484aa";
    "5cb0a9dc";
    "76f988da";
    "983e5152";
    "a831c66d";
    "b00327c8";
    "bf597fc7";
    "c6e00bf3";
    "d5a79147";
    "06ca6351";
    "14292967";
    "27b70a85";
    "2e1b2138";
    "4d2c6dfc";
    "53380d13";
    "650a7354";
    "766a0abb";
    "81c2c92e";
    "92722c85";
    "a2bfe8a1";
    "a81a664b";
    "c24b8b70";
    "c76c51a3";
    "d192e819";
    "d6990624";
    "f40e3585";
    "106aa070";
    "19a4c116";
    "1e376c08";
    "2748774c";
    "34b0bcb5";
    "391c0cb3";
    "4ed8aa4a";
    "5b9cca4f";
    "682e6ff3";
    "748f82ee";
    "78a5636f";
    "84c87814";
    "8cc70208";
    "90befffa";
    "a4506ceb";
    "bef9a3f7";
    "c67178f2";
  |]

let round_constants_384_512 =
  [|
    "428a2f98d728ae22";
    "7137449123ef65cd";
    "b5c0fbcfec4d3b2f";
    "e9b5dba58189dbbc";
    "3956c25bf348b538";
    "59f111f1b605d019";
    "923f82a4af194f9b";
    "ab1c5ed5da6d8118";
    "d807aa98a3030242";
    "12835b0145706fbe";
    "243185be4ee4b28c";
    "550c7dc3d5ffb4e2";
    "72be5d74f27b896f";
    "80deb1fe3b1696b1";
    "9bdc06a725c71235";
    "c19bf174cf692694";
    "e49b69c19ef14ad2";
    "efbe4786384f25e3";
    "0fc19dc68b8cd5b5";
    "240ca1cc77ac9c65";
    "2de92c6f592b0275";
    "4a7484aa6ea6e483";
    "5cb0a9dcbd41fbd4";
    "76f988da831153b5";
    "983e5152ee66dfab";
    "a831c66d2db43210";
    "b00327c898fb213f";
    "bf597fc7beef0ee4";
    "c6e00bf33da88fc2";
    "d5a79147930aa725";
    "06ca6351e003826f";
    "142929670a0e6e70";
    "27b70a8546d22ffc";
    "2e1b21385c26c926";
    "4d2c6dfc5ac42aed";
    "53380d139d95b3df";
    "650a73548baf63de";
    "766a0abb3c77b2a8";
    "81c2c92e47edaee6";
    "92722c851482353b";
    "a2bfe8a14cf10364";
    "a81a664bbc423001";
    "c24b8b70d0f89791";
    "c76c51a30654be30";
    "d192e819d6ef5218";
    "d69906245565a910";
    "f40e35855771202a";
    "106aa07032bbd1b8";
    "19a4c116b8d2d0c8";
    "1e376c085141ab53";
    "2748774cdf8eeb99";
    "34b0bcb5e19b48a8";
    "391c0cb3c5c95a63";
    "4ed8aa4ae3418acb";
    "5b9cca4f7763e373";
    "682e6ff3d6b2b8a3";
    "748f82ee5defb2fc";
    "78a5636f43172f60";
    "84c87814a1f0ab72";
    "8cc702081a6439ec";
    "90befffa23631e28";
    "a4506cebde82bde9";
    "bef9a3f7b2c67915";
    "c67178f2e372532b";
    "ca273eceea26619c";
    "d186b8c721c0c207";
    "eada7dd6cde0eb1e";
    "f57d4f7fee6ed178";
    "06f067aa72176fba";
    "0a637dc5a2c898a6";
    "113f9804bef90dae";
    "1b710b35131c471b";
    "28db77f523047d84";
    "32caab7b40c72493";
    "3c9ebe0a15c9bebc";
    "431d67c49c100d4c";
    "4cc5d4becb3e42b6";
    "597f299cfc657e2a";
    "5fcb6fab3ad6faec";
    "6c44198c4a475817";
  |]

let loop_bound_224_256 = 64

let loop_bound_384_512 = 80

module Sha224 : VARIANT = struct
  let word_size = word_size_224_256

  let block_size = word_size_224_256

  let sum_constants = sum_constants_224_256

  let sigma_constants = sigma_constants_224_256

  let round_constants = round_constants_224_256

  let init_hash =
    [|
      "c1059ed8";
      "367cd507";
      "3070dd17";
      "f70e5939";
      "ffc00b31";
      "68581511";
      "64f98fa7";
      "befa4fa4";
    |]

  let loop_bound = loop_bound_224_256

  let digest_blocks = 7
end

module Sha256 : VARIANT = struct
  let word_size = word_size_224_256

  let block_size = block_size_224_256

  let sum_constants = sum_constants_224_256

  let sigma_constants = sigma_constants_224_256

  let round_constants = round_constants_224_256

  let init_hash =
    [|
      "6a09e667";
      "bb67ae85";
      "3c6ef372";
      "a54ff53a";
      "510e527f";
      "9b05688c";
      "1f83d9ab";
      "5be0cd19";
    |]

  let loop_bound = loop_bound_224_256

  let digest_blocks = 8
end

module Sha384 : VARIANT = struct
  let word_size = word_size_384_512

  let block_size = block_size_384_512

  let sum_constants = sum_constants_384_512

  let sigma_constants = sigma_constants_384_512

  let round_constants = round_constants_384_512

  let init_hash =
    [|
      "cbbb9d5dc1059ed8";
      "629a292a367cd507";
      "9159015a3070dd17";
      "152fecd8f70e5939";
      "67332667ffc00b31";
      "8eb44a8768581511";
      "db0c2e0d64f98fa7";
      "47b5481dbefa4fa4";
    |]

  let loop_bound = loop_bound_384_512

  let digest_blocks = 6
end

module Sha512 : VARIANT = struct
  let word_size = word_size_384_512

  let block_size = block_size_384_512

  let sum_constants = sum_constants_384_512

  let sigma_constants = sigma_constants_384_512

  let round_constants = round_constants_384_512

  let init_hash =
    [|
      "6a09e667f3bcc908";
      "bb67ae8584caa73b";
      "3c6ef372fe94f82b";
      "a54ff53a5f1d36f1";
      "510e527fade682d1";
      "9b05688c2b3e6c1f";
      "1f83d9abfb41bd6b";
      "5be0cd19137e2179";
    |]

  let loop_bound = loop_bound_384_512

  let digest_blocks = 8
end
