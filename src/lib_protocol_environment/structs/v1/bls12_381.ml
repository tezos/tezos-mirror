(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.ch>                      *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This file replaces the implementation of Tezos_crypto.BLS12_381

   The dependency bls12-381 used by Tezos_crypto before V4 started was
   considering Fq12 as a field. bls12-381.1.0.0 introduced a breaking API
   changes, changing Fq12 signature to a group. Functions like
   check_bytes, add, one, negate and order have been removed from the API
   (https://gitlab.com/dannywillems/ocaml-bls12-381/-/commit/1dfb8bd813d12539b4007af095f3125646477319).
   A package bls12-381-legacy.0.4.3 (version used up to v3) has been released
   without depending on the virtual package bls12-381 to be used in Tezos in
   harmony with the new implementations of bls12-381-unix.
   The content of this file is simply the content of Tezos_crypto.BLS12_381
   using bls12-381-legacy instead of the virtual package and its implementation.
*)

module Fr = Bls12_381_legacy.Fr
module Fq12 = Bls12_381_legacy.Fq12
module Gt = Bls12_381_legacy.Fq12
module G1 = Bls12_381_legacy.G1.Uncompressed
module G2 = Bls12_381_legacy.G2.Uncompressed
include Bls12_381_legacy.Pairing
