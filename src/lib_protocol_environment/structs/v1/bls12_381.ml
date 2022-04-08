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

   Removed values cited above can be mocked with anything because it was not
   used in the environment. Only Fq12.eq and Fq12.one is required to be
   correctly implemented. Therefore, the implementation is replaced with a
   `failwith "Not implemented".

   Also, bls12-381.1.0.0 added Bls12_381.Pairing.pairing_check which does
   exactly the same job than the Michelson instruction IPairing_check_bls12_381
   is expected to do. Therefore, the complete module Pairing is not required
   anymore in Tezos_crypto.
*)

module Fr = Bls12_381.Fr
module G1 = Bls12_381.G1
module G2 = Bls12_381.G2

module Fq12 = struct
  include Bls12_381.Fq12

  let check_bytes _x = failwith "Not implemented"

  let add _x _y = failwith "Not implemented"

  let negate _x = failwith "Not implemented"
end

(* bls12-381.3.0.0 introduces GT as the prime subgroup of Fq12 of order
   Fr.order. These two modules are algebraically different. However, in
   environments < V4, GT was considered to be Fq12, and the implementation of
   pairing was returning a point in Fq12, which is not algebraically strict
   enough. It should be in GT.
   Therefore, we must serialize a point in GT and deserialize it in Fq12.

   The conversions are not required for further environments as
   Bls12_381.Pairing.pairing_check is used and abstract the algebraic
   structures.
   Using F12.of_bytes_exn is not problematic as GT is a subgroup of Fq12.
   Therefore any serialized value of GT will give a valid point by
   Fq12.of_bytes_exn, supposing they follow the same serialisation rules.
*)
module Gt = Fq12

let gt_to_fq12 x = Bls12_381.GT.to_bytes x |> Fq12.of_bytes_exn

let final_exponentiation_opt x =
  Bls12_381.Pairing.final_exponentiation_opt x |> Option.map gt_to_fq12

let pairing g1 g2 = Bls12_381.Pairing.pairing g1 g2 |> gt_to_fq12

let miller_loop = Bls12_381.Pairing.miller_loop
