(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {frozen : Tez_repr.t; weighted_delegated : Tez_repr.t}

let make ~frozen ~weighted_delegated = {frozen; weighted_delegated}

let get_frozen {frozen; _} = frozen

let encoding =
  let open Data_encoding in
  conv
    (fun {frozen; weighted_delegated} -> (frozen, weighted_delegated))
    (fun (frozen, weighted_delegated) -> {frozen; weighted_delegated})
    (obj2 (req "frozen" Tez_repr.encoding) (req "delegated" Tez_repr.encoding))

let zero = make ~frozen:Tez_repr.zero ~weighted_delegated:Tez_repr.zero

let ( +? ) {frozen = f1; weighted_delegated = d1}
    {frozen = f2; weighted_delegated = d2} =
  let open Result_syntax in
  let* frozen = Tez_repr.(f1 +? f2) in
  let+ weighted_delegated = Tez_repr.(d1 +? d2) in
  {frozen; weighted_delegated}

let staking_weight {frozen; weighted_delegated} =
  let frozen = Tez_repr.to_mutez frozen in
  let weighted_delegated = Tez_repr.to_mutez weighted_delegated in
  Int64.add frozen weighted_delegated

let compare s1 s2 = Int64.compare (staking_weight s1) (staking_weight s2)
