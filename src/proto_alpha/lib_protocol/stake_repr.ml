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

type t = {frozen : Tez_repr.t; delegated : Tez_repr.t}

let make ~frozen ~delegated = {frozen; delegated}

let total {frozen; delegated} = Tez_repr.(frozen +? delegated)

let get_frozen {frozen; _} = frozen

let encoding =
  let open Data_encoding in
  conv
    (fun {frozen; delegated} -> (frozen, delegated))
    (fun (frozen, delegated) -> {frozen; delegated})
    (obj2 (req "frozen" Tez_repr.encoding) (req "delegated" Tez_repr.encoding))

let zero = make ~frozen:Tez_repr.zero ~delegated:Tez_repr.zero

let ( +? ) {frozen = f1; delegated = d1} {frozen = f2; delegated = d2} =
  let open Result_syntax in
  let* frozen = Tez_repr.(f1 +? f2) in
  let+ delegated = Tez_repr.(d1 +? d2) in
  {frozen; delegated}
