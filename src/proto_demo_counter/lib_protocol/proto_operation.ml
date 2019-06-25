(*****************************************************************************)

(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

type t = IncrA | IncrB | Transfer of Int32.t

let encoding =
  let open Data_encoding in
  let casea =
    let dest = function IncrA -> Some () | _ -> None in
    let const () = IncrA in
    case ~title:"IncrA" (Tag 0) (obj1 (req "IncrA" empty)) dest const
  in
  let caseb =
    let dest = function IncrB -> Some () | _ -> None in
    let const () = IncrB in
    case ~title:"IncrB" (Tag 1) (obj1 (req "IncrB" empty)) dest const
  in
  let casec =
    let dest = function Transfer i -> Some i | _ -> None in
    let const i = Transfer i in
    case ~title:"Transfer" (Tag 2) (obj1 (req "Transfer" int32)) dest const
  in
  union [casea; caseb; casec]

let to_pii t =
  match t with IncrA -> (0l, 0l) | IncrB -> (1l, 0l) | Transfer i -> (2l, i)

let compare x y =
  let (a, b) = to_pii x in
  let (u, v) = to_pii y in
  let open Compare.Int32 in
  if a = u then compare b v else compare a u
