(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type proposal = Protocol_hash.t

type ballot = {yays_per_roll : int; nays_per_roll : int; passes_per_roll : int}

let ballot_encoding =
  let open Data_encoding in
  conv
    (fun {yays_per_roll; nays_per_roll; passes_per_roll} ->
      (yays_per_roll, nays_per_roll, passes_per_roll))
    (fun (yays_per_roll, nays_per_roll, passes_per_roll) ->
      let open Constants_repr in
      if
        Compare.Int.( >= ) yays_per_roll 0
        && Compare.Int.( >= ) nays_per_roll 0
        && Compare.Int.( >= ) passes_per_roll 0
        && Compare.Int.( = )
             (yays_per_roll + nays_per_roll + passes_per_roll)
             fixed.votes_per_roll
      then {yays_per_roll; nays_per_roll; passes_per_roll}
      else invalid_arg "ballot_encoding")
  @@ obj3
       (req "yays_per_roll" uint16)
       (req "nays_per_roll" uint16)
       (req "passes_per_roll" uint16)
