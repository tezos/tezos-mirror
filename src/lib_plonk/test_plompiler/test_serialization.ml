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

open Plompiler
open Plonk_test
open Helpers
open LibCircuit

open Utils (LibCircuit)

let test_serialization () =
  let module Hash = Gadget.Poseidon128.V (LibCircuit) in
  let poseidon x =
    let* x = input ~kind:`Public x in
    Hash.digest (LibCircuit.to_list [x])
  in
  let LibCircuit.{cs; _} = LibCircuit.get_cs (poseidon @@ si 1) in
  let path = "test_serialization.json" in
  Plompiler.Utils.save_cs_to_file path [] cs ;
  let _, cs' = Plompiler.Utils.load_cs_from_file path in
  assert (
    List.for_all2
      (fun g g' -> Array.for_all2 Csir.CS.raw_constraint_equal g g')
      cs
      cs')

let tests =
  List.map
    (fun (name, test) -> Alcotest.test_case name `Quick test)
    [("Poseidon128", test_serialization)]
