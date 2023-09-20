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
open LibCircuit
module CS = Plonk.Circuit
module Hash = Poseidon128.V (LibCircuit)
module Main = Plonk.Main_protocol
module Helpers = Plonk_test.Helpers

open Plonk_test.Helpers.Utils (LibCircuit)

let hash array =
  let ctx = Poseidon128.P.init ~input_length:(Array.length array) () in
  let ctx = Poseidon128.P.digest ctx array in
  Poseidon128.P.get ctx

let build_circuit x1 x2 x3 x4 y () =
  let* expected = begin_input_com Fun.id |: Input.scalar y |> end_input_com in
  let* x1, x2 =
    begin_input_com (fun a b -> (a, b))
    |: Input.scalar x1 |: Input.scalar x2 |> end_input_com
  in
  let* x3 = input ~kind:`Public (Input.scalar x3) in
  let* x4 = input (Input.scalar x4) in
  let* out = Hash.digest ~input_length:4 (to_list [x1; x2; x3; x4]) in
  with_bool_check (equal out expected)

let test_input_com () =
  let w = S.zero in
  let circuit = build_circuit w w w w w () in

  let cs = get_cs ~optimize:true circuit in
  assert (cs.input_com_sizes = [1; 2]) ;
  assert (cs.public_input_size = 1) ;
  let plonk_circuit = Plonk.Circuit.to_plonk cs in
  let cname = "" in
  let circuits_map = Plonk.SMap.singleton cname (plonk_circuit, 1) in
  let zero_knowledge = false in
  let pp_prv, pp_vrf =
    Main.setup ~zero_knowledge circuits_map ~srs:Helpers.srs
  in
  let x1, x2, x3, x4 = (S.random (), S.random (), S.random (), S.random ()) in
  let y = hash [|x1; x2; x3; x4|] in
  let private_inputs = Solver.solve cs.solver [|y; x1; x2; x3; x4|] in
  assert (CS.sat cs private_inputs) ;
  let input_com_y = Main.input_commit pp_prv [|y|] in
  let input_com_x1x2 = Main.input_commit ~shift:1 pp_prv [|x1; x2|] in
  let public = [|x3|] in
  let input_commitments = [input_com_y; input_com_x1x2] in
  let verifier_input_commitments =
    [input_com_y.public; input_com_x1x2.public]
  in
  let inputs = Main.{input_commitments; witness = private_inputs} in
  let inputs_map = Plonk.SMap.singleton cname [inputs] in
  let verifier_inputs =
    Plonk.SMap.singleton cname ([public], [verifier_input_commitments])
  in
  let proof = Main.prove pp_prv ~inputs:inputs_map in
  let verif_ok = Main.verify pp_vrf ~inputs:verifier_inputs proof in
  assert verif_ok

let tests = [Alcotest.test_case "Input commitment" `Quick test_input_com]
