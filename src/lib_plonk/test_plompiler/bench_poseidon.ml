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
open LibCircuit
module CS = Plonk.Circuit
module Hash = Poseidon128.V (LibCircuit)
module Main = Plonk.Main_protocol

open Helpers.Utils (LibCircuit)

let hash x =
  let ctx = Poseidon128.P.init ~input_length:1 () in
  let ctx = Poseidon128.P.digest ctx [|x|] in
  Poseidon128.P.get ctx

let rec repeated_hash n x = if n <= 0 then x else repeated_hash (n - 1) (hash x)

let build_poseidon_circuit ~nb_hashes_per_circuit x y () =
  let* start = input ~kind:`Public @@ Input.scalar x in
  let* expected = input @@ Input.scalar y in
  let* out =
    foldM
      (fun acc _ ->
        let* acc = Hash.digest ~input_length:1 (to_list [acc]) in
        ret acc)
      start
      (List.init nb_hashes_per_circuit Fun.id)
  in
  with_bool_check (equal out expected)

let benchmark () =
  let nb_hashes_per_circuit = 100 in
  let nb_proofs = 2 in

  let x = S.random () in
  let y = repeated_hash nb_hashes_per_circuit x in
  let circuit = build_poseidon_circuit ~nb_hashes_per_circuit x y () in

  let cs = get_cs ~optimize:true circuit in
  let initial, _ = get_inputs circuit in
  let private_inputs = Solver.solve cs.solver initial in

  assert (CS.sat cs.cs [] private_inputs) ;

  let inputs = Main.{witness = private_inputs; input_commitments = []} in
  let plonk_circuit = Plonk.Circuit.to_plonk cs in
  let cname = "" in
  let replicate x = List.init nb_proofs (Fun.const x) in
  let circuits_map = Plonk.SMap.singleton cname (plonk_circuit, nb_proofs) in
  let inputs_map = Plonk.SMap.singleton cname (replicate inputs) in

  let zero_knowledge = false in
  Printf.printf
    "\nIterated Poseidon circuit of %d hashes\n"
    nb_hashes_per_circuit ;
  Printf.printf "Constraints per circuit: %d\n" Array.(concat cs.cs |> length) ;
  Printf.printf "Number of proofs: %d\n" nb_proofs ;
  Printf.printf
    "Total number of Poseidon hashes involved: %d\n"
    (nb_hashes_per_circuit * nb_proofs) ;
  Format.print_flush () ;

  let pp_prv, pp_vrf =
    Helpers.Time.time "Setup" @@ fun () ->
    Main.setup ~zero_knowledge circuits_map ~srs:Helpers.srs
  in

  let proof =
    Helpers.Time.time "Prove" @@ fun () -> Main.prove pp_prv ~inputs:inputs_map
  in

  let verifier_inputs = Main.to_verifier_inputs pp_prv inputs_map in

  let verif_ok =
    Helpers.Time.time "Verify" @@ fun () ->
    Main.verify pp_vrf ~inputs:verifier_inputs proof
  in
  assert verif_ok

let bench = [Alcotest.test_case "Poseidon benchmark" `Slow benchmark]
