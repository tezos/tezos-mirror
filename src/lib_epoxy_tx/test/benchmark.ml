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

open Epoxy_tx.Tx_rollup
open Plompiler
module H = Helpers.V (LibCircuit)
module HashPV = Plompiler.Anemoi128
module SchnorrPV = Plompiler.Schnorr (HashPV)
module Schnorr = SchnorrPV.P

let sks : Schnorr.sk array =
  Array.init Constants.max_nb_accounts (fun _ ->
      Mec.Curve.Jubjub.AffineEdwards.Scalar.random ())

let pks = Array.map Schnorr.neuterize sks

let circuit_inputs ~nb_batches ~batch_size =
  Plonk_test.Helpers.Time.time "prepare" (fun () ->
      let init_state =
        P.make_state
          [
            (pks.(0), Z.of_int 1_000_000, [||]);
            (pks.(0), Z.of_int 1_000_000, [||]);
          ]
      in
      (* Batches and intermediate states *)
      let rs =
        P.generate_transactions
          ~nb_batches
          ~batch_size
          ~src_pos:Z.zero
          ~dst_pos:(Z.of_int Constants.max_nb_tickets)
          ~amount:
            {
              id = Constants.tez_id;
              amount =
                Types.P.Bounded.make ~bound:Constants.Bound.max_amount Z.one;
            }
          ~fee:(Z.of_int 2)
          ~sks
          init_state
      in
      let batch, s = List.hd rs in
      let c = H.inner_batch batch init_state s in
      let initial, _public_input_size = LibCircuit.get_inputs c in
      let r = LibCircuit.get_cs ~optimize:true c in
      let private_inputs = Solver.solve r.solver initial in
      let circ = Plonk.Circuit.to_plonk r in
      let solver = Solver.solve r.solver in
      let hd = private_inputs in
      let cs, _ =
        List.fold_left
          (fun (acc, acc_s) (batch, s) ->
            let c = H.inner_batch batch acc_s s in
            let initial, _ = LibCircuit.get_inputs c in
            let private_inputs = solver initial in
            (private_inputs :: acc, s))
          ([], s)
          (List.tl rs)
      in
      (circ, hd :: List.rev cs))

let benchmark ~nb_batches ~batch_size () =
  Format.printf "nb_batches %i batch_size %i\n%!" nb_batches batch_size ;
  let circ_map, private_inputs =
    let circ, private_inputs = circuit_inputs ~nb_batches ~batch_size in
    let circ_map = Plonk.SMap.(add "batch" (circ, nb_batches) empty) in
    let private_inputs = Plonk.SMap.(add "batch" private_inputs empty) in
    (circ_map, private_inputs)
  in
  Gc.full_major () ;
  let module H = Plonk_test.Helpers.Make (Plonk.Main_protocol) in
  H.test_circuits ~name:"benchmark" ~verbose:true circ_map private_inputs

let benchmark_meta ~nb_batches ~batch_size () =
  let module PIs = struct
    let get_pi_module _ =
      (module Epoxy_tx.Tx_rollup.PI_parameters_predicate_private_batch
      : Aplonk.Pi_parameters.CircuitPI)
  end in
  let module H = Plonk_test.Helpers.Make (Aplonk.Main_protocol.Make (PIs)) in
  let circuit, witness = circuit_inputs ~nb_batches ~batch_size in
  let circuit_map = Plonk.SMap.singleton "" (circuit, nb_batches) in
  let inputs = Plonk.SMap.singleton "" witness in
  Gc.full_major () ;
  H.test_circuits ~name:"benchmark_meta" ~verbose:true circuit_map inputs

let tests =
  let nb_rep = 2 in
  [
    Alcotest.test_case "Private batch benchmark" `Slow (fun () ->
        ignore
        @@ List.map
             (fun (nb_batches, batch_size) ->
               Plonk_test.Helpers.Time.bench_test_circuit
                 ~nb_rep
                 (benchmark ~nb_batches ~batch_size)
                 ())
             [(4, 1)]);
    Alcotest.test_case "Private batch benchmark aPlonK" `Slow (fun () ->
        ignore
        @@ List.map
             (fun (nb_batches, batch_size) ->
               Plonk_test.Helpers.Time.bench_test_circuit
                 ~nb_rep
                 (benchmark_meta ~nb_batches ~batch_size)
                 ())
             [(4, 1)]);
  ]
