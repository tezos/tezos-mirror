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

module Main = Plonk.Main_protocol
module H = Plonk_test.Helpers.Make (Main)
module S = Plompiler.S

(* let () =
   let k = 16 in
   let public_input_size = 10 in
   let start_build_circuit = Unix.gettimeofday () in
   let case =
     Plonk_test.Cases.Big_circuit.make ~nb_proofs:1 ~public_input_size ~k
     |> List.hd
   in
   let end_build_circuit = Unix.gettimeofday () in
   Printf.printf "\n\nNumber of gates : %d\nNumber of public inputs : %d\n"
     ((1 lsl k) - 1)
     public_input_size;
   Printf.printf "Dummy circuit built in %f ms.\n"
     ((end_build_circuit -. start_build_circuit) *. 1000.);
   H.run_test_case ~verbose:true ~zero_knowledge:false case () *)

let rc_circuit n nb_range_checks nb_bits =
  let open Plompiler.LibCircuit in
  let inputs =
    List.init nb_range_checks (fun i ->
        Input.scalar (Plompiler.S.of_z (Z.of_int (i + 1))))
  in
  Printf.printf "\nnb_bits : %d" nb_bits ;
  Printf.printf "\nbound : %d\n" (1 lsl nb_bits) ;
  let range_checks r_list =
    let* b =
      mapM
        (Num.is_upper_bounded_unsafe
           ~nb_bits
           ~bound:Z.((one lsl nb_bits) - one))
        r_list
    in
    Bool.band_list b
  in
  (* This circuit has 2^n dummy constraints, takes [inputs] as inputs, and if
     [rc] is true, it adds a range check on every input *)
  let circuit rc inputs =
    let* inputs = mapM input inputs in
    let* i1 = constant_scalar S.zero in
    let l = List.init (1 lsl n) (Fun.const i1) in
    let i = ref 0 in
    let* foo =
      foldM
        (fun acc a ->
          i := !i + 1 ;
          let i = S.of_z (Z.of_int !i) in
          Num.custom ~qc:i ~ql:i acc a)
        i1
        l
    in
    let* rc = if rc then range_checks inputs else equal foo foo in
    Bool.assert_true rc
  in
  (circuit true inputs, circuit false inputs)

let () =
  let open Plonk_test.Cases in
  let nb_constraints = 13 in
  let nb_range_checks = 1 lsl 6 in
  let nb_bits = 16 in
  let start_build_circuit = Unix.gettimeofday () in
  let with_rc, no_rc = rc_circuit nb_constraints nb_range_checks nb_bits in
  let range_checks = List.init nb_range_checks (fun i -> i + 1) in
  let base_witness = !!range_checks in
  Printf.printf "\n\n------------------\n" ;
  let cs_with_rc = Plompiler.LibCircuit.get_cs with_rc in
  let witness_with_rc = Plompiler.Solver.solve cs_with_rc.solver base_witness in
  let circuit_with_rc = Plonk.Circuit.to_plonk cs_with_rc in
  Printf.printf "\n\n------------------\n" ;
  (* Note that range checks are not the same as the ones in the previous
     circuit ; it is just indexes I took to make it work *)
  let range_checks =
    Plonk.SMap.singleton
      "w0"
      List.(
        rev
          (init nb_range_checks (fun i ->
               ((1 lsl (nb_constraints + 1)) - i - 1, nb_bits))))
  in
  let cs_no_rc = Plompiler.LibCircuit.get_cs no_rc in
  let witness_no_rc = Plompiler.Solver.solve cs_no_rc.solver base_witness in
  let circuit_no_rc = Plonk.Circuit.to_plonk ~range_checks cs_no_rc in
  let end_build_circuit = Unix.gettimeofday () in
  Printf.printf
    "Dummy circuit built in %f ms.\n"
    ((end_build_circuit -. start_build_circuit) *. 1000.) ;
  Printf.printf "\n\n------------------\n" ;
  Plonk_test.Helpers.(
    Time.bench_test_circuit ~nb_rep:1 (fun () ->
        H.test_circuit ~name:"" ~verbose:true circuit_with_rc witness_with_rc))
    () ;
  Gc.full_major () ;
  Printf.printf "\n\n------------------\n" ;
  Plonk_test.Helpers.(
    Time.bench_test_circuit ~nb_rep:1 (fun () ->
        H.test_circuit ~name:"" ~verbose:true circuit_no_rc witness_no_rc))
    ()
