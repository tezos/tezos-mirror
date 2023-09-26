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
open Kzg.Bls
open Plonk.Identities
module L = LibCircuit
module PP = Plonk.Polynomial_protocol.Make (Aggregation.Polynomial_commitment)
module Main = Plonk.Main_protocol.Make (PP)
module Circuits = Aplonk.Circuit.V (Aplonk.Main_protocol.Main_Pack)

module Rollup_PIs = struct
  open Aplonk.Pi_parameters

  let get_pi_module _ = (module Rollup_example : CircuitPI)
end

let cs_of_repr circuit =
  let cs = L.get_cs circuit in
  let plonk_circuit = Plonk.Circuit.to_plonk cs in
  (plonk_circuit, cs.solver)

(* Tests on Circuit functions *)
module Internal = struct
  module Main = Plonk.Main_protocol
  module H = Plonk_test.Helpers.Make (Main)
  module Aplonk_main = Aplonk.Main_protocol.Make (Rollup_PIs)

  open Plonk_test.Helpers.Utils (L)

  (* Calls Plonk.(setup, prove & verify) on a plompiler circuit with the given inputs *)
  let test_plompiler_circuit ~name circuit inputs =
    let circuit, solver = cs_of_repr circuit in
    let witness =
      try Plompiler.Solver.solve solver inputs
      with e ->
        print_string "\nSolver failure\n" ;
        raise e
    in
    H.test_circuit ~name ~zero_knowledge:false circuit witness

  let test_pi () =
    let log = 4 in
    let n = 1 lsl log in
    let domain = Domain.build_power_of_two log in
    let p1 = S.random () in
    let p2 = S.random () in
    let x = S.random () in
    let expected_PIx =
      let pi_array =
        Array.of_list (p1 :: p2 :: List.init (n - 2) (fun _ -> S.zero))
      in
      let pi_poly = Evaluations.interpolation_fft2 domain pi_array in
      Poly.(evaluate (opposite pi_poly) x)
    in
    let generator = Domain.get domain 1 in
    let circuit =
      let open L in
      let n = S.of_int n in
      let* p1 = input (Input.scalar p1) in
      let* p2 = input (Input.scalar p2) in
      let* x = input (Input.scalar x) in
      let* exp = input (Input.scalar expected_PIx) in
      let* zs = Circuits.Constraints.compute_zs x n in
      let* res = Circuits.Gates.cs_pi ~generator ~n ~x ~zs [p1; p2] in
      assert_equal exp res
    in
    let inputs = [|p1; p2; x; expected_PIx|] in
    test_plompiler_circuit ~name:"test_pi" circuit inputs

  let test_t () =
    let d = 10 in
    let nb_t = 3 in
    let n = 20 in
    let t_list =
      List.init nb_t (fun _ ->
          Poly.of_coefficients (List.init d (fun i -> (S.random (), i))))
    in
    let x = S.random () in
    let xn = S.pow x (Z.of_int n) in
    let x_ni = Array.init nb_t (fun i -> S.pow xn (Z.of_int i)) in
    let t_list = List.map (fun t -> Poly.evaluate t x) t_list in
    let _, t_exp =
      List.fold_left
        (fun (i, acc) t_val -> (i + 1, S.(acc + (x_ni.(i) * t_val))))
        (0, S.zero)
        t_list
    in
    let circuit =
      let open L in
      let* xn = input (Input.scalar xn) in
      let* t_list = mapM (fun x -> input (Input.scalar x)) t_list in
      let* t_exp = input (Input.scalar t_exp) in
      let* t_res = Circuits.Constraints.compute_t xn t_list in
      assert_equal t_exp t_res
    in
    let inputs = Array.of_list ((xn :: t_list) @ [t_exp]) in
    test_plompiler_circuit ~name:"test_t" circuit inputs

  let test_sum_alpha () =
    let n = 20 in
    let alpha = S.random () in
    let list_val = List.init n (fun _ -> S.random ()) in
    let exp =
      List.fold_left
        (fun acc x -> S.((alpha * acc) + x))
        S.zero
        (List.rev list_val)
    in
    let circuit =
      let open L in
      let* list_val = mapM (fun x -> input (Input.scalar x)) list_val in
      let* alpha = input (Input.scalar alpha) in
      let* exp = input (Input.scalar exp) in
      let* res = Circuits.Constraints.sum_alpha_i list_val alpha in
      assert_equal exp res
    in
    let inputs = Array.of_list (list_val @ [alpha; exp]) in
    test_plompiler_circuit ~name:"test_sum_alpha" circuit inputs

  let tests =
    [
      Alcotest.test_case "Sum_alpha_i" `Quick test_sum_alpha;
      Alcotest.test_case "PI" `Quick test_pi;
      Alcotest.test_case "T" `Quick test_pi;
    ]
end

(* Tests on Main *)
module External = struct
  module Main = Aplonk.Main_protocol.Make (Rollup_PIs)

  (* Builds n witness
     witness is a function that takes a scalar pi_start & a solver and returns a witness that starts with pi_starts and its second element
  *)
  let get_witness (solver, witness) n =
    Printf.printf "\n\nnb_proofs = %d" n ;
    let pi_start = ref (S.random ()) in
    let inputs =
      List.init n (fun _ ->
          let x, pi_end = witness ~pi_start:!pi_start solver in
          pi_start := pi_end ;
          x)
    in
    inputs

  (* Run Pack & aPlonk proto on the given circuit *)
  let compare_protos
      ?(pi =
        (module Aplonk.Pi_parameters.Rollup_example
        : Aplonk.Pi_parameters.CircuitPI)) (circuit, witness) nb_proofs () =
    let module PI = (val pi) in
    let module PIs = struct
      let get_pi_module _ = (module PI : Aplonk.Pi_parameters.CircuitPI)
    end in
    let circuit, solver = cs_of_repr circuit in
    let witness = get_witness (solver, witness) nb_proofs in
    let circuit_map = Kzg.SMap.singleton "" (circuit, nb_proofs) in
    let inputs = Kzg.SMap.singleton "" witness in
    Printf.printf "\n\n*** SUPER ***" ;
    let module Helpers = Plonk_test.Helpers.Make (Aplonk.Main_protocol.Make (PIs)) in
    Helpers.test_circuits ~name:"compare_protos : aplonk" circuit_map inputs ;
    Printf.printf "\n\n*** PLONK ***" ;
    let module Helpers = Plonk_test.Helpers.Make (Plonk.Main_protocol) in
    Helpers.test_circuits ~name:"compare_protos : Pack" circuit_map inputs

  let first_test_proto_rollup nb_proofs () =
    let module PI = Aplonk.Pi_parameters.Rollup_example in
    let module Helpers =
      Plonk_test.Helpers.Make (Aplonk.Main_protocol.Make (Rollup_PIs)) in
    let l = PI.nb_inner in
    let circuit =
      let wires =
        let a = [0; 1; 2; 7; 8] in
        let b = [3; 4; 5; 9; 10] in
        let c = [2; 5; 6; 11; 12] in
        let d = [0; 0; 0; 0; 0] in
        let e = [0; 0; 0; 0; 0] in
        [|a; b; c; d; e|]
      in
      let gates =
        Plonk.Circuit.make_gates
          ~linear:
            [
              (0, S.[one; one; zero; zero; zero]);
              (1, S.[one; zero; zero; zero; zero]);
              (2, S.[mone; mone; mone; zero; zero]);
            ]
          ~linear_g:[(1, S.[one; zero; zero; zero; zero])]
          ~qm:S.[zero; zero; one; zero; zero]
          ~qc:S.[zero; one; zero; zero; zero]
          ~qecc_ed_add:S.[zero; zero; zero; one; zero]
          ()
      in
      Plonk.Circuit.make ~wires ~gates ~public_input_size:l ()
    in
    let witness ~pi_start _witness_aux =
      let x0 = pi_start in
      let x1 = S.random () in
      let x3 = S.random () in
      let x4 = S.random () in
      let x2 = S.(x0 + x3 + x4) in
      let x5 = S.(x1 + one) in
      let x6 = S.(x2 * x5) in
      let x7 = S.zero in
      let x8 = S.one in
      let x9 = S.zero in
      let x10 = S.one in
      let x11 = S.zero in
      let x12 = S.one in
      ([|x0; x1; x2; x3; x4; x5; x6; x7; x8; x9; x10; x11; x12|], x1)
    in
    let witness = get_witness (None, witness) nb_proofs in
    let circuit_map = Kzg.SMap.singleton "" (circuit, nb_proofs) in
    let inputs = Kzg.SMap.singleton "" witness in
    Helpers.test_circuits ~name:"first_test_proto_rollup" circuit_map inputs

  let compare_protos_dummy_circuit =
    let poseidon l =
      let module Poseidon = Plompiler.Poseidon128.V (Plompiler.LibCircuit) in
      Poseidon.digest l
    in
    let p1 =
      S.
        ( of_string
            "0x73c016a42ded9578b5ea25de7ec0e3782f0c718f6f0fbadd194e42926f661b51",
          of_string
            "0x289e87a2d3521b5779c9166b837edc5ef9472e8bc04e463277bfabd432243cca"
        )
    in
    let p2 =
      S.
        ( of_string
            "0x664321a58246e2f6eb69ae39f5c84210bae8e5c46641ae5c76d6f7c2b67fc475",
          of_string
            "0x362e1500d24eee9ee000a46c8e8ce8538bb22a7f1784b49880ed502c9793d457"
        )
    in
    let circuit =
      let module Edwards = Plompiler.JubjubEdwards (L) in
      let open L in
      let* x0 = input ~kind:`Public (Input.scalar S.one) in
      let* x1 = input ~kind:`Public (Input.scalar S.one) in
      let* p1 = Edwards.input_point p1 in
      let* p2 = Edwards.input_point p2 in
      let* sum = Edwards.add p1 p2 in
      let u = Edwards.get_x_coordinate sum in
      let v = Edwards.get_y_coordinate sum in
      let* res = poseidon (to_list [x0; x1; u; v]) in
      assert_equal res res
    in
    let witness ~pi_start solver =
      let x1 = S.random () in
      let u1, v1 = p1 in
      let u2, v2 = p2 in
      let witness =
        try Plompiler.Solver.solve solver [|pi_start; x1; u1; v1; u2; v2|]
        with e ->
          print_string "\nSolver failure\n" ;
          raise e
      in
      (witness, x1)
    in
    compare_protos (circuit, witness)

  let tests =
    [
      Alcotest.test_case "test proto" `Quick (first_test_proto_rollup 3);
      Alcotest.test_case "compare protos" `Slow (compare_protos_dummy_circuit 3);
    ]
end
