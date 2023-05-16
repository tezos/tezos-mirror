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

open Plonk_test
open Helpers

let nb_proofs = 12

let circuit_size = 4

module Scalar = Plonk.Bls.Scalar
(* open Helpers *)

module Port : sig
  val make : unit -> int
end = struct
  let port = ref 50000

  let make () =
    let p = !port in
    port := p + 1 ;
    p
end

module type DP_for_tests = sig
  include Distributed_prover.S

  module Worker_Main : Distribution.Main_protocol.S

  val pp_file : string

  val get_distributed_pp : MP.prover_public_parameters -> bytes
end

module DP_PlonK (Main : Distribution.Main_protocol.S) = struct
  include Distributed_prover.Make (Main)
  module Worker_Main = Main

  let pp_file = Filenames.plonk_pp_file

  let get_distributed_pp pp_prover =
    Plompiler.Utils.to_bytes MP.prover_public_parameters_t pp_prover
end

module DP_aPlonk (PI : Aplonk.Pi_parameters.S) = struct
  include Distributed_prover.Super_impl (PI)
  module Worker_Main = Distributed_prover.Main_Pack

  let pp_file = Filenames.meta_pp_file

  let get_distributed_pp pp_prover =
    let ({main_pp; _} : MP.prover_public_parameters) = pp_prover in
    Plompiler.Utils.to_bytes
      Distributed_prover.Main_Pack.prover_public_parameters_t
      main_pp
end

module No_public_input_PIs = struct
  let get_pi_module _ =
    (module Aplonk.Pi_parameters.No_public_input
    : Aplonk.Pi_parameters.CircuitPI)
end

module Rollup_example_PIs = struct
  let get_pi_module _ =
    (module Aplonk.Pi_parameters.Rollup_example : Aplonk.Pi_parameters.CircuitPI)
end

module DP_Kzg () = DP_PlonK (Distributed_prover.Main_Kzg)
module DP_Pack () = DP_PlonK (Distributed_prover.Main_Pack)
module DP_Meta () = DP_aPlonk (Rollup_example_PIs)

let srs = srs

module Circuit_Builder = struct
  let base nb_proofs k =
    let open Cases in
    let _, circuit_map, witness, _ =
      Big_circuit.make ~nb_proofs ~public_input_size:2 ~k |> aggregate_cases
    in
    (* FIXME Multicircuit with Meta-PlonK doesn’t work *)
    (* let circuit_map =
         Plonk.SMap.(union_disjoint circuit_map (update_keys (fun i -> i ^ "2") circuit_map))
       in *)
    (circuit_map, witness)

  let range_checks nb_proofs _ =
    let open Cases in
    let _, circuit_map, witness, _ =
      List.init nb_proofs (Fun.const Range_Checks.valid) |> aggregate_cases
    in
    (circuit_map, witness)
end

module Helpers (DP : DP_for_tests) = struct
  module MP = DP.MP
  module Runner = Master_runner.Make (DP.D)

  let parse_nodes s =
    (* Expected as ip:port;ip:port... *)
    let node_strings = String.split_on_char ',' s in
    List.mapi
      (fun i ns ->
        let n = String.split_on_char ':' ns in
        Runner.
          {
            ip = List.hd n;
            port = int_of_string @@ List.nth n 1;
            name = "worker" ^ string_of_int i;
          })
      node_strings

  let run_master ?nb_proofs ?(circuit_size = circuit_size) ~self_node ~nodes ()
      =
    let nb_proofs =
      match nb_proofs with None -> List.length nodes | Some n -> n
    in
    let circuit_map, x_map = Circuit_Builder.base nb_proofs circuit_size in
    let pp_prover, pp_verifier =
      MP.setup ~zero_knowledge:false circuit_map ~srs
    in
    let oc = open_out DP.pp_file in
    let b = DP.get_distributed_pp pp_prover in
    output_bytes oc b ;
    close_out oc ;
    let inputs =
      Plonk.SMap.map
        (List.map (fun witness -> MP.{witness; input_commitments = []}))
        x_map
    in
    let verifier_inputs = MP.to_verifier_inputs pp_prover inputs in

    let t1 = Unix.gettimeofday () in

    let proof =
      Runner.run
        ~self_node
        ~nodes
        DP.(distributed_prover_main ~inputs pp_prover)
    in
    let t2 = Unix.gettimeofday () in
    Printf.printf "Prover time: %4.2f\n" (t2 -. t1) ;
    assert (MP.verify pp_verifier ~inputs:verifier_inputs proof)
end
