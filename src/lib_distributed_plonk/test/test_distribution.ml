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

open Distributed_plonk_test.Distribution_helpers
open Distributed_plonk

let test_distribution dp () =
  let module DP = (val dp : DP_for_tests) in
  let module Worker0 = Worker.Make (DP.Worker_Main) in
  let module Worker1 = Worker.Make (DP.Worker_Main) in
  let module Master = DP.D in
  let loopback = "127.0.0.1" in
  let master_port = Port.make () in
  let worker0_port = Port.make () in
  let worker1_port = Port.make () in
  let worker0_config =
    let open Worker0.D in
    Remote
      {
        Remote_config.node_name = "worker0";
        Remote_config.local_port = worker0_port;
        Remote_config.connection_backlog = 10;
        Remote_config.node_ip = loopback;
        Remote_config.remote_nodes = [];
      }
  in
  let worker1_config =
    let open Worker1.D in
    Remote
      {
        Remote_config.node_name = "worker1";
        Remote_config.local_port = worker1_port;
        Remote_config.connection_backlog = 10;
        Remote_config.node_ip = loopback;
        Remote_config.remote_nodes = [];
      }
  in
  let master_config =
    let open Master in
    Remote
      {
        Remote_config.node_name = "master";
        Remote_config.local_port = master_port;
        Remote_config.connection_backlog = 10;
        Remote_config.node_ip = loopback;
        Remote_config.remote_nodes =
          [
            (loopback, worker0_port, "worker0");
            (loopback, worker1_port, "worker1");
          ];
      }
  in
  let circuit_map, x_map = circuits_inputs nb_proofs circuit_size in
  let pp_prover, pp_verifier =
    DP.MP.setup ~zero_knowledge:false circuit_map ~srs
  in
  let b = DP.get_distributed_pp pp_prover in
  let oc = open_out DP.pp_file in
  output_bytes oc b ;
  close_out oc ;
  let inputs =
    Plonk.SMap.map
      (List.map (fun witness -> DP.MP.{witness; input_commitments = []}))
      x_map
  in
  let master_proc m ~ret () =
    let open Master in
    let* nodes = get_remote_nodes in
    let* pid_to_send_to = get_self_pid in
    (* Dummy worker process in the master *)
    let* () = register "worker" (fun _pid () -> return ()) in
    (* spawn and monitor a process on the remote node atomically *)
    let* remote_pids =
      mapM
        (fun n ->
          let+ pid, _ref =
            spawn ~monitor:true n (Registered "worker") pid_to_send_to
          in
          pid)
        nodes
    in
    let+ r = m ~workers:remote_pids in
    ret := Some r ;
    ()
  in
  let ret = ref None in
  Lwt.async (fun () ->
      Worker0.D.run_node
        ~process:(fun () ->
          Worker0.D.register "worker" Worker0.(worker_proc DP.pp_file))
        worker0_config) ;
  Lwt.async (fun () ->
      Worker1.D.run_node
        ~process:(fun () ->
          Worker1.D.register "worker" Worker1.(worker_proc DP.pp_file))
        worker1_config) ;
  Lwt_main.run
    (let open Master_runner.Make (Master) in
    Master.run_node
      ~process:(master_proc DP.(distributed_prover_main ~inputs pp_prover) ~ret)
      master_config) ;
  let proof = Option.get !ret in
  let verifier_inputs = DP.MP.to_verifier_inputs pp_prover inputs in
  assert (DP.MP.verify pp_verifier ~inputs:verifier_inputs proof)

let tests =
  List.map
    (fun (name, f) -> Alcotest.test_case name `Quick f)
    [
      ("test_distribution_kzg", test_distribution (module DP_Kzg ()));
      ("test_distribution_kzg_pack", test_distribution (module DP_Pack ()));
      ("test_distribution_meta", test_distribution (module DP_Meta ()));
    ]
