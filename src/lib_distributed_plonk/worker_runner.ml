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

open Distributed_plonk
open Communication
module Plonk_Worker = Worker.Make (Distributed_prover.Main_Kzg)
module APlonk_Worker = Worker.Make (Distributed_prover.Main_Pack)

(** Executable for running a worker.  *)

let () =
  let logs_level =
    Option.bind (Sys.getenv_opt "DP_LOGS") (fun s ->
        Result.get_ok @@ Logs.level_of_string s)
  in
  let args = Sys.argv in
  (* set the log level and the reporter*)
  Logs.Src.set_level Distributed_wrapper.Logger.log_src logs_level ;
  Logs.set_reporter @@ Distributed_wrapper.Logger.lwt_reporter () ;
  let ip = args.(1) in
  let port = int_of_string args.(2) in
  let name = args.(3) in
  let plonk_or_aplonk = args.(4) in
  let (module Worker : Worker.S), pp_file =
    match plonk_or_aplonk with
    | "plonk" ->
        ((module Plonk_Worker), Distributed_plonk.Filenames.plonk_pp_file)
    | "aplonk" ->
        ((module APlonk_Worker), Distributed_plonk.Filenames.meta_pp_file)
    | _ -> failwith "prover_worker : args.(4) must be 'plonk' or 'aplonk'"
  in
  let module D = Worker.D in
  let worker_config ~ip ~port ~name =
    D.Remote
      {
        D.Remote_config.node_name = name;
        D.Remote_config.local_port = port;
        D.Remote_config.connection_backlog = 10;
        D.Remote_config.node_ip = ip;
        D.Remote_config.remote_nodes = [];
      }
  in
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Lwt.(
    Lwt_main.run
      ( D.run_node
          ~process:(fun () -> D.register "worker" (Worker.worker_proc pp_file))
          (worker_config ~ip ~port ~name)
      >>= fun () -> fst @@ wait () ))
