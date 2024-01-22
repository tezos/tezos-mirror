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
open Communication.Distributed_wrapper

module Make (D : Enriched_process with type 'a io = 'a Lwt.t) : sig
  type node = {name : string; ip : string; port : int}

  type 'a master_task := workers:Distributed.Process_id.t list -> 'a D.t

  val run : self_node:node -> nodes:node list -> 'a master_task -> 'a
end = struct
  type node = {name : string; ip : string; port : int}

  let master_config ~ip ~port ~nodes =
    D.Remote
      {
        D.Remote_config.node_name = "master";
        D.Remote_config.local_port = port;
        D.Remote_config.connection_backlog = 10;
        D.Remote_config.node_ip = ip;
        D.Remote_config.remote_nodes = nodes;
      }

  let master_proc m ~ret () =
    let open D in
    let* nodes = get_remote_nodes in
    let* pid_to_send_to = get_self_pid in
    (* Dummy worker process in the master *)
    (* FIXME: why do we need this? *)
    let* () = register "worker" (fun _pid () -> D.return ()) in
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

  let run ~self_node ~nodes m =
    let logs_level =
      Option.bind (Sys.getenv_opt "DP_LOGS") (fun s ->
          Result.get_ok @@ Logs.level_of_string s)
    in
    Logs.Src.set_level Logger.log_src logs_level ;
    Logs.set_reporter @@ Logger.lwt_reporter () ;
    let ret = ref None in
    Lwt.Exception_filter.(set handle_all_except_runtime) ;
    Lwt_main.run
      (D.run_node
         ~process:(master_proc m ~ret)
         (master_config
            ~ip:self_node.ip
            ~port:self_node.port
            ~nodes:(List.map (fun n -> (n.ip, n.port, n.name)) nodes))) ;
    Option.get !ret
end
