(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type argument =
  (* If you're considering adding [--endpoint] and [--rpc-addr],
     beware that, for the moment, they are automatically computed in
     {!create} below *)
  | Data_dir
  | Symbolic_block_caching_time of int

module Parameters = struct
  type persistent_state = {
    arguments : string list;
    mutable pending_ready : unit option Lwt.u list;
    rpc_port : int;
    runner : Runner.t option;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "proxy_server"

  let default_colors = Log.Color.[|FG.gray; BG.gray|]
end

let rpc_host = Constant.default_host

let rpc_scheme = "http"

open Parameters
include Daemon.Make (Parameters)

let trigger_ready t value =
  let pending = t.persistent_state.pending_ready in
  t.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready t =
  (match t.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready t (Some ())

let handle_event t ({name; _} : event) =
  match name with "starting_proxy_rpc_server.v0" -> set_ready t | _ -> ()

(** Returns the [--endpoint] and [--rpc-addr] arguments corresponding to [?rpc_port]
    and [node]. Also returns the chosen rpc port, in case it was omitted
    and got generated. *)
let connection_arguments_and_port ?rpc_port node =
  let rpc_port =
    match rpc_port with None -> Port.fresh () | Some port -> port
  in
  ( [
      "--endpoint";
      sf "http://%s:%d" Constant.default_host (Node.rpc_port node);
      (* "-l"; <- to debug RPC delegations to the node

         Note that if you want to debug the proxy server's RPC server,
         set TEZOS_LOG to "rpc->debug", just like you would do with a node.
      *)
      "--rpc-addr";
      Format.sprintf "%s://%s:%d" rpc_scheme rpc_host rpc_port;
    ],
    rpc_port )

let spawn ?rpc_port ?(args = []) node =
  let args = (connection_arguments_and_port ?rpc_port node |> fst) @ args in
  Process.spawn
    ~name:Parameters.base_default_name
    ~color:Parameters.default_colors.(0)
    (Uses.path Constant.octez_proxy_server)
    args

let create ?runner ?name ?rpc_port ?(args = []) node =
  let path = Uses.path Constant.octez_proxy_server in
  let user_arguments =
    List.map
      (function
        | Data_dir -> ["--data-dir"; Node.data_dir node]
        | Symbolic_block_caching_time s ->
            ["--sym-block-caching-time"; Int.to_string s])
      args
    |> List.concat
  in
  let arguments, rpc_port =
    connection_arguments_and_port ?rpc_port node |> fun (args, rpc_port) ->
    (args @ user_arguments, rpc_port)
  in
  let t =
    create ?runner ~path ?name {arguments; pending_ready = []; rpc_port; runner}
  in
  on_event t (handle_event t) ;
  return t

let rpc_port ({persistent_state; _} : t) = persistent_state.rpc_port

let runner node = node.persistent_state.runner

let run ?(on_terminate = fun _ -> ()) ?event_level ?event_sections_levels
    endpoint arguments =
  let arguments = endpoint.persistent_state.arguments @ arguments in
  let on_terminate status =
    on_terminate status ;
    unit
  in
  run
    ?event_level
    ?event_sections_levels
    endpoint
    {ready = false}
    arguments
    ~on_terminate

let check_event ?where node name promise =
  let* result = promise in
  match result with
  | None ->
      raise (Terminated_before_event {daemon = node.name; event = name; where})
  | Some x -> return x

let wait_for_ready t =
  match t.status with
  | Running {session_state = {ready = true}; _} -> unit
  | Not_running | Running {session_state = {ready = false}; _} ->
      let promise, resolver = Lwt.task () in
      t.persistent_state.pending_ready <-
        resolver :: t.persistent_state.pending_ready ;
      check_event t "starting_proxy_rpc_server.v0" promise

let init ?runner ?name ?rpc_port ?event_level ?event_sections_levels ?args node
    =
  let* endpoint = create ?runner ?name ?rpc_port ?args node in
  let* () = run ?event_level ?event_sections_levels endpoint [] in
  let* () = wait_for_ready endpoint in
  return endpoint

let as_rpc_endpoint t =
  let state = t.persistent_state in
  {Endpoint.scheme = rpc_scheme; host = rpc_host; port = state.rpc_port}
