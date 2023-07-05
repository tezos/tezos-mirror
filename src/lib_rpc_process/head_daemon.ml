(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Parameters

module Events = struct
  include Internal_event.Simple

  let section = ["octez_rpc_server"]

  let daemon_error =
    declare_1
      ~section
      ~name:"octez_rpc_server_daemon_error"
      ~msg:"Daemon thrown an error: {error}"
      ~level:Notice
      ~pp1:Error_monad.pp_print_trace
      ("error", Error_monad.trace_encoding)

  let new_head =
    declare_1
      ~section
      ~name:"new_head"
      ~msg:"New head received at level ({level})"
      ~level:Notice
      ("level", Data_encoding.int32)

  let synchronized =
    declare_1
      ~section
      ~name:"synchronized"
      ~msg:"Store synchronized up to level {level}"
      ~level:Notice
      ("level", Data_encoding.int32)

  let shutting_head_daemon =
    declare_0
      ~section
      ~name:"shutting_head_daemon"
      ~msg:"shutting down head daemon"
      ~level:Info
      ()
end

module Daemon = struct
  type t = {
    daemon : unit tzresult Lwt.t;
    head_stream_stopper : Tezos_rpc.Context.stopper;
  }

  (** [make_stream_daemon ~on_head ~head_stream] calls [on_head] on
      each newly received value from [head_stream].

      It returns a couple [(p, stopper)] where [p] is a promise
      resolving when the stream closes and [stopper] is a function
      closing the stream. *)
  let make_stream_daemon ~on_head ~head_stream =
    let open Lwt_result_syntax in
    let* head_stream, head_stream_stopper = head_stream in
    let rec stream_processor () =
      let*! head_element = Lwt_stream.get head_stream in
      match head_element with
      | None -> return_unit
      | Some element ->
          let*! processed_head = on_head element in
          let*! () =
            match processed_head with
            | Ok () -> Lwt.return_unit
            | Error trace -> Events.(emit daemon_error) trace
          in
          stream_processor ()
    in
    return {daemon = stream_processor (); head_stream_stopper}

  let shutdown {head_stream_stopper; _} =
    let open Lwt_syntax in
    let* () = Events.(emit shutting_head_daemon) () in
    head_stream_stopper () ;
    return_unit
end

let handle_new_head _dynamic_store _parameters
    (_block_hash, (header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let*! () = Events.(emit new_head) header.shell.level in
  (* TODO: Synchronize the store *)
  return_unit

let init dynamic_store parameters =
  let ctx =
    Forward_handler.build_socket_redirection_ctx parameters.rpc_comm_socket_path
  in
  let module CustomRetryClient = struct
    include RPC_client_unix.RetryClient

    let call ?ctx:_ = call ~ctx
  end in
  let module Custom_rpc_client =
    RPC_client.Make (Resto_cohttp_client.Client.OfCohttp (CustomRetryClient)) in
  let rpc_config =
    Custom_rpc_client.
      {
        media_type = Media_type.Command_line.Any;
        endpoint = Uri.of_string Forward_handler.socket_forwarding_uri;
        logger = null_logger;
      }
  in
  let rpc_ctxt =
    new Custom_rpc_client.http_ctxt
      rpc_config
      (Media_type.Command_line.of_command_line rpc_config.media_type)
  in
  Daemon.make_stream_daemon
    ~on_head:(handle_new_head dynamic_store parameters)
    ~head_stream:(Tezos_shell_services.Monitor_services.heads rpc_ctxt `Main)
