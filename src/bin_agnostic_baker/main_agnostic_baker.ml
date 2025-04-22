(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint for the agnostic baker binary.

   We distinguish two cases:
   1. If the binary is called against a `--help`, `--version` or `man` command, then
   there is no reason to connect to a node, find the current protocol etc.
   2. Otherwise, we run the agnostic baker daemon, which first obtains the
   current protocol from the connected node, and then it monitors the chain
   to determine when to switch to a new protocol baker process. *)

let[@warning "-32"] may_start_profiler baking_dir =
  match Tezos_profiler_unix.Profiler_instance.selected_backend () with
  | Some {instance_maker; _} ->
      let profiler_maker = instance_maker ~directory:baking_dir in
      Agnostic_baker_profiler.init profiler_maker
  | None -> ()

let lwt_run ~args () =
  let open Lwt_result_syntax in
  let base_dir =
    Option.value
      ~default:Agnostic_baker_config.default_base_dir
      (Run_args.get_base_dir args)
  in
  let full =
    new Tezos_client_base_unix.Client_context_unix.unix_full
      ~chain:Agnostic_baker_config.default_chain
      ~block:Agnostic_baker_config.default_block
      ~confirmations:None
      ~password_filename:None
      ~base_dir:Agnostic_baker_config.default_base_dir
      ~rpc_config:Tezos_rpc_http_client_unix.RPC_client_unix.default_config
      ~verbose_rpc_error_diagnostics:false
  in
  let* parsed, _remaining =
    Agnostic_baker_config.parse_config_args
      full
      (List.tl args |> Option.value ~default:[])
  in
  let parsed_config_file = parsed.Client_config.parsed_config_file in
  let parsed_args = parsed.Client_config.parsed_args in
  let*! () =
    Client_main_run.init_logging
      ?parsed_args
      ?parsed_config_file
      (module Agnostic_baker_config)
      ~base_dir
      ()
  in
  () [@profiler.overwrite may_start_profiler base_dir] ;
  let daemon =
    Daemon.Baker.create
      ~node_endpoint:(Run_args.get_endpoint args)
      ~keep_alive:(Run_args.keep_alive args)
  in
  let* (_ : unit) = Daemon.Baker.run daemon in
  let*! () = Lwt_utils.never_ending () in
  return_unit

let run ~args () =
  let open Lwt_syntax in
  let main_promise =
    Lwt.catch (lwt_run ~args) (function
        | Failure msg -> failwith "%s" msg
        | exn -> failwith "%s" (Printexc.to_string exn))
  in
  Stdlib.exit
    (Tezos_base_unix.Event_loop.main_run (fun () ->
         let* retcode =
           let* r = Lwt_exit.wrap_and_exit main_promise in
           match r with
           | Ok () -> Lwt.return 0
           | Error errs ->
               Format.eprintf "%a" Error_monad.pp_print_trace errs ;
               Lwt.return 1
         in
         Format.pp_print_flush Format.err_formatter () ;
         Format.pp_print_flush Format.std_formatter () ;
         let* () = Tezos_base_unix.Internal_event_unix.close () in
         return retcode))

let () =
  let args = Array.to_list Sys.argv in
  if
    Run_args.(
      only_exe args || is_help_cmd args || is_version_cmd args
      || is_man_cmd args)
  then
    (* No need to run the baker commands, we just need to get their description,
       therefore we do not obtain the protocol plugin. *)
    Client_main_run.run
      (module Agnostic_baker_config)
      ~select_commands:(fun _ _ ->
        Lwt_result_syntax.return @@ Commands.baker_commands ())
  else run ~args ()
