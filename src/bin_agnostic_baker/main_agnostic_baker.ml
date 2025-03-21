(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(* Main entrypoint for the agnostic baker binary.

   We distinguish two cases:
   1. If the binary is called against a `--help` or `--version` command, then
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
      ~default:Tezos_client_base_unix.Client_config.Cfg_file.default.base_dir
      (Run_args.get_base_dir args)
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init
      ~config:(Parameters.log_config ~base_dir)
      ()
  in
  () [@profiler.overwrite may_start_profiler base_dir] ;
  let daemon = Daemon.create ~node_endpoint:(Run_args.get_endpoint args) in
  let* (_ : unit) = Daemon.run daemon in
  let*! () = Lwt_utils.never_ending () in
  return_unit

let run ~args () =
  let open Lwt_result_syntax in
  let main_promise =
    Lwt.catch (lwt_run ~args) (function
        | Failure msg -> failwith "%s" msg
        | exn -> failwith "%s" (Printexc.to_string exn))
  in
  Stdlib.exit
    (Tezos_base_unix.Event_loop.main_run (fun () ->
         let*! retcode =
           let*! r = Lwt_exit.wrap_and_exit main_promise in
           match r with
           | Ok () -> Lwt.return 0
           | Error errs ->
               Format.eprintf "%a" Error_monad.pp_print_trace errs ;
               Lwt.return 1
         in
         Format.pp_print_flush Format.err_formatter () ;
         Format.pp_print_flush Format.std_formatter () ;
         let*! () = Tezos_base_unix.Internal_event_unix.close () in
         Lwt.return retcode))

let () =
  let open Tezos_client_base_unix in
  let args = Array.to_list Sys.argv in
  if Run_args.(is_help_cmd args || is_version_cmd args) then
    Client_main_run.run
      (module Daemon_config)
      ~select_commands:(fun _ _ -> Lwt_result_syntax.return_nil)
  else run ~args ()
