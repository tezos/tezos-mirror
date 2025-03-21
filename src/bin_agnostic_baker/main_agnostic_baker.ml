(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let run () =
  let open Lwt_result_syntax in
  let Run_args.{node_endpoint; base_dir; baker_args} =
    Run_args.parse_args Sys.argv
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init
      ~config:(Parameters.log_config ~base_dir)
      ()
  in
  let daemon = Daemon.create ~node_endpoint ~baker_args in
  let* (_ : unit) = Daemon.run daemon in
  let*! () = Lwt_utils.never_ending () in
  return_unit

let () =
  let open Lwt_result_syntax in
  let main_promise =
    Lwt.catch run (function
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
