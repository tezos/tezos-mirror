(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let run () =
  let open Lwt_result_syntax in
  let Run_args.{node_endpoint; base_dir; binaries_directory; baker_args} =
    Run_args.parse_args Sys.argv
  in
  let*! () =
    Tezos_base_unix.Internal_event_unix.init
      ~config:(Parameters.log_config ~base_dir)
      ()
  in
  let daemon = Daemon.create ~binaries_directory ~node_endpoint ~baker_args in
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
    (Lwt_main.run
       (let*! retcode =
          let*! r = Lwt_exit.wrap_and_exit main_promise in
          match r with
          | Ok () -> Lwt.return 0
          | Error errs ->
              Format.eprintf "%a" Error_monad.pp_print_trace errs ;
              Lwt.return 1
        in
        Format.pp_print_flush Format.err_formatter () ;
        Format.pp_print_flush Format.std_formatter () ;
        Lwt.return retcode))
