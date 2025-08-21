(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let handle_error = function
  | Ok () -> Lwt.return 0
  | Error [Tezos_clic.Version] ->
      let version = Tezos_version_value.Bin_version.octez_version_string in
      Format.printf "%s\n" version ;
      Lwt.return 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Lwt.return 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Lwt.return 1

let main () =
  let open Lwt_result_syntax in
  Random.self_init () ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stdout)
        Format.std_formatter
        Details) ;
  ignore
    Tezos_clic.(
      setup_formatter
        ~isatty:(Unix.isatty Unix.stderr)
        Format.err_formatter
        Details) ;
  let ctxt = () in
  let*! retcode =
    Lwt.catch
      (fun () ->
        let*! r =
          let* (), args =
            Tezos_clic.parse_global_options
              global_options
              ctxt
              (Array.to_list Sys.argv |> Stdlib.List.tl)
          in
          let commands =
            Tezos_clic.add_manual
              ~executable_name
              ~global_options
              (if Unix.isatty Unix.stdout then Tezos_clic.Ansi
               else Tezos_clic.Plain)
              Format.std_formatter
              Commands.p2p_node_commands
          in
          Tezos_clic.dispatch commands ctxt args
        in
        handle_error r)
      (function
        | Failure message ->
            Format.eprintf
              "@{<error>@{<title>Fatal error@}@}@.  @[<h 0>%a@]@."
              Format.pp_print_text
              message ;
            Lwt.return 1
        | exn ->
            Format.printf
              "@{<error>@{<title>Fatal error@}@}@.  @[<h 0>%a@]@."
              Format.pp_print_text
              (Printexc.to_string exn) ;
            Lwt.return 1)
  in
  Format.pp_print_flush Format.err_formatter () ;
  Format.pp_print_flush Format.std_formatter () ;
  Lwt.return retcode

let () =
  Stdlib.exit
  @@ Tezos_base_unix.Event_loop.main_run ~process_name:"p2p node"
  @@ fun () -> Lwt_exit.wrap_and_forward @@ main ()
