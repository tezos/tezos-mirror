(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let log_config () =
  let open Tezos_base_unix.Internal_event_unix in
  let config = make_with_defaults ~verbosity:Notice () in
  init ~config ()

let run_command =
  let open Tezos_clic in
  command
    ~desc:"Start Floodgate to spam an EVM-compatible endpoint"
    no_options
    (prefixes ["run"] stop)
    (fun () () ->
      let open Lwt_result_syntax in
      let*! () = log_config () in
      let*! () = Events.is_ready () in
      return_unit)

let commands = [run_command]

let executable_name = Filename.basename Sys.executable_name

let global_options = Tezos_clic.no_options

let dispatch args =
  let open Lwt_result_syntax in
  let commands =
    Tezos_clic.add_manual
      ~executable_name
      ~global_options
      (if Unix.isatty Unix.stdout then Tezos_clic.Ansi else Tezos_clic.Plain)
      Format.std_formatter
      commands
  in
  let* (), remaining_args =
    Tezos_clic.parse_global_options global_options () args
  in
  Tezos_clic.dispatch commands () remaining_args

let handle_error = function
  | Ok _ -> ()
  | Error [Tezos_clic.Version] ->
      Format.printf "floodgate.0.0~dev\n" ;
      exit 0
  | Error [Tezos_clic.Help command] ->
      Tezos_clic.usage
        Format.std_formatter
        ~executable_name
        ~global_options
        (match command with None -> [] | Some c -> [c]) ;
      Stdlib.exit 0
  | Error errs ->
      Tezos_clic.pp_cli_errors
        Format.err_formatter
        ~executable_name
        ~global_options
        ~default:Error_monad.pp
        errs ;
      Stdlib.exit 1

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let () =
  Random.self_init () ;
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.std_formatter
        (if Unix.isatty Unix.stdout then Ansi else Plain)
        Short)
  in
  let _ =
    Tezos_clic.(
      setup_formatter
        Format.err_formatter
        (if Unix.isatty Unix.stderr then Ansi else Plain)
        Short)
  in
  Lwt.Exception_filter.(set handle_all_except_runtime) ;
  Lwt_main.run (Lwt_exit.wrap_and_exit (dispatch (argv ()))) |> handle_error
