(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Configuration

let main ~config:_ () =
  let open Lwt_result_syntax in
  let*! () = Tezos_base_unix.Internal_event_unix.init () in
  let*! () = Event.starting_observer () in
  Lwt_result.return ()

let run =
  let open Tezos_clic in
  command
    ~desc:"Run Etherlink's governance observer."
    (args5
       Args.metrics_addr
       Args.metrics_port
       Args.endpoint
       Args.etherlink_mainnet
       Args.etherlink_ghostnet)
    (prefixes ["run"] stop)
    (fun
      ( metrics_addr,
        metrics_port,
        endpoint,
        etherlink_mainnet_arg,
        etherlink_ghostnet_arg )
      ()
    ->
      let contracts =
        match (etherlink_mainnet_arg, etherlink_ghostnet_arg) with
        | true, true -> network_argument_not_found ~error:Both_args
        | false, false -> network_argument_not_found ~error:No_args
        | true, false -> mainnet_contracts
        | false, true -> ghostnet_contracts
      in
      let config =
        {
          prometheus = {metrics_addr; metrics_port};
          endpoint = Uri.of_string endpoint;
          contracts;
        }
      in
      main ~config ())

let commands = [run]

let global_options = Tezos_clic.no_options

let executable_name = Filename.basename Sys.executable_name

let argv () = Array.to_list Sys.argv |> List.tl |> Stdlib.Option.get

let parse_options () =
  let open Lwt_result_syntax in
  let args = argv () in
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

let () =
  Lwt_main.run (parse_options ())
  |> Result.iter_error (Format.printf "ERROR: %a%!" Error_monad.pp_print_trace)
