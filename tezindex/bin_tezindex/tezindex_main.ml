(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Lwt_result_syntax

let _print_error logger e =
  let () =
    Log.error logger (fun () ->
        Format.asprintf "%a@." Error_monad.pp_print_trace e)
  in
  Lwt.return_unit

let run cctxt _config =
  let logger = Log.logger () in
  let* () = Client_confirmations.wait_for_bootstrapped cctxt in
  let () = Log.info logger (fun () -> "Node bootstrapped") in
  let main =
    let*! () = Lwt.join [General_archiver.Loops.balance_update_loop cctxt] in
    Lwt.return_unit
  in
  let*! out = Lwt.join [main] in
  return out

let select_commands _http_ctxt (_config : Client_config.cli_args) =
  let group =
    {Tezos_clic.name = "tezindex"; Tezos_clic.title = "A Tezos indexer"}
  in
  return
    [
      Tezos_clic.command
        ~group
        ~desc:"run the Octez indexer"
        Tezos_clic.(
          args4
            (Config.rpc_listen_addr_arg ())
            (Config.external_rpc_listen_addr_arg ())
            (Config.db_name_arg ())
            (Config.watched_address_arg ()))
        (Tezos_clic.fixed ["run"])
        (fun ( rpc_listen_addr,
               external_rpc_listen_addr,
               db_name,
               watched_addresses )
             cctxt
           ->
          Log.verbosity := DEBUG ;
          let logger = Log.logger () in
          List.iter
            (fun h -> Log.info logger (fun () -> Protocol_hash.to_b58check h))
            !General_archiver.supported_protocols ;
          let config =
            Config.get_config
              cctxt#get_base_dir
              rpc_listen_addr
              external_rpc_listen_addr
              db_name
              watched_addresses
          in
          Config.pp_config logger config ;
          run cctxt config);
    ]

module M024 = PtTALLiN_machine.M
module Malpha = Alpha_machine.M

let () = Client_main_run.run (module Config) ~select_commands
