(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Agnostic_baker_plugin = struct
  let hash = Registerer.Registered.hash

  let register_commands () =
    Client_commands.register Protocol.hash @@ fun _network ->
    List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
    @@ Baking_commands.baker_commands ()

  let select_commands _ _ =
    let open Lwt_result_syntax in
    return
      (List.map
         (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
         (Baking_commands.baker_commands ()))

  (* This call is not strictly necessary as the parameters are initialized
     lazily the first time a Sapling operation (validation or forging) is
     done. This is what the client does.
     For a long running binary however it is important to make sure that the
     parameters files are there at the start and avoid failing much later while
     validating an operation. Plus paying this cost upfront means that the first
     validation will not be more expensive. *)
  let init_sapling_params () = Tezos_sapling.Core.Validator.init_params ()

  let run_baker_binary ~baker_args ~cancel_promise ~logs_path =
    let module Config = struct
      include Daemon_config

      let default_daily_logs_path = logs_path
    end in
    register_commands () ;
    init_sapling_params () ;
    Lwt.pick
      [
        Client_main_run.lwt_run
          (module Config)
          ~select_commands
          ~cmd_args:baker_args
          ();
        cancel_promise;
      ]
end

let () =
  Protocol_plugin.register_agnostic_baker_plugin (module Agnostic_baker_plugin)
