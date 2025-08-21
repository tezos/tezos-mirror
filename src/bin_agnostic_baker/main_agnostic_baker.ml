(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let () =
  Stdlib.exit
    (Tezos_base_unix.Event_loop.main_run ~process_name:"baker" (fun () ->
         let open Lwt_syntax in
         let* retcode =
           Lwt_exit.wrap_and_exit
           @@ Client_main_run.lwt_run
                ~disable_logging:true
                (module Agnostic_baker_config)
                ~select_commands:(fun _ _ ->
                  Lwt_result_syntax.return
                  @@ Commands.baker_commands (module Agnostic_baker_config))
                ()
         in
         Format.pp_print_flush Format.err_formatter () ;
         Format.pp_print_flush Format.std_formatter () ;
         let* () = Tezos_base_unix.Internal_event_unix.close () in
         return retcode))
