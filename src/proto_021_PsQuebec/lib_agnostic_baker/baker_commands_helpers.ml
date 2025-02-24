(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

let map_commands () =
  List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
  @@ Baking_commands.baker_commands ()
