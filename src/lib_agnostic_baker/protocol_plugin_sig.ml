(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module type BAKER_COMMANDS_HELPERS = sig
  val map_commands :
    unit -> Tezos_client_base.Client_context.full Tezos_clic.command list
end

module type S = sig
  val protocol_hash : Protocol_hash.t

  module Baker_commands_helpers : BAKER_COMMANDS_HELPERS
end
