(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module Plugin : Protocol_plugin_sig.S = struct
  let protocol_hash = Protocol.hash

  module Baker_commands_helpers = Baker_commands_helpers
  module Accuser_commands_helpers = Accuser_commands_helpers
end

let () = Protocol_plugins.register (module Plugin)
