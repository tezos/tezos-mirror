(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

module type BAKER_COMMANDS_HELPERS = sig
  (** [run_baker ~configuration ~baking_mode ~sources ~cctxt] is the main running
      function signature that all protocol plugins will need to implement. It
      requires the [~configuration] which contains all the possible CLI arguments
      for the agnostic baker, together with a [~baking_mode] argument and delegates
      list given by [~sources] in the client context [~cctxt].
      
      Depending on the protocol, the arguments can be transformed in the corresponding
      plugin, but the structure of the list of arguments will not grow or shrink, to
      prevent incompatibilities at migrations. *)
  val run_baker :
    configuration:Configuration.t ->
    baking_mode:string option ->
    sources:Tezos_crypto.Signature.public_key_hash list ->
    cctxt:Tezos_client_base.Client_context.full ->
    unit Error_monad.tzresult Lwt.t

  val run_vdf_daemon :
    cctxt:Tezos_client_base.Client_context.full ->
    keep_alive:bool ->
    unit tzresult Lwt.t
end

module type ACCUSER_COMMANDS_HELPERS = sig
  (** [run ~cctxt ~preserved_levels ~keep_alive] is the main running function signature
      that all protocol plugins will implement for the accuser. *)
  val run :
    cctxt:Tezos_client_base.Client_context.full ->
    preserved_levels:int ->
    keep_alive:bool ->
    unit tzresult Lwt.t
end

module type S = sig
  val protocol_hash : Protocol_hash.t

  module Baker_commands_helpers : BAKER_COMMANDS_HELPERS

  module Accuser_commands_helpers : ACCUSER_COMMANDS_HELPERS
end
