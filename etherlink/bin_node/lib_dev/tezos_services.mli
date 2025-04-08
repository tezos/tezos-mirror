(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Imported_protocol = Tezos_protocol_021_PsQuebec.Protocol
module Imported_protocol_parameters = Tezos_protocol_021_PsQuebec_parameters
module Alpha_context = Imported_protocol.Alpha_context

module Tezlink_protocols : sig
  type protocols

  val current : protocols
end

module Tezlink_version : sig
  type version

  val mock : version
end

module Protocol_types : sig
  module Constants : sig
    type fixed = Alpha_context.Constants.fixed

    type parametric = Alpha_context.Constants.Parametric.t

    type t = {fixed : fixed; parametric : parametric}

    val values_to_fixed :
      (int * int * int * int * int * int * int * int * int * int)
      * (int * int * int * Z.t) ->
      (fixed, error trace) result
  end
end

type level = {
  level : int32;
      (** The level of the block relative to genesis. This
      is also the Shell's notion of level. *)
  cycle : int32;
      (** The current cycle's number. Note that cycles are a protocol-specific
      notion. As a result, the cycle number starts at 0 with the first block of
      the first version of protocol alpha. *)
  cycle_position : int32;
      (** The current level of the block relative to the first block of the current
      cycle. *)
}

(* Query types *)
type level_query = {offset : int32}

(* Param types *)

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

type contract = Imported_protocol.Alpha_context.Contract.t

(** Container for the implementations necessary to answer tezos RPC requests. *)
type tezos_services_implementation = {
  current_level : chain -> block -> level_query -> level tzresult Lwt.t;
  version : unit -> Tezlink_version.version tzresult Lwt.t;
  protocols : unit -> Tezlink_protocols.protocols tzresult Lwt.t;
  balance : chain -> block -> contract -> Ethereum_types.quantity tzresult Lwt.t;
}

(* THIS IS THE ENTRYPOINT *)
val register_tezlink_services : tezos_services_implementation -> Evm_directory.t
