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

type block = Tezos_shell_services.Block_services.block

type chain = Tezos_shell_services.Chain_services.chain

type contract = Imported_protocol.Alpha_context.Contract.t

(** Container for the implementations necessary to answer tezos RPC requests. *)
type tezos_services_implementation = {
  current_level :
    chain -> block -> offset:int32 -> Tezos_types.level tzresult Lwt.t;
  version : unit -> Tezlink_version.version tzresult Lwt.t;
  protocols : unit -> Tezlink_protocols.protocols tzresult Lwt.t;
  balance :
    chain -> block -> contract -> Ethereum_types.quantity tzresult Lwt.t;
  constants : chain -> block -> Tezlink_constants.t tzresult Lwt.t;
}

(* THIS IS THE ENTRYPOINT *)
val register_tezlink_services : tezos_services_implementation -> Evm_directory.t
