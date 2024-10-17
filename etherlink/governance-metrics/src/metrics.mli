(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [start_server ()] will start a new metric server based on the configuration
    given when executing the program. *)
val start_server : config:Configuration.configuration -> unit -> unit Lwt.t

type governance_contract = Sequencer | Kernel | Security_kernel

(** Helper to provide the actual contract address associated to the governance
    contract type based on the configuration given when executing the program. *)
val governance_to_contract :
  config:Configuration.configuration -> governance_contract -> string

(** Helper to provide the string representation of the governance contract type. *)
val governance_to_string : governance_contract -> string

module GovernanceMetrics : sig
  module Storage : sig
    (** [set_configuration ~config contract] will set the configuration metrics
        based on [config] and the [contract] type. *)
    val set_configuration :
      config:Contract_type.config -> governance_contract -> unit

    (** [set_remaining_blocks contract value] will set the remaining blocks of the
        current period of the [contract] type to [value]. *)
    val set_remaining_blocks : governance_contract -> Z.t -> unit

    (** [set_current_period_type contract value] will set the current period type
        period of the [contract] type to [value]. *)
    val set_current_period_type : governance_contract -> int -> unit

    (** [set_current_period_index contract value] will set the current period index
        period of the [contract] type to [value]. *)
    val set_current_period_index : governance_contract -> Z.t -> unit
  end
end
