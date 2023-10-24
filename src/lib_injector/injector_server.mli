(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** This module defines an instance of the injector functor used in the
    standalone injector server binary in [bin_injector_server], which is
    independent of the rollup node and is meant to inject manager operations
    (only transactions are currently supported).

    Other modules in this library which are only used for the injector binary
    are prefixed with [injector_server*]. The injector server supports
    multiple protocols. Each protocol is supported using a plugin module
    [proto_*/lib_injector/injector_plugin.ml] *)

module Configuration : sig
  type tag = Transaction

  type fee_parameters = Injector_common.fee_parameter

  val tags : tag trace

  val string_of_purpose : tag -> string

  val default_fee_parameters : Injector_common.fee_parameter
end

type state = {
  cctxt : Client_context.full;
  fee_parameters : Configuration.fee_parameters;
      (** [minimal_block_delay] and [delay_increment_per_round] are protocol
      constants required for the injector plugin to compute the time remaining
      until the following block *)
  minimal_block_delay : int64;
  delay_increment_per_round : int64;
}

include
  Injector_sigs.S
    with type state := state
     and type tag := Configuration.tag
     and type operation := Injector_server_operation.t
