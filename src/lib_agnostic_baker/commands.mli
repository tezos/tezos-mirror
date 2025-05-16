(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Trilitech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

(** [baker_commands] is the list of CLI commands available for
    controlling a baker process in the Tezos client context. *)
val baker_commands :
  Tezos_client_base.Client_context.full Tezos_clic.command list

(** [accuser_commands] behaves similarly to [baker_commands], but for the
    accuser process. *)
val accuser_commands :
  Tezos_client_base.Client_context.full Tezos_clic.command list
