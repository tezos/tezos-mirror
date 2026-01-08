(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Duo_context : sig
  include Duo_context_sig.EXPORTED

  val make_index :
    Tezos_context_disk.Context.index ->
    Tezos_context_brassaia.Tezos_context.Context.index ->
    index
end

module Duo_memory_context : sig
  include Duo_context_sig.EXPORTED

  val make_index :
    Tezos_context_memory.Context.index ->
    Tezos_context_brassaia_memory.Tezos_context_memory.Context.index ->
    index
end

module Duo_irmin_tezedge_context : sig
  include Duo_context_sig.EXPORTED

  val make_index :
    Tezos_context_disk.Context.index ->
    Tezos_context_tezedge.Context.index ->
    index
end
