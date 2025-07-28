(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_protocol_environment

module C = struct
  include Tezos_context_tezedge.Context

  let set_protocol = add_protocol
end

include Register (C)

let impl_name = "tezedge"

let wrap_context ctxt =
  Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name

let unwrap_disk_context : Context.t -> C.t = function
  | Context.Context {ctxt; kind = Context; _} -> ctxt
  | Context.Context t ->
      err_implementation_mismatch ~expected:impl_name ~got:t.impl_name
