(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_protocol_environment
open Tezos_context_brassaia
open Context

module C = struct
  include Tezos_context.Context

  let set_protocol = add_protocol
end

include Register (C)

let impl_name = "brassaia"

let checkout index context_hash =
  let open Lwt_syntax in
  let+ oc = Tezos_context.Context.checkout index context_hash in
  Option.map
    (fun ctxt ->
      Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name)
    oc

let checkout_exn index context_hash =
  let open Lwt_syntax in
  let+ ctxt = Tezos_context.Context.checkout_exn index context_hash in
  Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name

let wrap_disk_context ctxt =
  Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name

let unwrap_disk_context : t -> Tezos_context.Context.t = function
  | Context.Context {ctxt; kind = Context; _} -> ctxt
  | Context.Context t ->
      err_implementation_mismatch ~expected:impl_name ~got:t.impl_name
