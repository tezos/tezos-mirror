(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_protocol_environment
open Context
module Profiler = Environment_profiler.Context_ops_profiler

module C = struct
  include Context_wrapper.Context

  let set_protocol = add_protocol
end

include Register (C)

let impl_name = "duo"

let checkout : C.index -> Context_hash.t -> t option Lwt.t =
 fun index context_hash ->
  let open Lwt_syntax in
  let* irmin_context =
    (Tezos_context.Context.checkout
       index.C.irmin_index
       context_hash
     [@profiler.span_s {verbosity = Notice} ["irmin"; "checkout"]])
  in
  let+ brassaia_context =
    (Tezos_context_brassaia.Tezos_context.Context.checkout
       index.brassaia_index
       context_hash
     [@profiler.span_s {verbosity = Notice} ["brassaia"; "checkout"]])
  in
  match (irmin_context, brassaia_context) with
  | Some irmin_context, Some brassaia_context ->
      Some
        (Context.make
           ~ops
           ~ctxt:{irmin_context; brassaia_context}
           ~kind:Context
           ~equality_witness
           ~impl_name)
  | _ -> None

let checkout_exn : C.index -> Context_hash.t -> t Lwt.t =
 fun index context_hash ->
  let open Lwt_syntax in
  let* irmin_context =
    (Tezos_context.Context.checkout_exn
       index.C.irmin_index
       context_hash
     [@profiler.span_s {verbosity = Notice} ["irmin"; "checkout_exn"]])
  in
  let+ brassaia_context =
    (Tezos_context_brassaia.Tezos_context.Context.checkout_exn
       index.C.brassaia_index
       context_hash
     [@profiler.span_s {verbosity = Notice} ["brassaia"; "checkout_exn"]])
  in
  Context.make
    ~ops
    ~ctxt:{irmin_context; brassaia_context}
    ~kind:Context
    ~equality_witness
    ~impl_name

let wrap_disk_context ctxt =
  Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name

let unwrap_disk_context : t -> C.t = function
  | Context.Context {ctxt; kind = Context; _} -> ctxt
  | Context.Context t ->
      err_implementation_mismatch ~expected:impl_name ~got:t.impl_name
