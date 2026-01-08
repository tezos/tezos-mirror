(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Duo_context_sig
open Context_wrapper

module Make (P : CONTEXT_PARAM) : sig
  include EXPORTED

  val make_index : P.index_1 -> P.index_2 -> index
end = struct
  module C = MakeContext (P)
  include C
  include Tezos_protocol_environment.Register (C)

  let impl_name = C.name

  let checkout :
      C.index ->
      Context_hash.t ->
      Tezos_protocol_environment.Context.t option Lwt.t =
   fun index context_hash ->
    let open Lwt_syntax in
    let* ctxt = C.checkout index context_hash in
    match ctxt with
    | Some ctxt ->
        Lwt.return_some
          (Tezos_protocol_environment.Context.make
             ~ops
             ~ctxt
             ~kind:Context
             ~equality_witness
             ~impl_name)
    | _ -> Lwt.return_none

  let checkout_exn :
      C.index -> Context_hash.t -> Tezos_protocol_environment.Context.t Lwt.t =
   fun index context_hash ->
    let open Lwt_syntax in
    let* ctxt = C.checkout_exn index context_hash in
    Lwt.return
      (Tezos_protocol_environment.Context.make
         ~ops
         ~ctxt
         ~kind:Context
         ~equality_witness
         ~impl_name)

  let wrap_context ctxt =
    Tezos_protocol_environment.Context.make
      ~ops
      ~ctxt
      ~kind:Context
      ~equality_witness
      ~impl_name

  let unwrap_context : Tezos_protocol_environment.Context.t -> C.t = function
    | Tezos_protocol_environment.Context.Context {ctxt; kind = Context; _} ->
        ctxt
    | Tezos_protocol_environment.Context.Context t ->
        Tezos_protocol_environment.err_implementation_mismatch
          ~expected:impl_name
          ~got:t.impl_name
end

module Duo_context = struct
  include Make (struct
    let backend_1 = Internal.Irmin_disk

    let backend_2 = Internal.Brassaia_disk

    type index_1 = Tezos_context_disk.Context.index

    type index_2 = Tezos_context_brassaia.Tezos_context.Context.index

    let make_index irmin_index brassaia_index : Internal.index =
      {
        index_1 = Irmin_disk_index irmin_index;
        index_2 = Brassaia_disk_index brassaia_index;
      }
  end)
end

module Duo_memory_context = Make (struct
  let backend_1 = Internal.Irmin_mem

  let backend_2 = Internal.Brassaia_mem

  type index_1 = Tezos_context_memory.Context.index

  type index_2 =
    Tezos_context_brassaia_memory.Tezos_context_memory.Context.index

  let make_index irmin_index brassaia_index : Internal.index =
    {
      index_1 = Irmin_mem_index irmin_index;
      index_2 = Brassaia_mem_index brassaia_index;
    }
end)

module Duo_irmin_tezedge_context = struct
  include Make (struct
    let backend_1 = Internal.Irmin_disk

    let backend_2 = Internal.Tezedge

    type index_1 = Tezos_context_disk.Context.index

    type index_2 = Tezos_tezedge_context.Tezedge_context.C.index

    let make_index irmin_index tezedge_index : Internal.index =
      {
        index_1 = Irmin_disk_index irmin_index;
        index_2 = Tezedge_index tezedge_index;
      }
  end)
end
