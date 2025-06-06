(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Context_wrapper

module Make
    (Irmin_Context : IRMIN_CONTEXT)
    (Brassaia_Context : BRASSAIA_CONTEXT) =
struct
  module Context = MakeContext (Irmin_Context) (Brassaia_Context)
  include Tezos_protocol_environment.Register (Context)

  let impl_name = Context.name

  let checkout :
      Context.index ->
      Context_hash.t ->
      Tezos_protocol_environment.Context.t option Lwt.t =
   fun index context_hash ->
    let open Lwt_syntax in
    let* ctxt = Context.checkout index context_hash in
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
      Context.index ->
      Context_hash.t ->
      Tezos_protocol_environment.Context.t Lwt.t =
   fun index context_hash ->
    let open Lwt_syntax in
    let* ctxt = Context.checkout_exn index context_hash in
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

  let unwrap_context : Tezos_protocol_environment.Context.t -> Context.t =
    function
    | Tezos_protocol_environment.Context.Context {ctxt; kind = Context; _} ->
        ctxt
    | Tezos_protocol_environment.Context.Context t ->
        Tezos_protocol_environment.err_implementation_mismatch
          ~expected:impl_name
          ~got:t.impl_name
end

module Duo_context =
  Make
    (struct
      let name = "Irmin_disk"

      include Tezos_context.Context
    end)
    (struct
      let name = "Brassaia_disk"

      include Tezos_context_brassaia.Tezos_context.Context
    end)

module Duo_memory_context =
  Make
    (struct
      let name = "Irmin_mem"

      include Tezos_context_memory.Context
    end)
    (struct
      let name = "Brassaia_mem"

      include Tezos_context_brassaia_memory.Tezos_context_memory.Context
    end)
