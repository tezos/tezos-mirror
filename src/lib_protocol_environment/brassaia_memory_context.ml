(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)
open Tezos_context_brassaia_memory

module M = struct
  include Tezos_context_memory.Context

  let set_protocol = add_protocol

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

open Environment_context

type t = M.t

include Environment_context.Register (M)

let impl_name = "brassaia_memory"

let project : Context.t -> t =
 fun (Context.Context t) ->
  match t.kind with
  | Context -> t.ctxt
  | _ ->
      Environment_context.err_implementation_mismatch
        ~expected:impl_name
        ~got:t.impl_name

let inject : t -> Context.t =
 fun ctxt -> Context.make ~ops ~ctxt ~kind:Context ~equality_witness ~impl_name

let empty = inject (Tezos_context_memory.Context.make_empty_context ())

let encoding : Context.t Data_encoding.t =
  let open Data_encoding in
  conv project inject M.encoding

let wrap_memory_context = inject

let unwrap_memory_context = project
