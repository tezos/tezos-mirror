(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module M = struct
  include Tezos_context_memory.Context

  let set_protocol = add_protocol

  let fork_test_chain c ~protocol:_ ~expiration:_ = Lwt.return c
end

open Environment_context

type t = M.t

include Environment_context.Register (M)

let impl_name = "memory"

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
