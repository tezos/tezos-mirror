(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* TODO TZX-26: Make Evm_node_state.t configurable by backend context *)

type repo = Irmin_context.repo

type tree = Irmin_context.tree

type 'a index = ('a, repo) Context_sigs.index

type rw_index = [`Read | `Write] index

type ro_index = [`Read] index

type 'a t = ('a, repo, tree) Context_sigs.t

type rw = [`Read | `Write] t

type hash = Irmin_context.hash

let load ~cache_size ?async_domain mode path =
  Irmin_context.load ~cache_size ?async_domain mode path

let reload index = Irmin_context.reload index

let commit ?message ctxt = Irmin_context.commit ?message ctxt

let checkout_exn index key = Irmin_context.checkout_exn index key

let empty index = Irmin_context.empty index

let split ctxt = Irmin_context.split ctxt

let gc index ?callback hash = Irmin_context.gc index ?callback hash

let wait_gc_completion index = Irmin_context.wait_gc_completion index

let context_hash_of_hash h = Irmin_context.context_hash_of_hash h

let hash_of_context_hash h = Irmin_context.hash_of_context_hash h

module PVMState = struct
  type value = tree

  let empty () = Irmin_context.PVMState.empty ()

  let get ctxt = Irmin_context.PVMState.get ctxt

  let set ctxt state = Irmin_context.PVMState.set ctxt state
end

(* TODO TZX-24: Only make `Wasm_internal` available when node is instantiated
 * with the Wasm PVM *)
module Wasm_internal = struct
  let to_irmin tree = tree

  let of_irmin tree = tree
end
