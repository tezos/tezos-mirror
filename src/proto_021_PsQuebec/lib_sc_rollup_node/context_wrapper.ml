(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(*                                                                           *)
(*****************************************************************************)

open Context
open Context_sigs

let err_implementation_mismatch ~got =
  Format.kasprintf invalid_arg "PVM Context implementation mismatch: got %s" got

module type S = sig
  type repo

  type tree

  val of_node_context : 'a Context.index -> ('a, repo) Context_sigs.index

  val to_node_context : ('a, repo) Context_sigs.index -> 'a Context.index

  val of_node_pvmstate : Context.pvmstate -> tree

  val to_node_pvmstate : tree -> Context.pvmstate
end

(* Context *)
let of_node_context : type repo tree.
    (repo, tree) equality_witness ->
    'a Context.index ->
    ('a, repo) Context_sigs.index =
 fun eqw (Index {equality_witness; index; _}) ->
  match Context.equiv equality_witness eqw with
  | Some Refl, Some Refl -> index
  | _ ->
      (* This could happen if the context backend was to change for a
         given pvm/rollup. For now we only use Irmin, if this changes,
         this will demand to provide migration functions from prior
         pmv_context to the next one. *)
      assert false

let to_node_context : type repo tree.
    (module Context_sigs.S with type tree = tree and type repo = repo) ->
    ('a, repo) Context_sigs.index ->
    'a Context.index =
 fun (module C) index ->
  Context.make_index
    ~index
    ~pvm_context_impl:(module C)
    ~equality_witness:C.equality_witness
    ~impl_name:C.impl_name

(* PVMState *)
let of_node_pvmstate : type repo tree.
    (repo, tree) equality_witness -> Context.pvmstate -> tree =
 fun eqw (PVMState {equality_witness; pvmstate; _}) ->
  match Context.equiv equality_witness eqw with
  | Some Refl, Some Refl -> pvmstate
  | _ -> assert false

let to_node_pvmstate : type tree.
    (module Context_sigs.S with type tree = tree) -> tree -> Context.pvmstate =
 fun (module C) pvmstate ->
  Context.make_pvmstate
    ~pvmstate
    ~pvm_context_impl:(module C)
    ~equality_witness:C.equality_witness
    ~impl_name:C.impl_name

module Irmin = struct
  module I = struct
    include Irmin_context

    let load ~cache_size path = load ~cache_size path
  end

  type repo = I.repo

  type tree = I.tree

  let of_node_context : 'a Context.index -> ('a, repo) Context_sigs.index =
   fun ctxt -> of_node_context I.equality_witness ctxt

  let to_node_context : ('a, repo) Context_sigs.index -> 'a Context.index =
   fun ctxt -> to_node_context (module I) ctxt

  let of_node_pvmstate : Context.pvmstate -> tree =
   fun c -> of_node_pvmstate I.equality_witness c

  let to_node_pvmstate : tree -> Context.pvmstate = to_node_pvmstate (module I)
end

module Riscv = struct
  module R = Riscv_context

  type repo = R.repo

  type tree = R.tree

  let of_node_context : 'a Context.index -> ('a, repo) Context_sigs.index =
   fun ctxt -> of_node_context R.equality_witness ctxt

  let to_node_context : ('a, repo) Context_sigs.index -> 'a Context.index =
   fun ctxt -> to_node_context (module R) ctxt

  let of_node_pvmstate : Context.pvmstate -> tree =
   fun c -> of_node_pvmstate R.equality_witness c

  let to_node_pvmstate : tree -> Context.pvmstate = to_node_pvmstate (module R)
end
