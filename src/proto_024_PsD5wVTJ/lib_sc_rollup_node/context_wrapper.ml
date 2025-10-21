(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(*                                                                           *)
(*****************************************************************************)

open Context
open Context_sigs

let err_implementation_mismatch ~got =
  Format.kasprintf invalid_arg "PVM Context implementation mismatch: got %s" got

module type S = sig
  type repo

  type tree

  type mut_state

  val of_node_context : 'a Context.t -> ('a, repo, tree) Context_sigs.t

  val to_node_context : ('a, repo, tree) Context_sigs.t -> 'a Context.t

  val of_node_pvmstate : Context.pvmstate -> tree

  val to_node_pvmstate : tree -> Context.pvmstate

  val from_imm : tree -> mut_state

  val to_imm : mut_state -> tree
end

(* Context *)
let of_node_context : type repo tree.
    (repo, tree) equality_witness ->
    'a Context.t ->
    ('a, repo, tree) Context_sigs.t =
 fun eqw (Context {equality_witness; tree; index; _}) ->
  match Context.equiv equality_witness eqw with
  | Some Refl, Some Refl -> {index; tree}
  | _ ->
      (* This could happen if the context backend was to change for a
         given pvm/rollup. For now we only use Irmin, if this changes,
         this will demand to provide migration functions from prior
         pmv_context to the next one. *)
      assert false

let to_node_context : type repo tree.
    (module Context_sigs.S with type tree = tree and type repo = repo) ->
    ('a, repo, tree) Context_sigs.t ->
    'a Context.t =
 fun (module C) {index; tree} ->
  Context.make
    ~index
    ~tree
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

  type mut_state = I.mut_state

  let of_node_context : 'a Context.t -> ('a, repo, tree) Context_sigs.t =
   fun ctxt -> of_node_context I.equality_witness ctxt

  let to_node_context : ('a, repo, tree) Context_sigs.t -> 'a Context.t =
   fun ctxt -> to_node_context (module I) ctxt

  let of_node_pvmstate : Context.pvmstate -> tree =
   fun c -> of_node_pvmstate I.equality_witness c

  let to_node_pvmstate : tree -> Context.pvmstate = to_node_pvmstate (module I)

  let from_imm : tree -> mut_state = I.from_imm

  let to_imm : mut_state -> tree = I.to_imm
end

module Riscv = struct
  module R = Riscv_context

  type repo = R.repo

  type tree = R.tree

  type mut_state = R.Mutable_state.t

  let of_node_context : 'a Context.t -> ('a, repo, tree) Context_sigs.t =
   fun ctxt -> of_node_context R.equality_witness ctxt

  let to_node_context : ('a, repo, tree) Context_sigs.t -> 'a Context.t =
   fun ctxt -> to_node_context (module R) ctxt

  let of_node_pvmstate : Context.pvmstate -> tree =
   fun c -> of_node_pvmstate R.equality_witness c

  let to_node_pvmstate : tree -> Context.pvmstate = to_node_pvmstate (module R)

  let from_imm : tree -> mut_state = R.Mutable_state.from_imm

  let to_imm : mut_state -> tree = R.Mutable_state.to_imm
end
