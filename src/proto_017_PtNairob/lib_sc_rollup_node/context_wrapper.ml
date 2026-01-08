(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
open Context_sigs

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
    ~impl_name:"Irmin"

module Irmin = struct
  module I = struct
    include Irmin_context

    let load ~cache_size path = load ~cache_size path
  end

  let of_node_context : 'a Context.t -> ('a, I.repo, I.tree) Context_sigs.t =
   fun ctxt -> of_node_context I.equality_witness ctxt

  let to_node_context : ('a, I.repo, I.tree) Context_sigs.t -> 'a Context.t =
   fun ctxt -> to_node_context (module I) ctxt

  let of_node_pvmstate : Context.pvmstate -> I.tree =
   fun c -> of_node_pvmstate I.equality_witness c

  let to_node_pvmstate : I.tree -> Context.pvmstate =
    to_node_pvmstate (module I)
end
