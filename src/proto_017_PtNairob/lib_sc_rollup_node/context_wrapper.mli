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

(** This modules offers functions to translate from node-context and
    node-pvmstate representation to those used in the PVM *)
open Context_sigs

(* Context *)
val of_node_context :
  ('repo, 'tree) equality_witness ->
  [`Read | `Write] Context.t ->
  ([`Read | `Write], 'repo, 'tree) Context_sigs.t

val to_node_context :
  (module Context_sigs.S with type tree = 'tree and type repo = 'repo) ->
  ('a, 'repo, 'tree) Context_sigs.t ->
  'a Context.t

(* PVMState *)
val of_node_pvmstate :
  ('repo, 'tree) equality_witness -> Context.pvmstate -> 'tree

val to_node_pvmstate :
  (module Context_sigs.S with type tree = 'tree) -> 'tree -> Context.pvmstate

(** Specialized module to handle translation to/from Irmin_context *)
module Irmin : sig
  val of_node_context :
    'a Context.t -> ('a, Irmin_context.repo, Irmin_context.tree) Context_sigs.t

  val to_node_context :
    ('a, Irmin_context.repo, Irmin_context.tree) Context_sigs.t -> 'a Context.t

  val of_node_pvmstate : Context.pvmstate -> Irmin_context.tree

  val to_node_pvmstate : Irmin_context.tree -> Context.pvmstate
end
