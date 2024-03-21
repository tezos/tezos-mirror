(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** This module provides overapproximation of memory footprint for
   Michelson-related values.

   These overapproximations are used by the cache to evaluate its own
   memory footprint and enforce declared limit over its size.

*)

(** [value_size ty v] returns an overapproximation of the size of the
   in-memory representation of [v] of type [ty]. *)
val value_size :
  ('a, _) Script_typed_ir.ty -> 'a -> Cache_memory_helpers.nodes_and_size

(** [lambda_size l] returns an overapproximation of the size of the
    internal IR for the Michelson lambda abstraction [l]. *)
val lambda_size :
  ('a, 'b) Script_typed_ir.lambda -> Cache_memory_helpers.nodes_and_size

(** [node_size root] returns the size of the in-memory representation
   of [root] in bytes. This is an over-approximation of the memory
   actually consumed by [root] since no sharing is taken into
   account. *)
val node_size :
  ('loc, 'prim) Micheline.node -> Cache_memory_helpers.nodes_and_size

(** Pointwise addition (reexport from {!Cache_memory_helpers}) *)
val ( ++ ) :
  Cache_memory_helpers.nodes_and_size ->
  Cache_memory_helpers.nodes_and_size ->
  Cache_memory_helpers.nodes_and_size

(** Zero vector (reexport from {!Cache_memory_helpers}) *)
val zero : Cache_memory_helpers.nodes_and_size

(**/**)

module Internal_for_tests : sig
  (** [ty_size ty] returns an overapproximation of the size of the
   in-memory representation of type [ty]. *)
  val ty_size :
    ('a, _) Script_typed_ir.ty -> Cache_memory_helpers.nodes_and_size

  (** [kinstr_size i] returns an overapproximation of the size of the
      internal IR [i]. *)
  val kinstr_size :
    ('a, 's, 'r, 'f) Script_typed_ir.kinstr ->
    Cache_memory_helpers.nodes_and_size

  val stack_prefix_preservation_witness_size :
    ( 'a,
      'b,
      'c,
      'd,
      'e,
      'f,
      'g,
      'h )
    Script_typed_ir.stack_prefix_preservation_witness ->
    Cache_memory_helpers.nodes_and_size
end
