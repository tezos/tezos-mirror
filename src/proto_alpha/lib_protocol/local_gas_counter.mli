(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** This module exposes an API for local gas counting. It provides a set of
    functions for updating a gas counter without applying it on an
    an [Alpha_context.context]. *)

(** A [local_gas_counter] is a wrapped [int]. *)
type local_gas_counter = Local_gas_counter of int [@@ocaml.unboxed]

(** A type for describing a context that is not up to date with respect to gas
    consumption. *)
type outdated_context

(*** [update_context gas_counter outdated_ctxt] returns a regular context,
      extracted from [outdated_ctxt] with [gas_counter] applied. *)
val update_context :
  local_gas_counter -> outdated_context -> Alpha_context.context

(** [local_gas_counter_and_outdated_context ctxt] returns the gas counter value
    corresponding to the remaining gas in the given context [ctxt] along with
    an [outdated_context] value. *)
val local_gas_counter_and_outdated_context :
  Alpha_context.context -> local_gas_counter * outdated_context

(** [use_gas_counter_in_context outdated_ctxt gas_counter f] first applies the
    [gas_counter] on the outdated context [outdated_ctxt], then invokes [f] on
    the resulting context, and returns a new [outdated_context] and a
    [local_gas_counter] value. *)
val use_gas_counter_in_context :
  outdated_context ->
  local_gas_counter ->
  (Alpha_context.context -> ('a * Alpha_context.context) tzresult Lwt.t) ->
  ('a * outdated_context * local_gas_counter) tzresult Lwt.t

(** [consume_opt amt cost] attempts to consume an [amt] of gas and returns the
    new remaining value wrapped in [Some]. If the resulting gas is negative
    [None] is returned. *)
val consume_opt :
  local_gas_counter -> Alpha_context.Gas.cost -> local_gas_counter option

(** [consume amt cost] attempts to consume an [amt] of gas and returns the
    new remaining value as a result. If the resulting gas is negative,
    an error [Gas.Operation_quota_exceeded] is instead returned. *)
val consume :
  local_gas_counter -> Alpha_context.Gas.cost -> local_gas_counter tzresult
