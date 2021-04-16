(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** This is the Michelson interpreter.

    This module offers a way to execute either a Michelson script or a
    Michelson instruction.

    Implementation details are documented in the .ml file.

*)

open Alpha_context
open Script_typed_cps_ir

type execution_trace =
  (Script.location * Gas.t * (Script.expr * string option) list) list

type error +=
  | Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error : Contract.t * Script.expr -> error

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Michelson_too_many_recursive_calls

type execution_result = {
  ctxt : context;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
}

type step_constants = {
  source : Contract.t;
  payer : Contract.t;
  self : Contract.t;
  amount : Tez.t;
  chain_id : Chain_id.t;
}

(** The interpreter uses a control stack with specific cases for loops
    and DIP. See the details in the implementation file. *)
type (_, _, _, _) continuation =
  | KNil : ('r, 'f, 'r, 'f) continuation
  | KCons :
      ('a, 's, 'b, 't) kinstr * ('b, 't, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KUndip :
      'b * ('b, 'a * 's, 'r, 'f) continuation
      -> ('a, 's, 'r, 'f) continuation
  | KLoop_in :
      ('a, 's, bool, 'a * 's) kinstr * ('a, 's, 'r, 'f) continuation
      -> (bool, 'a * 's, 'r, 'f) continuation
  | KLoop_in_left :
      ('a, 's, ('a, 'b) Script_typed_ir.union, 's) kinstr
      * ('b, 's, 'r, 'f) continuation
      -> (('a, 'b) Script_typed_ir.union, 's, 'r, 'f) continuation
  | KIter :
      ('a, 'b * 's, 'b, 's) kinstr * 'a list * ('b, 's, 'r, 'f) continuation
      -> ('b, 's, 'r, 'f) continuation
  | KList_mapping :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b Script_typed_ir.boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('c, 's, 'r, 'f) continuation
  | KList_mapped :
      ('a, 'c * 's, 'b, 'c * 's) kinstr
      * 'a list
      * 'b list
      * int
      * ('b Script_typed_ir.boxed_list, 'c * 's, 'r, 'f) continuation
      -> ('b, 'c * 's, 'r, 'f) continuation
  | KMap_mapping :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) Script_typed_ir.map
      * (('a, 'c) Script_typed_ir.map, 'd * 's, 'r, 'f) continuation
      -> ('d, 's, 'r, 'f) continuation
  | KMap_mapped :
      ('a * 'b, 'd * 's, 'c, 'd * 's) kinstr
      * ('a * 'b) list
      * ('a, 'c) Script_typed_ir.map
      * 'a
      * (('a, 'c) Script_typed_ir.map, 'd * 's, 'r, 'f) continuation
      -> ('c, 'd * 's, 'r, 'f) continuation

type ('a, 's, 'b, 'f, 'u) logging_function =
  ('a, 's, 'b, 'f) Script_typed_cps_ir.kinstr ->
  context ->
  Script.location ->
  'u Script_typed_ir.stack_ty ->
  'u ->
  unit

(** [STEP_LOGGER] is the module type of logging
    modules as passed to the Michelson interpreter.
    Note that logging must be performed by side-effects
    on an underlying log structure. *)
module type STEP_LOGGER = sig
  (** [log_interp] is called at each call of the internal
      function [interp]. [interp] is called when starting
      the interpretation of a script and subsequently
      at each [Exec] instruction. *)
  val log_interp : ('a, 's, 'b, 'f, 'u) logging_function

  (** [log_entry] is called {i before} executing
      each instruction but {i after} gas for
      this instruction has been successfully consumed. *)
  val log_entry : ('a, 's, 'b, 'f, 'a * 's) logging_function

  val log_control : ('a, 's, 'b, 'f) continuation -> unit

  (** [log_exit] is called {i after} executing each
      instruction. *)
  val log_exit : ('a, 's, 'b, 'f, 'u) logging_function

  (** [get_log] allows to obtain an execution trace, if
      any was produced. *)
  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

val step :
  logger option ->
  context ->
  step_constants ->
  ('bef, 'aft) Script_typed_ir.descr ->
  'bef ->
  ('aft * context) tzresult Lwt.t

val execute :
  ?logger:logger ->
  Alpha_context.t ->
  Script_ir_translator.unparsing_mode ->
  step_constants ->
  script:Script.t ->
  entrypoint:string ->
  parameter:Script.expr ->
  internal:bool ->
  execution_result tzresult Lwt.t

(** [kstep logger ctxt step_constants kinstr accu stack] interprets the
    script represented by [kinstr] under the context [ctxt]. This will
    turn a stack whose topmost element is [accu] and remaining elements
    [stack] into a new accumulator and a new stack. This function also
    returns an updated context. If [logger] is given, [kstep] calls back
    its functions at specific points of the execution. The execution is
    parameterized by some [step_constants]. *)
val kstep :
  logger option ->
  context ->
  step_constants ->
  ('a, 's, 'r, 'f) Script_typed_cps_ir.kinstr ->
  'a ->
  's ->
  ('r * 'f * context) tzresult Lwt.t

(** Internal interpretation loop
    ============================

    The following types and the following function are exposed
    in the interface to allow the inference of a gas model in
    snoop.

    Strictly speaking, they should not be considered as part of
    the interface since they expose implementation details that
    may change in the future.

*)

module Internals : sig
  (** Internally, the interpretation loop uses a local gas counter. *)
  type local_gas_counter = int

  (** During the evaluation, the gas level in the context is outdated. *)
  type outdated_context = OutDatedContext of context [@@unboxed]

  (** [run logger ctxt step_constants local_gas_counter i k ks accu stack]
    evaluates [k] (having [i] as predecessor) under the control flow
    stack [ks] and the A-stack represented by [accu] and [stack]. *)
  val run :
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    ('c, 'u, 'd, 'v) kinstr ->
    ('a, 's, 'b, 't) kinstr ->
    ('b, 't, 'r, 'f) continuation ->
    'a ->
    's ->
    ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

  val next :
    logger option ->
    outdated_context * step_constants ->
    local_gas_counter ->
    ('a, 's, 'r, 'f) continuation ->
    'a ->
    's ->
    ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t
end
