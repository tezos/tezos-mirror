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

open Alpha_context

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

(** [STEP_LOGGER] is the module type of logging
    modules as passed to the Michelson interpreter.
    Note that logging must be performed by side-effects
    on an underlying log structure. *)
module type STEP_LOGGER = sig
  (** [log_interp] is called at each call of the internal
      function [interp]. [interp] is called when starting
      the interpretation of a script and subsequently
      at each [Exec] instruction. *)
  val log_interp :
    context -> ('bef, 'aft) Script_typed_ir.descr -> 'bef -> unit

  (** [log_entry] is called {i before} executing
      each instruction but {i after} gas for
      this instruction has been successfully consumed. *)
  val log_entry : context -> ('bef, 'aft) Script_typed_ir.descr -> 'bef -> unit

  (** [log_exit] is called {i after} executing each
      instruction. *)
  val log_exit : context -> ('bef, 'aft) Script_typed_ir.descr -> 'aft -> unit

  (** [get_log] allows to obtain an execution trace, if
      any was produced. *)
  val get_log : unit -> execution_trace option tzresult Lwt.t
end

type logger = (module STEP_LOGGER)

val step :
  logger ->
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
