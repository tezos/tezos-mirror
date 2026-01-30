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

(** This is the Michelson interpreter.

    This module offers a way to execute either a Michelson script or a
    Michelson instruction.

    Implementation details are documented in the .ml file.

*)

open Alpha_context
open Script_typed_ir

type error += Reject of Script.location * Script.expr * execution_trace option

type error += Overflow of Script.location * execution_trace option

type error += Runtime_contract_error of Contract_hash.t

type error += Bad_contract_parameter of Contract.t (* `Permanent *)

type error += Cannot_serialize_failure

type error += Cannot_serialize_storage

type error += Michelson_too_many_recursive_calls

(** The result from script interpretation. *)
type execution_result = {
  script : Script_ir_translator.ex_script;
  code_size : int;
  storage : Script.expr;
  lazy_storage_diff : Lazy_storage.diffs option;
  operations : packed_internal_operation list;
  ticket_diffs : Z.t Ticket_token_map.t;
  ticket_receipt : Ticket_receipt.t;
  address_registry_diff : Address_registry.diff list;
  balance_updates : Receipt.balance_updates;
}

type step_constants = Script_typed_ir.step_constants = {
  sender : Destination.t;
  payer : Signature.public_key_hash;
  self : Contract_hash.t;
  amount : Tez.t;
  balance : Tez.t;
  chain_id : Chain_id.t;
  now : Script_timestamp.t;
  level : Script_int.n Script_int.num;
}

(** [execute ?logger ctxt ~cached_script mode step_constant ~script
   ~entrypoint ~parameter ~internal] interprets the [script]'s
   [entrypoint] for a given [parameter].

   This will update the local storage of the contract
   [step_constants.self]. Other pieces of contextual information
   ([sender], [payer], [amount], and [chain_id]) are also passed in
   [step_constant].

   [internal] is [true] if and only if the execution happens within an
   internal operation.

   [mode] is the unparsing mode, as declared by
   {!Script_ir_translator}.

   [cached_script] is the cached elaboration of [script], that is the
   well typed abstract syntax tree produced by the type elaboration of
   [script] during a previous execution and stored in the in-memory
   cache.

*)
val execute :
  ?logger:logger ->
  Alpha_context.t ->
  cached_script:Script_ir_translator.ex_script option ->
  Script_ir_unparser.unparsing_mode ->
  step_constants ->
  script:Script.t ->
  entrypoint:Entrypoint.t ->
  parameter:Script.expr ->
  internal:bool ->
  (execution_result * context) tzresult Lwt.t

(** [execute_with_typed_parameter ?logger ctxt ~cached_script mode
   step_constant ~script ~entrypoint loc ~parameter_ty ~parameter ~internal]
   interprets the [script]'s [entrypoint] for a given (typed) [parameter].

   See {!execute} for more details about the function's arguments.
*)
val execute_with_typed_parameter :
  ?logger:logger ->
  Alpha_context.context ->
  cached_script:Script_ir_translator.ex_script option ->
  Script_ir_unparser.unparsing_mode ->
  step_constants ->
  script:Script.t ->
  entrypoint:Entrypoint.t ->
  parameter_ty:('a, _) Script_typed_ir.ty ->
  location:Script.location ->
  parameter:'a ->
  internal:bool ->
  (execution_result * context) tzresult Lwt.t

(** Internal interpretation loop
    ============================

    The following types and the following functions are exposed
    in the interface to allow the inference of a gas model in
    snoop.

    Strictly speaking, they should not be considered as part of
    the interface since they expose implementation details that
    may change in the future.

*)

module Internals : sig
  (** Internally, the interpretation loop uses a local gas counter. *)

  (** [next logger (ctxt, step_constants) local_gas_counter ks accu
      stack] is an internal function which interprets the continuation
      [ks] to execute the interpreter on the current A-stack. *)
  val next :
    logger option ->
    Local_gas_counter.outdated_context * step_constants ->
    Local_gas_counter.local_gas_counter ->
    ('a, 's) stack_ty ->
    ('a, 's, 'r, 'f) continuation ->
    'a ->
    's ->
    ('r
    * 'f
    * Local_gas_counter.outdated_context
    * Local_gas_counter.local_gas_counter)
    tzresult
    Lwt.t

  val step :
    Local_gas_counter.outdated_context * step_constants ->
    Local_gas_counter.local_gas_counter ->
    ('a, 's, 'r, 'f) Script_typed_ir.kinstr ->
    'a ->
    's ->
    ('r
    * 'f
    * Local_gas_counter.outdated_context
    * Local_gas_counter.local_gas_counter)
    tzresult
    Lwt.t

  val step_descr :
    logger option ->
    context ->
    Script_typed_ir.step_constants ->
    ('a, 's, 'r, 'f) Script_typed_ir.kdescr ->
    'a ->
    's ->
    ('r * 'f * context) tzresult Lwt.t

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
    ('a, 's) stack_ty ->
    ('a, 's, 'r, 'f) Script_typed_ir.kinstr ->
    'a ->
    's ->
    ('r * 'f * context) tzresult Lwt.t

  module Raw : sig
    open Local_gas_counter
    open Script_interpreter_defs

    val kmap_exit : ('a, 'b, 'c, 'e, 'f, 'm, 'n, 'o) kmap_exit_type

    val kmap_enter : ('a, 'b, 'c, 'd, 'f, 'i, 'j, 'k) kmap_enter_type

    val klist_exit : ('a, 'b, 'c, 'd, 'e, 'i, 'j) klist_exit_type

    val klist_enter : ('a, 'b, 'c, 'd, 'e, 'f, 'j) klist_enter_type

    val kloop_in_left : ('a, 'b, 'c, 'd, 'e, 'f, 'g) kloop_in_left_type

    val kloop_in : ('a, 'b, 'c, 'r, 'f, 's) kloop_in_type

    val kiter : ('a, 'b, 's, 'r, 'f, 'c) kiter_type

    val next :
      outdated_context * step_constants ->
      local_gas_counter ->
      ('a, 's, 'r, 'f) continuation ->
      'a ->
      's ->
      ('r * 'f * outdated_context * local_gas_counter) tzresult Lwt.t

    val ilist_map : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i) ilist_map_type

    val ilist_iter : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'cmp) ilist_iter_type

    val iset_iter : ('a, 'b, 'c, 'd, 'e, 'f, 'g) iset_iter_type

    val imap_map : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j) imap_map_type

    val imap_iter : ('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'cmp) imap_iter_type

    val imul_teznat : ('a, 'b, 'c, 'd, 'e, 'f) imul_teznat_type

    val imul_nattez : ('a, 'b, 'c, 'd, 'e, 'f) imul_nattez_type

    val ilsl_nat : ('a, 'b, 'c, 'd, 'e, 'f) ilsl_nat_type

    val ilsr_nat : ('a, 'b, 'c, 'd, 'e, 'f) ilsr_nat_type

    val ifailwith : ifailwith_type

    val iexec : ('a, 'b, 'c, 'd, 'e, 'f, 'g) iexec_type

    val iview : ('a, 'b, 'c, 'd, 'e, 'f, 'i, 'o) iview_type

    val step : ('a, 's, 'b, 't, 'r, 'f) step_type
  end
end
