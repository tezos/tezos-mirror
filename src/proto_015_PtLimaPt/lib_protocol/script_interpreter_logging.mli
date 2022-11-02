(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Script_typed_ir

(** An existential container for an instruction paired with its
    initial stack type. This is used internally to pack together
    execution branches with different initial stack types but
    the same final stack type (which we want to compute). *)
type ('r, 'f) ex_init_stack_ty =
  | Ex_init_stack_ty :
      ('a, 's) stack_ty * ('a, 's, 'r, 'f) kinstr
      -> ('r, 'f) ex_init_stack_ty

(** [log_kinstr logger sty instr] returns [instr] prefixed by an
    [ILog] instruction to log the first instruction in [instr]. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake. *)
val log_kinstr :
  logger ->
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('a, 'b, 'c, 'd) kinstr

(** [log_entry logger ctxt gas instr sty accu stack] simply calls
    [logger.log_entry] function with the appropriate arguments. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake.*)
val log_entry :
  logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('a, 'b) stack_ty ->
  'a ->
  'b ->
  unit

(** [log_exit logger ctxt gas loc instr sty accu stack] simply calls
    [logger.log_exit] function with the appropriate arguments. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake.*)
val log_exit :
  logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  Alpha_context.Script.location ->
  ('c, 'd, 'e, 'f) kinstr ->
  ('g, 'h) stack_ty ->
  'g ->
  'h ->
  unit

(** [log_control logger continuation] simply calls [logger.log_control]
    function with the appropriate arguments. Note that [logger] value
    is only available when logging is enables, so the type system
    protects us from calling this by mistake.*)
val log_control : logger -> ('a, 'b, 'c, 'd) continuation -> unit

(** [instrument_cont logger sty] creates a function instrumenting
    continuations starting from the stack type described by [sty].
    Instrumentation consists in wrapping inner continuations in
    [KLog] continuation so that logging continues. *)
val instrument_cont :
  logger ->
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) Script_interpreter_defs.cont_instrumentation

(** [log_next_continuation logger sty cont] instruments the next
    continuation in [cont] with [KLog] continuations to ensure
    logging.

    This instrumentation has a performance cost, but importantly, it
    is only ever paid when logging is enabled. Otherwise, the
    possibility to instrument the script is costless. Note also that
    [logger] value is only available when logging is enabled, so the
    type system protects us from calling this by mistake. *)
val log_next_continuation :
  logger ->
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) continuation ->
  ('a, 'b, 'c, 'd) continuation tzresult

(** [log_next_kinstr logger sty instr] instruments the next instruction
    in [instr] with [ILog] instructions to make sure it will be logged.
    This instrumentation has a performance cost, but importantly, it is
    only ever paid when logging is enabled. Otherwise, the possibility
    to instrument the script is costless. Note also that [logger] value
    is only available when logging is enables, so the type system protects
    us from calling this by mistake. *)
val log_next_kinstr :
  logger ->
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('a, 'b, 'c, 'd) kinstr tzresult

(* [kinstr_final_stack_type sty instr] computes the stack type after
   [instr] has been executed, assuming [sty] is the type of the stack
   prior to execution. *)
val kinstr_final_stack_type :
  ('a, 'b) stack_ty ->
  ('a, 'b, 'c, 'd) kinstr ->
  ('c, 'd) stack_ty option tzresult

(* The same as [kinstr_final_stack_type], but selects from multiple
   possible execution branches. If the first instr ends with FAILWITH,
   it will try the next and so on. Note that all instructions must
   result in the same stack type. *)
val branched_final_stack_type :
  ('r, 'f) ex_init_stack_ty list -> ('r, 'f) stack_ty option tzresult

(** [dipn_stack_ty witness stack_ty] returns the type of the stack
    on which instructions inside dipped block will be operating. *)
val dipn_stack_ty :
  ('a, 's, 'e, 'z, 'c, 'u, 'd, 'w) stack_prefix_preservation_witness ->
  ('c, 'u) stack_ty ->
  ('a, 's) stack_ty
