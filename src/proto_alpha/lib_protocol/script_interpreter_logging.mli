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

(** [log_kinstr logger sty instr] returns [instr] prefixed by an
    [ILog] instruction to log the first instruction in [instr]. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake. *)
val log_kinstr :
  Script_typed_ir.logger ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr

(** [log_entry logger ctxt gas instr sty accu stack] simply calls
    [logger.log_entry] function with the appropriate arguments. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake.*)
val log_entry :
  Script_typed_ir.logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  'a ->
  'b ->
  unit

(** [log_exit logger ctxt gas loc instr sty accu stack] simply calls
    [logger.log_exit] function with the appropriate arguments. Note
    that [logger] value is only available when logging is enables, so
    the type system protects us from calling this by mistake.*)
val log_exit :
  Script_typed_ir.logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  Alpha_context.Script.location ->
  ('c, 'd, 'e, 'f) Script_typed_ir.kinstr ->
  ('g, 'h) Script_typed_ir.stack_ty ->
  'g ->
  'h ->
  unit

(** [log_next_kinstr logger sty instr] instruments the next instruction
    in [instr] with [ILog] instructions to make sure it will be logged.
    This instrumentation has a performance cost, but importantly, it is
    only ever paid when logging is enabled. Otherwise, the possibility
    to instrument the script is costless. Note also that [logger] value
    is only available when logging is enables, so the type system protects
    us from calling this by mistake. *)
val log_next_kinstr :
  Script_typed_ir.logger ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr tzresult

(** [log_control logger continuation] simply calls [logger.log_control]
    function with the appropriate arguments. Note that [logger] value
    is only available when logging is enables, so the type system
    protects us from calling this by mistake.*)
val log_control :
  Script_typed_ir.logger ->
  ('a, 'b, 'c, 'd) Script_typed_ir.continuation ->
  unit
