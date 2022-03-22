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

val get_log :
  Script_typed_ir.logger option ->
  (Script_typed_ir.execution_trace option, error trace) result Lwt.t

val log_kinstr :
  Script_typed_ir.logger ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr

val log_entry :
  Script_typed_ir.logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  'a ->
  'b ->
  unit

val log_exit :
  Script_typed_ir.logger ->
  Local_gas_counter.outdated_context ->
  Local_gas_counter.local_gas_counter ->
  ('a, 'b) Script_typed_ir.kinfo ->
  ('c, 'd, 'e, 'f) Script_typed_ir.kinstr ->
  ('g, 'h) Script_typed_ir.stack_ty ->
  'g ->
  'h ->
  unit

val log_next_kinstr :
  Script_typed_ir.logger ->
  ('a, 'b) Script_typed_ir.stack_ty ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr ->
  ('a, 'b, 'c, 'd) Script_typed_ir.kinstr tzresult

val log_control :
  Script_typed_ir.logger ->
  ('a, 'b, 'c, 'd) Script_typed_ir.continuation ->
  unit
