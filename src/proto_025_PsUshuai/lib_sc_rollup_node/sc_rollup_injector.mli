(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Manager operation for a given L1 operation. *)
val injector_operation_to_manager :
  L1_operation.t -> Protocol.Alpha_context.packed_manager_operation

(** [collapse_statuses ~arities statuses] turns the statuses of the manager
    operations a batch was made of into one status per injected operation.
    [arities] gives, in order, how many manager operations each of them took.
    An operation counts as failed as soon as one of its manager operations did,
    and is reported at the index of the last one, the operation proper. *)
val collapse_statuses :
  arities:int list ->
  Injector_sigs.simulation_status list ->
  Injector_sigs.simulation_status list

(** [operation_fits op] is [false] when [op] is too large to be injected as a
    manager operation. The injector leaves such an operation in its queue for
    ever, blocking the operations behind it, so a caller has to keep one out. *)
val operation_fits : L1_operation.t -> bool

(** [manager_operations_of_injector_operation op] is the batch of manager
    operations [op] is injected as, the last of which is the operation proper.
    A refutation move carrying an inbox proof is preceded by the
    [Sc_rollup_add_messages] publishing its index proof. *)
val manager_operations_of_injector_operation :
  L1_operation.t -> Protocol.Alpha_context.packed_manager_operation list

(** L1 operation corresponding to a manager operation if any. *)
val injector_operation_of_manager :
  'a Protocol.Alpha_context.manager_operation -> L1_operation.t option
