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

(**
  Abstraction layer for the public inputs to the ZKRU aPlonk circuits.

  As explained in the documentation, circuits in ZKRUs will be grouped into
  three categories: pending (public) operations, private batches and
  fee circuit. Each of these expects a different set of public inputs.
*)

(** Public inputs expected by circuits that handle single public
    L2 operations. *)
type pending_op_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fee : Zk_rollup_scalar.t;
  exit_validity : bool;
  zk_rollup : Zk_rollup_repr.t;
  l2_op : Zk_rollup_operation_repr.t;
}

(** Public inputs expected by circuits that handle a batch of private
    L2 operations. *)
type private_batch_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
  zk_rollup : Zk_rollup_repr.t;
}

(** Public inputs expected by the circuit that handles the L2 fees. *)
type fee_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
}

type t =
  | Pending_op of pending_op_public_inputs
  | Private_batch of private_batch_public_inputs
  | Fee of fee_public_inputs

(** Conversion to the type the aPlonk verifier expects. *)
val to_scalar_array : t -> Zk_rollup_scalar.t array
