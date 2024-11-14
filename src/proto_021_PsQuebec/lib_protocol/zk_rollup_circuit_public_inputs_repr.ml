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

type pending_op_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fee : Zk_rollup_scalar.t;
  exit_validity : bool;
  zk_rollup : Zk_rollup_repr.t;
  l2_op : Zk_rollup_operation_repr.t;
}

type private_batch_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
  zk_rollup : Zk_rollup_repr.t;
}

type fee_public_inputs = {
  old_state : Zk_rollup_state_repr.t;
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
}

type t =
  | Pending_op of pending_op_public_inputs
  | Private_batch of private_batch_public_inputs
  | Fee of fee_public_inputs

let bool_to_scalar b =
  if b then Zk_rollup_scalar.of_z Z.one else Zk_rollup_scalar.of_z Z.zero

let to_scalar_array = function
  | Pending_op {old_state; new_state; fee; exit_validity; zk_rollup; l2_op} ->
      Array.concat
        [
          old_state;
          new_state;
          [|
            fee;
            bool_to_scalar exit_validity;
            Zk_rollup_repr.to_scalar zk_rollup;
          |];
          Zk_rollup_operation_repr.to_scalar_array l2_op;
        ]
  | Private_batch {old_state; new_state; fees; zk_rollup} ->
      Array.concat
        [old_state; new_state; [|fees; Zk_rollup_repr.to_scalar zk_rollup|]]
  | Fee {old_state; new_state; fees} ->
      Array.concat [old_state; new_state; [|fees|]]
