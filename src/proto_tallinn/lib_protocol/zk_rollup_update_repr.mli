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

(** Payload of a ZK Rollup update operation.
    The operator only needs to send a subset of the public inputs
    defined in {!Zk_rollup_circuit_public_inputs_repr}, the rest
    is provided by the protocol.
*)

(** Minimal subset of public inputs for the public L2 operations' circuits. *)
type op_pi = {
  new_state : Zk_rollup_state_repr.t;
  fee : Zk_rollup_scalar.t;
  exit_validity : bool;
}

(** Minimal subset of public inputs for the circuits for batches of
    private L2 operations *)
type private_inner_pi = {
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
}

(** Minimal subset of public inputs for the "fee" circuit. *)
type fee_pi = {new_state : Zk_rollup_state_repr.t}

(** Payload of an update operation.
    Includes the proof and the public inputs that are needed to verify it.
    Each set of public inputs also carries the string that identifies the
    circuit which they are for. *)
type t = {
  pending_pis : (string * op_pi) list;
  private_pis : (string * private_inner_pi) list;
  fee_pi : fee_pi;
  proof : Plonk.proof;
}

val encoding : t Data_encoding.t
