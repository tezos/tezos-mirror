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

type op_pi = {
  new_state : Zk_rollup_state_repr.t;
  fee : Zk_rollup_scalar.t;
  exit_validity : bool;
}

type private_inner_pi = {
  new_state : Zk_rollup_state_repr.t;
  fees : Zk_rollup_scalar.t;
}

type fee_pi = {new_state : Zk_rollup_state_repr.t}

(* Data sent to an update operation *)
type t = {
  pending_pis : (string * op_pi) list;
  private_pis : (string * private_inner_pi) list;
  fee_pi : fee_pi;
  proof : Plonk.proof;
}

let op_pi_encoding : op_pi Data_encoding.t =
  Data_encoding.(
    conv
      (fun {new_state; fee; exit_validity} -> (new_state, fee, exit_validity))
      (fun (new_state, fee, exit_validity) -> {new_state; fee; exit_validity})
      (obj3
         (req "new_state" Zk_rollup_state_repr.encoding)
         (req "fee" Plonk.scalar_encoding)
         (req "exit_validity" bool)))

let private_inner_pi_encoding : private_inner_pi Data_encoding.t =
  Data_encoding.(
    conv
      (fun ({new_state; fees} : private_inner_pi) -> (new_state, fees))
      (fun (new_state, fees) -> {new_state; fees})
      (obj2
         (req "new_state" Zk_rollup_state_repr.encoding)
         (req "fee" Plonk.scalar_encoding)))

let fee_pi_encoding : fee_pi Data_encoding.t =
  Data_encoding.(
    conv
      (fun {new_state} -> new_state)
      (fun new_state -> {new_state})
      (obj1 (req "new_state" Zk_rollup_state_repr.encoding)))

let encoding : t Data_encoding.t =
  Data_encoding.(
    conv
      (fun {pending_pis; private_pis; fee_pi; proof} ->
        (pending_pis, private_pis, fee_pi, proof))
      (fun (pending_pis, private_pis, fee_pi, proof) ->
        {pending_pis; private_pis; fee_pi; proof})
      (obj4
         (req "pending_pis" (list @@ tup2 (string Plain) op_pi_encoding))
         (req
            "private_pis"
            (list @@ tup2 (string Plain) private_inner_pi_encoding))
         (req "fee_pi" fee_pi_encoding)
         (req "proof" Plonk.proof_encoding)))
