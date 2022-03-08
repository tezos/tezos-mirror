(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

module Verifier_storage : sig
  include
    Tx_rollup_l2_storage_sig.STORAGE
      with type t = Context.tree
       and type 'a m = ('a, error) result Lwt.t
end

module Verifier_context : sig
  include Tx_rollup_l2_context_sig.CONTEXT with type t = Verifier_storage.t
end

(** [verify_proof message proof ~agreed ~rejected ~max_proof_size] verifies
    a Merkle proof for a L2 message, starting from the state [agreed]. If the
    [proof] is correct, and the final Merkle hash is not equal to [rejected],
    then [verify_proof] passes.
    Note that if the proof is larger than [max_proof_size] and the final
    Merkle hash is equal to [rejected], the needed proof for the rejected
    commitment is too large, thus, [verify_proof] passes and the commitment
    is rejected. *)
val verify_proof :
  Tx_rollup_l2_apply.parameters ->
  Tx_rollup_message.t ->
  Tx_rollup_l2_proof.t ->
  agreed:Tx_rollup_commitment.message_result ->
  rejected:Tx_rollup_message_result_hash.t ->
  max_proof_size:int ->
  unit tzresult Lwt.t
