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

open Tx_rollup_errors_repr

module Verifier_storage :
  Tx_rollup_l2_storage_sig.STORAGE
    with type t = Context.tree
     and type 'a m = ('a, error) result Lwt.t = struct
  type t = Context.tree

  type 'a m = ('a, error) result Lwt.t

  module Syntax = struct
    let ( let* ) = ( >>=? )

    let ( let+ ) = ( >|=? )

    let return = return

    let fail e = Lwt.return (Error e)

    let catch (m : 'a m) k h = m >>= function Ok x -> k x | Error e -> h e

    let list_fold_left_m = List.fold_left_es
  end

  let path k = [Bytes.to_string k]

  let get store key = Context.Tree.find store (path key) >>= return

  let set store key value = Context.Tree.add store (path key) value >>= return

  let remove store key = Context.Tree.remove store (path key) >>= return
end

module Verifier_context = Tx_rollup_l2_context.Make (Verifier_storage)
module Verifier_apply = Tx_rollup_l2_apply.Make (Verifier_context)

let hash_message_result after withdraw =
  Alpha_context.Tx_rollup_commitment.hash_message_result
    {context_hash = after; withdrawals_merkle_root = withdraw}

(** [after_hash_when_proof_failed before] produces the
    {!Alpha_context.Tx_rollup_message_result_hash} expected if a proof failed.
    That is, the after hash is the same as [before] and it produced zero
    withdrawals. *)
let after_hash_when_proof_failed before =
  let open Alpha_context in
  hash_message_result before Tx_rollup_withdraw.Merkle.empty

(** [compute_proof_after_hash ~max_proof_size agreed proof message] computes the
    after hash expected while verifying [proof] on [message] starting from
    [agreed].

    Note that if the proof is incorrect this function fails and the commit
    can not be rejected. *)
let compute_proof_after_hash ~max_proof_size parameters agreed proof message =
  let proof_length =
    Data_encoding.Binary.length Tx_rollup_l2_proof.encoding proof
  in
  let proof_is_too_long = Compare.Int.(proof_length > max_proof_size) in
  let before = match proof.before with `Node x -> x | `Value x -> x in
  let agreed_is_correct = Context_hash.(before = agreed) in
  fail_unless
    agreed_is_correct
    (Proof_invalid_before {provided = before; agreed})
  >>=? fun () ->
  Context.verify_stream_proof proof (fun tree ->
      Verifier_apply.apply_message tree parameters message >>= function
      | Ok (tree, (_, withdrawals)) -> Lwt.return (tree, withdrawals)
      | Error _ -> Lwt.return (tree, []))
  >>= fun res ->
  match res with
  | (Ok _ | Error (`Stream_too_short _)) when proof_is_too_long ->
      (* If the proof is larger than [proof_is_too_long] we care about 2 cases:

         - The proof verification succedeed but should not be considered valid
           since it is larger than the size limit
         - The proof verification failed because it was truncated but was
           already larger than the size limit

         In those two cases, the expected after hash is
         [after_hash_when_proof_failed] because the correct commitment is
         "we were not able to apply this message, so after is the same
         as before"
      *)
      return (after_hash_when_proof_failed agreed)
  | Ok (tree, withdrawals) ->
      (* The proof is small enough, we compare the computed hash with the
         committed one *)
      let tree_hash = Context.Tree.hash tree in
      return
        (hash_message_result
           tree_hash
           (Alpha_context.Tx_rollup_withdraw.Merkle.merklize_list withdrawals))
  | Error _ ->
      (* Finally, the proof verification leads to an internal Irmin error *)
      fail Proof_failed_to_reject

let verify_proof parameters message proof
    ~(agreed : Alpha_context.Tx_rollup_commitment.message_result) ~rejected
    ~max_proof_size =
  compute_proof_after_hash
    parameters
    agreed.context_hash
    ~max_proof_size
    proof
    message
  >>=? fun computed_result ->
  if Alpha_context.Tx_rollup_message_result_hash.(computed_result <> rejected)
  then return_unit
  else fail Proof_produced_rejected_state
