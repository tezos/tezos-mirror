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
open Alpha_context

(* {{Note}} This model should be part of [Tx_rollup_gas].
   Unfortunately, this is not possible, because this module is defined
   on top of [Alpha_context], while [Tx_rollup_gas] is defined on top
   of [Raw_context]. *)

let verify_proof_model message_size proof_size =
  let open Saturation_repr in
  (* The cost of verifiying the proof depends bilinearly on the size
     of the message (that is expected to capture the algoritmic
     complexity of computation to make) and the size of the proof
     (that is expected to capture the overhead of the storage). *)
  let proof_size_coeff = safe_int 124 in
  let message_size_coeff = safe_int 8_416 in

  let ( * ) = mul in
  let ( + ) = add in

  (proof_size_coeff * safe_int proof_size)
  + (message_size_coeff * safe_int message_size)

let consume_verify_proof_cost ctxt ~message_size ~proof_size =
  let max_proof_size =
    Alpha_context.Constants.tx_rollup_rejection_max_proof_size ctxt
  in
  (* We are interested in having a safe over-approximation of the
     overhead of the proof interpretation. We have trained the model
     on data coming from contexts of various “size” (i.e., number of
     leafs), but there is an edge case when it comes to proof
     verification that is hard to consider correctly: when the context
     is empty, the size is ridiculously small, no matter how many
     transactions are executed.

     As a safety net, we systematically compute a gas cost as if the
     proof is at least the big enough to declare the message as
     invalid (using the [tx_rollup_rejection_max_proof_size]
     parametric constant). *)
  Gas.consume ctxt
  @@ verify_proof_model message_size (Compare.Int.max proof_size max_proof_size)

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

let hash_message_result ctxt after withdraw =
  Tx_rollup_hash.message_result
    ctxt
    {context_hash = after; withdraw_list_hash = withdraw}

(** [after_hash_when_proof_failed before] produces the
    {!Alpha_context.Tx_rollup_message_result_hash} expected if a proof failed.
    That is, the after hash is the same as [before] and it produced zero
    withdrawals. *)
let after_hash_when_proof_failed ctxt before =
  hash_message_result ctxt before Tx_rollup_withdraw_list_hash.empty

let verify_l2_proof proof parameters message =
  Context.verify_stream_proof proof (fun tree ->
      Verifier_apply.apply_message tree parameters message >>= function
      | Ok (tree, (_, withdrawals)) -> Lwt.return (tree, withdrawals)
      | Error _ -> Lwt.return (tree, []))

(** [compute_proof_after_hash ~max_proof_size agreed proof message] computes the
    after hash expected while verifying [proof] on [message] starting from
    [agreed].

    Note that if the proof is incorrect this function fails and the commit
    can not be rejected. *)
let compute_proof_after_hash ~proof_length ~max_proof_size ctxt parameters
    agreed (proof : Tx_rollup_l2_proof.t) message =
  let message_length =
    Data_encoding.Binary.length Tx_rollup_message.encoding message
  in
  (* When considering “proof large enough to make a batch invalid,
     even if truncated”, we actually need to take into consideration
     the size of the message.

     [max_proof_size] is the upper bound, but we need to make room for
     the message itself. So the real limit for the proof size is
     reduced to that end. This way, we save a bit of TPS compared to
     just having a lower [max_proof_size] constant. *)
  let max_proof_size = max_proof_size - message_length in
  let proof_is_too_long = Compare.Int.(proof_length > max_proof_size) in
  let before = match proof.before with `Node x -> x | `Value x -> x in
  let agreed_is_correct = Context_hash.(before = agreed) in
  fail_unless
    agreed_is_correct
    (Proof_invalid_before {provided = before; agreed})
  >>=? fun () ->
  consume_verify_proof_cost
    ctxt
    ~message_size:message_length
    ~proof_size:proof_length
  >>?= fun ctxt ->
  verify_l2_proof proof parameters message >>= fun res ->
  match res with
  | (Ok _ | Error (`Stream_too_short _)) when proof_is_too_long ->
      (* If the proof is larger than [max_proof_size] we care about 2 cases:

         - The proof verification succedeed but should not be considered valid
           since it is larger than the size limit
         - The proof verification failed because it was truncated but was
           already larger than the size limit

         In those two cases, the expected after hash is
         [after_hash_when_proof_failed] because the correct commitment is
         "we were not able to apply this message, so after is the same
         as before"
      *)
      after_hash_when_proof_failed ctxt agreed >>?= fun res -> return res
  | Ok (tree, withdrawals) ->
      (* The proof is small enough, we compare the computed hash with the
         committed one *)
      let tree_hash = Context.Tree.hash tree in
      Tx_rollup_hash.withdraw_list ctxt withdrawals
      >>?= fun (ctxt, withdrawals) ->
      hash_message_result ctxt tree_hash withdrawals >>?= fun res -> return res
  | Error _ ->
      (* Finally, the proof verification leads to an internal Irmin error *)
      tzfail Proof_failed_to_reject

let verify_proof ctxt parameters message proof ~proof_length
    ~(agreed : Tx_rollup_message_result.t) ~rejected ~max_proof_size =
  compute_proof_after_hash
    ctxt
    parameters
    agreed.context_hash
    ~proof_length
    ~max_proof_size
    proof
    message
  >>=? fun (ctxt, computed_result) ->
  if Alpha_context.Tx_rollup_message_result_hash.(computed_result <> rejected)
  then return ctxt
  else tzfail Proof_produced_rejected_state

module Internal_for_tests = struct
  let verify_l2_proof = verify_l2_proof
end
