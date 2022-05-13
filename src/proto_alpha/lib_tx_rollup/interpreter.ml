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

type error += Tx_rollup_message_proof_too_large of {limit : int; actual : int}

let () =
  register_error_kind
    ~id:"tx_rollup.node.message_proof_too_large"
    ~title:"Message's application proof is too large"
    ~description:
      "The proof associated to the application of the message is too large"
    ~pp:(fun ppf (limit, actual) ->
      Format.fprintf
        ppf
        "The message produces a proof of size %d where the protocol limit is \
         %d. It will be rejected by the protocol."
        limit
        actual)
    `Permanent
    Data_encoding.(obj2 (req "limit" int31) (req "actual" int31))
    (function
      | Tx_rollup_message_proof_too_large {limit; actual} -> Some (limit, actual)
      | _ -> None)
    (fun (limit, actual) -> Tx_rollup_message_proof_too_large {limit; actual})

(** Interpret a message in the context. The function needs to be synchronised
    with the [Tx_rollup_l2_verifier] module of the protocol, in particular
    the proof size boundaries. *)
let interpret_message ~rejection_max_proof_size ctxt l2_parameters message =
  let open Lwt_result_syntax in
  let* proof, res = Prover_apply.apply_message ctxt l2_parameters message in
  let proof_size = Prover_apply.proof_size proof in
  let result =
    if proof_size > rejection_max_proof_size then
      (* The proof is too large, we can not commit this state. The
         result is discarded. *)
      Inbox.Discarded
        [
          Tx_rollup_message_proof_too_large
            {limit = rejection_max_proof_size; actual = proof_size};
        ]
    else res.Context.result
  in
  return (res.Context.tree, result)

let interpret_messages ~rejection_max_proof_size ctxt l2_parameters messages =
  let open Lwt_result_syntax in
  let ctxt_hash = Context.hash ctxt in
  let* tree_hash = Context.tree_hash_of_context ctxt in
  let+ ctxt, _ctxt_hash, _tree_hash, rev_contents =
    List.fold_left_es
      (fun (ctxt, ctxt_hash, tree_hash, acc) message ->
        let* tree, result =
          interpret_message ~rejection_max_proof_size ctxt l2_parameters message
        in
        let* ctxt, ctxt_hash, tree_hash =
          match result with
          | Inbox.Interpreted _ ->
              (* The message was successfully interpreted but the status in
                 [result] may indicate that the application failed. The context
                 may have been modified with e.g. updated counters. *)
              let tree_hash = Context.hash_tree tree in
              let*! ctxt, ctxt_hash = Context.add_tree ctxt tree in
              return (ctxt, ctxt_hash, tree_hash)
          | Inbox.Discarded _ ->
              (* The message was discarded before attempting to interpret it. The
                 context is not modified. For instance if a batch is unparsable,
                 or the BLS signature is incorrect, or a counter is wrong, etc. *)
              return (ctxt, ctxt_hash, tree_hash)
        in
        let inbox_message =
          Inbox.
            {
              message;
              result;
              l2_context_hash = {irmin_hash = ctxt_hash; tree_hash};
            }
        in
        return (ctxt, ctxt_hash, tree_hash, inbox_message :: acc))
      (ctxt, ctxt_hash, tree_hash, [])
      messages
  in
  match rev_contents with
  | [] -> (ctxt, None)
  | _ ->
      let contents = List.rev rev_contents in
      (ctxt, Some contents)

let interpret_batch ~rejection_max_proof_size ctxt l2_parameters batch =
  let open Lwt_result_syntax in
  let batch_bytes =
    Data_encoding.Binary.to_string_exn
      Protocol.Tx_rollup_l2_batch.encoding
      batch
  in
  let message, _ =
    Protocol.Alpha_context.Tx_rollup_message.make_batch batch_bytes
  in
  let* _tree, result =
    interpret_message ~rejection_max_proof_size ctxt l2_parameters message
  in
  match result with Inbox.Discarded trace -> fail trace | _ -> return ()
