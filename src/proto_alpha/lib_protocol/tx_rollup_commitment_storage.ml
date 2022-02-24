(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

let just_ctxt (ctxt, _, _) = ctxt

open Tx_rollup_commitment_repr

let get_prev_level ctxt tx_rollup level =
  Tx_rollup_inbox_storage.get_adjacent_levels ctxt level tx_rollup
  >|=? fun (ctxt, predecessor_level, _) -> (ctxt, predecessor_level)

let check_commitment_predecessor_hash ctxt tx_rollup
    (commitment : Tx_rollup_commitment_repr.t) =
  let level = commitment.level in
  (* Check that level has the correct predecessor *)
  get_prev_level ctxt tx_rollup level >>=? fun (ctxt, predecessor_level) ->
  match (predecessor_level, commitment.predecessor) with
  | (None, None) -> return ctxt
  | (Some _, None) | (None, Some _) -> fail Wrong_commitment_predecessor_level
  | (Some predecessor_level, Some hash) ->
      (* The predecessor level must include this commitment*)
      Storage.Tx_rollup.Commitment.get ctxt (predecessor_level, tx_rollup)
      >>=? fun (ctxt, predecesor_commitment) ->
      let expected_hash =
        Tx_rollup_commitment_repr.hash predecesor_commitment.commitment
      in
      fail_unless
        Tx_rollup_commitment_repr.Commitment_hash.(hash = expected_hash)
        Missing_commitment_predecessor
      >>=? fun () -> return ctxt

let add_commitment ctxt tx_rollup pkh (commitment : Tx_rollup_commitment_repr.t)
    =
  let key = (commitment.level, tx_rollup) in
  Tx_rollup_inbox_storage.get_metadata ctxt commitment.level tx_rollup
  >>=? fun (ctxt, {inbox_length; hash; _}) ->
  let actual_len = List.length commitment.batches in
  fail_unless
    Compare.Int.(Int32.to_int inbox_length = actual_len)
    Wrong_batch_count
  >>=? fun () ->
  fail_unless
    (Tx_rollup_inbox_repr.equal_hash commitment.inbox_hash hash)
    Wrong_inbox_hash
  >>=? fun () ->
  Storage.Tx_rollup.Commitment.mem ctxt key >>=? fun (ctxt, old_commitment) ->
  fail_when old_commitment (Level_already_has_commitment commitment.level)
  >>=? fun () ->
  check_commitment_predecessor_hash ctxt tx_rollup commitment >>=? fun ctxt ->
  let current_level = (Raw_context.current_level ctxt).level in
  let submitted : Tx_rollup_commitment_repr.Submitted_commitment.t =
    {commitment; committer = pkh; submitted_at = current_level}
  in
  Storage.Tx_rollup.Commitment.add ctxt key submitted >|=? just_ctxt

let get_commitment :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Raw_level_repr.t ->
    (Raw_context.t * Tx_rollup_commitment_repr.Submitted_commitment.t option)
    tzresult
    Lwt.t =
 fun ctxt tx_rollup level ->
  Storage.Tx_rollup.State.find ctxt tx_rollup >>=? fun (ctxt, state) ->
  match state with
  | None -> fail @@ Tx_rollup_state_storage.Tx_rollup_does_not_exist tx_rollup
  | Some _ -> Storage.Tx_rollup.Commitment.find ctxt (level, tx_rollup)
