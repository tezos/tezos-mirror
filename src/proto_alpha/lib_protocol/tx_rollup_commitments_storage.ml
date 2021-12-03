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

open Tx_rollup_commitments_repr

(** Return commitments in the order that they werre submitted *)
let get_or_empty_commitments :
    Raw_context.t ->
    Raw_level_repr.t * Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_commitments_repr.t) tzresult Lwt.t =
 fun ctxt key ->
  Storage.Tx_rollup.Commitment_list.find ctxt key >|=? fun (ctxt, commitment) ->
  Option.fold
    commitment
    ~none:(ctxt, Tx_rollup_commitments_repr.empty)
    ~some:(fun l -> (ctxt, List.rev l))

let get_prev_level ctxt tx_rollup level =
  Tx_rollup_inbox_storage.get_adjacent_levels ctxt level tx_rollup
  >|=? fun (ctxt, predecessor_level, _) -> (ctxt, predecessor_level)

let check_commitment_predecessor_hash ctxt tx_rollup (commitment : Commitment.t)
    =
  let level = commitment.level in
  (* Check that level has the correct predecessor *)
  get_prev_level ctxt tx_rollup level >>=? fun (ctxt, predecessor_level) ->
  match (predecessor_level, commitment.predecessor) with
  | (None, None) -> return ctxt
  | (Some _, None) | (None, Some _) -> fail Wrong_commitment_predecessor_level
  | (Some predecessor_level, Some hash) ->
      (* The predecessor level must include this commitment*)
      get_or_empty_commitments ctxt (predecessor_level, tx_rollup)
      >>=? fun (ctxt, predecesor_commitments) ->
      fail_unless
        (Tx_rollup_commitments_repr.commitment_exists
           predecesor_commitments
           hash)
        Missing_commitment_predecessor
      >>=? fun () -> return ctxt

let add_commitment ctxt tx_rollup contract (commitment : Commitment.t) =
  let key = (commitment.level, tx_rollup) in
  get_or_empty_commitments ctxt key >>=? fun (ctxt, pending) ->
  Tx_rollup_inbox_storage.get ctxt ~level:(`Level commitment.level) tx_rollup
  >>=? fun (ctxt, inbox) ->
  let expected_len = List.length inbox.contents in
  let actual_len = List.length commitment.batches in
  fail_unless Compare.Int.(expected_len = actual_len) Wrong_batch_count
  >>=? fun () ->
  check_commitment_predecessor_hash ctxt tx_rollup commitment >>=? fun ctxt ->
  Tx_rollup_commitments_repr.append
    pending
    contract
    commitment
    (Raw_context.current_level ctxt).level
  >>?= fun new_pending ->
  Storage.Tx_rollup.Commitment_list.add ctxt key new_pending >|=? just_ctxt

let get_commitments :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Raw_level_repr.t ->
    (Raw_context.t * Tx_rollup_commitments_repr.t) tzresult Lwt.t =
 fun ctxt tx_rollup level ->
  Storage.Tx_rollup.State.find ctxt tx_rollup >>=? fun (ctxt, state) ->
  match state with
  | None -> fail @@ Tx_rollup_state_storage.Tx_rollup_does_not_exist tx_rollup
  | Some _ -> get_or_empty_commitments ctxt (level, tx_rollup)
