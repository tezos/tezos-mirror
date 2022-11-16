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

open Tx_rollup_commitment_repr
open Tx_rollup_errors_repr

(*

   {{Note}} The functions of this module ignore storage allocations on
   purposes. This is because any storage allocated here is done under
   the condition that a user has agreed to freeze a significant bond
   of tez.

   Not only this bond covers the maximum number of bytes a transaction
   rollup can allocate, but it can be recovered iff the storage
   associated with this bond is deallocated. In other word, rollup
   operators have an incentive to keep the storage clean.

   {{Note inbox}} The only storage that is not directly covered by the
   bond are the inboxes. As a consequence, inboxes allocations are
   still recorded normally. However, as soon as an inbox is committed
   to, then it needs to be deleted for the bond to be retreived (as
   part of the commitment finalization). As a consequence, we
   virtually free the storage by an inbox (as accounted for by the
   rollup) when it is committed to.

 *)

let check_message_result ctxt {messages; _} result ~path ~index =
  (match result with
  | `Hash hash -> ok (ctxt, hash)
  | `Result result -> Tx_rollup_hash_builder.message_result ctxt result)
  >>? fun (ctxt, computed) ->
  Tx_rollup_gas.consume_check_path_commitment_cost ctxt >>? fun ctxt ->
  let cond =
    match
      Merkle.check_path
        path
        index
        computed
        messages.Tx_rollup_commitment_repr.Compact.root
    with
    | Ok x -> x
    | Error _ -> false
  in
  error_unless
    cond
    Tx_rollup_errors_repr.(
      Wrong_rejection_hash
        {provided = computed; expected = `Valid_path (messages.root, index)})
  >>? fun () -> ok ctxt

let adjust_commitments_count ctxt tx_rollup pkh ~(dir : [`Incr | `Decr]) =
  let delta = match dir with `Incr -> 1 | `Decr -> -1 in
  Storage.Tx_rollup.Commitment_bond.find (ctxt, tx_rollup) pkh
  >>=? fun (ctxt, commitment) ->
  let count =
    match commitment with Some count -> count + delta | None -> delta
  in
  fail_when Compare.Int.(count < 0) (Commitment_bond_negative count)
  >>=? fun () ->
  Storage.Tx_rollup.Commitment_bond.add (ctxt, tx_rollup) pkh count
  >>=? fun (ctxt, _, _) -> return ctxt

let remove_bond :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Signature.public_key_hash ->
    Raw_context.t tzresult Lwt.t =
 fun ctxt tx_rollup contract ->
  Storage.Tx_rollup.Commitment_bond.find (ctxt, tx_rollup) contract
  >>=? fun (ctxt, bond) ->
  match bond with
  | None -> tzfail (Bond_does_not_exist contract)
  | Some 0 ->
      Storage.Tx_rollup.Commitment_bond.remove (ctxt, tx_rollup) contract
      >>=? fun (ctxt, _, _) -> return ctxt
  | Some _ -> tzfail (Bond_in_use contract)

let slash_bond ctxt tx_rollup contract =
  Storage.Tx_rollup.Commitment_bond.find (ctxt, tx_rollup) contract
  >>=? fun (ctxt, bond_counter) ->
  match bond_counter with
  | None -> return (ctxt, false)
  | Some c ->
      Storage.Tx_rollup.Commitment_bond.remove (ctxt, tx_rollup) contract
      >>=? fun (ctxt, _, _) -> return (ctxt, Compare.Int.(0 < c))

let find :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_level_repr.t ->
    (Raw_context.t * Submitted_commitment.t option) tzresult Lwt.t =
 fun ctxt tx_rollup state level ->
  if Tx_rollup_state_repr.has_valid_commitment_at state level then
    Storage.Tx_rollup.Commitment.find (ctxt, tx_rollup) level
    >>=? fun (ctxt, commitment) ->
    match commitment with
    | None ->
        Tx_rollup_state_storage.assert_exist ctxt tx_rollup >>=? fun ctxt ->
        return (ctxt, None)
    | Some res -> return (ctxt, Some res)
  else return (ctxt, None)

let get :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_level_repr.t ->
    (Raw_context.t * Submitted_commitment.t) tzresult Lwt.t =
 fun ctxt tx_rollup state level ->
  find ctxt tx_rollup state level >>=? fun (ctxt, commitment) ->
  match commitment with
  | None -> tzfail @@ Tx_rollup_errors_repr.Commitment_does_not_exist level
  | Some commitment -> return (ctxt, commitment)

let get_finalized :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_level_repr.t ->
    (Raw_context.t * Submitted_commitment.t) tzresult Lwt.t =
 fun ctxt tx_rollup state level ->
  let window = Tx_rollup_state_repr.finalized_commitments_range state in
  (match window with
  | Some (first, last) ->
      error_unless
        Tx_rollup_level_repr.(first <= level && level <= last)
        (Tx_rollup_errors_repr.No_finalized_commitment_for_level {level; window})
  | None ->
      error
        (Tx_rollup_errors_repr.No_finalized_commitment_for_level {level; window}))
  >>?= fun () ->
  Storage.Tx_rollup.Commitment.find (ctxt, tx_rollup) level
  >>=? fun (ctxt, commitment) ->
  match commitment with
  | None -> tzfail @@ Tx_rollup_errors_repr.Commitment_does_not_exist level
  | Some commitment -> return (ctxt, commitment)

let check_commitment_level current_level state commitment =
  Tx_rollup_state_repr.next_commitment_level state current_level
  >>? fun expected_level ->
  error_when
    Tx_rollup_level_repr.(commitment.level < expected_level)
    (Level_already_has_commitment commitment.level)
  >>? fun () ->
  error_when
    Tx_rollup_level_repr.(expected_level < commitment.level)
    (Commitment_too_early
       {provided = commitment.level; expected = expected_level})

(** [check_commitment_predecessor ctxt tx_rollup state commitment]
    will raise an error if the [predecessor] field of [commitment] is
    not consistent with the context, assuming its [level] field is
    correct. *)
let check_commitment_predecessor ctxt state commitment =
  match
    ( commitment.predecessor,
      Tx_rollup_state_repr.next_commitment_predecessor state )
  with
  | Some pred_hash, Some expected_hash when Hash.(pred_hash = expected_hash) ->
      return ctxt
  | None, None -> return ctxt
  | provided, expected -> tzfail (Wrong_predecessor_hash {provided; expected})

let check_commitment_batches_and_merkle_root ctxt state inbox commitment =
  let Tx_rollup_inbox_repr.{inbox_length; merkle_root; _} = inbox in
  fail_unless
    Compare.List_length_with.(commitment.messages = inbox_length)
    Wrong_batch_count
  >>=? fun () ->
  fail_unless
    Tx_rollup_inbox_repr.Merkle.(commitment.inbox_merkle_root = merkle_root)
    Wrong_inbox_hash
  >>=? fun () -> return (ctxt, state)

let add_commitment ctxt tx_rollup state pkh commitment =
  let commitment_limit =
    Constants_storage.tx_rollup_max_commitments_count ctxt
  in
  fail_when
    Compare.Int.(
      Tx_rollup_state_repr.commitments_count state >= commitment_limit)
    Too_many_commitments
  >>=? fun () ->
  (* Check the commitment has the correct values *)
  let current_level = (Raw_context.current_level ctxt).level in
  check_commitment_level current_level state commitment >>?= fun () ->
  check_commitment_predecessor ctxt state commitment >>=? fun ctxt ->
  Tx_rollup_inbox_storage.get ctxt commitment.level tx_rollup
  >>=? fun (ctxt, inbox) ->
  check_commitment_batches_and_merkle_root ctxt state inbox commitment
  >>=? fun (ctxt, state) ->
  (* De we need to slash someone? *)
  Storage.Tx_rollup.Commitment.find (ctxt, tx_rollup) commitment.level
  >>=? fun (ctxt, invalid_commitment) ->
  Option.map_e
    (fun x ->
      let to_slash = x.Submitted_commitment.committer in
      error_when Signature.Public_key_hash.(pkh = to_slash) Invalid_committer
      >>? fun () -> ok to_slash)
    invalid_commitment
  >>?= fun to_slash ->
  (* Everything has been sorted out, let’s update the storage *)
  Tx_rollup_gas.consume_compact_commitment_cost ctxt inbox.inbox_length
  >>?= fun ctxt ->
  let commitment = Tx_rollup_commitment_repr.Full.compact commitment in
  Tx_rollup_hash_builder.compact_commitment ctxt commitment
  >>?= fun (ctxt, commitment_hash) ->
  let submitted : Tx_rollup_commitment_repr.Submitted_commitment.t =
    {
      commitment;
      commitment_hash;
      committer = pkh;
      submitted_at = current_level;
      finalized_at = None;
    }
  in
  Storage.Tx_rollup.Commitment.add (ctxt, tx_rollup) commitment.level submitted
  >>=? fun (ctxt, _commitment_size_alloc, _) ->
  (* See {{Note}} for a rationale on why ignoring storage allocation is safe. *)
  Tx_rollup_state_repr.record_commitment_creation
    state
    commitment.level
    commitment_hash
  >>?= fun state ->
  adjust_commitments_count ctxt tx_rollup pkh ~dir:`Incr >>=? fun ctxt ->
  return (ctxt, state, to_slash)

let pending_bonded_commitments :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Signature.public_key_hash ->
    (Raw_context.t * int) tzresult Lwt.t =
 fun ctxt tx_rollup pkh ->
  Storage.Tx_rollup.Commitment_bond.find (ctxt, tx_rollup) pkh
  >|=? fun (ctxt, pending) -> (ctxt, Option.value ~default:0 pending)

let has_bond :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Signature.public_key_hash ->
    (Raw_context.t * bool) tzresult Lwt.t =
 fun ctxt tx_rollup pkh ->
  Storage.Tx_rollup.Commitment_bond.find (ctxt, tx_rollup) pkh
  >|=? fun (ctxt, pending) -> (ctxt, Option.is_some pending)

let finalize_commitment ctxt rollup state =
  match Tx_rollup_state_repr.next_commitment_to_finalize state with
  | Some oldest_inbox_level ->
      (* Since the commitment head is not null, we know the oldest
         inbox has a commitment. *)
      get ctxt rollup state oldest_inbox_level >>=? fun (ctxt, commitment) ->
      (* Is the finality period for this commitment over? *)
      let finality_period = Constants_storage.tx_rollup_finality_period ctxt in
      let current_level = (Raw_context.current_level ctxt).level in
      fail_when
        Raw_level_repr.(
          current_level < add commitment.submitted_at finality_period)
        No_commitment_to_finalize
      >>=? fun () ->
      (* We remove the inbox *)
      Tx_rollup_inbox_storage.remove ctxt oldest_inbox_level rollup
      >>=? fun ctxt ->
      (* We update the commitment to mark it as finalized *)
      Storage.Tx_rollup.Commitment.update
        (ctxt, rollup)
        oldest_inbox_level
        {commitment with finalized_at = Some current_level}
      >>=? fun (ctxt, _commitment_size_alloc) ->
      (* See {{Note}} for a rationale on why ignoring storage
         allocation is safe. *)
      (* We update the state *)
      Tx_rollup_state_repr.record_inbox_deletion state oldest_inbox_level
      >>?= fun state -> return (ctxt, state, oldest_inbox_level)
  | None -> tzfail No_commitment_to_finalize

let remove_commitment ctxt rollup state =
  match Tx_rollup_state_repr.next_commitment_to_remove state with
  | Some tail ->
      (* We check the commitment is old enough *)
      get ctxt rollup state tail >>=? fun (ctxt, commitment) ->
      (match commitment.finalized_at with
      | Some finalized_at ->
          let withdraw_period =
            Constants_storage.tx_rollup_withdraw_period ctxt
          in
          let current_level = (Raw_context.current_level ctxt).level in
          fail_when
            Raw_level_repr.(current_level < add finalized_at withdraw_period)
            Remove_commitment_too_early
      | None ->
          (* unreachable code if the implementation is correct *)
          tzfail (Internal_error "Missing finalized_at field"))
      >>=? fun () ->
      (* Decrement the bond count of the committer *)
      adjust_commitments_count ctxt rollup commitment.committer ~dir:`Decr
      >>=? fun ctxt ->
      (* We remove the commitment *)
      Storage.Tx_rollup.Commitment.remove (ctxt, rollup) tail
      >>=? fun (ctxt, _freed_size, _existed) ->
      (* See {{Note}} for a rationale on why ignoring storage
         allocation is safe. *)
      Tx_rollup_reveal_storage.remove ctxt rollup tail >>=? fun ctxt ->
      (* We update the state *)
      let msg_hash = commitment.commitment.messages.last_result_message_hash in
      Tx_rollup_state_repr.record_commitment_deletion
        state
        tail
        commitment.commitment_hash
        msg_hash
      >>?= fun state -> return (ctxt, state, tail)
  | None -> tzfail No_commitment_to_remove

let check_agreed_and_disputed_results ctxt tx_rollup state
    (submitted_commitment : Submitted_commitment.t) ~agreed_result
    ~agreed_result_path ~disputed_result ~disputed_position
    ~disputed_result_path =
  let commitment = submitted_commitment.commitment in
  Tx_rollup_state_repr.check_level_can_be_rejected state commitment.level
  >>?= fun () ->
  check_message_result
    ctxt
    commitment
    (`Hash disputed_result)
    ~path:disputed_result_path
    ~index:disputed_position
  >>?= fun ctxt ->
  if Compare.Int.(disputed_position = 0) then
    Tx_rollup_hash_builder.message_result ctxt agreed_result
    >>?= fun (ctxt, agreed) ->
    match Tx_rollup_level_repr.pred commitment.level with
    | None ->
        let expected = Tx_rollup_message_result_hash_repr.init in
        fail_unless
          Tx_rollup_message_result_hash_repr.(agreed = expected)
          (Wrong_rejection_hash {provided = agreed; expected = `Hash expected})
        >>=? fun () -> return ctxt
    | Some pred_level -> (
        Storage.Tx_rollup.Commitment.find (ctxt, tx_rollup) pred_level
        >>=? fun (ctxt, candidate) ->
        match candidate with
        | Some commitment ->
            let expected =
              commitment.commitment.messages.last_result_message_hash
            in
            fail_unless
              Tx_rollup_message_result_hash_repr.(agreed = expected)
              (Wrong_rejection_hash
                 {provided = agreed; expected = `Hash expected})
            >>=? fun () -> return ctxt
        | None -> (
            match Tx_rollup_state_repr.last_removed_commitment_hashes state with
            | Some (last_hash, _) ->
                fail_unless
                  Tx_rollup_message_result_hash_repr.(agreed = last_hash)
                  (Wrong_rejection_hash
                     {provided = agreed; expected = `Hash last_hash})
                >>=? fun () -> return ctxt
            | None -> tzfail (Internal_error "Missing commitment predecessor")))
  else
    check_message_result
      ctxt
      commitment
      (`Result agreed_result)
      ~path:agreed_result_path
      ~index:(disputed_position - 1)
    >>?= fun ctxt -> return ctxt

let reject_commitment ctxt rollup state level =
  Tx_rollup_state_repr.check_level_can_be_rejected state level >>?= fun () ->
  (* Fetching the next predecessor hash to be used *)
  (match Tx_rollup_level_repr.pred level with
  | Some pred_level ->
      find ctxt rollup state pred_level >>=? fun (ctxt, pred_commitment) ->
      let pred_hash =
        Option.map
          (fun (x : Submitted_commitment.t) -> x.commitment_hash)
          pred_commitment
      in
      return (ctxt, pred_hash)
  | None -> return (ctxt, None))
  (* We record in the state *)
  >>=? fun (ctxt, pred_hash) ->
  Tx_rollup_state_repr.record_commitment_rejection state level pred_hash
  >>?= fun state -> return (ctxt, state)
