(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

let find :
    Raw_context.t ->
    Tx_rollup_level_repr.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t option) tzresult Lwt.t =
 fun ctxt level tx_rollup ->
  Storage.Tx_rollup.Inbox.find (ctxt, tx_rollup) level

let get :
    Raw_context.t ->
    Tx_rollup_level_repr.t ->
    Tx_rollup_repr.t ->
    (Raw_context.t * Tx_rollup_inbox_repr.t) tzresult Lwt.t =
 fun ctxt level tx_rollup ->
  find ctxt level tx_rollup >>=? function
  | _, None -> tzfail (Inbox_does_not_exist (tx_rollup, level))
  | ctxt, Some inbox -> return (ctxt, inbox)

(** [prepare_inbox ctxt rollup state level] prepares the metadata
    for an inbox at [level], which may imply creating it if it does
    not already exist. *)
let prepare_inbox :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Raw_level_repr.t ->
    (Raw_context.t
    * Tx_rollup_state_repr.t
    * Tx_rollup_level_repr.t
    * Tx_rollup_inbox_repr.t
    * Z.t)
    tzresult
    Lwt.t =
 fun ctxt rollup state level ->
  (* First, check if there are too many inboxes *)
  fail_when
    Compare.Int.(
      Constants_storage.tx_rollup_max_inboxes_count ctxt
      <= Tx_rollup_state_repr.inboxes_count state)
    Too_many_inboxes
  >>=? fun () ->
  let current_levels = Tx_rollup_state_repr.head_levels state in
  match current_levels with
  | Some (_, tezos_lvl) when Raw_level_repr.(level < tezos_lvl) ->
      tzfail (Internal_error "Trying to write into an inbox from the past")
  | Some (tx_lvl, tezos_lvl) when Raw_level_repr.(tezos_lvl = level) ->
      (* An inbox should already exists *)
      Storage.Tx_rollup.Inbox.get (ctxt, rollup) tx_lvl
      >>=? fun (ctxt, metadata) -> return (ctxt, state, tx_lvl, metadata, Z.zero)
  | _ ->
      let pred_level_and_tx_level =
        Option.bind current_levels (fun (tx_level, tezos_level) ->
            Option.map (fun pred -> (pred, tezos_level))
            @@ Tx_rollup_level_repr.pred tx_level)
      in
      (match pred_level_and_tx_level with
      | None -> return (ctxt, state)
      | Some (tx_level, tezos_level) ->
          find ctxt tx_level rollup >>=? fun (ctxt, minbox) ->
          (* If the previous inbox is no longer in the storage, then
             quite some Tezos blocks have been created without any
             activity regarding this rollup. We can consider the inbox
             was empty, it does not change much. *)
          let final_size =
            match minbox with Some inbox -> inbox.cumulated_size | None -> 0
          in
          let hard_limit =
            Constants_storage.tx_rollup_hard_size_limit_per_inbox ctxt
          in
          let factor =
            Constants_storage.tx_rollup_cost_per_byte_ema_factor ctxt
          in
          let diff = Raw_level_repr.diff level tezos_level in
          (* Only [diff = Int32.one] should be checked
             theoretically. If [diff < Int32.one], it likely
             means there is a problem in the state machine since
             this function was called twice for the same
             level. This problem is caught at other
             places. However, if this assumption is broken, I
             prefer to consider that it counts as if there was
             no empty blocks between the first call and the
             second call to this function. *)
          let elapsed =
            if Compare.Int32.(diff <= Int32.one) then 0 else Int32.to_int diff
          in
          let state =
            Tx_rollup_state_repr.update_burn_per_byte
              state
              ~elapsed
              ~factor
              ~final_size
              ~hard_limit
          in
          Storage.Tx_rollup.State.add ctxt rollup state >|=? fun (ctxt, _, _) ->
          (ctxt, state))
      >>=? fun (ctxt, state) ->
      (* We need a new inbox *)
      Tx_rollup_state_repr.record_inbox_creation state level
      >>?= fun (state, tx_level, paid_storage_space_diff) ->
      let inbox = Tx_rollup_inbox_repr.empty in
      Storage.Tx_rollup.Inbox.init (ctxt, rollup) tx_level inbox
      >>=? fun (ctxt, _inbox_size_alloc) ->
      (* Storage accounting is done by
         [Tx_rollup_state_repr.record_inbox_creation], so we can
         ignore [inbox_size_alloc]. *)
      return (ctxt, state, tx_level, inbox, paid_storage_space_diff)

(** [update_inbox inbox msg_size] updates [metadata] to account
    for a new message of [msg_size] bytes. *)
let update_inbox :
    Tx_rollup_inbox_repr.t ->
    int ->
    Tx_rollup_inbox_repr.Merkle.root ->
    Tx_rollup_inbox_repr.t =
 fun metadata msg_size merkle_root ->
  Tx_rollup_inbox_repr.
    {
      inbox_length = 1 + metadata.inbox_length;
      cumulated_size = msg_size + metadata.cumulated_size;
      merkle_root;
    }

let append_message :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_message_repr.t ->
    (Raw_context.t * Tx_rollup_state_repr.t * Z.t) tzresult Lwt.t =
 fun ctxt rollup state message ->
  let level = (Raw_context.current_level ctxt).level in
  let message_size = Tx_rollup_message_repr.size message in
  (* Update the burn cost to pay for appending new messages *)
  prepare_inbox ctxt rollup state level
  >>=? fun ( ctxt,
             new_state,
             tx_level,
             inbox,
             paid_storage_space_diff_for_init_inbox ) ->
  fail_when
    Compare.Int.(
      inbox.inbox_length
      >= Constants_storage.tx_rollup_max_messages_per_inbox ctxt)
    (Inbox_count_would_exceed_limit rollup)
  >>=? fun () ->
  Tx_rollup_hash_builder.message ctxt message >>?= fun (ctxt, message_hash) ->
  Tx_rollup_gas.consume_add_message_cost ctxt >>?= fun ctxt ->
  let ctxt, inbox_merkle_root =
    Raw_context.Tx_rollup.add_message ctxt rollup message_hash
  in
  let new_inbox = update_inbox inbox message_size inbox_merkle_root in
  let new_size = new_inbox.cumulated_size in
  let inbox_limit =
    Constants_storage.tx_rollup_hard_size_limit_per_inbox ctxt
  in
  fail_unless
    Compare.Int.(new_size <= inbox_limit)
    (Inbox_size_would_exceed_limit rollup)
  >>=? fun () ->
  (* Checks have passed, so we can actually record in the storage. *)
  Storage.Tx_rollup.Inbox.add (ctxt, rollup) tx_level new_inbox
  >>=? fun (ctxt, new_inbox_size_alloc, _) ->
  Tx_rollup_state_repr.adjust_storage_allocation
    new_state
    ~delta:Z.(of_int new_inbox_size_alloc)
  >>?= fun (new_state, paid_storage_space_diff) ->
  return
    ( ctxt,
      new_state,
      Z.add paid_storage_space_diff_for_init_inbox paid_storage_space_diff )

let remove :
    Raw_context.t ->
    Tx_rollup_level_repr.t ->
    Tx_rollup_repr.t ->
    Raw_context.t tzresult Lwt.t =
 fun ctxt level rollup ->
  Storage.Tx_rollup.Inbox.remove (ctxt, rollup) level
  >>=? fun (ctxt, _freed, _) -> return ctxt

let check_message_hash :
    Raw_context.t ->
    Tx_rollup_level_repr.t ->
    Tx_rollup_repr.t ->
    position:int ->
    Tx_rollup_message_repr.t ->
    Tx_rollup_inbox_repr.Merkle.path ->
    Raw_context.t tzresult Lwt.t =
 fun ctxt level tx_rollup ~position message path ->
  Storage.Tx_rollup.Inbox.get (ctxt, tx_rollup) level >>=? fun (ctxt, inbox) ->
  Tx_rollup_hash_builder.message ctxt message >>?= fun (ctxt, message_hash) ->
  Tx_rollup_gas.consume_check_path_inbox_cost ctxt >>?= fun ctxt ->
  Tx_rollup_inbox_repr.Merkle.check_path
    path
    position
    message_hash
    inbox.merkle_root
  >>?= fun b ->
  fail_unless b (Wrong_message_path {expected = inbox.merkle_root})
  >>=? fun () -> return ctxt
