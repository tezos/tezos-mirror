(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let fresh_tx_rollup_from_current_nonce ctxt =
  Raw_context.increment_origination_nonce ctxt >|? fun (ctxt, nonce) ->
  (ctxt, Tx_rollup_repr.originated_tx_rollup nonce)

let originate ctxt =
  fresh_tx_rollup_from_current_nonce ctxt >>?= fun (ctxt, tx_rollup) ->
  Tx_rollup_state_storage.init ctxt tx_rollup >|=? fun ctxt -> (ctxt, tx_rollup)

let update_tx_rollups_at_block_finalization :
    Raw_context.t -> Raw_context.t tzresult Lwt.t =
 fun ctxt ->
  let (ctxt, rollups) = Raw_context.get_tx_rollup_with_messages ctxt in
  Tx_rollup_repr.Set.fold_es
    (fun tx_rollup ctxt ->
      Tx_rollup_state_storage.get ctxt tx_rollup >>=? fun (ctxt, state) ->
      match Tx_rollup_state_repr.head_level state with
      | Some (tx_level, _) ->
          Tx_rollup_inbox_storage.get ctxt tx_level tx_rollup
          >>=? fun (ctxt, inbox) ->
          let hard_limit =
            Constants_storage.tx_rollup_hard_size_limit_per_inbox ctxt
          in
          let state =
            Tx_rollup_state_repr.update_burn_per_byte
              state
              ~final_size:inbox.cumulated_size
              ~hard_limit
          in
          Storage.Tx_rollup.State.add ctxt tx_rollup state
          >|=? fun (ctxt, _, _) -> ctxt
      | None -> (* this cannot happen *) return ctxt)
    rollups
    ctxt
