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
open Tx_rollup_withdraw_repr

let add :
    Raw_context.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_level_repr.t ->
    message_index:int ->
    withdraw_position:int ->
    (Raw_context.t * Tx_rollup_state_repr.t * Z.t) tzresult Lwt.t =
 fun ctxt state tx_rollup commitment_lvl ~message_index ~withdraw_position ->
  Storage.Tx_rollup.Consumed_withdraw.find
    ((ctxt, commitment_lvl), tx_rollup)
    (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2627

       inbox length is in int32 replace message_index by int32 *)
    (Int32.of_int message_index)
  >>=? fun (ctxt, consumed_withdraw_opt) ->
  Withdrawal_accounting.set
    (Option.value ~default:Withdrawal_accounting.empty consumed_withdraw_opt)
    withdraw_position
  >>?= fun consumed_withdraw ->
  Storage.Tx_rollup.Consumed_withdraw.add
    ((ctxt, commitment_lvl), tx_rollup)
    (Int32.of_int message_index)
    consumed_withdraw
  >>=? fun (ctxt, newly_alloacted_size, _is_new) ->
  Tx_rollup_state_repr.adjust_storage_allocation
    state
    ~delta:(Z.of_int newly_alloacted_size)
  >>?= fun (state, paid_storage_size_diff) ->
  return (ctxt, state, paid_storage_size_diff)

let mem :
    Raw_context.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_level_repr.t ->
    message_index:int ->
    withdraw_position:int ->
    (bool * Raw_context.t) tzresult Lwt.t =
 fun ctxt tx_rollup commitment_lvl ~message_index ~withdraw_position ->
  Storage.Tx_rollup.Consumed_withdraw.find
    ((ctxt, commitment_lvl), tx_rollup)
    (* TODO/TORU: https://gitlab.com/tezos/tezos/-/issues/2627

       inbox length is in int32 replace message_index by int32 *)
    (Int32.of_int message_index)
  >>=? fun (ctxt, consumed_withdraw_opt) ->
  Option.map_e
    (fun s -> Withdrawal_accounting.get s withdraw_position)
    consumed_withdraw_opt
  >>?= fun consumed_withdraw_opt ->
  let already_consumed = Option.value ~default:false consumed_withdraw_opt in
  return (already_consumed, ctxt)

let remove :
    Raw_context.t ->
    Tx_rollup_state_repr.t ->
    Tx_rollup_repr.t ->
    Tx_rollup_level_repr.t ->
    inbox_length:int32 ->
    (Raw_context.t * Tx_rollup_state_repr.t) tzresult Lwt.t =
 fun ctxt state rollup level ~inbox_length ->
  let rec remove_withdrawal_accounting ctxt state i len ~acc_freed_size =
    if Compare.Int32.(i < len) then
      Storage.Tx_rollup.Consumed_withdraw.remove ((ctxt, level), rollup) i
      >>=? fun (ctxt, freed_size, _) ->
      let acc_freed_size = acc_freed_size + freed_size in
      remove_withdrawal_accounting ctxt state (Int32.succ i) len ~acc_freed_size
    else return (ctxt, state, acc_freed_size)
  in
  (* for each message in the inbox, the storage contains one set of
     executed withdrawals that should be removed *)
  remove_withdrawal_accounting ctxt state 0l inbox_length ~acc_freed_size:0
  >>=? fun (ctxt, state, freed_size) ->
  (* while we free storage space and adjust storage
     allocation, the returned [_paid_storage_size_diff]
     is always 0. Therefore, we neglect
     [_paid_storage_size_diff]. *)
  Tx_rollup_state_repr.adjust_storage_allocation
    state
    ~delta:(Z.of_int freed_size |> Z.neg)
  >>?= fun (state, _paid_storage_size_diff) -> return (ctxt, state)
