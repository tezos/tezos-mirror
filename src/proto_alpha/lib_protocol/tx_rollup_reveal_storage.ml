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

let record ctxt tx_rollup state level ~message_position =
  Storage.Tx_rollup.Revealed_withdrawals.find (ctxt, level) tx_rollup
  >>=? fun (ctxt, revealed_withdrawals_opt) ->
  Bitset.add
    (Option.value ~default:Bitset.empty revealed_withdrawals_opt)
    message_position
  >>?= fun revealed_withdrawals ->
  Storage.Tx_rollup.Revealed_withdrawals.add
    (ctxt, level)
    tx_rollup
    revealed_withdrawals
  >>=? fun (ctxt, new_size, _is_new) ->
  Tx_rollup_state_repr.adjust_storage_allocation
    state
    ~delta:(Z.of_int new_size)
  >>?= fun (state, diff) -> return (ctxt, state, diff)

let mem ctxt tx_rollup level ~message_position =
  Storage.Tx_rollup.Revealed_withdrawals.find (ctxt, level) tx_rollup
  >>=? fun (ctxt, revealed_withdrawals_opt) ->
  match revealed_withdrawals_opt with
  | Some field ->
      Bitset.mem field message_position >>?= fun res -> return (ctxt, res)
  | None -> return (ctxt, false)

let remove ctxt tx_rollup state level =
  Storage.Tx_rollup.Revealed_withdrawals.remove (ctxt, level) tx_rollup
  >>=? fun (ctxt, freed_size, _existed) ->
  Tx_rollup_state_repr.adjust_storage_allocation
    state
    ~delta:Z.(neg @@ of_int freed_size)
  >>?= fun (state, _) -> return (ctxt, state)
