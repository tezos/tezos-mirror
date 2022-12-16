(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Sc_rollup_errors

let init ctxt rollup =
  Storage.Sc_rollup.Staker_index_counter.init
    (ctxt, rollup)
    Sc_rollup_staker_index_repr.zero

let fresh_staker_index ctxt rollup staker =
  let open Lwt_result_syntax in
  (* This is safe because this storage is initialized at the rollup creation
      wit {!init_storage} .*)
  let* staker_index =
    Storage.Sc_rollup.Staker_index_counter.get (ctxt, rollup)
  in
  let* ctxt =
    Storage.Sc_rollup.Staker_index_counter.update
      (ctxt, rollup)
      (Sc_rollup_staker_index_repr.succ staker_index)
  in
  let* ctxt, _size, _existed =
    Storage.Sc_rollup.Staker_index.add (ctxt, rollup) staker staker_index
  in
  let* ctxt, _size, _existed =
    Storage.Sc_rollup.Stakers_by_rollup.add (ctxt, rollup) staker_index
  in
  return (ctxt, staker_index)

let find_staker_index_unsafe ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, res = Storage.Sc_rollup.Staker_index.find (ctxt, rollup) staker in
  match res with
  | None -> tzfail Sc_rollup_not_staked
  | Some staker_index -> return (ctxt, staker_index)

let find_staker_index ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, res = Storage.Sc_rollup.Last_cemented_commitment.mem ctxt rollup in
  if not res then tzfail (Sc_rollup_does_not_exist rollup)
  else find_staker_index_unsafe ctxt rollup staker

let remove_staker ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, staker_index = find_staker_index ctxt rollup staker in

  let* ctxt, staker_index_size_diff =
    Storage.Sc_rollup.Staker_index.remove_existing (ctxt, rollup) staker
  in
  let* ctxt, stakers_by_rollup_size_diff, _existed =
    Storage.Sc_rollup.Stakers_by_rollup.remove (ctxt, rollup) staker_index
  in
  return (ctxt, staker_index_size_diff + stakers_by_rollup_size_diff)
