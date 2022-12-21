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
      in {!Sc_rollup_storage.originate} .*)
  let* staker_index =
    Storage.Sc_rollup.Staker_index_counter.get (ctxt, rollup)
  in
  let* ctxt =
    Storage.Sc_rollup.Staker_index_counter.update
      (ctxt, rollup)
      (Sc_rollup_staker_index_repr.succ staker_index)
  in
  let* ctxt, _size =
    Storage.Sc_rollup.Staker_index.init (ctxt, rollup) staker staker_index
  in
  let* ctxt, _size_diff =
    Storage.Sc_rollup.Stakers.init
      (ctxt, rollup)
      staker_index
      Raw_level_repr.root
  in
  return (ctxt, staker_index)

let find_staker_index_unsafe ctxt rollup staker =
  Storage.Sc_rollup.Staker_index.find (ctxt, rollup) staker

let get_staker_index_unsafe ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, staker_index_opt = find_staker_index_unsafe ctxt rollup staker in
  match staker_index_opt with
  | None -> tzfail Sc_rollup_not_staked
  | Some staker_index -> return (ctxt, staker_index)

let remove_staker ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, staker_index = get_staker_index_unsafe ctxt rollup staker in
  let* ctxt, _size_diff =
    Storage.Sc_rollup.Staker_index.remove_existing (ctxt, rollup) staker
  in
  let* ctxt, _size_diff =
    Storage.Sc_rollup.Stakers.remove_existing (ctxt, rollup) staker_index
  in
  return ctxt

let list_stakers_uncarbonated ctxt rollup =
  Storage.Sc_rollup.Staker_index.keys_unaccounted (ctxt, rollup)

let is_active ctxt rollup staker_index =
  Storage.Sc_rollup.Stakers.mem (ctxt, rollup) staker_index

let is_staker ctxt rollup staker =
  Storage.Sc_rollup.Staker_index.mem (ctxt, rollup) staker
