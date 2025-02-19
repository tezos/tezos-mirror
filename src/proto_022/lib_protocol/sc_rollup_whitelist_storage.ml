(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let is_private ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, rollup_is_public =
    Storage.Sc_rollup.Whitelist.is_empty (ctxt, rollup)
  in
  return (ctxt, not rollup_is_public)

let init_whitelist ctxt rollup_address ~whitelist =
  let open Lwt_result_syntax in
  let* ctxt, used_storage =
    List.fold_left_es
      (fun (ctxt, size) e ->
        let* ctxt, size_e =
          (* the storage fails when there key already exists. This is
             only to improve the UX so that it returns a cleaner
             error. *)
          trace Sc_rollup_errors.Sc_rollup_duplicated_key_in_whitelist
          @@ Storage.Sc_rollup.Whitelist.init (ctxt, rollup_address) e
        in
        return (ctxt, Z.add size (Z.of_int size_e)))
      (ctxt, Z.zero)
      whitelist
  in
  return (ctxt, used_storage)

let init ctxt rollup_address ~whitelist ~origination_level =
  let open Lwt_result_syntax in
  let* ctxt, used_storage = init_whitelist ctxt rollup_address ~whitelist in
  let* ctxt, _whitelist_update_storage =
    Storage.Sc_rollup.Last_whitelist_update.init
      ctxt
      rollup_address
      {outbox_level = origination_level; message_index = Z.zero}
  in
  return (ctxt, used_storage)

let check_access_to_private_rollup ctxt rollup staker =
  let open Lwt_result_syntax in
  let* ctxt, rollup_is_private = is_private ctxt rollup in
  if rollup_is_private then
    let* ctxt, staker_in_whitelist =
      Storage.Sc_rollup.Whitelist.mem (ctxt, rollup) staker
    in
    let* () =
      fail_when
        (not staker_in_whitelist)
        Sc_rollup_errors.Sc_rollup_staker_not_in_whitelist
    in
    return ctxt
  else return ctxt

let find_whitelist_uncarbonated ctxt rollup_address =
  let open Lwt_result_syntax in
  let* _, is_private = is_private ctxt rollup_address in
  if is_private then
    let*! elts =
      Storage.Sc_rollup.Whitelist.fold_keys_unaccounted
        (ctxt, rollup_address)
        ~order:`Sorted
        ~init:[]
        ~f:(fun pkh acc -> Lwt.return (pkh :: acc))
    in
    return (Some elts)
  else return None

let replace ctxt rollup ~whitelist =
  let open Lwt_result_syntax in
  let* ctxt = Storage.Sc_rollup.Whitelist.clear (ctxt, rollup) in
  init_whitelist ~whitelist ctxt rollup

let make_public ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt = Storage.Sc_rollup.Whitelist.clear (ctxt, rollup) in
  let* used_storage =
    Storage.Sc_rollup.Whitelist_used_storage_space.find ctxt rollup
  in
  let used_storage = Option.value ~default:Z.zero used_storage in
  let*! ctxt =
    Storage.Sc_rollup.Whitelist_used_storage_space.remove ctxt rollup
  in
  return (ctxt, used_storage)

let adjust_storage_space ctxt rollup ~new_storage_size =
  let open Lwt_result_syntax in
  let* used_storage =
    Storage.Sc_rollup.Whitelist_used_storage_space.find ctxt rollup
  in
  let used_storage = Option.value ~default:Z.zero used_storage in
  let storage_diff = Z.sub new_storage_size used_storage in
  if Compare.Z.(storage_diff = Z.zero) then return (ctxt, Z.zero)
  else
    let*! ctxt =
      Storage.Sc_rollup.Whitelist_used_storage_space.add
        ctxt
        rollup
        new_storage_size
    in
    let* paid_storage =
      Storage.Sc_rollup.Whitelist_paid_storage_space.find ctxt rollup
    in
    let paid_storage = Option.value ~default:Z.zero paid_storage in
    let diff = Z.sub new_storage_size paid_storage in
    if Compare.Z.(Z.zero < diff) then
      let*! ctxt =
        Storage.Sc_rollup.Whitelist_paid_storage_space.add
          ctxt
          rollup
          new_storage_size
      in
      return (ctxt, diff)
    else return (ctxt, Z.zero)

let get_last_whitelist_update = Storage.Sc_rollup.Last_whitelist_update.get

(** TODO: https://gitlab.com/tezos/tezos/-/issues/6186
    Do not consider storage diffs for small updates to the storage. *)
let set_last_whitelist_update ctxt rollup update =
  let open Lwt_result_syntax in
  let* ctxt, diff_size, _ =
    Storage.Sc_rollup.Last_whitelist_update.add ctxt rollup update
  in
  return (ctxt, Z.of_int diff_size)
