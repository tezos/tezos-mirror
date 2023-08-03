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

let init ctxt rollup_address ~whitelist =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (ctxt, size) e ->
      let* ctxt, size_e =
        (* the storage fails when there key already exists. This is
           only to improve the UX so that it returns a cleaner
           error. *)
        trace Sc_rollup_errors.Sc_rollup_duplicated_key_in_whitelist
        @@ Storage.Sc_rollup.Whitelist.init (ctxt, rollup_address) e
      in
      return (ctxt, size + size_e))
    (ctxt, 0)
    whitelist

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
