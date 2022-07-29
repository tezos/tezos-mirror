(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
module Store = Storage.Sc_rollup
module Commitment = Sc_rollup_commitment_repr
module Commitment_hash = Commitment.Hash

let get_commitment_unsafe ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  let* ctxt, res = Store.Commitments.find (ctxt, rollup) commitment in
  match res with
  | None -> fail (Sc_rollup_unknown_commitment commitment)
  | Some commitment -> return (commitment, ctxt)

let last_cemented_commitment ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* ctxt, res = Store.Last_cemented_commitment.find ctxt rollup in
  match res with
  | None -> fail (Sc_rollup_does_not_exist rollup)
  | Some lcc -> return (lcc, ctxt)

let get_commitment ctxt rollup commitment =
  let open Lwt_tzresult_syntax in
  (* Assert that a last cemented commitment exists. *)
  let* _lcc, ctxt = last_cemented_commitment ctxt rollup in
  get_commitment_unsafe ctxt rollup commitment

let last_cemented_commitment_hash_with_level ctxt rollup =
  let open Lwt_tzresult_syntax in
  let* commitment_hash, ctxt = last_cemented_commitment ctxt rollup in
  if Commitment_hash.(commitment_hash = zero) then
    let+ initial_level = Storage.Sc_rollup.Initial_level.get ctxt rollup in
    (commitment_hash, initial_level, ctxt)
  else
    let+ {inbox_level; _}, ctxt =
      get_commitment_unsafe ctxt rollup commitment_hash
    in
    (commitment_hash, inbox_level, ctxt)

let set_commitment_added ctxt rollup node new_value =
  let open Lwt_tzresult_syntax in
  let* ctxt, res = Store.Commitment_added.find (ctxt, rollup) node in
  match res with
  | Some old_value ->
      (* No need to re-add the read value *)
      return (0, old_value, ctxt)
  | None ->
      let* ctxt, size_diff, _was_bound =
        Store.Commitment_added.add (ctxt, rollup) node new_value
      in
      return (size_diff, new_value, ctxt)

let get_predecessor_unsafe ctxt rollup node =
  let open Lwt_tzresult_syntax in
  let* commitment, ctxt = get_commitment_unsafe ctxt rollup node in
  return (commitment.predecessor, ctxt)
