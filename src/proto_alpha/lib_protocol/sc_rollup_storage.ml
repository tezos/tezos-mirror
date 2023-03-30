(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [new_address ctxt] produces an address completely
    determined by an operation hash and an origination counter, and
    accounts for gas spent. *)
let new_address ctxt =
  let open Result_syntax in
  let* ctxt, nonce = Raw_context.increment_origination_nonce ctxt in
  let* ctxt =
    Raw_context.consume_gas ctxt Sc_rollup_costs.Constants.cost_serialize_nonce
  in
  match Data_encoding.Binary.to_bytes_opt Origination_nonce.encoding nonce with
  | None -> error Sc_rollup_address_generation
  | Some nonce_bytes ->
      let bytes_len = Bytes.length nonce_bytes in
      let+ ctxt =
        Raw_context.consume_gas
          ctxt
          (Sc_rollup_costs.cost_hash_bytes ~bytes_len)
      in
      (ctxt, Sc_rollup_repr.Address.hash_bytes [nonce_bytes])

let originate ctxt ~kind ~parameters_ty ~genesis_commitment =
  let open Lwt_result_syntax in
  let*? ctxt, genesis_commitment_hash =
    Sc_rollup_commitment_storage.hash ctxt genesis_commitment
  in
  let*? ctxt, address = new_address ctxt in
  let* ctxt, pvm_kind_size = Store.PVM_kind.init ctxt address kind in
  let origination_level = (Raw_context.current_level ctxt).level in
  let* ctxt, genesis_info_size =
    Store.Genesis_info.init
      ctxt
      address
      {commitment_hash = genesis_commitment_hash; level = origination_level}
  in
  let* ctxt, param_ty_size_diff =
    Store.Parameters_type.init ctxt address parameters_ty
  in
  let* ctxt = Sc_rollup_staker_index_storage.init ctxt address in
  let* ctxt, lcc_size_diff =
    Store.Last_cemented_commitment.init ctxt address genesis_commitment_hash
  in
  let* ctxt, commitment_size_diff =
    Store.Commitments.init
      (ctxt, address)
      genesis_commitment_hash
      genesis_commitment
  in
  (* Those stores [Store.Commitment_added] and [Store.Commitment_stake_count]
     are going to be used to look this bootstrap commitment.
     This commitment is added here so the
     [sc_rollup_state_storage.deallocate] function does not have to handle a
     edge case.
  *)
  let* ctxt, commitment_added_size_diff =
    Store.Commitment_added.init
      (ctxt, address)
      genesis_commitment_hash
      origination_level
  in
  (* Those stores [Store.Commitment_first_publication_level] and
     [Store.Commitment_count_per_inbox_level] are populated with dummy values,
     in order the [sc_rollup_state_storage.deallocate_commitment_metadata]
     function does not have to handle an edge case of genesis commitment hash.
  *)
  let* ctxt, commitment_first_publication_level_diff =
    Store.Commitment_first_publication_level.init
      (ctxt, address)
      origination_level
      origination_level
  in
  let* ctxt, commitments_per_inbox_level_diff =
    Store.Commitments_per_inbox_level.init
      (ctxt, address)
      origination_level
      [genesis_commitment_hash]
  in
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let size =
    Z.of_int
      (origination_size + stored_kind_size + addresses_size + lcc_size_diff
     + commitment_size_diff + commitment_added_size_diff
     + commitment_first_publication_level_diff + param_ty_size_diff
     + pvm_kind_size + genesis_info_size + commitments_per_inbox_level_diff)
  in
  return (address, size, genesis_commitment_hash, ctxt)

let kind ctxt address =
  let open Lwt_result_syntax in
  let* ctxt, kind_opt = Store.PVM_kind.find ctxt address in
  match kind_opt with
  | Some k -> return (ctxt, k)
  | None -> tzfail (Sc_rollup_errors.Sc_rollup_does_not_exist address)

let list_unaccounted ctxt =
  let open Lwt_syntax in
  let+ res = Store.PVM_kind.keys_unaccounted ctxt in
  Result.return res

let genesis_info ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, genesis_info = Store.Genesis_info.find ctxt rollup in
  match genesis_info with
  | None -> tzfail (Sc_rollup_does_not_exist rollup)
  | Some genesis_info -> return (ctxt, genesis_info)

let get_metadata ctxt rollup =
  let open Lwt_result_syntax in
  let* ctxt, genesis_info = genesis_info ctxt rollup in
  let metadata : Sc_rollup_metadata_repr.t =
    {address = rollup; origination_level = genesis_info.level}
  in
  return (ctxt, metadata)

let parameters_type ctxt rollup =
  let open Lwt_result_syntax in
  let+ ctxt, res = Store.Parameters_type.find ctxt rollup in
  (res, ctxt)

let must_exist ctxt rollup =
  let open Lwt_result_syntax in
  let+ ctxt, _info = genesis_info ctxt rollup in
  ctxt
