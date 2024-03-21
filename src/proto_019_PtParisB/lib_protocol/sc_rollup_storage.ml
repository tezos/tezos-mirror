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
  | None -> tzfail Sc_rollup_address_generation
  | Some nonce_bytes ->
      let bytes_len = Bytes.length nonce_bytes in
      let+ ctxt =
        Raw_context.consume_gas
          ctxt
          (Sc_rollup_costs.cost_hash_bytes ~bytes_len)
      in
      (ctxt, Sc_rollup_repr.Address.hash_bytes [nonce_bytes])

let init_genesis_info ctxt address genesis_commitment =
  let open Lwt_result_syntax in
  let*? ctxt, commitment_hash =
    Sc_rollup_commitment_storage.hash ctxt genesis_commitment
  in
  (* The [genesis_commitment.inbox_level] is equal to the current level. *)
  let genesis_info : Commitment.genesis_info =
    {commitment_hash; level = genesis_commitment.inbox_level}
  in
  let* ctxt, size = Store.Genesis_info.init ctxt address genesis_info in
  return (ctxt, genesis_info, size)

let init_commitment_storage ctxt address
    ({commitment_hash = genesis_commitment_hash; level = origination_level} :
      Commitment.genesis_info) genesis_commitment =
  let open Lwt_result_syntax in
  let* ctxt, lcc_size =
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
  return
    ( ctxt,
      lcc_size + commitment_size_diff + commitment_added_size_diff
      + commitment_first_publication_level_diff
      + commitments_per_inbox_level_diff )

let check_whitelist ctxt whitelist =
  let open Result_syntax in
  match whitelist with
  | Some whitelist ->
      let private_enabled = Constants_storage.sc_rollup_private_enable ctxt in
      (* The whitelist must be None when the feature is deactivated. *)
      let* () =
        error_unless
          private_enabled
          Sc_rollup_errors.Sc_rollup_whitelist_disabled
      in
      (* The origination fails with an empty list. *)
      error_when
        (List.is_empty whitelist)
        Sc_rollup_errors.Sc_rollup_empty_whitelist
  | None -> Ok ()

let raw_originate ?whitelist ctxt ~kind ~parameters_ty ~genesis_commitment
    ~address =
  let open Lwt_result_syntax in
  let*? () = check_whitelist ctxt whitelist in
  let* ctxt, pvm_kind_size = Store.PVM_kind.init ctxt address kind in
  let* ctxt, param_ty_size =
    Store.Parameters_type.init ctxt address parameters_ty
  in
  let* ctxt = Sc_rollup_staker_index_storage.init ctxt address in
  let* ctxt, genesis_info, genesis_info_size_diff =
    init_genesis_info ctxt address genesis_commitment
  in
  let* ctxt, commitment_size_diff =
    init_commitment_storage ctxt address genesis_info genesis_commitment
  in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/4551
     There is no need to have both `origination_size` and the size of storage.
     We should remove one of them. *)
  let addresses_size = 2 * Sc_rollup_repr.Address.size in
  let stored_kind_size = 2 (* because tag_size of kind encoding is 16bits. *) in
  let origination_size = Constants_storage.sc_rollup_origination_size ctxt in
  let* ctxt, whitelist_size =
    let*? () = check_whitelist ctxt whitelist in
    match whitelist with
    | Some whitelist ->
        let* ctxt, new_storage_size =
          Sc_rollup_whitelist_storage.init
            ~whitelist
            ctxt
            address
            ~origination_level:genesis_info.level
        in
        Sc_rollup_whitelist_storage.adjust_storage_space
          ctxt
          address
          ~new_storage_size
    | None -> return (ctxt, Z.zero)
  in
  let size =
    Z.(
      add
        (of_int
           (origination_size + stored_kind_size + addresses_size + param_ty_size
          + pvm_kind_size + genesis_info_size_diff + commitment_size_diff))
        whitelist_size)
  in
  return (size, genesis_info.commitment_hash, ctxt)

let originate ?whitelist ctxt ~kind ~parameters_ty ~genesis_commitment =
  let open Lwt_result_syntax in
  let*? ctxt, address = new_address ctxt in
  let* size, genesis_commitment, ctxt =
    raw_originate
      ?whitelist
      ctxt
      ~kind
      ~parameters_ty
      ~genesis_commitment
      ~address
  in
  return (address, size, genesis_commitment, ctxt)

let kind ctxt address =
  let open Lwt_result_syntax in
  let* ctxt, kind_opt = Store.PVM_kind.find ctxt address in
  match kind_opt with
  | Some k -> return (ctxt, k)
  | None -> tzfail (Sc_rollup_errors.Sc_rollup_does_not_exist address)

let list_unaccounted ctxt =
  let open Lwt_result_syntax in
  let*! res = Store.PVM_kind.keys_unaccounted ctxt in
  return res

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
  let* ctxt, exists = Store.Genesis_info.mem ctxt rollup in
  if exists then return ctxt else tzfail (Sc_rollup_does_not_exist rollup)
