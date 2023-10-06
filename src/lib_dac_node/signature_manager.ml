(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 Trili Tech  <contact@trili.tech>                  *)
(* Copyright (c) 2023 Marigold  <contact@marigold.dev>                       *)
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

module Aggregate_signature = Tezos_crypto.Aggregate_signature

type error +=
  | Cannot_convert_root_page_hash_to_bytes of string
  | Cannot_compute_aggregate_signature of string
  | Public_key_for_witness_not_available of int * string
  | Public_key_is_non_committee_member of Aggregate_signature.public_key_hash
  | Signature_verification_failed of
      (Aggregate_signature.public_key * Aggregate_signature.t * string)
  | Public_key_for_committee_member_not_available of
      Aggregate_signature.public_key_hash

let () =
  register_error_kind
    `Permanent
    ~id:"cannot_extract_root_page_hash_to_sign"
    ~title:"Cannot convert root hash page to byte sequence"
    ~description:"Cannot convert root hash page to byte sequence"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Cannot convert root hash page to byte sequence: %s"
        b58_hash)
    Data_encoding.(obj1 (req "hash" (string' Plain)))
    (function
      | Cannot_convert_root_page_hash_to_bytes b58_hash -> Some b58_hash
      | _ -> None)
    (fun b58_hash -> Cannot_convert_root_page_hash_to_bytes b58_hash) ;
  register_error_kind
    `Permanent
    ~id:"cannot_compute_root_hash_aggregate_signature"
    ~title:"Cannot compute aggregate signature of root page hash"
    ~description:"Cannot compute aggregate signature of root page hash"
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Cannot compute aggregate signature of root page hash: %s"
        b58_hash)
    Data_encoding.(obj1 (req "hash" (string' Plain)))
    (function
      | Cannot_compute_aggregate_signature b58_hash -> Some b58_hash | _ -> None)
    (fun b58_hash -> Cannot_compute_aggregate_signature b58_hash) ;
  register_error_kind
    `Permanent
    ~id:"public_key_of_witness_not_available"
    ~title:
      "Public key of witness committee member not available for verifying \
       signature"
    ~description:
      "Public key of witness committee member not available for verifying \
       signature"
    ~pp:(fun ppf (witness_index, b58_hash) ->
      Format.fprintf
        ppf
        "Public key of committee member %d not available for verifying \
         signature of root page hash %s"
        witness_index
        b58_hash)
    Data_encoding.(
      obj2 (req "witness_index" int31) (req "hash" (string' Plain)))
    (function
      | Public_key_for_witness_not_available (index, hash) -> Some (index, hash)
      | _ -> None)
    (fun (index, hash) -> Public_key_for_witness_not_available (index, hash)) ;
  register_error_kind
    `Permanent
    ~id:"Public_key_is_non_committee_member"
    ~title:"Public key hash is not a committee member"
    ~description:
      "Public key hash is not associated with any committee member registered \
       with this Dac coordinator."
    ~pp:(fun ppf committee_member_pkh ->
      Format.fprintf
        ppf
        "Expected public key to be an active committee member but was not: %s"
        (Tezos_crypto.Aggregate_signature.Public_key_hash.to_short_b58check
           committee_member_pkh))
    Data_encoding.(
      obj1
        (req
           "public_key_hash"
           Tezos_crypto.Aggregate_signature.Public_key_hash.encoding))
    (function
      | Public_key_is_non_committee_member committee_member_pkh ->
          Some committee_member_pkh
      | _ -> None)
    (fun committee_member_pkh ->
      Public_key_is_non_committee_member committee_member_pkh) ;
  register_error_kind
    `Permanent
    ~id:"signature_verification_failed"
    ~title:"Signature verification failed"
    ~description:"Signature verification failed."
    ~pp:(fun ppf (pk, signature, root_hash) ->
      let pk = Aggregate_signature.Public_key.to_short_b58check pk in
      let signature = Aggregate_signature.to_short_b58check signature in
      Format.fprintf
        ppf
        "Could not verify signature \"%s\" given public key \"%s\" and \
         root_hash \"%s\": Signature verification failed."
        pk
        signature
        root_hash)
    Data_encoding.(
      obj3
        (req "public_key" Aggregate_signature.Public_key.encoding)
        (req "signature" Aggregate_signature.encoding)
        (req "root_hash" (string' Plain)))
    (function
      | Signature_verification_failed (pk, signature, root_hash) ->
          Some (pk, signature, root_hash)
      | _ -> None)
    (function
      | pk, signature, root_hash ->
          Signature_verification_failed (pk, signature, root_hash)) ;
  register_error_kind
    `Permanent
    ~id:"public_key_of_committee_member_not_available"
    ~title:"Public key of committee member not available."
    ~description:"Public key of committee member not available."
    ~pp:(fun ppf b58_hash ->
      Format.fprintf
        ppf
        "Public key of committee member %s not available."
        b58_hash)
    Data_encoding.(obj1 (req "hash" (string' Plain)))
    (function
      | Public_key_for_committee_member_not_available hash ->
          Some (Aggregate_signature.Public_key_hash.to_b58check hash)
      | _ -> None)
    (fun hash ->
      Public_key_for_committee_member_not_available
        (Aggregate_signature.Public_key_hash.of_b58check_exn hash))

(* TODO: https://gitlab.com/tezos/tezos/-/issues/5578
     Check that computed witness field is correct with respect to signatures.
*)
let compute_signatures_with_witnesses rev_indexed_signatures =
  let open Lwt_result_syntax in
  List.fold_left_es
    (fun (signatures, witnesses) signature_opt ->
      match signature_opt with
      | None -> return (signatures, witnesses)
      | Some (index, signature) ->
          let bit = Z.shift_left Z.one index in
          let witnesses = Z.logor witnesses bit in
          return (signature :: signatures, witnesses))
    ([], Z.zero)
    rev_indexed_signatures

let verify_signature ((module Plugin) : Dac_plugin.t) pk signature root_hash =
  let root_hash_bytes = Dac_plugin.hash_to_bytes root_hash in
  fail_unless
    (Aggregate_signature.check pk signature root_hash_bytes)
    (Signature_verification_failed (pk, signature, Plugin.to_hex root_hash))

let add_dac_member_signature dac_plugin signature_store signature =
  let open Lwt_result_syntax in
  let*? root_hash =
    Dac_plugin.raw_to_hash dac_plugin (Signature_repr.get_root_hash signature)
  in
  Store.Signature_store.add
    signature_store
    ~primary_key:root_hash
    ~secondary_key:(Signature_repr.get_signer_pkh signature)
    (Signature_repr.get_signature signature)

let rev_find_indexed_signatures node_store dac_members_pkh root_hash =
  let open Lwt_result_syntax in
  List.rev_mapi_es
    (fun index dac_member_pk ->
      let+ (signature_opt : Aggregate_signature.t option) =
        Store.Signature_store.find
          node_store
          ~primary_key:root_hash
          ~secondary_key:dac_member_pk
      in
      Option.map (fun signature -> (index, signature)) signature_opt)
    dac_members_pkh

let update_aggregate_sig_store node_store dac_members_pk_opt root_hash =
  let open Lwt_result_syntax in
  let* rev_indexed_signature =
    rev_find_indexed_signatures node_store dac_members_pk_opt root_hash
  in
  let* signatures, witnesses =
    compute_signatures_with_witnesses rev_indexed_signature
  in
  let raw_root_hash = Dac_plugin.hash_to_raw root_hash in
  let final_signature =
    Tezos_crypto.Aggregate_signature.aggregate_signature_opt signatures
  in
  match final_signature with
  | None ->
      tzfail
      @@ Cannot_compute_aggregate_signature
           (Hex.show @@ Dac_plugin.hash_to_hex root_hash)
  | Some aggregate_signature ->
      let* () =
        Store.Certificate_store.add
          node_store
          raw_root_hash
          Certificate_repr.(
            V0 (V0.make raw_root_hash aggregate_signature witnesses))
      in
      return @@ (aggregate_signature, witnesses)

let check_dac_member_has_signed signature_store root_hash dac_member_pkh =
  Store.Signature_store.mem
    signature_store
    ~primary_key:root_hash
    ~secondary_key:dac_member_pkh

let check_is_dac_member dac_committee signer_pkh =
  Option.is_some
  @@ List.find
       (fun pkh -> Aggregate_signature.Public_key_hash.equal signer_pkh pkh)
       dac_committee

let check_coordinator_knows_root_hash dac_plugin page_store root_hash =
  let open Lwt_result_syntax in
  let ((module Plugin) : Dac_plugin.t) = dac_plugin in
  let*! has_payload =
    Page_store.Filesystem.mem (module Plugin) page_store root_hash
  in
  match has_payload with
  | Error _ ->
      tzfail
      @@ Page_store.Cannot_read_page_from_page_storage (Plugin.to_hex root_hash)
  (* Return an HTTP 404 error when hash provided in signature is unknown *)
  | Ok false -> raise Not_found
  | Ok true -> return_unit

let should_update_certificate dac_plugin get_public_key_opt ro_node_store
    committee_members signature =
  let open Lwt_result_syntax in
  let ((module Plugin) : Dac_plugin.t) = dac_plugin in
  let*? root_hash =
    Dac_plugin.raw_to_hash dac_plugin (Signature_repr.get_root_hash signature)
  in
  let signer_pkh = Signature_repr.get_signer_pkh signature in
  let* () =
    fail_unless
      (check_is_dac_member committee_members signer_pkh)
      (Public_key_is_non_committee_member signer_pkh)
  in
  let pub_key_opt = get_public_key_opt signer_pkh in
  let* pub_key =
    Option.fold_f
      ~none:(fun () ->
        tzfail (Public_key_for_committee_member_not_available signer_pkh))
      ~some:return
      pub_key_opt
  in
  let* dac_member_has_signed =
    check_dac_member_has_signed ro_node_store root_hash signer_pkh
  in
  if dac_member_has_signed then return_false
  else
    let* () =
      verify_signature
        dac_plugin
        pub_key
        (Signature_repr.get_signature signature)
        root_hash
    in
    return_true

let stream_certificate_update dac_plugin committee_members certificate
    certificate_streamers =
  let root_hash = Certificate_repr.get_root_hash certificate in
  let open Result_syntax in
  let* () =
    Certificate_streamers.push
      dac_plugin
      certificate_streamers
      root_hash
      certificate
  in
  if
    Certificate_repr.all_committee_members_have_signed
      committee_members
      certificate
  then
    let _ =
      Certificate_streamers.close dac_plugin certificate_streamers root_hash
    in
    return_unit
  else return_unit

let handle_put_dac_member_signature dac_plugin get_public_key_opt
    certificate_streamers_opt rw_node_store page_store committee_members
    committee_member_signature =
  let open Lwt_result_syntax in
  let ((module Plugin) : Dac_plugin.t) = dac_plugin in
  let raw_root_hash = Signature_repr.get_root_hash committee_member_signature in
  let*? root_hash = Dac_plugin.raw_to_hash dac_plugin raw_root_hash in
  let* () = check_coordinator_knows_root_hash dac_plugin page_store root_hash in
  let* should_update_certificate =
    should_update_certificate
      dac_plugin
      get_public_key_opt
      rw_node_store
      committee_members
      committee_member_signature
  in
  if should_update_certificate then
    let* () =
      add_dac_member_signature
        dac_plugin
        rw_node_store
        committee_member_signature
    in
    let* aggregate_signature, witnesses =
      update_aggregate_sig_store rw_node_store committee_members root_hash
    in
    let*? () =
      Option.iter_e
        (stream_certificate_update
           dac_plugin
           committee_members
           Certificate_repr.(
             V0 (V0.make raw_root_hash aggregate_signature witnesses)))
        certificate_streamers_opt
    in
    return_unit
  else return_unit

module Coordinator = struct
  let handle_put_dac_member_signature ctx dac_plugin rw_node_store page_store
      dac_member_signature =
    let committee_members = Node_context.Coordinator.committee_members ctx in
    let get_public_key_opt committee_member_address =
      List.find_map
        (fun Wallet_account.Coordinator.{public_key_hash; public_key} ->
          if
            Tezos_crypto.Aggregate_signature.Public_key_hash.(
              committee_member_address <> public_key_hash)
          then None
          else Some public_key)
        ctx.committee_members
    in
    handle_put_dac_member_signature
      dac_plugin
      get_public_key_opt
      (Some ctx.certificate_streamers)
      rw_node_store
      page_store
      committee_members
      dac_member_signature
end
