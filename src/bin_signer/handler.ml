(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
module Events = Signer_events.Handler

module High_watermark = struct
  let encoding =
    let open Data_encoding in
    let raw_hash =
      conv Tezos_crypto.Blake2B.to_bytes Tezos_crypto.Blake2B.of_bytes_exn bytes
    in
    conv
      (List.map (fun (chain_id, marks) ->
           (Chain_id.to_b58check chain_id, marks)))
      (List.map (fun (chain_id, marks) ->
           (Chain_id.of_b58check_exn chain_id, marks)))
    @@ assoc
    @@ conv
         (List.map (fun (pkh, mark) ->
              (Tezos_crypto.Signature.Public_key_hash.to_b58check pkh, mark)))
         (List.map (fun (pkh, mark) ->
              (Tezos_crypto.Signature.Public_key_hash.of_b58check_exn pkh, mark)))
    @@ assoc
    @@ obj4
         (req "level" int32)
         (opt "round" int32)
         (req "hash" raw_hash)
         (opt "signature" (dynamic_size Tezos_crypto.Signature.encoding))

  let get_level_and_round_for_tenderbake_block bytes =
    (* <watermark(1)><chain_id(4)><level(4)><proto_level(1)><predecessor(32)><timestamp(8)><validation_passes(1)><oph(32)><FITNESS>... *)
    (* FITNESS=<len(4)><version(1)><len(4)><level(4)><len(4)><locked_round(0 OR 4)><len(4)><predecessor_round(4)><len(4)><round(4)> *)
    let open Lwt_result_syntax in
    try
      let level = Bytes.get_int32_be bytes (1 + 4) in
      let fitness_offset = 1 + 4 + 4 + 1 + 32 + 8 + 1 + 32 in
      let fitness_length =
        Bytes.get_int32_be bytes fitness_offset |> Int32.to_int
      in
      let round =
        Bytes.get_int32_be bytes (fitness_offset + fitness_length + 4 - 4)
      in
      return (level, Some round)
    with exn ->
      failwith
        "Failed to retrieve level and round of a Tenderbake block: %s"
        (Printexc.to_string exn)

  let get_level_and_round_for_tenderbake_attestation
      (pkh : Signature.public_key_hash) bytes =
    (* <watermark(1)><chain_id(4)><branch(32)><kind(1)><slot(2)><level(4)><round(4)>... *)
    let open Lwt_result_syntax in
    try
      let level_offset =
        match pkh with
        | Bls _ ->
            (* Slot is not part of the signed payload when
               signing with a tz4 address *)
            1 + 4 + 32 + 1
        | Ed25519 _ | Secp256k1 _ | P256 _ -> 1 + 4 + 32 + 1 + 2
      in
      let level = Bytes.get_int32_be bytes level_offset in
      let round = Bytes.get_int32_be bytes (level_offset + 4) in
      return (level, Some round)
    with exn ->
      failwith
        "Failed to retrieve level and round of an attestation or \
         preattestation (%s)"
        (Printexc.to_string exn)

  let check_mark name
      (previous_level, previous_round_opt, previous_hash, previous_signature_opt)
      level round_opt hash =
    let open Lwt_result_syntax in
    let round = Option.value ~default:0l round_opt in
    match (previous_round_opt, previous_signature_opt) with
    | None, None ->
        if previous_level >= level then
          failwith
            "%s level %ld not above high watermark %ld"
            name
            level
            previous_level
        else return_none
    | None, Some signature ->
        if previous_level > level then
          failwith
            "%s level %ld below high watermark %ld"
            name
            level
            previous_level
        else if previous_level = level then
          if previous_hash <> hash then
            failwith
              "%s level %ld already signed with different data"
              name
              level
          else return_some signature
        else return_none
    | Some previous_round, None ->
        if previous_level > level then
          failwith
            "%s level %ld not above high watermark %ld"
            name
            level
            previous_level
        else if previous_level = level && previous_round >= round then
          failwith
            "%s level %ld and round %ld not above high watermark (%ld, %ld)"
            name
            level
            round
            previous_level
            previous_round
        else return_none
    | Some previous_round, Some signature ->
        if previous_level > level then
          failwith
            "%s level %ld below high watermark %ld"
            name
            level
            previous_level
        else if previous_level = level then
          if previous_round > round then
            failwith
              "%s level %ld and round %ld not above high watermark (%ld,%ld)"
              name
              level
              round
              previous_level
              previous_round
          else if previous_round = round then
            if previous_hash <> hash then
              failwith
                "%s level %ld and round %ld already signed with different data"
                name
                level
                round
            else return_some signature
          else return_none
        else return_none

  let mark_if_block_or_attestation (cctxt : #Client_context.wallet) pkh bytes
      sign =
    let open Lwt_result_syntax in
    let mark art name get_level_and_round =
      let file = name ^ "_high_watermark" in
      cctxt#with_lock @@ fun () ->
      let* all = cctxt#load file ~default:[] encoding in
      if Bytes.length bytes < 9 then
        failwith "byte sequence too short to be %s %s" art name
      else
        let hash = Tezos_crypto.Blake2B.hash_bytes [bytes] in
        let chain_id = Chain_id.of_bytes_exn (Bytes.sub bytes 1 4) in
        let* level, round_opt = get_level_and_round () in
        let* o =
          match
            Option.bind
              (List.assoc_opt ~equal:Chain_id.equal chain_id all)
              (List.assoc_opt
                 ~equal:Tezos_crypto.Signature.Public_key_hash.equal
                 pkh)
          with
          | None -> return_none
          | Some mark -> check_mark name mark level round_opt hash
        in
        match o with
        | Some signature -> return signature
        | None ->
            let* signature = sign bytes in
            let rec update = function
              | [] ->
                  [
                    (chain_id, [(pkh, (level, round_opt, hash, Some signature))]);
                  ]
              | (e_chain_id, marks) :: rest ->
                  if chain_id = e_chain_id then
                    let marks =
                      (pkh, (level, round_opt, hash, Some signature))
                      :: List.filter (fun (pkh', _) -> pkh <> pkh') marks
                    in
                    (e_chain_id, marks) :: rest
                  else (e_chain_id, marks) :: update rest
            in
            let* () = cctxt#write file (update all) encoding in
            return signature
    in
    if Bytes.length bytes = 0 then sign bytes
    else
      match TzEndian.get_uint8 bytes 0 with
      | 0x01 ->
          (* Emmy block *)
          mark "a" "block" (fun () -> return (TzEndian.get_int32 bytes 5, None))
      | 0x02 ->
          (* Emmy endorsement *)
          mark "an" "endorsement" (fun () ->
              return (TzEndian.get_int32 bytes (Bytes.length bytes - 4), None))
      | 0x11 ->
          mark "a" "block" (fun () ->
              (* tenderbake block *)
              get_level_and_round_for_tenderbake_block bytes)
      | 0x12 ->
          (* tenderbake preattestation *)
          mark "a" "preattestation" (fun () ->
              get_level_and_round_for_tenderbake_attestation pkh bytes)
      | 0x13 ->
          (* tenderbake attestation *)
          mark "an" "attestation" (fun () ->
              get_level_and_round_for_tenderbake_attestation pkh bytes)
      | _ -> sign bytes
end

module Authorized_key = Client_aliases.Alias (struct
  include Tezos_crypto.Signature.Public_key

  let name = "authorized_key"

  let to_source s = Lwt.return_ok (to_b58check s)

  let of_source t = Lwt.return (of_b58check t)
end)

let check_magic_byte name magic_bytes data =
  let open Lwt_result_syntax in
  match magic_bytes with
  | None -> return_unit
  | Some magic_bytes ->
      let byte = TzEndian.get_uint8 data 0 in
      if Bytes.length data < 1 then
        let failure = "can't sign empty data" in
        let*! () = Events.(emit signing_data_failure) (name, failure) in
        failwith "%s" failure
      else if List.mem ~equal:Int.equal byte magic_bytes then return_unit
      else
        let failure = Format.sprintf "magic byte 0x%02X not allowed" byte in
        let*! () = Events.(emit signing_data_failure) (name, failure) in
        failwith "%s" failure

let check_authorization cctxt pkh data require_auth signature =
  let open Lwt_result_syntax in
  match (require_auth, signature) with
  | false, _ -> return_unit
  | true, None -> failwith "missing authentication signature field"
  | true, Some signature ->
      let to_sign = Signer_messages.Sign.Request.to_sign ~pkh ~data in
      let* keys = Authorized_key.load cctxt in
      if
        List.exists
          (fun (_, key) -> Tezos_crypto.Signature.check key signature to_sign)
          keys
      then return_unit
      else failwith "invalid authentication signature"

let sign ?signing_version ?magic_bytes ~check_high_watermark ~require_auth
    (cctxt : #Client_context.wallet)
    Signer_messages.Sign.Request.{pkh; data; signature} =
  let open Lwt_result_syntax in
  let pkh, version =
    match pkh with
    | Pkh pkh -> (pkh, signing_version)
    | Pkh_with_version (pkh, req_version) ->
        ( pkh,
          match signing_version with
          | Some _v -> signing_version
          | None -> Some req_version )
  in
  let*! () =
    Events.(emit request_for_signing)
      (Bytes.length data, pkh, TzEndian.get_uint8 data 0)
  in
  let* name, _pkh, sk_uri = Client_keys.get_key cctxt pkh in
  let* () = check_magic_byte name magic_bytes data in
  let* () = check_authorization cctxt pkh data require_auth signature in
  let*! () = Events.(emit signing_data) name in
  let sign bytes =
    match version with
    | Some Version_0 ->
        let* s = Client_keys.V0.sign cctxt sk_uri bytes in
        return (Signature.V_latest.Of_V0.signature s)
    | Some Version_1 ->
        let* s = Client_keys.V1.sign cctxt sk_uri bytes in
        return (Signature.V_latest.Of_V1.signature s)
    | Some Version_2 ->
        let* s = Client_keys.V2.sign cctxt sk_uri bytes in
        return (Signature.V_latest.Of_V2.signature s)
    | Some Version_3 -> Client_keys.V3.sign cctxt sk_uri bytes
    | None -> Client_keys.V_latest.sign cctxt sk_uri bytes
  in
  if check_high_watermark then
    High_watermark.mark_if_block_or_attestation cctxt pkh data sign
  else sign data

let deterministic_nonce (cctxt : #Client_context.wallet)
    Signer_messages.Deterministic_nonce.Request.{pkh; data; signature}
    ~require_auth =
  let open Lwt_result_syntax in
  let*! () =
    Events.(emit request_for_deterministic_nonce) (Bytes.length data, pkh)
  in
  let* () = check_authorization cctxt pkh data require_auth signature in
  let* name, _pkh, sk_uri = Client_keys.get_key cctxt pkh in
  let*! () = Events.(emit creating_nonce) name in
  Client_keys.deterministic_nonce sk_uri data

let deterministic_nonce_hash (cctxt : #Client_context.wallet)
    Signer_messages.Deterministic_nonce_hash.Request.{pkh; data; signature}
    ~require_auth =
  let open Lwt_result_syntax in
  let*! () =
    Events.(emit request_for_deterministic_nonce_hash) (Bytes.length data, pkh)
  in
  let* () = check_authorization cctxt pkh data require_auth signature in
  let* name, _pkh, sk_uri = Client_keys.get_key cctxt pkh in
  let*! () = Events.(emit creating_nonce_hash) name in
  Client_keys.deterministic_nonce_hash sk_uri data

let supports_deterministic_nonces (cctxt : #Client_context.wallet) pkh =
  let open Lwt_result_syntax in
  let*! () = Events.(emit request_for_supports_deterministic_nonces) pkh in
  let* name, _pkh, sk_uri = Client_keys.get_key cctxt pkh in
  let*! () = Events.(emit supports_deterministic_nonces) name in
  Client_keys.supports_deterministic_nonces sk_uri

let public_key (cctxt : #Client_context.wallet) pkh =
  let open Lwt_result_syntax in
  let*! () = Events.(emit request_for_public_key) pkh in
  let* all_keys = Client_keys.list_keys cctxt in
  match
    List.find_opt
      (fun (_, h, _, _) -> Tezos_crypto.Signature.Public_key_hash.equal h pkh)
      all_keys
  with
  | None | Some (_, _, None, _) ->
      let*! () = Events.(emit not_found_public_key) pkh in
      Lwt.fail Not_found
  | Some (name, _, Some pk, _) ->
      let*! () = Events.(emit found_public_key) (pkh, name) in
      return pk

let known_keys (cctxt : #Client_context.wallet) =
  let open Lwt_result_syntax in
  let*! () = Events.(emit request_for_known_keys ()) in
  let+ all_keys = Client_keys.list_keys cctxt in
  List.map (fun (_, pkh, _, _) -> pkh) all_keys

let bls_prove_possession (cctxt : #Client_context.wallet) ?override_pk pkh =
  let open Lwt_result_syntax in
  let*! () = Events.(emit request_for_proof_of_possession pkh) in
  let* _name, _pkh, sk_uri = Client_keys.get_key cctxt pkh in
  Client_keys.bls_prove_possession cctxt ?override_pk sk_uri
