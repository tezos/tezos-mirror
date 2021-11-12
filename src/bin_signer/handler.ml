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
    let raw_hash = conv Blake2B.to_bytes Blake2B.of_bytes_exn bytes in
    conv
      (List.map (fun (chain_id, marks) ->
           (Chain_id.to_b58check chain_id, marks)))
      (List.map (fun (chain_id, marks) ->
           (Chain_id.of_b58check_exn chain_id, marks)))
    @@ assoc
    @@ conv
         (List.map (fun (pkh, mark) ->
              (Signature.Public_key_hash.to_b58check pkh, mark)))
         (List.map (fun (pkh, mark) ->
              (Signature.Public_key_hash.of_b58check_exn pkh, mark)))
    @@ assoc
    @@ obj4
         (req "level" int32)
         (opt "round" int32)
         (req "hash" raw_hash)
         (opt "signature" Signature.encoding)

  let get_level_and_round_for_tenderbake_block bytes =
    (* <watermark(1)><chain_id(4)><level(4)><proto_level(1)><predecessor(32)><timestamp(8)><validation_passes(1)><oph(32)><FITNESS>... *)
    (* FITNESS=<len(4)><version(1)><len(4)><level(4)><len(4)><locked_round(0 OR 4)><len(4)><predecessor_round(4)><len(4)><round(4)> *)
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

  let get_level_and_round_for_tenderbake_endorsement bytes =
    (* <watermark(1)><chain_id(4)><branch(32)><kind(1)><slot(2)><level(4)><round(4)>... *)
    try
      let level_offset = 1 + 4 + 32 + 1 + 2 in
      let level = Bytes.get_int32_be bytes level_offset in
      let round = Bytes.get_int32_be bytes (level_offset + 4) in
      return (level, Some round)
    with exn ->
      failwith
        "Failed to retrieve level and round of an endorsement or \
         preendorsement (%s)"
        (Printexc.to_string exn)

  let check_mark name
      (previous_level, previous_round_opt, previous_hash, previous_signature_opt)
      level round_opt hash =
    let round = Option.value ~default:0l round_opt in
    match (previous_round_opt, previous_signature_opt) with
    | (None, None) ->
        if previous_level >= level then
          failwith
            "%s level %ld not above high watermark %ld"
            name
            level
            previous_level
        else return_none
    | (None, Some signature) ->
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
    | (Some previous_round, None) ->
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
    | (Some previous_round, Some signature) ->
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

  let mark_if_block_or_endorsement (cctxt : #Client_context.wallet) pkh bytes
      sign =
    let mark art name get_level_and_round =
      let file = name ^ "_high_watermark" in
      cctxt#with_lock @@ fun () ->
      cctxt#load file ~default:[] encoding >>=? fun all ->
      if Bytes.length bytes < 9 then
        failwith "byte sequence too short to be %s %s" art name
      else
        let hash = Blake2B.hash_bytes [bytes] in
        let chain_id = Chain_id.of_bytes_exn (Bytes.sub bytes 1 4) in
        get_level_and_round () >>=? fun (level, round_opt) ->
        (match
           Option.bind
             (List.assoc_opt ~equal:Chain_id.equal chain_id all)
             (List.assoc_opt ~equal:Signature.Public_key_hash.equal pkh)
         with
        | None -> return_none
        | Some mark -> check_mark name mark level round_opt hash)
        >>=? function
        | Some signature -> return signature
        | None ->
            sign bytes >>=? fun signature ->
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
            cctxt#write file (update all) encoding >>=? fun () ->
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
          (* tenderbake preendorsement *)
          mark "a" "preendorsement" (fun () ->
              get_level_and_round_for_tenderbake_endorsement bytes)
      | 0x13 ->
          (* tenderbake endorsement *)
          mark "a" "endorsement" (fun () ->
              get_level_and_round_for_tenderbake_endorsement bytes)
      | _ -> sign bytes
end

module Authorized_key = Client_aliases.Alias (struct
  include Signature.Public_key

  let name = "authorized_key"

  let to_source s = return (to_b58check s)

  let of_source t = Lwt.return (of_b58check t)
end)

let check_magic_byte magic_bytes data =
  match magic_bytes with
  | None -> return_unit
  | Some magic_bytes ->
      let byte = TzEndian.get_uint8 data 0 in
      if Bytes.length data > 1 && List.mem ~equal:Int.equal byte magic_bytes
      then return_unit
      else failwith "magic byte 0x%02X not allowed" byte

let check_authorization cctxt pkh data require_auth signature =
  match (require_auth, signature) with
  | (false, _) -> return_unit
  | (true, None) -> failwith "missing authentication signature field"
  | (true, Some signature) ->
      let to_sign = Signer_messages.Sign.Request.to_sign ~pkh ~data in
      Authorized_key.load cctxt >>=? fun keys ->
      if
        List.exists (fun (_, key) -> Signature.check key signature to_sign) keys
      then return_unit
      else failwith "invalid authentication signature"

let sign ?magic_bytes ~check_high_watermark ~require_auth
    (cctxt : #Client_context.wallet)
    Signer_messages.Sign.Request.{pkh; data; signature} =
  Events.(emit request_for_signing)
    (Bytes.length data, pkh, TzEndian.get_uint8 data 0)
  >>= fun () ->
  check_magic_byte magic_bytes data >>=? fun () ->
  check_authorization cctxt pkh data require_auth signature >>=? fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  Events.(emit signing_data) name >>= fun () ->
  let sign = Client_keys.sign cctxt sk_uri in
  if check_high_watermark then
    High_watermark.mark_if_block_or_endorsement cctxt pkh data sign
  else sign data

let deterministic_nonce (cctxt : #Client_context.wallet)
    Signer_messages.Deterministic_nonce.Request.{pkh; data; signature}
    ~require_auth =
  Events.(emit request_for_deterministic_nonce) (Bytes.length data, pkh)
  >>= fun () ->
  check_authorization cctxt pkh data require_auth signature >>=? fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  Events.(emit creating_nonce) name >>= fun () ->
  Client_keys.deterministic_nonce sk_uri data

let deterministic_nonce_hash (cctxt : #Client_context.wallet)
    Signer_messages.Deterministic_nonce_hash.Request.{pkh; data; signature}
    ~require_auth =
  Events.(emit request_for_deterministic_nonce_hash) (Bytes.length data, pkh)
  >>= fun () ->
  check_authorization cctxt pkh data require_auth signature >>=? fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  Events.(emit creating_nonce_hash) name >>= fun () ->
  Client_keys.deterministic_nonce_hash sk_uri data

let supports_deterministic_nonces (cctxt : #Client_context.wallet) pkh =
  Events.(emit request_for_supports_deterministic_nonces) pkh >>= fun () ->
  Client_keys.get_key cctxt pkh >>=? fun (name, _pkh, sk_uri) ->
  Events.(emit supports_deterministic_nonces) name >>= fun () ->
  Client_keys.supports_deterministic_nonces sk_uri

let public_key (cctxt : #Client_context.wallet) pkh =
  Events.(emit request_for_public_key) pkh >>= fun () ->
  Client_keys.list_keys cctxt >>=? fun all_keys ->
  match
    List.find_opt
      (fun (_, h, _, _) -> Signature.Public_key_hash.equal h pkh)
      all_keys
  with
  | None | Some (_, _, None, _) ->
      Events.(emit not_found_public_key) pkh >>= fun () -> Lwt.fail Not_found
  | Some (name, _, Some pk, _) ->
      Events.(emit found_public_key) (pkh, name) >>= fun () -> return pk
