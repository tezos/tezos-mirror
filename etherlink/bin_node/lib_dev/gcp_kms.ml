(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  pool : Octez_connpool.t;
  key_path : string;
  gcp_key : Configuration.gcp_key;
}

let service_name = "Gcp_kms"

let kms_uri = Uri.of_string "https://cloudkms.googleapis.com/"

(* TODO: Support auth token refresh *)
let auth () =
  let open Result_syntax in
  match Sys.getenv "EVM_NODE_GCP_KMS_ACCESS_TOKEN" with
  | (exception Not_found) | "" ->
      error_with
        "Environment variable ACCESS_TOKEN must be set for signing with GCP KMS"
  | s -> return s

let from_gcp_key gcp_key =
  let open Lwt_syntax in
  let kms_handler =
    {
      pool = Octez_connpool.make ~n:4 kms_uri;
      key_path =
        Format.sprintf
          "v1/projects/%s/locations/%s/keyRings/%s/cryptoKeys/%s/cryptoKeyVersions/%d"
          gcp_key.Configuration.project
          gcp_key.region
          gcp_key.keyring
          gcp_key.key
          gcp_key.version;
      gcp_key;
    }
  in
  Lwt.dont_wait (fun () -> Octez_connpool.warm kms_handler.pool) ignore ;
  return kms_handler

let gcp_key k = k.gcp_key

(** Routes for GCP KMS, see
    https://cloud.google.com/kms/docs/reference/rest#rest-resource:-v1.projects.locations.keyrings.cryptokeys.cryptokeyversions
    for reference. *)
module Route = struct
  let asymmetric_sign t = t.key_path ^ ":asymmetricSign"

  let public_key t = t.key_path ^ "/publicKey"
end

let request_header () =
  let open Result_syntax in
  let* access_token = auth () in
  return
  @@ Cohttp.Header.of_list
       [
         ("authorization", Format.sprintf "Bearer %s" access_token);
         ("content-type", "application/json");
         ("accept", "application/json");
       ]

let get_public_key kms_handler =
  let open Lwt_result_syntax in
  let*? headers = request_header () in
  let* response, body_str =
    Octez_connpool.get ~headers kms_handler.pool (Route.public_key kms_handler)
  in
  if Cohttp.Response.status response = `OK then
    match Ezjsonm.value_from_string_result body_str with
    | Ok json -> (
        match Ezjsonm.find_opt json ["pem"] with
        | Some (`String pem) -> (
            match X509.Public_key.decode_pem pem with
            | Ok pem -> return pem
            | Error (`Msg err) ->
                failwith "Could not decode pem public key (%s)" err)
        | _ ->
            failwith
              "Could not fetch the public key from the KMS response (%s)"
              (Ezjsonm.value_to_string ~minify:true json))
    | Error err ->
        failwith
          "Could not parse response from KMS (%s)"
          (Ezjsonm.read_error_description err)
  else
    failwith
      "Could not retreive the public key from the KMS (%a)"
      Cohttp.Response.pp_hum
      response

let extract_tail der_str n =
  let len = String.length der_str in
  if len < n then error_with "input is too short"
  else Result.return (String.sub der_str (len - n) n)

let public_key kms_handler =
  let open Lwt_result_syntax in
  let* pem = get_public_key kms_handler in
  let key_str = X509.Public_key.encode_der pem in
  let*? key_str = extract_tail key_str 65 in
  match
    Signature.Public_key.of_bytes_without_validation
      (Bytes.unsafe_of_string (Format.sprintf "\x02%s" key_str))
  with
  | Some res -> return res
  | _ -> failwith "Not a valid public key"

let ecdsa_sig =
  Asn.S.(
    sequence2
      (required ~label:"r" unsigned_integer)
      (required ~label:"s" unsigned_integer))

let pad32 s =
  if String.length s < 32 then Ok (String.make (32 - String.length s) '\x00' ^ s)
  else if String.length s = 32 then Ok s
  else error_with "Unexpected length for r or s: %d" (String.length s)

let digest payload =
  Tezos_crypto.(
    Base64.encode_string @@ Blake2B.to_string @@ Blake2B.hash_bytes [payload])

let sign_rpc kms_handler digest =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.with_tzresult
    ~service_name
    (Format.sprintf "POST %s" kms_handler.key_path)
  @@ fun _ ->
  let* response, body =
    let*? headers = request_header () in
    Octez_connpool.post
      ~headers
      ~body:
        (Cohttp_lwt.Body.of_string
           (Format.sprintf {|{"digest": {"sha256": "%s"}}|} digest))
      kms_handler.pool
      (Route.asymmetric_sign kms_handler)
  in
  if Cohttp.Response.status response = `OK then
    match Ezjsonm.value_from_string_result body with
    | Ok json -> (
        match Ezjsonm.find_opt json ["signature"] with
        | Some (`String b64_sig) -> return b64_sig
        | _ ->
            failwith
              "Cannot use the json returned by the KMS (%s)"
              (Ezjsonm.value_to_string ~minify:true json))
    | Error err ->
        failwith
          "Could not decode the response of the kms (%s)"
          (Ezjsonm.read_error_description err)
  else failwith "KMS failed %a" Cohttp.Response.pp_hum response

let signature_of_b64encoded b64_sig =
  let open Lwt_result_syntax in
  let* bytes_sig =
    match Base64.decode b64_sig with
    | Ok signature -> return signature
    | Error (`Msg err) -> failwith "Could not decode the signature (%s)" err
  in
  let* signature =
    match Asn.(decode (codec ber ecdsa_sig) bytes_sig) with
    | Ok ((r, s), "") ->
        let*? r = pad32 r in
        let*? s = pad32 s in
        return (r ^ s)
    | Ok (_, rst) ->
        failwith "Decoding did not consume the full signature (left %S)" rst
    | Error e -> failwith "ASN.1 parse error: %a" Asn.pp_error e
  in
  let*? p256_sig = Signature.P256.of_string signature in
  return (Signature.of_p256 p256_sig)

let sign kms_handler payload =
  let open Lwt_result_syntax in
  let digest = digest payload in
  let* b64_sig = sign_rpc kms_handler digest in
  signature_of_b64encoded b64_sig
