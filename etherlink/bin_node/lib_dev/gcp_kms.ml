(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type 'a handler = {
  pool : Octez_connpool.t;
  key_path : string;
  config : Configuration.gcp_kms;
  gcp_key : Configuration.gcp_key;
  public_key : 'a;
  mutable auth_token : string;
}

(* A partial KMS handler can already interact with the service,
   but is lacking some information related to the nature of the key being held
   by the KMS (meaning it is not yet possible to sign). *)
type partial = unit handler

(* A handler with all the necessary information to perform signing requests and
   interpret their responses. *)
type t = Signature.public_key handler

type signature_algorithm = EC_SIGN_P256_SHA256 | EC_SIGN_SECP256K1_SHA256

let signature_algorithm (kms : t) =
  match kms.public_key with
  | P256 _ -> EC_SIGN_P256_SHA256
  | Secp256k1 _ -> EC_SIGN_SECP256K1_SHA256
  | _ -> assert false

type hash_algorithm = Blake2B | Keccak256

let signature_algorithm_of_string =
  let open Result_syntax in
  function
  | "EC_SIGN_P256_SHA256" -> return EC_SIGN_P256_SHA256
  | "EC_SIGN_SECP256K1_SHA256" -> return EC_SIGN_SECP256K1_SHA256
  | algo -> tzfail (error_of_fmt "Unsupported algorithm %s" algo)

let service_name = "Gcp_kms"

let kms_uri = Uri.of_string "https://cloudkms.googleapis.com/"

let metadata_server_host = "metadata.google.internal"

let metadata_server_uri =
  Uri.make
    ~scheme:"http"
    ~host:metadata_server_host
    ~path:"/computeMetadata/v1/instance/service-accounts/default/token"
    ()

let run_without_error cmd args =
  let open Lwt_syntax in
  let+ status =
    Tezos_stdlib_unix.Process_manager.with_process_full
      (cmd, Array.of_list (cmd :: args))
      (fun pc -> pc#status)
  in
  match status with Unix.WEXITED 0 -> true | _ -> false

let rec retry ~timeout ~backoff ~on_error remaining_count k =
  let open Lwt_syntax in
  if remaining_count > 0 then
    let* try_result = protect (fun () -> Misc.with_timeout timeout k) in
    match try_result with
    | Error err ->
        let* () = on_error err in
        let* () = Lwt_unix.sleep (float_of_int backoff) in
        retry ~timeout ~backoff ~on_error (remaining_count - 1) k
    | Ok res -> return res
  else Lwt_exit.exit_and_raise Node_error.exit_code_when_gcp_kms_auth_error

let get_token_using_gcloud gcloud_path () =
  Tezos_stdlib_unix.Process_manager.with_process_full
    (gcloud_path, [|"gcloud"; "auth"; "print-access-token"; "--quiet"|])
  @@ fun pc ->
  let open Lwt_result_syntax in
  let*! status = pc#status in
  let* () =
    when_ (status <> WEXITED 0) @@ fun () ->
    match status with
    | WEXITED i -> failwith "gcloud exited with code %d" i
    | WSIGNALED i -> failwith "gcloud was kill by signal %d" i
    | WSTOPPED i -> failwith "gcloud was stopped by signal %d" i
  in
  Lwt_result.ok (Lwt_io.read_line pc#stdout)

let get_token_using_metadata_server () =
  let open Lwt_result_syntax in
  let*! resp, body =
    Cohttp_lwt_unix.Client.get
      ~headers:
        (Cohttp.Header.of_list
           [("Accept", "application/json"); ("Metadata-Flavor", "Google")])
      metadata_server_uri
  in
  let* () =
    when_ (Cohttp.Response.status resp <> `OK) @@ fun () ->
    failwith
      "Could not fetch a token from the Metadata server (HTTP code %d)"
      Cohttp.(Code.code_of_status @@ Response.status resp)
  in
  let*! body = Cohttp_lwt.Body.to_string body in
  match Ezjsonm.value_from_string_result body with
  | Ok json -> (
      match Ezjsonm.find_opt json ["access_token"] with
      | Some (`String access_token) -> return access_token
      | _ ->
          failwith
            "Could not fetch the key access_token from the Metadata response \
             (%s)"
            (Ezjsonm.value_to_string ~minify:true json))
  | Error err ->
      failwith
        "Could not parse response from Metadata server (%s)"
        (Ezjsonm.read_error_description err)

let get_token config =
  let open Lwt_syntax in
  let get =
    match config.Configuration.authentication_method with
    | Gcloud_auth ->
        fun () ->
          trace
            (error_of_fmt "Could not fetch a new access token using gcloud")
            (get_token_using_gcloud config.gcloud_path ())
    | Metadata_server ->
        fun () ->
          trace
            (error_of_fmt
               "Could not fetch a new access token using the metadata server")
            (get_token_using_metadata_server ())
  in
  let* token =
    retry
      ~timeout:config.authentication_timeout_sec
      ~backoff:config.authentication_retry_backoff_sec
      ~on_error:Gcp_kms_events.cannot_refresh_access_token
      config.Configuration.authentication_retries
      get
  in
  let* () = Gcp_kms_events.new_token () in
  return token

let rec wait_and_refresh config t =
  let open Lwt_syntax in
  let* () =
    Lwt_unix.sleep
      (float_of_int config.Configuration.authentication_frequency_min *. 60.)
  in
  let* token = get_token config in
  t.auth_token <- token ;
  wait_and_refresh config t

let gcp_key k = k.gcp_key

(** Routes for GCP KMS, see
    https://cloud.google.com/kms/docs/reference/rest#rest-resource:-v1.projects.locations.keyrings.cryptokeys.cryptokeyversions
    for reference. *)
module Route = struct
  let asymmetric_sign t = t.key_path ^ ":asymmetricSign"

  let public_key t = t.key_path ^ "/publicKey"
end

let request_header t =
  Cohttp.Header.of_list
    [
      ("authorization", Format.sprintf "Bearer %s" t.auth_token);
      ("content-type", "application/json");
      ("accept", "application/json");
    ]

let get_public_key kms_handler =
  let open Lwt_result_syntax in
  let headers = request_header kms_handler in
  let* response, body_str =
    Octez_connpool.get
      ~timeout:2.0
      ~headers
      kms_handler.pool
      (Route.public_key kms_handler)
  in
  if Cohttp.Response.status response = `OK then
    match Ezjsonm.value_from_string_result body_str with
    | Ok json -> (
        match
          (Ezjsonm.find_opt json ["algorithm"], Ezjsonm.find_opt json ["pem"])
        with
        | Some (`String algo), Some (`String pem) ->
            let*? t = signature_algorithm_of_string algo in
            return (t, pem)
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

let pem_to_der pem =
  let lines = String.split_on_char '\n' pem in
  let b64 =
    lines
    |> List.filter (fun l -> not (String.starts_with ~prefix:"-----" l))
    |> String.concat ""
  in
  Base64.decode_exn b64

let tag = function
  | EC_SIGN_SECP256K1_SHA256 -> '\x01'
  | EC_SIGN_P256_SHA256 -> '\x02'

let key_of_pem algo pem =
  let open Result_syntax in
  let der = pem_to_der pem in
  let* key_str = extract_tail der 65 in
  match
    Signature.Public_key.of_bytes_without_validation
      (Bytes.unsafe_of_string (Format.sprintf "%c%s" (tag algo) key_str))
  with
  | Some res -> return res
  | _ -> error_with "Not a valid public key"

let public_key kms_handler =
  let open Lwt_result_syntax in
  let* algo, pem = get_public_key kms_handler in
  let*? key = key_of_pem algo pem in
  return key

let ecdsa_sig =
  Asn.S.(
    sequence2
      (required ~label:"r" unsigned_integer)
      (required ~label:"s" unsigned_integer))

let pad32 s =
  if String.length s < 32 then Ok (String.make (32 - String.length s) '\x00' ^ s)
  else if String.length s = 32 then Ok s
  else error_with "Unexpected length for r or s: %d" (String.length s)

let digest hash payload =
  Base64.encode_string
  @@
  match hash with
  | Blake2B -> Tezos_crypto.(Blake2B.to_string @@ Blake2B.hash_bytes [payload])
  | Keccak256 -> Digestif.KECCAK_256.(to_raw_string @@ digest_bytes payload)

let rec sign_rpc ~allow_refresh kms_handler digest =
  let open Lwt_result_syntax in
  Octez_telemetry.Trace.with_tzresult
    ~service_name
    (Format.sprintf "POST %s" kms_handler.key_path)
  @@ fun _ ->
  let headers = request_header kms_handler in
  let* response, body =
    Octez_connpool.post
      ~timeout:2.0
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
  else if allow_refresh && Cohttp.Response.status response = `Unauthorized then (
    let*! () = Gcp_kms_events.invalidated_token () in
    let*! token = get_token kms_handler.config in
    kms_handler.auth_token <- token ;
    sign_rpc ~allow_refresh:false kms_handler digest)
  else failwith "KMS failed %a" Cohttp.Response.pp_hum response

let signature_of_b64encoded algo b64_sig =
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
  match algo with
  | EC_SIGN_P256_SHA256 ->
      let*? p256_sig = Signature.P256.of_string signature in
      return (Signature.of_p256 p256_sig)
  | EC_SIGN_SECP256K1_SHA256 ->
      let*? secp256k1_sig = Signature.Secp256k1.of_string signature in
      return (Signature.of_secp256k1 secp256k1_sig)

let sign kms_handler hash payload =
  let open Lwt_result_syntax in
  let digest = digest hash payload in
  let* b64_sig = sign_rpc ~allow_refresh:true kms_handler digest in
  signature_of_b64encoded (signature_algorithm kms_handler) b64_sig

let assert_authentication_method config =
  let open Lwt_result_syntax in
  match config.Configuration.authentication_method with
  | Configuration.Gcloud_auth ->
      let*! can_use = run_without_error "which" [config.gcloud_path] in
      unless can_use (fun () ->
          failwith "gcloud executable (%s) not found" config.gcloud_path)
  | Metadata_server ->
      (* We check the metadata server is available *)
      let*! resolv =
        Lwt_unix.getaddrinfo
          (* [Uri.host] will always return something since it is a top-level constant *)
          metadata_server_host
          "80"
          [AI_FAMILY PF_INET; AI_SOCKTYPE SOCK_STREAM]
      in
      when_ (resolv = []) (fun () ->
          failwith
            "Could not resolve %s, meaning the metadata server is not available"
            metadata_server_host)

let from_gcp_key (config : Configuration.gcp_kms) gcp_key =
  let open Lwt_result_syntax in
  (* We fail early on startup, and keep the retry and backoff strategy for
     refreshing token only. *)
  let* () = assert_authentication_method config in
  let*! auth_token = get_token {config with authentication_retries = 1} in
  (* We first construct a partial handler *)
  let kms_handler : partial =
    {
      pool = Octez_connpool.make ~n:config.pool_size kms_uri;
      key_path =
        Format.sprintf
          "v1/projects/%s/locations/%s/keyRings/%s/cryptoKeys/%s/cryptoKeyVersions/%d"
          gcp_key.Configuration.project
          gcp_key.region
          gcp_key.keyring
          gcp_key.key
          gcp_key.version;
      gcp_key;
      config;
      public_key = ();
      auth_token;
    }
  in
  (* We fetch the necessary information which will allow us to interpret the
     KMS response and wrap them into the correct Signature.t constructor. *)
  let* public_key = public_key kms_handler in
  let kms_handler = {kms_handler with public_key} in
  let*! () = Gcp_kms_events.is_ready public_key in
  Lwt.dont_wait (fun () -> Octez_connpool.warm kms_handler.pool) ignore ;
  Lwt.dont_wait (fun () -> wait_and_refresh config kms_handler) ignore ;
  (* We return the full handler *)
  return kms_handler

let public_key t = t.public_key

let ethereum_address_opt kms =
  let pk = public_key (kms : t) in
  match pk with
  | Secp256k1 s ->
      let str = Signature.Secp256k1.eth_address_of_public_key s in
      let (`Hex hex) = Hex.of_bytes str in
      Some Ethereum_types.(Address (Hex hex))
  | _ -> None
