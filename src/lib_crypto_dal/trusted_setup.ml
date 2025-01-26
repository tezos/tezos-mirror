(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_crypto.Hacl.Hash
open TzMonad

type download_request = {
  uri : string;
  expected_sha : Hex.t;
  output_file : string;
}

let dal_trusted_setup_folder =
  Tezos_base.Dal_srs.trusted_setup_preferred_directory ()

let defaults =
  let ( // ) = Filename.concat in
  let url = "https://assets.nomadic-labs.cloud/dal_trusted_setup" in
  [
    {
      output_file = dal_trusted_setup_folder // "srsu_zcash_g1";
      expected_sha =
        `Hex "c48ce4add1de2a7561108f17bf0c16bc1e93c0bff24bc7da465c24e0b4b2653e";
      uri = url ^ "/srsu_g1";
    };
    {
      output_file = dal_trusted_setup_folder // "srsu_zcash_g2";
      expected_sha =
        `Hex "e7fbe747ae3648a5b664d8f8bd7c524996f7ed07f3331f905d2e73767d580f7c";
      uri = url ^ "/srsu_g2";
    };
  ]

let output_files =
  ( dal_trusted_setup_folder ^ "/srsu_zcash_g1",
    dal_trusted_setup_folder ^ "/srsu_zcash_g2" )

let create_parent_directories path =
  let dir = Filename.dirname path in
  Lwt_utils_unix.create_dir dir

(** [compute_file_sha256 file] computes SHA-256 hash of [file]. *)
let compute_file_sha256 (file : string) : Hex.t tzresult Lwt.t =
  let open Lwt_result_syntax in
  let sha_ctx = SHA256.init () in
  Lwt_io.with_file ~mode:Lwt_io.input file (fun ic ->
      let rec loop () =
        (* Reading chunks by chunks (4 KB) *)
        let*! chunk = Lwt_io.read ~count:4096 ic in
        if chunk = "" then return_unit
        else (
          SHA256.update sha_ctx (Bytes.of_string chunk) ;
          loop ())
      in
      let* () = loop () in
      let computed_sha = Hex.of_bytes (SHA256.finish sha_ctx) in
      return computed_sha)

(* [download_and_check {uri; expected_sha256; output_file}] downloads a file from [uri], stocks it in [output_file]
   then compares the hash of obtained file with [expected_sha256].
*)
let download_and_check {uri; expected_sha; output_file} : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! () = Event.(emit download) uri in
  let uri = Uri.of_string uri in
  let*! response, body = Cohttp_lwt_unix.Client.get uri in
  match Cohttp.Response.status response with
  | `OK ->
      let sha_ctx = SHA256.init () in
      let*! () =
        Lwt_io.with_file ~mode:Lwt_io.output output_file (fun oc ->
            Cohttp_lwt.Body.to_stream body
            |> Lwt_stream.iter_s (fun chunk ->
                   SHA256.update sha_ctx (Bytes.of_string chunk) ;
                   Lwt_io.write oc chunk))
      in
      let computed_sha = Hex.of_bytes (SHA256.finish sha_ctx) in
      if computed_sha = expected_sha then
        let*! () = Event.(emit successful_download) output_file in
        return_unit
      else tzfail (Errors.Mismatched_SHA {expected_sha; computed_sha})
  | status -> tzfail (Errors.Download_status {status})

(** [download_if_required {uri; expected_sha; output_file}]
    verifies if [output_file] already exists with expected SHA-256 hash.
    If not, downloads [uri] and checks hash. *)
let download_if_required infos : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! file_exists = Lwt_unix.file_exists infos.output_file in
  if file_exists then
    (* SHA-256 hash of local file is computed *)
    let* current_sha = compute_file_sha256 infos.output_file in
    if current_sha = infos.expected_sha then
      (* No need to redownload file *)
      let*! () = Event.(emit valid_file) infos.output_file in
      return_unit
    else
      (* Hash is not the expected one, redownload performed *)
      let*! () = Event.(emit invalid_file) infos.output_file in
      download_and_check infos
  else
    (* File does not exist, let's download it. *)
    let*! () = Event.(emit file_not_found) infos.output_file in
    download_and_check infos

let download_list (reqs : download_request list) : unit tzresult Lwt.t =
  let open Lwt_result_syntax in
  let*! download_results =
    Lwt.map Result_syntax.tzjoin
    @@ Lwt_list.map_p
         (fun info ->
           let*! () = create_parent_directories info.output_file in
           download_if_required info)
         reqs
  in
  Lwt.return download_results
