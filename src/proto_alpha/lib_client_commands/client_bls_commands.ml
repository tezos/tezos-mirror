(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_clic

let group = {name = "BLS"; title = "BLS utility commands"}

let signature_parameter ~name ~desc =
  param
    ~name
    ~desc
    (parameter (fun (cctxt : #Protocol_client_context.full) s ->
         let open Lwt_result_syntax in
         match Signature.Bls.of_b58check_opt s with
         | Some s -> return s
         | None -> cctxt#error "Failed to read a BLS signature"))

let commands () =
  let open Lwt_result_syntax in
  [
    command
      ~group
      ~desc:"Aggregate BLS signatures"
      no_options
      (prefixes ["aggregate"; "bls"; "signatures"]
      @@ seq_of_param
      @@ signature_parameter
           ~name:"BLS signature"
           ~desc:"B58 encoded BLS signature")
      (fun () sigs (cctxt : #Protocol_client_context.full) ->
        let aggregated_signature = Signature.Bls.aggregate_signature_opt sigs in
        match aggregated_signature with
        | Some aggregated_signature ->
            let*! () =
              cctxt#message
                "%a"
                Signature.pp
                (Signature.Bls aggregated_signature)
            in
            return_unit
        | None -> cctxt#error "Failed to aggregate the signatures");
    command
      ~group
      ~desc:"Create a BLS proof by signing a public key"
      no_options
      (prefixes ["create"; "bls"; "proof"; "for"]
      @@ Client_keys.Secret_key.source_param @@ stop)
      (fun () sk_uri (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* pk_uri = Client_keys.neuterize sk_uri in
        let* pk = Client_keys.public_key pk_uri in
        match pk with
        | Bls _ ->
            let msg =
              Data_encoding.Binary.to_bytes_exn Signature.Public_key.encoding pk
            in
            let* proof = Client_keys.sign cctxt sk_uri msg in
            let*! () = cctxt#message "%a" Signature.pp proof in
            return_unit
        | _ -> cctxt#error "Failed to produce a proof: input is not a BLS key");
  ]
