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

let public_key_parameter ~name ~desc =
  param
    ~name
    ~desc
    (parameter (fun (cctxt : #Protocol_client_context.full) s ->
         let open Lwt_result_syntax in
         match Signature.Bls.Public_key.of_b58check_opt s with
         | Some pk -> return pk
         | None -> cctxt#error "Failed to read a BLS public key"))

type public_key_with_proof = {
  pk : Signature.Bls.Public_key.t;
  proof : Signature.Bls.t;
}

let public_key_with_proof_encoding =
  let open Data_encoding in
  conv
    (fun {pk; proof} -> (pk, proof))
    (fun (pk, proof) -> {pk; proof})
    (obj2
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "proof" Signature.Bls.encoding))

type public_key_and_public_key_hash = {
  pk : Signature.Bls.Public_key.t;
  pkh : Signature.Bls.Public_key_hash.t;
}

let public_key_and_public_key_hash_encoding =
  let open Data_encoding in
  conv
    (fun {pk; pkh} -> (pk, pkh))
    (fun (pk, pkh) -> {pk; pkh})
    (obj2
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "public_key_hash" Signature.Bls.Public_key_hash.encoding))

let check_public_key_with_proof pk proof =
  let msg =
    Data_encoding.Binary.to_bytes_exn
      Signature.Public_key.encoding
      (Signature.Bls pk)
  in
  Signature.Bls.check pk proof msg

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
    command
      ~group
      ~desc:"Check a BLS proof"
      no_options
      (prefixes ["check"; "bls"; "proof"]
      @@ signature_parameter
           ~name:"BLS proof"
           ~desc:"BLS proof is B58 encoded BLS signature"
      @@ prefixes ["for"]
      @@ public_key_parameter
           ~name:"BLS public key"
           ~desc:"B58 encoded BLS public key"
      @@ stop)
      (fun () proof pk (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let is_valid = check_public_key_with_proof pk proof in
        if is_valid then
          let*! () = cctxt#message "Proof check is successful." in
          return_unit
        else cctxt#error "Invalid proof");
    command
      ~group
      ~desc:"Aggregate BLS public keys after checking their BLS proofs"
      no_options
      (prefixes ["aggregate"; "bls"; "public"; "keys"]
      @@ Client_proto_args.json_encoded_param
           ~name:"list of BLS public keys with proofs"
           ~desc:"list of B58 encoded BLS public key and signature"
           (Data_encoding.list public_key_with_proof_encoding)
      @@ stop)
      (fun () pks_with_pops (cctxt : #Protocol_client_context.full) ->
        let pop_checks =
          List.for_all
            (fun (s : public_key_with_proof) ->
              check_public_key_with_proof s.pk s.proof)
            pks_with_pops
        in
        if pop_checks then
          let pks =
            List.map (fun (s : public_key_with_proof) -> s.pk) pks_with_pops
          in
          let pk = Signature.Bls.aggregate_public_key_opt pks in
          match pk with
          | Some pk ->
              let pkh = Signature.Bls.Public_key.hash pk in
              let json =
                Data_encoding.Json.construct
                  public_key_and_public_key_hash_encoding
                  {pk; pkh}
              in
              let*! () = cctxt#message "%a@." Data_encoding.Json.pp json in
              return_unit
          | None -> cctxt#error "Failed to aggregate the public keys"
        else cctxt#error "Failed to check proofs");
  ]
