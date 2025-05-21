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

let secret_key_parameter ~name ~desc =
  param
    ~name
    ~desc
    (parameter (fun (cctxt : #Protocol_client_context.full) s ->
         let open Lwt_result_syntax in
         match Signature.Bls.Secret_key.of_b58check_opt s with
         | Some sk -> return sk
         | None -> cctxt#error "Failed to read a BLS secret key"))

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

type threshold_secret_key = {id : int; sk : Signature.Bls.Secret_key.t}

let threshold_secret_key_encoding =
  let open Data_encoding in
  conv
    (fun {id; sk} -> (id, sk))
    (fun (id, sk) -> {id; sk})
    (obj2 (req "id" int8) (req "secret_key" Signature.Bls.Secret_key.encoding))

type threshold_keys = {
  pk : Signature.Bls.Public_key.t;
  pkh : Signature.Bls.Public_key_hash.t;
  secret_shares : threshold_secret_key list;
}

let threshold_keys_encoding =
  let open Data_encoding in
  conv
    (fun {pk; pkh; secret_shares} -> (pk, pkh, secret_shares))
    (fun (pk, pkh, secret_shares) -> {pk; pkh; secret_shares})
    (obj3
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "public_key_hash" Signature.Bls.Public_key_hash.encoding)
       (req "secret_shares" (list threshold_secret_key_encoding)))

type threshold_signature = {id : int; signature : Signature.Bls.t}

let threshold_signature_encoding =
  let open Data_encoding in
  conv
    (fun {id; signature} -> (id, signature))
    (fun (id, signature) -> {id; signature})
    (obj2 (req "id" int8) (req "signature" Signature.Bls.encoding))

let check_public_key_with_proof pk proof =
  Signature.Bls.pop_verify pk (Signature.Bls.to_bytes proof)

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
        let* proof = Client_keys.bls_prove_possession cctxt sk_uri in
        let*! () = cctxt#message "%a" Signature.Bls.pp proof in
        return_unit);
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
    command
      ~group
      ~desc:
        "Shamir's Secret Sharing: share a secret key between N participants so \
         that any M participants can collaboratively sign messages, while \
         fewer than M participants cannot produce a valid signature"
      no_options
      (prefixes ["share"; "bls"; "secret"; "key"]
      @@ secret_key_parameter
           ~name:"BLS secret key"
           ~desc:"B58 encoded BLS secret key"
      @@ prefixes ["between"]
      @@ Tezos_clic.param
           ~name:"shares"
           ~desc:"Number of shares (N)"
           Client_proto_args.int_parameter
      @@ prefixes ["shares"; "with"; "threshold"]
      @@ Tezos_clic.param
           ~name:"threshold"
           ~desc:"Number of required signatures (M)"
           Client_proto_args.int_parameter
      @@ stop)
      (fun () sk n m (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let pk, pkh, secret_shares =
          Signature.Bls.generate_threshold_key sk ~n ~m
        in
        let secret_shares = List.map (fun (id, sk) -> {id; sk}) secret_shares in
        let json =
          Data_encoding.Json.construct
            threshold_keys_encoding
            {pk; pkh; secret_shares}
        in
        let*! () = cctxt#message "%a@." Data_encoding.Json.pp json in
        return_unit);
    command
      ~group
      ~desc:"Threshold BLS signatures"
      no_options
      (prefixes ["threshold"; "bls"; "signatures"]
      @@ Client_proto_args.json_encoded_param
           ~name:"list of BLS identifiers with signatures"
           ~desc:"list of BLS identifier (int) and B58 encoded BLS signature"
           (Data_encoding.list threshold_signature_encoding)
      @@ stop)
      (fun () sigs (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* sigs =
          List.map_es
            (fun (s : threshold_signature) -> return (s.id, s.signature))
            sigs
        in
        let threshold_signature = Signature.Bls.threshold_signature_opt sigs in
        match threshold_signature with
        | Some threshold_signature ->
            let*! () =
              cctxt#message
                "%a"
                Signature.pp
                (Signature.Bls threshold_signature)
            in
            return_unit
        | None -> cctxt#error "Failed to produce the threshold signature");
  ]
