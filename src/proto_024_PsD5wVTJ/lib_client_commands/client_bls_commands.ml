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

let public_key_param =
  parameter (fun (cctxt : #Protocol_client_context.full) s ->
      let open Lwt_result_syntax in
      match Signature.Bls.Public_key.of_b58check_opt s with
      | Some pk -> return pk
      | None -> cctxt#error "Failed to read a BLS public key")

let public_key_parameter ~name ~desc = param ~name ~desc public_key_param

let public_key_arg ~doc ?short ~long ~placeholder ?env () =
  arg ~doc ?short ~long ~placeholder ?env public_key_param

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

type public_key_with_proofs = {
  pk : Signature.Bls.Public_key.t;
  proofs : Signature.Bls.t list;
}

let public_key_with_proofs_encoding =
  let open Data_encoding in
  conv
    (fun {pk; proofs} -> (pk, proofs))
    (fun (pk, proofs) -> {pk; proofs})
    (obj2
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "proofs" (list Signature.Bls.encoding)))

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
  proof : Signature.Bls.t;
  secret_shares : threshold_secret_key list;
}

let threshold_keys_encoding =
  let open Data_encoding in
  conv
    (fun {pk; pkh; proof; secret_shares} -> (pk, pkh, proof, secret_shares))
    (fun (pk, pkh, proof, secret_shares) -> {pk; pkh; proof; secret_shares})
    (obj4
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "public_key_hash" Signature.Bls.Public_key_hash.encoding)
       (req "proof" Signature.Bls.encoding)
       (req "secret_shares" (list threshold_secret_key_encoding)))

type threshold_signature_share = {id : int; signature : Signature.Bls.t}

let threshold_signature_share_encoding =
  let open Data_encoding in
  conv
    (fun {id; signature} -> (id, signature))
    (fun (id, signature) -> {id; signature})
    (obj2 (req "id" int8) (req "signature" Signature.Bls.encoding))

type threshold_signature = {
  pk : Signature.Bls.Public_key.t;
  msg : Bytes.t;
  signature_shares : threshold_signature_share list;
}

let threshold_signature_encoding =
  let open Data_encoding in
  conv
    (fun {pk; msg; signature_shares} -> (pk, msg, signature_shares))
    (fun (pk, msg, signature_shares) -> {pk; msg; signature_shares})
    (obj3
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "message" bytes)
       (req "signature_shares" (list threshold_signature_share_encoding)))

type aggregate_signature = {
  pk : Signature.Bls.Public_key.t;
  msg : Bytes.t;
  signature_shares : Signature.Bls.t list;
}

let aggregate_signature_encoding =
  let open Data_encoding in
  conv
    (fun {pk; msg; signature_shares} -> (pk, msg, signature_shares))
    (fun (pk, msg, signature_shares) -> {pk; msg; signature_shares})
    (obj3
       (req "public_key" Signature.Bls.Public_key.encoding)
       (req "message" bytes)
       (req "signature_shares" (list Signature.Bls.encoding)))

let check_public_key_with_proof pk ?override_pk proof =
  Signature.Bls.pop_verify pk ?msg:override_pk (Signature.Bls.to_bytes proof)

let commands () =
  let open Lwt_result_syntax in
  [
    command
      ~group
      ~desc:
        "Construct an aggregate BLS signature from signature shares and check \
         if it is a valid signature of a message under a public key"
      no_options
      (prefixes ["aggregate"; "bls"; "signatures"]
      @@ Client_proto_args.json_encoded_param
           ~name:"input"
           ~desc:"a public key, a message and a list of signature shares"
           aggregate_signature_encoding
      @@ stop)
      (fun () inp (cctxt : #Protocol_client_context.full) ->
        let aggregated_signature =
          Signature.Bls.aggregate_signature_opt inp.signature_shares
        in
        match aggregated_signature with
        | Some aggregated_signature ->
            let is_valid =
              Signature.Bls.check inp.pk aggregated_signature inp.msg
            in
            if is_valid then
              let*! () =
                cctxt#message
                  "%a"
                  Signature.pp
                  (Signature.Bls aggregated_signature)
              in
              return_unit
            else cctxt#error "Failed to aggregate the signatures"
        | None -> cctxt#error "Failed to aggregate the signatures");
    command
      ~group
      ~desc:"Create a BLS proof by signing a public key"
      (args1
         (public_key_arg
            ~doc:"Change the public key to sign for the proof"
            ~long:"override-public-key"
            ~placeholder:"OVERRIDE_PUBLIC_KEY"
            ()))
      (prefixes ["create"; "bls"; "proof"; "for"]
      @@ Client_keys.Secret_key.source_param @@ stop)
      (fun override_pk sk_uri (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* proof =
          Client_keys.bls_prove_possession cctxt ?override_pk sk_uri
        in
        let*! () = cctxt#message "%a" Signature.Bls.pp proof in
        return_unit);
    command
      ~group
      ~desc:"Check a BLS proof"
      (args1
         (public_key_arg
            ~doc:"Change the public key of the proof"
            ~long:"override-public-key"
            ~placeholder:"PUBLIC_KEY"
            ()))
      (prefixes ["check"; "bls"; "proof"]
      @@ signature_parameter
           ~name:"BLS proof"
           ~desc:"BLS proof is B58 encoded BLS signature"
      @@ prefixes ["for"]
      @@ public_key_parameter
           ~name:"BLS public key"
           ~desc:"B58 encoded BLS public key"
      @@ stop)
      (fun override_pk proof pk (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let is_valid = check_public_key_with_proof pk ?override_pk proof in
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
      ~desc:"Aggregate BLS proofs"
      no_options
      (prefixes ["aggregate"; "bls"; "proofs"]
      @@ Client_proto_args.json_encoded_param
           ~name:"input"
           ~desc:"a public key and a list of proofs crafted for this public key"
           public_key_with_proofs_encoding
      @@ stop)
      (fun () pk_with_proofs (cctxt : #Protocol_client_context.full) ->
        let aggregated_proof =
          Signature.Bls.aggregate_signature_opt pk_with_proofs.proofs
        in
        match aggregated_proof with
        | Some proof ->
            let is_valid =
              check_public_key_with_proof pk_with_proofs.pk proof
            in
            if is_valid then
              let*! () =
                cctxt#message "%a" Signature.pp (Signature.Bls proof)
              in
              return_unit
            else cctxt#error "Aggregated proof is invalid"
        | None -> cctxt#error "Failed to aggregate the proofs");
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
        let pk, pkh, proof, secret_shares =
          Signature.Bls.generate_threshold_key sk ~n ~m
        in
        let proof = Signature.Bls.of_bytes_exn proof in
        let secret_shares = List.map (fun (id, sk) -> {id; sk}) secret_shares in
        let json =
          Data_encoding.Json.construct
            threshold_keys_encoding
            {pk; pkh; proof; secret_shares}
        in
        let*! () = cctxt#message "%a@." Data_encoding.Json.pp json in
        return_unit);
    command
      ~group
      ~desc:
        "Construct a threshold BLS signature from signature shares and check \
         if it is a valid signature of a message under a public key"
      no_options
      (prefixes ["threshold"; "bls"; "signatures"]
      @@ Client_proto_args.json_encoded_param
           ~name:"input"
           ~desc:"a public key, a message and a list of signature shares"
           threshold_signature_encoding
      @@ stop)
      (fun () inp (cctxt : #Protocol_client_context.full) ->
        let open Lwt_result_syntax in
        let* sigs =
          List.map_es
            (fun (s : threshold_signature_share) -> return (s.id, s.signature))
            inp.signature_shares
        in
        let threshold_signature = Signature.Bls.threshold_signature_opt sigs in
        match threshold_signature with
        | Some threshold_signature ->
            let is_valid =
              Signature.Bls.check inp.pk threshold_signature inp.msg
            in
            if is_valid then
              let*! () =
                cctxt#message
                  "%a"
                  Signature.pp
                  (Signature.Bls threshold_signature)
              in
              return_unit
            else cctxt#error "Failed to produce the threshold signature"
        | None -> cctxt#error "Failed to produce the threshold signature");
  ]
