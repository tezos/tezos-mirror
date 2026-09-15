(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(* Testing
   -------
   Component:    Signer
   Invocation:   dune exec tezt/tests/main.exe -- --file signer_test.ml
   Subject:      Run the baker and signer while performing transfers
*)

let team = Tag.layer1

let register_signer_test ~__FILE__ ~title ~tags ~uses ?supports f protocols =
  let supported_launch_mode = Signer.[Http; Socket; Local] in
  let mk_title = function
    | Signer.Http -> title ^ "(http)"
    | Socket -> title ^ "(tcp socket)"
    | Local -> title ^ "(unix socket)"
  in
  let mk_tags = function
    | Signer.Http -> "http" :: tags
    | Socket -> "tcp" :: tags
    | Local -> "unix" :: tags
  in
  List.iter
    (fun launch_mode ->
      Protocol.register_test
        ~__FILE__
        ~title:(mk_title launch_mode)
        ~tags:(mk_tags launch_mode)
        ~uses
        ?supports
        (f launch_mode)
        protocols)
    supported_launch_mode

(* same as `baker_test`, `baker_test.ml` but using the signer *)
let signer_test protocol launch_mode ~keys =
  (* init the signer and import all the bootstrap_keys *)
  let* signer = Signer.init ~launch_mode ~keys () in
  let* parameter_file =
    Protocol.write_parameter_file
      ~overwrite_bootstrap_accounts:(Some (List.map (fun k -> (k, None)) keys))
      ~base:(Right (protocol, None))
      []
  in
  let* node, client =
    Client.init_with_protocol
      ~keys:[Constant.activator]
      `Client
      ~protocol
      ~timestamp:Now
      ~parameter_file
      ()
  in
  let* _ =
    (* tell the baker to ask the signer for the bootstrap keys *)
    let uri = Signer.uri signer in
    Lwt_list.iter_s
      (fun account ->
        let Account.{alias; public_key_hash; _} = account in
        Client.import_signer_key client ~alias ~public_key_hash ~signer:uri)
      keys
  in
  let level_2_promise = Node.wait_for_level node 2 in
  let level_3_promise = Node.wait_for_level node 3 in
  let* _baker = Agnostic_baker.init node client in
  let* _ = level_2_promise in
  Log.info "New head arrive level 2" ;
  let* _ = level_3_promise in
  Log.info "New head arrive level 3" ;
  return client

let signer_simple_test =
  register_signer_test
    ~__FILE__
    ~title:"signer test"
    ~tags:[team; "node"; "baker"; "tz1"]
    ~uses:(fun _protocol ->
      [Constant.octez_signer; Constant.octez_agnostic_baker])
  @@ fun launch_mode protocol ->
  let* _ =
    signer_test
      protocol
      launch_mode
      ~keys:(Account.Bootstrap.keys |> Array.to_list)
  in
  unit

let signer_magic_bytes_test =
  register_signer_test
    ~__FILE__
    ~title:"signer magic-bytes test"
    ~tags:[team; "signer"; "magicbytes"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  let* signer =
    Signer.init ~launch_mode ~keys:[Constant.tz4_account] ~magic_byte:"0x03" ()
  in
  let* () =
    let Account.{alias; public_key_hash; _} = Constant.tz4_account in
    Client.import_signer_key
      client
      ~alias
      ~public_key_hash
      ~signer:(Signer.uri signer)
  in
  (* Check allowed magic byte. *)
  let* _ =
    Client.sign_bytes ~signer:Constant.tz4_account.alias ~data:"0x03" client
  in
  (* Check unallowed magic byte. *)
  let* () =
    Client.spawn_sign_bytes
      ~signer:Constant.tz4_account.alias
      ~data:"0x04"
      client
    |> Process.check_error ~msg:(rex "magic byte 0x04 not allowed\n")
  in
  unit

let signer_bls_test =
  register_signer_test
    ~__FILE__
    ~title:"BLS signer test"
    ~tags:[team; "node"; "baker"; "bls"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  let* signer =
    Signer.init
      ~launch_mode
      ~keys:[Constant.tz4_account]
      ~allow_to_prove_possession:true
      ()
  in
  let* () =
    let Account.{alias; public_key_hash; _} = Constant.tz4_account in
    Client.import_signer_key
      client
      ~alias
      ~public_key_hash
      ~signer:(Signer.uri signer)
  in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:Constant.tz4_account.public_key_hash
      ~burn_cap:(Tez.of_int 1)
      client
  in
  let* () = Client.bake_for_and_wait client in
  let get_balance_tz4 client =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:Constant.tz4_account.public_key_hash
         ()
  in
  let* balance_0 = get_balance_tz4 client in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 5)
      ~giver:Constant.tz4_account.public_key_hash
      ~receiver:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* balance_1 = get_balance_tz4 client in
  Check.((Tez.mutez_int64 balance_0 > Tez.mutez_int64 balance_1) int64)
    ~error_msg:"Tz4 sender %s has decreased balance after transfer" ;
  unit

let signer_known_remote_keys_test =
  register_signer_test
    ~__FILE__
    ~title:"Known remote keys signer test"
    ~tags:[team; "signer"; "remote"; "keys"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  let keys = [Constant.tz4_account; Constant.bootstrap1; Constant.bootstrap2] in
  let* signer = Signer.init ~launch_mode ~keys () in
  let process =
    Client.spawn_list_known_remote_keys client (Signer.uri signer)
  in
  let* () =
    Process.check_error ~msg:(rex "List known keys request not allowed") process
  in
  let* signer = Signer.init ~launch_mode ~keys ~allow_list_known_keys:true () in
  let* pkhs = Client.list_known_remote_keys client (Signer.uri signer) in
  let expected =
    keys
    |> List.map (fun Account.{public_key_hash; _} -> public_key_hash)
    |> List.sort String.compare
  in
  let found = List.sort String.compare pkhs in
  if List.equal String.equal expected found then unit
  else
    let pp = Format.(pp_print_list pp_print_string) in
    Test.fail "@[<v 2>expected:@,%a@]@,@[<v 2>found:@,%a@]" pp expected pp found

let signer_prove_possession_test =
  register_signer_test
    ~__FILE__
    ~title:"Prove possession of tz4 test"
    ~tags:[team; "signer"; "prove"; "possession"; "keys"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  let keys = [Constant.tz4_account] in
  let alias = "alias_ko" in
  let* signer = Signer.init ~launch_mode ~keys () in
  let* () =
    Client.import_signer_key
      ~alias
      client
      ~signer:(Signer.uri signer)
      ~public_key_hash:Constant.tz4_account.public_key_hash
  in
  let process =
    Client.spawn_set_consensus_key
      client
      ~account:Constant.bootstrap1.alias
      ~key:alias
  in
  let* () =
    Process.check_error
      ~msg:(rex "Request to prove possession is not allowed")
      process
  in
  let alias = "alias_ok" in
  let* signer =
    Signer.init ~launch_mode ~keys ~allow_to_prove_possession:true ()
  in
  let* () =
    Client.import_signer_key
      ~force:true
      ~alias
      client
      ~signer:(Signer.uri signer)
      ~public_key_hash:Constant.tz4_account.public_key_hash
  in
  let process =
    Client.spawn_set_consensus_key
      client
      ~account:Constant.bootstrap2.alias
      ~key:alias
  in
  Process.check process

let signer_highwatermark_test =
  register_signer_test
    ~__FILE__
    ~title:"Check highwatermark consistency"
    ~tags:[team; "signer"; "highwatermark"]
    ~uses:(fun _ -> [Constant.octez_signer])
    ~supports:Protocol.(From_protocol (number S023))
  @@ fun launch_mode protocol ->
  let consensus_rights_delay = 1 in
  let consensus_committee_size = 256 in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Right (protocol, None))
      ([
         (["consensus_committee_size"], `Int consensus_committee_size);
         (["consensus_threshold_size"], `Int 200);
         (["minimal_block_delay"], `String "2");
         (["delay_increment_per_round"], `String "0");
         (["blocks_per_cycle"], `Int 4);
         (["nonce_revelation_threshold"], `Int 1);
       ]
      |> Protocol.parameters_with_custom_consensus_rights_delay
           ~protocol
           ~consensus_rights_delay)
  in
  let* node, client =
    Client.init_with_protocol
      `Client
      ~protocol
      ~parameter_file
      ~timestamp:Now
      ()
  in
  let* consensus_key1 = Client.gen_and_show_keys ~sig_alg:"p256" client in
  let keys = [Constant.tz4_account; consensus_key1] in
  let* signer =
    Signer.init
      ~launch_mode
      ~keys
      ~check_highwatermark:true
      ~allow_to_prove_possession:true
      ()
  in
  let* () =
    Client.import_signer_key
      ~force:true
      ~alias:Constant.tz4_account.alias
      client
      ~signer:(Signer.uri signer)
      ~public_key_hash:Constant.tz4_account.public_key_hash
  in
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap1.alias
      ~pk:Constant.tz4_account.alias
      client
  in
  let* () =
    Client.import_signer_key
      ~force:true
      ~alias:consensus_key1.alias
      client
      ~signer:(Signer.uri signer)
      ~public_key_hash:consensus_key1.public_key_hash
  in
  let* () =
    Client.update_consensus_key
      ~src:Constant.bootstrap2.alias
      ~pk:consensus_key1.alias
      client
  in
  let keys =
    List.map
      (fun (account : Account.key) -> account.public_key_hash)
      [
        consensus_key1;
        Constant.tz4_account;
        Constant.bootstrap1;
        Constant.bootstrap2;
        Constant.bootstrap3;
        Constant.bootstrap4;
        Constant.bootstrap5;
      ]
  in
  Log.info "Bake until BLS consensus keys are activated" ;
  let* _ = Client.bake_for_and_wait ~keys ~count:10 client in

  Log.info "Preattest with all the keys to update the highwatermarks" ;
  let* _ = Client.preattest_for ~key:keys client in

  let* current_lvl = Node.get_level node in
  let base_dir = Signer.base_dir signer in
  let preattestation_highwatermarks_file =
    base_dir // "preattestation_high_watermarks"
  in
  let preattestation_highwatermarks =
    JSON.parse_file preattestation_highwatermarks_file
  in
  let attestation_highwatermarks_file =
    base_dir // "attestation_high_watermarks"
  in
  let attestation_highwatermarks =
    JSON.parse_file attestation_highwatermarks_file
  in
  let check_highwatermark pkh lvl json =
    let u = JSON.unannotate json in
    match u with
    | `O [(_, x)] ->
        let x = JSON.annotate ~origin:"" x in
        let level = JSON.(x |-> pkh |-> "level" |> as_int) in
        Check.(
          (lvl = level)
            int
            ~error_msg:"Highwatermark Level expected was %L, got %R")
    | _ -> assert false
  in
  Log.info "Check that highwatermark level are correct for the signer keys" ;
  List.iter
    (fun pkh ->
      check_highwatermark pkh current_lvl preattestation_highwatermarks ;
      check_highwatermark pkh (current_lvl - 1) attestation_highwatermarks)
    [consensus_key1.public_key_hash; Constant.tz4_account.public_key_hash] ;

  Log.info "Rewrite highwatermarks level to a higher value" ;
  let* _ = Client.bake_for_and_wait ~keys ~count:2 client in
  let reset_highwatermark_level file highwatermarks level =
    let highwatermarks_s = JSON.encode highwatermarks in
    let contents =
      Re.replace_string
        (Re.compile (Re.Perl.re (sf {|"level": %d|} level)))
        ~by:{|"level": 100|}
        highwatermarks_s
    in
    write_file file ~contents
  in
  let () =
    reset_highwatermark_level
      preattestation_highwatermarks_file
      preattestation_highwatermarks
      current_lvl ;
    reset_highwatermark_level
      attestation_highwatermarks_file
      attestation_highwatermarks
      (current_lvl - 1)
  in

  Log.info
    "Check that signing a preattestation with a lower level than the \
     highwatermark fails" ;
  let preattest =
    Client.spawn_preattest_for ~key:[consensus_key1.public_key_hash] client
  in
  let* stdout = Process.check_and_read_stdout preattest in
  let* () =
    if
      stdout
      =~! rex {|preattestation level ([\d]+) below high watermark ([\d]+)|}
    then unit
    else
      Test.fail
        "The preattest call should have returned a level below high watermark \
         error"
  in

  Log.info
    "Check that signing an attestation with a lower level than the \
     highwatermark fails" ;
  let attest =
    Client.spawn_attest_for ~key:[Constant.tz4_account.public_key_hash] client
  in
  let* stdout = Process.check_and_read_stdout attest in
  let* () =
    if stdout =~! rex {|attestation level ([\d]+) below high watermark ([\d]+)|}
    then unit
    else
      Test.fail
        "The attest call should have returned a level below high watermark \
         error"
  in

  unit

let signer_bls_proof_command_test () =
  Test.register
    ~__FILE__
    ~title:"Signer: create bls proof command"
    ~tags:[team; "signer"; "bls"; "proof"; "command"]
    ~uses:[Constant.octez_signer]
  @@ fun () ->
  let* signer = Signer.create ~keys:[Constant.tz4_account] () in
  let sk_uri =
    match Constant.tz4_account.secret_key with
    | Unencrypted sk -> "unencrypted:" ^ sk
    | _ -> Test.fail "Expected unencrypted BLS secret key"
  in
  Log.info "Creating BLS proof of possession" ;
  let* proof = Signer.bls_prove_possession ~sk_uri signer in
  let expected =
    Operation.Manager.create_proof_of_possession ~signer:Constant.tz4_account
  in
  let expected =
    match expected with
    | Some proof -> proof
    | None -> Test.fail "Expected a BLS proof of possession"
  in
  Check.((proof = expected) ~__LOC__ string)
    ~error_msg:"Expected BLS proof %R, got %L" ;
  Log.info "BLS proof of possession: %s" proof ;
  Log.info "Creating BLS proof of possession with --override-public-key" ;
  let override_pk =
    "BLpk1xXdveUYh7YFsyf6LwGWfv5zAfLvnMG71byiMFDZc4CkXzZPVko3Dz4sD43Ln5uFNvdjiQJY"
  in
  let* proof_with_override =
    Signer.bls_prove_possession ~override_pk ~sk_uri signer
  in
  Log.info "BLS proof with override: %s" proof_with_override ;
  Check.((proof_with_override <> proof) ~__LOC__ string)
    ~error_msg:"Expected proof with override to differ from default proof" ;
  unit

let signer_mldsa44_test ~authenticate =
  register_signer_test
    ~__FILE__
    ~title:
      (if authenticate then "signer tz5 keys with authentication test"
       else "signer tz5 keys test")
    ~tags:
      ([team; "signer"; "mldsa44"]
      @ if authenticate then ["authentication"] else [])
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* _node, client = Client.init_with_protocol ~protocol `Client () in
  (* Generate the key in a throwaway client, so that [client] only knows it
     as a remote key. *)
  let keygen_client = Client.create () in
  let alias = "remote_mldsa44" in
  let* _ = Client.gen_keys ~alias ~sig_alg:"mldsa44" keygen_client in
  let* key = Client.show_address ~alias keygen_client in
  let* signer =
    Signer.init ~launch_mode ~keys:[key] ~require_authentication:authenticate ()
  in
  let* () =
    Client.import_signer_key
      client
      ~alias
      ~public_key_hash:key.public_key_hash
      ~signer:(Signer.uri signer)
  in
  (* Importing a remote key makes the client fetch its public key from the
     signer. *)
  let* output =
    Client.spawn_show_address ~alias client |> Process.check_and_read_stdout
  in
  let imported_public_key = output =~* rex "Public Key: ?(\\w*)" in
  Check.(
    (imported_public_key = Some key.public_key)
      (option string)
      ~error_msg:
        "Expected the public key fetched from the signer to be %R, got %L") ;
  let message = "signed by " ^ key.public_key_hash in
  (* With authentication, [client] signs its requests with a local tz5 key
     that the signer authorizes. Signing fails until the key is authorized. *)
  let* () =
    if authenticate then
      let auth_alias = "auth_mldsa44" in
      let* _ = Client.gen_keys ~alias:auth_alias ~sig_alg:"mldsa44" client in
      let* auth_key = Client.show_address ~alias:auth_alias client in
      let* () =
        Client.spawn_sign_message client message ~src:alias
        |> Process.check_error
             ~msg:(rex "no authorized key was found in the wallet")
      in
      Signer.add_authorized_key signer auth_key
    else unit
  in
  let* signature = Client.sign_message client message ~src:alias in
  Client.check_message client ~src:alias ~signature message

(* [authentication_payload ~request_tag ~pkh ~override_pk] rebuilds
   the bytes that one of the authorized keys must sign for a signer
   started with [--require-authentication] to answer, the way
   [Signer_messages.Bls_prove_possession.Request.to_sign] builds them:
   [\x04 | tag | pkh | override_pk], where [request_tag] is the tag of
   the request being authorized (1 for a signing request, 4 for a
   proof of possession).

   The signer protocol has no freshness, so this payload is the only
   thing that scopes an authentication signature. It is rebuilt here
   rather than linked from the signer's own code, so that a change to
   what goes on the wire has to be made deliberately in both
   places. *)
let authentication_payload ~request_tag ~pkh ~override_pk =
  let pkh = Tezos_crypto.Signature.Public_key_hash.of_b58check_exn pkh in
  let override_pk_bytes =
    match override_pk with
    | None -> Bytes.empty
    | Some pk ->
        Tezos_crypto.Signature.Bls.Public_key.of_b58check_exn pk
        |> Data_encoding.Binary.to_bytes_exn
             Tezos_crypto.Signature.Bls.Public_key.encoding
  in
  Bytes.concat
    Bytes.empty
    [
      Bytes.of_string "\x04";
      Bytes.make 1 (Char.chr request_tag);
      Tezos_crypto.Signature.Public_key_hash.to_bytes pkh;
      override_pk_bytes;
    ]
  |> Hex.of_bytes |> Hex.show

(* [prove_possession_frame ~pkh ~override_pk ~authentication] builds the
   payload of a proof-of-possession request the way [Signer_messages.Request]
   lays it out for a socket signer: the [\x07] case tag, the public key hash
   with its own algorithm tag, a presence byte for the optional overridden
   public key, then the authentication signature. That last field is read to
   the end of the frame, so it takes no byte at all when absent. Built here
   rather than linked from the signer's own encodings, for the same reason as
   [authentication_payload]. *)
let prove_possession_frame ~pkh ~override_pk ~authentication =
  let open Tezos_crypto.Signature in
  let override_pk_bytes =
    match override_pk with
    | None -> Bytes.make 1 '\x00'
    | Some pk ->
        Bytes.cat
          (Bytes.make 1 '\x01')
          (Data_encoding.Binary.to_bytes_exn
             Bls.Public_key.encoding
             (Bls.Public_key.of_b58check_exn pk))
  in
  let authentication_bytes =
    match authentication with
    | None -> Bytes.empty
    | Some signature -> to_bytes (of_b58check_exn signature)
  in
  Bytes.concat
    Bytes.empty
    [
      Bytes.make 1 '\x07';
      Public_key_hash.to_bytes (Public_key_hash.of_b58check_exn pkh);
      override_pk_bytes;
      authentication_bytes;
    ]

(* [socket_roundtrip ~uri payload] sends [payload] as one message on the
   socket signer listening at [uri] and returns the one it answers with.
   Messages are framed by a big-endian 16-bit length, as
   [Tezos_base_unix.Socket] does. *)
let socket_roundtrip ~uri payload =
  let address =
    match Uri.scheme uri with
    | Some "unix" -> Unix.ADDR_UNIX (Uri.path uri)
    | Some "tcp" ->
        let host =
          match Uri.host uri with
          | Some host -> host
          | None -> Constant.default_host
        in
        let port =
          match Uri.port uri with
          | Some port -> port
          | None -> Test.fail "no port in signer URI %s" (Uri.to_string uri)
        in
        Unix.ADDR_INET (Unix.inet_addr_of_string host, port)
    | _ -> Test.fail "not a socket signer URI: %s" (Uri.to_string uri)
  in
  let fd =
    Lwt_unix.socket (Unix.domain_of_sockaddr address) Unix.SOCK_STREAM 0
  in
  Lwt.finalize
    (fun () ->
      let* () = Lwt_unix.connect fd address in
      let write buf =
        let length = Bytes.length buf in
        let rec loop offset =
          if offset >= length then unit
          else
            let* written = Lwt_unix.write fd buf offset (length - offset) in
            loop (offset + written)
        in
        loop 0
      in
      let read length =
        let buf = Bytes.create length in
        let rec loop offset =
          if offset >= length then return buf
          else
            let* n = Lwt_unix.read fd buf offset (length - offset) in
            if n = 0 then
              Test.fail
                "signer closed the connection after %d of %d bytes"
                offset
                length
            else loop (offset + n)
        in
        loop 0
      in
      let header = Bytes.create 2 in
      Bytes.set_uint16_be header 0 (Bytes.length payload) ;
      let* () = write (Bytes.cat header payload) in
      let* header = read 2 in
      read (Bytes.get_uint16_be header 0))
    (fun () -> Lwt_unix.close fd)

let signer_bls_pop_authentication_test =
  register_signer_test
    ~__FILE__
    ~title:"BLS proof of possession authentication test"
    ~tags:[team; "signer"; "bls"; "possession"; "authentication"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun launch_mode protocol ->
  let* node, client = Client.init_with_protocol `Client ~protocol () in
  let key = Constant.tz4_account in
  let alias = "remote_bls_key" in
  (* The public key the proofs below are made over instead of the
     signer's own. Any BLS public key works: what matters is that the
     caller, not the signer, chooses it. *)
  let override_pk =
    "BLpk1xXdveUYh7YFsyf6LwGWfv5zAfLvnMG71byiMFDZc4CkXzZPVko3Dz4sD43Ln5uFNvdjiQJY"
  in
  let* signer =
    Signer.init
      ~launch_mode
      ~keys:[key]
      ~allow_to_prove_possession:true
      ~require_authentication:true
      ()
  in
  (* [bootstrap1] is the only key the signer accepts as an
     authorization, and [client] holds its secret key. [bootstrap2] is
     a key [client] holds too, but which the signer knows nothing
     about. *)
  let* () = Signer.add_authorized_key signer Constant.bootstrap1 in
  let* () =
    Client.import_signer_key
      ~alias
      ~signer:(Signer.uri signer)
      ~public_key_hash:key.public_key_hash
      client
  in
  Log.info
    "An authorized caller gets a proof, over the key itself and over a public \
     key of its choosing" ;
  let* proof = Client.create_bls_proof ~signer:alias client in
  let* () = Client.check_bls_proof ~pk:key.public_key ~proof client in
  let* proof_over_override =
    Client.create_bls_proof ~override_pk ~signer:alias client
  in
  let* () =
    Client.check_bls_proof
      ~override_pk
      ~pk:key.public_key
      ~proof:proof_over_override
      client
  in
  Log.info "A caller holding none of the authorized keys gets nothing" ;
  let* unauthorized_client = Client.init ~endpoint:(Node node) ~keys:[] () in
  let* () =
    Client.import_signer_key
      ~alias
      ~signer:(Signer.uri signer)
      ~public_key_hash:key.public_key_hash
      unauthorized_client
  in
  let* () =
    Client.spawn_create_bls_proof ~override_pk ~signer:alias unauthorized_client
    |> Process.check_error
         ~msg:(rex "no authorized key was found in the wallet")
  in
  Log.info "On the wire: what an authentication signature is bound to" ;
  (* Request tags, from [Signer_messages]. An authentication signature covers
     the tag of the request it authorizes, so that it cannot be replayed on
     another kind of request. *)
  let sign_request_tag = 1 and prove_possession_request_tag = 4 in
  let authenticate ~request_tag ~override_pk ~authorizer =
    let payload =
      authentication_payload ~request_tag ~pkh:key.public_key_hash ~override_pk
    in
    Client.sign_bytes ~signer:authorizer ~data:("0x" ^ payload) client
  in
  (* [wire_cases check] runs the cases below on whichever transport the signer
     was started with: [check ~case ~expected ?override_pk ?authentication ()]
     issues one proof-of-possession request there and checks the answer. Both
     daemons call the same [Handler.bls_prove_possession], so what each
     transport contributes is the wiring that carries [signature] and
     [require_auth] to it; running the same cases on each is what covers that
     wiring. *)
  let wire_cases
      (check :
        case:string ->
        expected:[`Proof of string | `Error of string] ->
        ?override_pk:string ->
        ?authentication:string ->
        unit ->
        unit Lwt.t) =
    let* () =
      check
        ~case:"a request without an authentication signature"
        ~expected:(`Error "missing authentication signature field")
        ~override_pk
        ()
    in
    (* The proof expected here is the one the authorized client already
       obtained, which is what gives the refusals below their meaning: it
       shows the payload signed above is the one the signer expects, so that
       the rest fails on the binding and not on a malformed request. *)
    let* authentication =
      authenticate
        ~request_tag:prove_possession_request_tag
        ~override_pk:(Some override_pk)
        ~authorizer:Constant.bootstrap1.alias
    in
    let* () =
      check
        ~case:"a correct authentication signature"
        ~expected:(`Proof proof_over_override)
        ~override_pk
        ~authentication
        ()
    in
    let* () =
      check
        ~case:"an authentication signature replayed on another public key"
        ~expected:(`Error "invalid authentication signature")
        ~override_pk:key.public_key
        ~authentication
        ()
    in
    let* () =
      check
        ~case:"an authentication signature replayed without the override"
        ~expected:(`Error "invalid authentication signature")
        ~authentication
        ()
    in
    let* signing_authentication =
      authenticate
        ~request_tag:sign_request_tag
        ~override_pk:(Some override_pk)
        ~authorizer:Constant.bootstrap1.alias
    in
    let* () =
      check
        ~case:"a signing authorization replayed as a proof of possession"
        ~expected:(`Error "invalid authentication signature")
        ~override_pk
        ~authentication:signing_authentication
        ()
    in
    let* unknown_authentication =
      authenticate
        ~request_tag:prove_possession_request_tag
        ~override_pk:(Some override_pk)
        ~authorizer:Constant.bootstrap2.alias
    in
    check
      ~case:"a well-formed request signed by a key the signer does not know"
      ~expected:(`Error "invalid authentication signature")
      ~override_pk
      ~authentication:unknown_authentication
      ()
  in
  match launch_mode with
  | Signer.Socket | Local ->
      wire_cases @@ fun ~case ~expected ?override_pk ?authentication () ->
      let* response =
        socket_roundtrip
          ~uri:(Signer.uri signer)
          (prove_possession_frame
             ~pkh:key.public_key_hash
             ~override_pk
             ~authentication)
      in
      (* The answer is an [Error_monad.result_encoding]: a [\x00] tag followed
         by the proof, or a [\x01] tag followed by the error trace, which
         carries the failure message as plain text. *)
      let body = Bytes.to_string response in
      let answered_as_expected =
        match expected with
        | `Proof proof ->
            String.equal
              body
              ("\x00"
              ^ Bytes.to_string
                  Tezos_crypto.Signature.Bls.(to_bytes (of_b58check_exn proof))
              )
        | `Error message ->
            String.length body > 0
            && Char.equal body.[0] '\x01'
            && body =~ rex message
      in
      if answered_as_expected then unit
      else
        Test.fail
          "%s: expected the signer to answer %s, got: %s"
          case
          (match expected with
          | `Proof proof -> proof
          | `Error message -> message)
          (Hex.show (Hex.of_bytes response))
  | Http ->
      let request ?override_pk ?authentication () =
        let query =
          List.filter_map
            Fun.id
            [
              Option.map (sf "bls_pk=%s") override_pk;
              Option.map (sf "authentication=%s") authentication;
            ]
          |> String.concat "&"
        in
        Curl.get_raw
        @@ sf
             "%s/bls_prove_possession/%s?%s"
             (Uri.to_string (Signer.uri signer))
             key.public_key_hash
             query
      in
      wire_cases @@ fun ~case ~expected ?override_pk ?authentication () ->
      let*! response = request ?override_pk ?authentication () in
      let expected =
        match expected with `Proof proof -> proof | `Error message -> message
      in
      if response =~ rex expected then unit
      else
        Test.fail
          "%s: expected the signer to answer %s, got: %s"
          case
          expected
          response

let register ~protocols =
  signer_simple_test protocols ;
  signer_magic_bytes_test protocols ;
  signer_bls_test protocols ;
  signer_mldsa44_test ~authenticate:false protocols ;
  signer_mldsa44_test ~authenticate:true protocols ;
  signer_known_remote_keys_test protocols ;
  signer_prove_possession_test
    (List.filter (fun p -> Protocol.number p > 022) protocols) ;
  signer_highwatermark_test protocols ;
  signer_bls_pop_authentication_test protocols ;
  signer_bls_proof_command_test ()
