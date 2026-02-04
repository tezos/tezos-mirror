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
         (["blocks_per_cycle"], `Int 2);
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
  let* _ = Client.bake_for_and_wait ~keys ~count:8 client in

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

let register ~protocols =
  signer_simple_test protocols ;
  signer_magic_bytes_test protocols ;
  signer_bls_test protocols ;
  signer_known_remote_keys_test protocols ;
  signer_prove_possession_test
    (List.filter (fun p -> Protocol.number p > 022) protocols) ;
  signer_highwatermark_test protocols
