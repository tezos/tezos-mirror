(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Validation components
   Invocation:   dune exec tezt/tests/main.exe -- --file "validate.ml"
   Subject:      Check the validation of blocks.
*)

let team = Tag.layer1

open Lwt.Infix

type state = Validated | Applied

let on_validation_event state node Node.{name; value; timestamp = _} =
  match name with
  | "validated_block.v0" -> (
      let hash = JSON.(value |> as_string) in
      match Hashtbl.find_opt state (node, hash) with
      | None -> Hashtbl.replace state (node, hash) Validated
      | Some Validated -> Test.fail "A block should not be validated twice"
      | Some Applied ->
          Test.fail "A block should not be validated after being applied")
  | "validation_and_application_success.v0" -> (
      let hash = JSON.(value |-> "block" |> as_string) in
      match Hashtbl.find_opt state (node, hash) with
      | None -> Test.fail "A block should be validated before being applied"
      | Some Validated -> Hashtbl.replace state (node, hash) Applied
      | Some Applied -> Test.fail "A block should not be applied twice")
  | _ -> ()

let wait_for_cluster_at_level cluster level =
  Lwt_list.iter_p
    (fun node ->
      let* _ = Node.wait_for_level node level in
      Lwt.return_unit)
    cluster

let validate_block =
  Protocol.register_test
    ~__FILE__
    ~title:"validate block"
    ~tags:[team; "node"; "validate"]
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let n1 = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [n1] [n2] ;
  let cluster = n1 :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let block_to_bake = 4 in
  let state = Hashtbl.create 11 in
  List.iter
    (fun node -> Node.on_event node (on_validation_event state node))
    cluster ;
  let* c1 = Client.(init ~endpoint:(Node n1) ()) in
  let* () = Client.activate_protocol ~protocol c1 in
  let current_level = ref 1 in
  let* () =
    repeat block_to_bake (fun () ->
        let* () = Client.bake_for c1 in
        incr current_level ;
        wait_for_cluster_at_level cluster !current_level)
  in
  let validated =
    Hashtbl.fold (fun _ value b -> value = Applied && b) state true
  in
  let expected_table_length = (block_to_bake + 1) * List.length cluster in
  if Hashtbl.length state <> expected_table_length || not validated then
    Test.fail "validateing of block did not executed as expected"
  else return ()

let forge_block ?client node ~key ~with_op =
  Log.info "Creating another node to forge a block" ;
  let* client =
    match client with
    | None -> Client.(init ~endpoint:(Node node) ())
    | Some c -> return c
  in
  let* node_level = Client.level client in
  let* node2 = Node.init [Synchronisation_threshold 0] in
  let* client2 = Client.(init ~endpoint:(Node node2) ()) in
  let* () =
    Client.Admin.trust_address ~endpoint:(Node node) ~peer:node2 client
  in
  let* () = Client.Admin.connect_address ~peer:node2 client in
  let* _ = Node.wait_for_level node2 node_level in
  let* node2_id = Node.wait_for_identity node2 in
  let* () = Client.Admin.kick_peer ~peer:node2_id client in
  let* () =
    if with_op then
      let* _ = Operation.Manager.(inject [make @@ transfer ()] client2) in
      unit
    else unit
  in
  let* () =
    (* We want an empty block, in tenderbake, we can simply propose
       so that there is no attestation operations. *)
    Client.propose_for ~key:[key] ~force:true client2
  in
  let* shell =
    Client.shell_header client2 >>= fun shell ->
    JSON.parse ~origin:"forge_fake_block" shell |> return
  in
  let* protocol_data =
    Client.RPC.call client2 @@ RPC.get_chain_block_header_protocol_data_raw ()
  in
  Log.info
    "Sufficient information retrieved: shutting down second node, restarting \
     first node" ;
  let* () = Node.terminate node2 in
  let block_header =
    JSON.update
      "protocol_data"
      (fun _ ->
        JSON.annotate ~origin:"block header crafting" (`String protocol_data))
      shell
  in
  return block_header

let propagate_validateable_bad_block =
  let blocks_to_bake = 4 in
  Protocol.register_test
    ~__FILE__
    ~title:"forge fake block"
    ~tags:[team; "validate"; "fake_block"; "propagation"]
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let node_client = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [node_client] [n2] ;
  let cluster = node_client :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let* client = Client.(init ~endpoint:(Node node_client) ()) in
  let* () = Client.activate_protocol ~protocol client in
  let bootstrap1 = Constant.bootstrap1.alias in
  let* () =
    List.init blocks_to_bake Fun.id
    |> List.map succ
    |> Lwt_list.iter_s (fun i ->
           let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
           wait_for_cluster_at_level cluster i)
  in
  let* block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:false
  in
  (* Put a bad context *)
  Log.info "Crafting a block header with a bad context hash" ;
  let dummy_context_hash =
    `String "CoUeJrcPBj3T3iJL3PY4jZHnmZa5rRZ87VQPdSBNBcwZRMWJGh9j"
  in
  let bad_block_header =
    JSON.update
      "context"
      (fun _ -> JSON.annotate ~origin:"bad context hash" dummy_context_hash)
      block_header
  in
  let* block_header_hex =
    Codec.encode ~name:"block_header" (JSON.unannotate bad_block_header)
    >>= fun hex -> String.trim hex |> return
  in
  Log.info "Re-signing the bad block header" ;
  (* Remove the signature *)
  let unsigned_block_header_hex =
    String.sub block_header_hex 0 (String.length block_header_hex - 128)
  in
  let* signature =
    Client.sign_block client unsigned_block_header_hex ~delegate:bootstrap1
    >>= fun s -> String.trim s |> return
  in
  let signed_bad_block_header_hex =
    String.concat "" [unsigned_block_header_hex; signature]
  in
  let injection_json : RPC_core.data =
    Data
      (`O
         [
           ("data", `String signed_bad_block_header_hex);
           ("operations", `A (List.init 4 (fun _ -> `A [])));
         ])
  in
  (* Wait all nodes to validate the block but fail on validation *)
  let expect_validate_failure node =
    Node.wait_for node "validation_failure.v0" (fun _ -> Some ())
  in
  let validate_waiter =
    (* Post Lima: the validate is not an over-approximation
       anymore and cannot even be considered validateable. *)
    expect_validate_failure node_client
  in
  let p =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
  in
  let* () = Process.check_error ~msg:(rex "Inconsistent hash") p in
  let* () =
    Lwt.pick
      [
        ( Lwt_unix.sleep 30. >>= fun () ->
          Test.fail "timeout while waiting for validate" );
        validate_waiter;
      ]
  in
  (* Also check that re-injecting the bad block fails *)
  let* () =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
    |> Process.check_error ~msg:(rex "Inconsistent hash")
  in
  Log.info
    "Bake a valid block and check the cluster receives it (ensuring the \
     cluster is still connected)." ;
  (* One final bake to ensure everyone is at the same level *)
  let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
  (* activation block + four blocks + the final bake *)
  wait_for_cluster_at_level cluster (1 + blocks_to_bake + 1)

let propagate_validateable_bad_block_payload =
  let blocks_to_bake = 4 in
  Protocol.register_test
    ~__FILE__
    ~title:"forge block with wrong payload"
    ~tags:[team; "validate"; "fake_block"; "propagation"; "payload"]
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  (* Expected topology is :
               N3
              /  \
      N1 -- N2    N5
              \  /
               N4
  *)
  Log.info "Setting up the node topology" ;
  let node_client = Node.create [Private_mode; Synchronisation_threshold 0] in
  let ring =
    Cluster.create ~name:"ring" 4 [Private_mode; Synchronisation_threshold 0]
  in
  let n2 = List.hd ring in
  Cluster.ring ring ;
  Cluster.connect [node_client] [n2] ;
  let cluster = node_client :: ring in
  Log.info "Starting up cluster" ;
  let* () =
    Cluster.start
      ~wait_connections:true
      ~event_sections_levels:[("validator.block", `Debug)]
      cluster
  in
  Log.info "Cluster initialized" ;
  let* client = Client.(init ~endpoint:(Node node_client) ()) in
  let* parameter_file =
    Protocol.write_parameter_file
      ~base:(Either.Right (protocol, None))
      [(["proof_of_work_threshold"], `String "-1")]
  in
  let* () = Client.activate_protocol ~parameter_file ~protocol client in
  let bootstrap1 = Constant.bootstrap1.alias in
  let* () =
    List.init blocks_to_bake Fun.id
    |> List.map succ
    |> Lwt_list.iter_s (fun i ->
           let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
           wait_for_cluster_at_level cluster i)
  in
  let* op_block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:true
  in
  let* block_header =
    forge_block ~client node_client ~key:bootstrap1 ~with_op:false
  in
  (* Put a bad context *)
  Log.info "Crafting a block header with a bad context hash" ;
  let bad_block_header =
    JSON.update
      "protocol_data"
      (fun _ ->
        JSON.annotate
          ~origin:"bad context hash"
          (`String JSON.(op_block_header |-> "protocol_data" |> as_string)))
      block_header
  in
  let* bad_block_header_hex =
    Codec.encode ~name:"block_header" (JSON.unannotate bad_block_header)
    >>= fun hex -> String.trim hex |> return
  in
  (* Remove the signature *)
  let unsigned_bad_block_header_hex =
    String.sub bad_block_header_hex 0 (String.length bad_block_header_hex - 128)
  in
  let* signature =
    Client.sign_block client unsigned_bad_block_header_hex ~delegate:bootstrap1
    >>= fun s -> String.trim s |> return
  in
  let signed_bad_block_header_hex =
    String.concat "" [unsigned_bad_block_header_hex; signature]
  in
  let injection_json : RPC_core.data =
    Data
      (`O
         [
           ("data", `String signed_bad_block_header_hex);
           ("operations", `A (List.init 4 (fun _ -> `A [])));
         ])
  in
  let expect_validate_failure node =
    Node.wait_for node "validation_failure.v0" (fun _ -> Some ())
  in
  let validate_waiter =
    (* Post Kathmandu: the validate is not an over-approximation
       anymore and cannot even be considered validateable. *)
    expect_validate_failure node_client
  in
  let p =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
  in
  let* () = Process.check_error ~msg:(rex "Invalid payload hash") p in
  let* () =
    Lwt.pick
      [
        ( Lwt_unix.sleep 10. >>= fun () ->
          Test.fail "timeout while waiting for validate" );
        validate_waiter;
      ]
  in
  (* Also check that re-injecting the bad block fails *)
  let* () =
    Client.spawn_rpc ~data:injection_json POST ["injection"; "block"] client
    |> Process.check_error ~msg:(rex "Invalid payload hash")
  in
  let* _ = Client.RPC.call client @@ RPC.get_chain_block () in
  Log.info
    "Bake a valid block and check the cluster receives it (ensuring the \
     cluster is still connected)." ;
  (* One final bake to ensure everyone is at the same level *)
  let* () = Client.bake_for_and_wait ~keys:[bootstrap1] client in
  (* activation block + four blocks + the final bake *)
  wait_for_cluster_at_level cluster (1 + blocks_to_bake + 1)

(* Security regression test
   -------------------------
   End-to-end regression for the P256 signature-malleability fix on
   [double_baking_evidence] operations.

   P256 (secp256r1, tz3) ECDSA signatures are malleable: for any valid
   signature [(r, s)] over a message, [(r, n - s mod n)] is also a valid
   signature for the *identical* message, where [n] is the curve order.
   Taking one legitimately-baked block from a tz3 baker, flipping
   [s -> n - s] yields a header with identical unsigned content but a
   different raw encoding (hence a different block hash). Submitting the
   original header plus the mutated header as [double_baking_evidence]
   used to be accepted, slashing a baker who only ever baked once.

   The fix (src/proto_025_PsUshuai/lib_plugin/block_validation.ml,
   [check_double_baking_evidence]) rejects evidence whose two headers share
   the same unsigned content. It is enforced both at mempool pre-filter
   time and by the shell during block validation (via
   [check_block_operation]). This test drives the full attack path through
   the normal injection RPC and asserts the forged evidence is REFUSED and
   never applies a slash.

   Registered for U025 only: this is where the plugin fix lives. On
   unfixed protocols the forged evidence is still accepted, so running it
   there would (correctly) fail. *)

let hex_to_bytes hex = Hex.to_bytes (`Hex hex)

let bytes_to_hex bytes = match Hex.of_bytes bytes with `Hex h -> h

(* [bytes_sub_32 n s] computes [n - s] for two 32-byte big-endian values
   represented as [Bytes.t], via a hand-written subtract-with-borrow. Since
   a valid ECDSA [s] value always satisfies [0 < s < n], this is simply
   [n - s] (no modular reduction needed in practice). *)
let bytes_sub_32 (a : Bytes.t) (b : Bytes.t) : Bytes.t =
  if Bytes.length a <> 32 || Bytes.length b <> 32 then
    Test.fail "bytes_sub_32: expected 32-byte inputs" ;
  let result = Bytes.create 32 in
  let borrow = ref 0 in
  for i = 31 downto 0 do
    let ai = Char.code (Bytes.get a i) in
    let bi = Char.code (Bytes.get b i) in
    let diff = ai - bi - !borrow in
    if diff < 0 then (
      Bytes.set result i (Char.chr (diff + 256)) ;
      borrow := 1)
    else (
      Bytes.set result i (Char.chr diff) ;
      borrow := 0)
  done ;
  if !borrow <> 0 then Test.fail "bytes_sub_32: unexpected borrow (a < b)" ;
  result

(* Splits a raw (hex-encoded) block header into its unsigned part and its
   64-byte (128 hex chars) trailing signature. *)
let split_signature block_header_hex =
  let len = String.length block_header_hex in
  let unsigned_hex = String.sub block_header_hex 0 (len - 128) in
  let signature_hex = String.sub block_header_hex (len - 128) 128 in
  (unsigned_hex, signature_hex)

(* The order of the P256 curve secp256r1, as a big-endian hex string
   (64 hex chars = 32 bytes):
   0xFFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551 *)
let p256_order_hex =
  "ffffffff00000000ffffffffffffffffbce6faada7179e84f3b9cac2fc632551"

let malleate_p256_signature_hex signature_hex =
  let sig_bytes = hex_to_bytes signature_hex in
  if Bytes.length sig_bytes <> 64 then
    Test.fail "malleate_p256_signature_hex: expected a 64-byte signature" ;
  let r = Bytes.sub sig_bytes 0 32 in
  let s = Bytes.sub sig_bytes 32 32 in
  let n = hex_to_bytes p256_order_hex in
  let s' = bytes_sub_32 n s in
  let sig' = Bytes.cat r s' in
  (bytes_to_hex sig', s, s')

let double_baking_evidence_p256_signature_malleability =
  Protocol.register_test
    ~__FILE__
    ~title:
      "security: double_baking_evidence rejects P256 signature-malleated \
       forged evidence"
    ~tags:
      [team; "security"; "double_baking"; "malleability"; "validate"; "p256"]
    ~uses:(fun _protocol -> [Constant.octez_codec])
  @@ fun protocol ->
  Log.info "Starting a single sandbox node" ;
  let* node = Node.init [Synchronisation_threshold 0] in
  let* client = Client.(init ~endpoint:(Node node) ()) in
  Log.info "Generating a P256 (tz3) delegate with baking rights from genesis" ;
  let* p256_keys =
    Client.stresstest_gen_keys
      ~alias_prefix:"tz3_delegate"
      ~sig_algo:"p256"
      1
      client
  in
  let p256_account =
    match p256_keys with
    | [k] -> k
    | _ -> Test.fail "expected exactly one generated P256 key"
  in
  let* parameter_file =
    Protocol.write_parameter_file
      ~additional_bootstrap_accounts:[(p256_account, None, true)]
      ~base:(Either.right (protocol, None))
      []
  in
  let* () = Client.activate_protocol ~parameter_file ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  Log.info "Baking one honestly-produced block with the P256 delegate" ;
  let* () = Client.bake_for_and_wait ~keys:[p256_account.alias] client in
  let* level = Client.level client in
  Log.info "Fetching the raw header and hash of block %d" level ;
  let block = string_of_int level in
  let* block_header_hex =
    Client.RPC.call client @@ RPC.get_chain_block_header_raw ~block ()
  in
  let* block_header_json =
    Client.RPC.call client @@ RPC.get_chain_block_header ~block ()
  in
  let hash1_b58 = JSON.(block_header_json |-> "hash" |> as_string) in
  let header1_hex = String.trim JSON.(block_header_hex |> as_string) in
  let unsigned_hex, signature1_hex = split_signature header1_hex in
  let signature2_hex, s, s' = malleate_p256_signature_hex signature1_hex in
  let header2_hex = unsigned_hex ^ signature2_hex in
  (* Sanity checks: the crux of the exploit precondition. *)
  if signature1_hex = signature2_hex then
    Test.fail
      "Malleated signature is identical to the original: the exploit \
       precondition does not hold" ;
  if Bytes.equal s s' then
    Test.fail "s == s': malleation did not change the s component" ;
  Log.info
    "Confirmed: signature1 <> signature2 (%s <> %s)"
    signature1_hex
    signature2_hex ;
  let unsigned_bytes = hex_to_bytes unsigned_hex in
  let* chain_id_b58 = Client.RPC.call client @@ RPC.get_chain_chain_id () in
  let chain_id = Tezos_crypto.Hashed.Chain_id.of_b58check_exn chain_id_b58 in
  let block_header_watermark =
    Tezos_crypto.Signature.Custom
      (Bytes.cat
         (Bytes.of_string "\x11")
         (Tezos_crypto.Hashed.Chain_id.to_bytes chain_id))
  in
  let public_key =
    Tezos_crypto.Signature.Public_key.of_b58check_exn p256_account.public_key
  in
  let signature1 = Tezos_crypto.Signature.of_hex_exn (`Hex signature1_hex) in
  let signature2 = Tezos_crypto.Signature.of_hex_exn (`Hex signature2_hex) in
  let valid1 =
    Tezos_crypto.Signature.check
      ~watermark:block_header_watermark
      public_key
      signature1
      unsigned_bytes
  in
  let valid2 =
    Tezos_crypto.Signature.check
      ~watermark:block_header_watermark
      public_key
      signature2
      unsigned_bytes
  in
  if not valid1 then
    Test.fail
      "The original block header signature does not verify (bug in test)" ;
  if not valid2 then
    Test.fail
      "The malleated P256 signature does NOT verify: the malleability exploit \
       precondition does not hold on this build" ;
  Log.info
    "Confirmed: both signatures are valid for the exact same content and the \
     same public key" ;
  let hash2 =
    Tezos_crypto.Hashed.Block_hash.hash_bytes [hex_to_bytes header2_hex]
  in
  let hash2_b58 = Tezos_crypto.Hashed.Block_hash.to_b58check hash2 in
  Log.info "hash1 = %s, hash2 = %s" hash1_b58 hash2_b58 ;
  if hash1_b58 = hash2_b58 then
    Test.fail "hash1 = hash2: malleation did not change the block hash" ;
  (* Order (header, hash) pairs so that hash1 < hash2, as the protocol's own
     double-baking check requires. *)
  let (bh1_hex, _hash1_b58), (bh2_hex, _hash2_b58) =
    if String.compare hash1_b58 hash2_b58 < 0 then
      ((header1_hex, hash1_b58), (header2_hex, hash2_b58))
    else ((header2_hex, hash2_b58), (header1_hex, hash1_b58))
  in
  let encoding_prefix = Protocol.encoding_prefix protocol in
  let* bh1_json =
    Codec.decode ~name:(encoding_prefix ^ ".block_header") bh1_hex
  in
  let* bh2_json =
    Codec.decode ~name:(encoding_prefix ^ ".block_header") bh2_hex
  in
  let* branch = Client.RPC.call client @@ RPC.get_chain_block_hash () in
  let operation_json =
    `O
      [
        ("branch", `String branch);
        ( "contents",
          `A
            [
              `O
                [
                  ("kind", `String "double_baking_evidence");
                  ("bh1", JSON.unannotate bh1_json);
                  ("bh2", JSON.unannotate bh2_json);
                ];
            ] );
      ]
  in
  Log.info "Encoding the forged double_baking_evidence operation" ;
  let* operation_unsigned_hex =
    Codec.encode ~name:(encoding_prefix ^ ".operation.unsigned") operation_json
    >>= fun hex -> String.trim hex |> return
  in
  (* [operation.unsigned] only encodes [shell_header + contents]. Anonymous
     operations (like [double_baking_evidence]) carry no real signature, but
     the binary encoding still requires a 64-byte signature suffix, encoded
     as all-zeros when [signature = None]. We splice that in manually to get
     valid injectable bytes. *)
  let zero_signature_hex = String.make 128 '0' in
  let operation_hex = operation_unsigned_hex ^ zero_signature_hex in
  Log.info
    "Injecting the forged evidence through the normal operation-injection RPC \
     path" ;
  let* injection =
    Lwt.catch
      (fun () ->
        let* op_hash =
          Client.RPC.call client
          @@ RPC.post_injection_operation (Data (`String operation_hex))
        in
        return (`Injected (JSON.as_string op_hash)))
      (fun _exn -> return `Rejected_at_injection)
  in
  match injection with
  | `Rejected_at_injection ->
      Log.info
        "FIX CONFIRMED: the forged double_baking_evidence was rejected at \
         injection time by the mempool pre-filter." ;
      unit
  | `Injected op_hash -> (
      Log.info
        "Operation %s accepted at injection; baking a block and checking it is \
         not applied"
        op_hash ;
      let* () = Client.bake_for_and_wait client in
      let* operations =
        Client.RPC.call client @@ RPC.get_chain_block_operations ()
      in
      (* double_baking_evidence is an anonymous operation: validation pass 2. *)
      let anonymous_ops = JSON.(operations |=> 2 |> as_list) in
      let found =
        List.find_opt
          (fun op -> JSON.(op |-> "hash" |> as_string) = op_hash)
          anonymous_ops
      in
      match found with
      | None ->
          Log.info
            "FIX CONFIRMED: the forged double_baking_evidence was not included \
             in the baked block (rejected by the mempool pre-filter / block \
             validation)." ;
          unit
      | Some op -> (
          let metadata = JSON.(op |-> "contents" |=> 0 |-> "metadata") in
          let punished_delegate =
            JSON.(metadata |-> "punished_delegate" |> as_string_opt)
          in
          match punished_delegate with
          | None ->
              Log.info
                "The forged evidence was included but applied no denunciation \
                 (no punished_delegate); no baker was slashed." ;
              unit
          | Some punished_delegate ->
              Test.fail
                "MITIGATION FAILED: forged double_baking_evidence built from a \
                 P256-signature-malleated honest block was applied and \
                 punished delegate %s -- the malleability fix is not \
                 effective."
                punished_delegate))

let register ~protocols =
  validate_block protocols ;
  propagate_validateable_bad_block protocols ;
  propagate_validateable_bad_block_payload protocols ;
  double_baking_evidence_p256_signature_malleability
    (List.filter (fun protocol -> Protocol.number protocol = 025) protocols)
