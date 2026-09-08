(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Michelson / Base58 decode bound
   Invocation:   dune exec tezt/tests/main.exe -- --file base58_decode_bound.ml
   Subject:      Behaviour of a Michelson base58 constant whose textual
                 representation is longer than the base58 decode bound
                 ([Base58.encoded_max_length] = 8192), on Ushuaia (U025, a
                 frozen, deployed protocol) and Alpha (the current
                 development protocol, where the fix's default bound
                 applies unconditionally).

   Consensus invariant under test
   ------------------------------
   U025 is a frozen, deployed protocol. The default 8192 bound added to base58
   decoding MUST NOT change the outcome of decoding any Michelson base58
   constant on U025 relative to the deployed (pre-fix) binary: a different
   accept/reject decision on a live protocol is a consensus break (a node on the
   new binary would fork off). "Rejecting an oversized constant" is therefore
   *not* an intrinsically correct result for U025 — only "the same result the
   deployed protocol produced" is.

   Two distinct decode surfaces are reached by Michelson base58 constants:

   - [key] / [key_hash] / [signature] / [chain_id] decode through
     [Signature.*.of_b58check] / [Chain_id.of_b58check], i.e. the shell crypto
     library, which is bounded (8192) for *every* protocol regardless of the
     environment version. On U025 this surface is *newly* bounded, but it is
     result-preserving: every registered Base58Check encoding is at most 5332
     characters (< 8192), so the bound never rejects a valid input — only inputs
     that were already invalid (and rejected) on the deployed protocol.

   - [address] / [contract] / [sc_rollup] / [zk_rollup] decode through
     [Destination.of_b58check] -> the protocol *environment's* [Base58.decode].
     Every protocol environment (U025's V16 included) wraps it with the same
     bound, unconditionally: no environment passes a way to opt out, since
     the bound never rejects a valid input for any registered encoding (see
     below), so there is nothing "frozen protocol" specific to preserve here.

   BLS12-381 (g1/g2/fr) and sapling literals are bytes/hex, not base58, so they
   do not reach any base58 decoder.

   That U025 is byte-for-byte result-identical to the pre-fix binary is verified
   by a differential baseline-vs-current battery documented in the MR (valid
   keys/hashes/signatures/chain-ids for all four curves accepted by both; every
   boundary length 5333/8191/8192/8193/9000 and oversized 50k/100k input, for
   key/key_hash/signature/chain_id/address, rejected identically by both). This
   test is the permanent regression guard for the same invariant.

   Forged-payload regression guard
   --------------------------------
   [Base58.register_encoding] used to wrap every encoding's [of_raw] with
   [assert (String.length s = length)], where [length] is the encoding's raw
   (pre-base58, pre-checksum) payload size. A string that is syntactically
   valid base58, has a *correct* checksum, and the *right* registered prefix,
   but the *wrong* payload length, reached that assert and crashed the
   process with [Assert_failure] instead of returning [None]. The wrapper now
   returns [None] on a payload of the wrong length, for every decode surface
   and every protocol.

   The text length bounds do not close this on their own, so both directions
   are covered:

   - an *oversized* payload ([forged_oversized_*]) gives a text longer than
     the encoding's [encoded_length] ([Base58.simple_decode], e.g.
     [chain_id]) or than [max_encoded_size ()] (the generic [Base58.decode],
     e.g. [key], [address]): it is rejected by the bound before decoding;

   - an *undersized* payload ([forged_undersized_*]) gives a text *shorter*
     than [encoded_length], which no text bound rejects: it decodes, passes
     the checksum and prefix checks, and is rejected only by the payload
     length check in [of_raw].

   [chain_id] goes through [Script_chain_id.of_b58check_opt] ->
   [Chain_id.of_b58check_opt], built via [Helpers.MakeB58] straight over
   [Tezos_crypto.Chain_id]'s own [b58check_encoding], i.e. [simple_decode] in
   the shell crypto library. [key] and [key_hash] go through
   [Signature.Public_key(_hash).of_b58check_opt], *curve unions*
   (Ed25519/Secp256k1/P256/Bls/Mldsa44) implemented via the *generic*
   [Base58.decode] (tries every registered encoding by matching the decoded
   raw-byte prefix). [address] goes through [Destination.of_b58check] -> the
   protocol environment's [Base58.decode]. *)

let team = Tag.layer1

(* bootstrap1's public key (a valid, in-bound ed25519 key). *)
let valid_key = "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav"

(* A valid, in-bound address (bootstrap1's implicit account). *)
let valid_address = Constant.bootstrap1.public_key_hash

(* A syntactically valid base58 string ([z] is in the default alphabet) of the
   requested length. Above 8192 it exceeds [Base58.encoded_max_length]. *)
let oversized n = String.make n 'z'

(* A checksum-valid, correctly [chain_id]-prefixed base58check string whose
   payload is 1000 bytes instead of the canonical 4 -- see the "Forged-payload
   regression guard" note above. [safe_encode] appends a correct checksum for
   whatever raw string it is given, so this is *not* random garbage: it
   decodes past the checksum and (raw-byte) prefix checks, and would have
   reached the [assert] in the pre-fix code. Its own base58 *text* need not
   start with [chain_id]'s usual "Net" prefix -- changing the payload's byte
   length shifts the encoded integer's magnitude class entirely, which
   generally changes every leading base58 digit, not just trailing ones; what
   matters is that the *decoded raw bytes* start with the right prefix, which
   [TzString.remove_prefix] in [simple_decode] checks after decoding, not
   before. *)
let forged_oversized_chain_id =
  Tezos_crypto.Base58.safe_encode
    (Tezos_crypto.Base58.Prefix.chain_id ^ String.make 1000 'A')

let key_store_script =
  "parameter unit;\nstorage key;\ncode { CDR; NIL operation; PAIR }\n"

let chain_id_store_script =
  "parameter unit;\nstorage chain_id;\ncode { CDR; NIL operation; PAIR }\n"

let test_decode_bound =
  Protocol.register_test
    ~__FILE__
    ~title:"Base58 decode bound: guarded rejection and U025 result-preservation"
    ~tags:[team; "client"; "michelson"; "base58"; "security"]
    ~uses_node:false
    ~supports:(Protocol.Between_protocols (025, 026))
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script = Temp.file "key_store.tz" in
  let* () = Helpers.write_file ~contents:key_store_script script in
  (* 1. Result-preservation guard. A valid, in-bound constant must still
     typecheck. On the frozen protocol U025 a regression here (e.g. lowering the
     cap below a valid encoding length) would be a consensus break. *)
  let* () = Client.typecheck_data ~data:(sf "%S" valid_key) ~typ:"key" client in
  let* () =
    Client.typecheck_data ~data:(sf "%S" valid_address) ~typ:"address" client
  in
  (* The same, carried by an actual operation (origination): a valid key
     constant in the initial storage is accepted on both protocols. *)
  let* _addr =
    Client.originate_contract
      ~alias:"b58_bound_ok"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:script
      ~init:(sf "%S" valid_key)
      ~burn_cap:(Tez.of_int 10)
      client
  in
  (* 2. [key] constant (of_b58check surface, bounded on every protocol). A large
     oversized constant is rejected without running the quadratic decoder. On
     Alpha this is the guard against the DoS; on U025 it is the deployed
     behaviour (the input is invalid and was already rejected there). The size
     is safe on both because this surface is bounded on both. *)
  let* () =
    Client.spawn_originate_contract
      ~alias:"b58_bound_bad"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:script
      ~init:(sf "%S" (oversized 100_000))
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error
         ~msg:(rex "invalid for type key|expected a valid public key")
  in
  let* () =
    Client.spawn_typecheck_data
      ~data:(sf "%S" (oversized 100_000))
      ~typ:"key"
      client
    |> Process.check_error ~msg:(rex "invalid for type key")
  in
  (* 3. [address] constant (Destination -> environment [Base58.decode]),
     bounded unconditionally on every protocol: a large oversized input is
     rejected without running the quadratic decoder, closing the DoS on
     every protocol including the frozen U025. *)
  let* () =
    Client.spawn_typecheck_data
      ~data:(sf "%S" (oversized 100_000))
      ~typ:"address"
      client
    |> Process.check_error ~msg:(rex "invalid for type address")
  in
  unit

(* Checksum-valid, correctly (raw-byte-)prefixed base58check strings whose
   payload is one byte *shorter* than the canonical one -- see the
   "Forged-payload regression guard" note above. Their text is shorter than
   the encoding's [encoded_length], so no text length bound rejects them:
   they decode, pass the checksum and prefix checks, and reach the payload
   length check in [Base58.register_encoding]'s [of_raw] wrapper (formerly an
   [assert]). *)
let forged_undersized_chain_id =
  (* A [chain_id] is 4 bytes. *)
  Tezos_crypto.Base58.safe_encode
    (Tezos_crypto.Base58.Prefix.chain_id ^ String.make 3 'A')

let forged_undersized_key =
  (* An Ed25519 public key is 32 bytes. *)
  Tezos_crypto.Base58.safe_encode
    (Tezos_crypto.Base58.Prefix.ed25519_public_key ^ String.make 31 'A')

let forged_undersized_address =
  (* An Ed25519 public key hash is 20 bytes. *)
  Tezos_crypto.Base58.safe_encode
    (Tezos_crypto.Base58.Prefix.ed25519_public_key_hash ^ String.make 19 'A')

(* Undersized counterpart of [test_forged_payload]: an undersized forged
   [chain_id], [key] or [address] constant must be rejected the same way as
   any other invalid constant of its type, at typecheck and at origination,
   and must not crash the client with [Assert_failure]. *)
let test_forged_undersized_payload =
  Protocol.register_test
    ~__FILE__
    ~title:"Base58 decode bound: forged undersized payload does not crash"
    ~tags:[team; "client"; "michelson"; "base58"; "security"; "forged"]
    ~uses_node:false
    ~supports:(Protocol.Between_protocols (025, 026))
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let check_rejected ~what ~typ process =
    let* stderr = Process.check_and_read_stderr ~expect_failure:true process in
    if stderr =~ rex "Assert_failure|Assertion failed" then
      Test.fail
        "%s: the forged undersized %s constant crashed the client:\n%s"
        what
        typ
        stderr ;
    if not (stderr =~ rex (sf "invalid for type %s" typ)) then
      Test.fail
        "%s: the forged undersized %s constant was not rejected as an invalid \
         %s:\n\
         %s"
        what
        typ
        typ
        stderr ;
    unit
  in
  Lwt_list.iter_s
    (fun (typ, forged) ->
      Log.info "Forged undersized %s: %s" typ forged ;
      let script = Temp.file (sf "%s_store_undersized.tz" typ) in
      let* () =
        Helpers.write_file
          ~contents:
            (sf
               "parameter unit;\n\
                storage %s;\n\
                code { CDR; NIL operation; PAIR }\n"
               typ)
          script
      in
      let* () =
        Client.spawn_typecheck_data ~data:(sf "%S" forged) ~typ client
        |> check_rejected ~what:"typecheck" ~typ
      in
      Client.spawn_originate_contract
        ~alias:(sf "b58_bound_forged_undersized_%s" typ)
        ~amount:Tez.zero
        ~src:Constant.bootstrap1.alias
        ~prg:script
        ~init:(sf "%S" forged)
        ~burn_cap:(Tez.of_int 10)
        client
      |> check_rejected ~what:"origination" ~typ)
    [
      ("chain_id", forged_undersized_chain_id);
      ("key", forged_undersized_key);
      ("address", forged_undersized_address);
    ]

(* Forged-payload guard (see the module comment): a checksum- and
   (raw-byte-)prefix-valid but wrong-payload-length [chain_id] constant must
   be rejected the same way as any other invalid [chain_id] -- not crash the
   client with [Assert_failure]. [Process.check_error] only matches a
   controlled tzresult-style error message; an uncaught exception's
   backtrace would not match, so this fails loudly if the assert is ever
   reached again on this surface. Run on both protocols: the bound applies
   unconditionally on every protocol (see the module comment), so both
   must behave identically here.

   Kept as its own test (rather than folded into [test_decode_bound]) so it
   can be run in isolation, e.g.:
     dune exec tezt/tests/main.exe -- --title 'forged'
*)
let test_forged_payload =
  Protocol.register_test
    ~__FILE__
    ~title:"Base58 decode bound: forged wrong-length payload does not crash"
    ~tags:[team; "client"; "michelson"; "base58"; "security"; "forged"]
    ~uses_node:false
    ~supports:(Protocol.Between_protocols (025, 026))
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let chain_id_script = Temp.file "chain_id_store.tz" in
  let* () =
    Helpers.write_file ~contents:chain_id_store_script chain_id_script
  in
  let* () =
    Client.spawn_originate_contract
      ~alias:"b58_bound_forged"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:chain_id_script
      ~init:(sf "%S" forged_oversized_chain_id)
      ~burn_cap:(Tez.of_int 10)
      client
    |> Process.check_error ~msg:(rex "invalid for type chain_id")
  in
  let* () =
    Client.spawn_typecheck_data
      ~data:(sf "%S" forged_oversized_chain_id)
      ~typ:"chain_id"
      client
    |> Process.check_error ~msg:(rex "invalid for type chain_id")
  in
  unit

(* Real-node counterpart to [test_forged_payload]: what is the actual
   RESULT of trying to APPLY the forged, checksum-valid, wrong-payload-length
   [chain_id] operation against a REAL node (client -> RPC ->
   [src/lib_shell/prevalidation.ml], which wraps operation validation in
   [protect], i.e. [Lwt.catch] over the whole chain) -- is it accepted into
   a block, or definitively rejected the same way any other malformed
   operation would be (not some special/different state)? And does the node
   keep progressing normally afterward, with the contract correctly NOT
   originated?

   Only run on U025, the deployed protocol. The node, client and baker are
   the ones built from this tree, i.e. with the default decode bound and the
   payload length check in [Base58.register_encoding]'s [of_raw] wrapper
   (formerly an [assert], which this forgery reached on the pre-fix code).
   The operation must be rejected, the client must not crash with
   [Assert_failure], and the chain must keep progressing. (Whether an
   uncaught exception deep in an external validator subprocess could itself
   misbehave is a separate question; this test only checks the observable
   RPC/CLI-level application result and post-injection chain liveness.) *)
let test_forged_payload_real_node =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Base58 decode bound: forged payload application result on a real node"
    ~tags:[team; "client"; "node"; "baker"; "michelson"; "base58"; "security"]
    ~uses:(fun _ -> [Constant.octez_agnostic_baker])
    ~supports:(Protocol.Between_protocols (025, 025))
  @@ fun protocol ->
  let* node, client =
    Client.init_with_protocol `Client ~protocol () ~timestamp:Now
  in
  let* baker = Agnostic_baker.init node client in
  let* (_ : int) = Node.wait_for_level node 2 in
  Log.info "Node and baker are up and producing blocks (reached level 2)." ;
  let chain_id_script = Temp.file "chain_id_store_real_node.tz" in
  let* () =
    Helpers.write_file ~contents:chain_id_store_script chain_id_script
  in
  let alias = "b58_bound_forged_real" in
  Log.info
    "Injecting the forged, oversized, checksum-valid chain_id operation \
     against the real node (non-mockup client -> real RPC injection through \
     prevalidation) ..." ;
  let origination_process =
    Client.spawn_originate_contract
      ~alias
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:chain_id_script
      ~init:(sf "%S" forged_oversized_chain_id)
      ~burn_cap:(Tez.of_int 10)
      client
  in
  (* Deliberately not asserting a specific error message here: we don't yet
     know, a priori, whether the client's own local dry-run/simulation
     step hits this bug before ever reaching the node, or whether it is the
     node's RPC handler that does -- only that the operation must NOT be
     accepted. [check_and_read_stderr ~expect_failure:true] tolerates any
     failure reason and gives us the raw text to inspect. *)
  let* stderr =
    Process.check_and_read_stderr ~expect_failure:true origination_process
  in
  Log.info "Raw client stderr from the origination attempt:\n%s" stderr ;
  let is_uncaught_assert =
    let contains sub =
      let re = Str.regexp_string sub in
      try
        ignore (Str.search_forward re stderr 0) ;
        true
      with Not_found -> false
    in
    contains "Assert_failure" || contains "Assertion failed"
  in
  Log.info
    "APPLICATION RESULT: the origination operation was REJECTED (client exit \
     code non-zero). is_uncaught_assert_in_client_stderr=%b -- see the raw \
     stderr above for the exact wording actually returned."
    is_uncaught_assert ;
  if is_uncaught_assert then
    Test.fail
      "The forged chain_id origination crashed the client with an uncaught \
       assertion instead of being cleanly rejected." ;
  Log.info "Confirming the contract was NOT originated ..." ;
  let* contract_registered =
    Lwt.catch
      (fun () ->
        let* (_ : Account.key) = Client.show_address ~alias client in
        Lwt.return_true)
      (fun _ -> Lwt.return_false)
  in
  if contract_registered then
    Test.fail
      "Contract alias %S is registered client-side even though origination was \
       rejected -- inconsistent state."
      alias
  else
    Log.info
      "Confirmed: no contract/alias %S was registered -- the rejected \
       operation left no trace, consistent with any other rejected \
       origination."
      alias ;
  let* level_before = Node.get_level node in
  Log.info "Level right after the rejected injection: %d" level_before ;
  Node.on_event node (fun {name; value; _} ->
      Log.info "[node-event] %s: %s" name (JSON.encode value)) ;
  Agnostic_baker.on_event baker (fun {name; value; _} ->
      Log.info "[baker-event] %s: %s" name (JSON.encode value)) ;
  Log.info
    "Confirming liveness: the chain keeps progressing normally after the \
     rejected injection (waiting for just +1 level, with full event logging \
     on, to see exactly what happens meanwhile) ..." ;
  let* (_ : int) = Node.wait_for_level node (level_before + 1) in
  Log.info
    "Node reached level %d (was %d right after the injection attempt) -- \
     baking continued normally."
    (level_before + 1)
    level_before ;
  let* () = Agnostic_baker.terminate baker in
  unit

(* A distinct *runtime* surface for the very same Base58 quadratic-decode
   defect: [UNPACK chain_id]. The PoC below (reported as a script that makes
   the interpreter "run forever") builds, at runtime, a [bytes] value of
   length 2^N by repeated [CONCAT] doubling, prepends the packed-Micheline
   prefix for a [String] of that length (0x05 pack magic, 0x01 String tag,
   4-byte length), and calls [UNPACK chain_id] on it. [UNPACK] deserializes
   the packed [String] node and hands its textual content to the [chain_id]
   parser, i.e. [Chain_id.of_b58check] -- the exact Base58 decoder that
   [test_decode_bound] exercises through *source* constants. The crucial
   difference is that here the oversized Base58 string is produced *inside the
   interpreter* and never appears in the script source, so none of the
   typecheck-/origination-time tests above reach it: this is a new, runtime
   surface of the same bug.

   The upfront [UNPACK] gas is charged linearly in the byte length
   ([cost_DECODING_MICHELINE_bytes] = 20/byte), but the Base58 decode of the
   string content is O(n^2) and *unmetered*: gas is consumed before the decode
   (the linear upfront charge) and after it (the [unpack_failed] charge), never
   during. The gas limit therefore cannot interrupt the quadratic decode -- it
   only decides whether the operation is ultimately rejected, once the CPU has
   already been spent. An attacker gets quadratic CPU for linear gas.

   Crucially, the gas model does NOT stop this, not even at the normal block
   limit (hard_gas_limit_per_operation = 1_040_000). Measured on a mockup, both
   U025 and Alpha, with an unbounded (pre-fix) build and gas pinned to that
   block limit:

     N=16 (64 KiB)  -> ~5s of Base58 decode, then gas-fail
     N=18 (256 KiB) -> ~73s, then gas-fail
     N=19 (512 KiB) -> minutes

   The wall time at a fixed N did not move when the gas limit was raised from 1M
   to 100M, confirming gas does not gate the work. The upfront linear charge is
   the only thing that eventually caps the reachable size on chain (~N=19 under
   the block limit), but that still buys minutes of validator CPU from a single
   operation. Note the contrast: generic UNPACK targets ([int], [string], lists,
   nested terms) do NOT reach Base58 and *are* handled by the gas model; only
   the Base58-backed targets ([chain_id]/[key]/[key_hash]/[address]/[signature])
   hit this unmetered path. With the bounded default the same call is rejected
   in ~0.35s *independently of N*, because the length check short-circuits before
   the quadratic decoder runs: the Base58 bound, not gas, is what caps the work.

   Regression guard: run the PoC at N=18 (a 2^18 = 256 KiB string) under the
   real block gas limit, on both U025 (frozen, deployed) and Alpha. With the
   bound the client rejects it in well under a second ([Gas limit exceeded]).
   Without the bound the same call runs the 256 KiB quadratic Base58 decode for
   ~73s. An [Lwt] watchdog (30s) turns that into an explicit failure instead of
   a silent test-wide timeout, and comfortably separates the ~0.35s bounded case
   from the ~73s regressed one.

   The unbounded decoder is deliberately *not* exercised here: doing so would
   run the quadratic decode (a real DoS) in CI. Its effect is documented
   above. *)
let unpack_chain_id_quadratic_script =
  "parameter int ;\n\
   storage unit ;\n\
   code { CAR ; PUSH bytes 0x7a ; SWAP ; DUP ; GT ; LOOP { SWAP ; DUP ; CONCAT \
   ; SWAP ; PUSH int -1 ; ADD ; DUP ; GT ; } ; DROP ; DUP ; SIZE ; BYTES ; \
   PUSH bytes 0x050100000000 ; OR ; CONCAT ; UNPACK chain_id ; DROP ; UNIT ; \
   NIL operation ; PAIR }\n"

let test_unpack_chain_id_decode_bound =
  Protocol.register_test
    ~__FILE__
    ~title:
      "Base58 decode bound: UNPACK chain_id quadratic decode is short-circuited"
    ~tags:[team; "client"; "michelson"; "base58"; "security"; "unpack"]
    ~uses_node:false
    ~supports:(Protocol.Between_protocols (025, 026))
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let script = Temp.file "unpack_chain_id_quadratic.tz" in
  let* () =
    Helpers.write_file ~contents:unpack_chain_id_quadratic_script script
  in
  (* 2^18 = 256 KiB string, run under the real block gas limit -- a realistic
     single-operation attack, not an artificially high gas budget. With the
     bound the client rejects it in ~0.35s; a *regressed* (unbounded) binary
     runs the 256 KiB quadratic Base58 decode for ~73s (the linear upfront gas
     charge passes at this size, so gas does not stop it). *)
  let n = "18" in
  let hard_gas_limit_per_operation = 1_040_000 in
  let watchdog_seconds = 30. in
  let process =
    Client.spawn_run_script
      ~prg:script
      ~storage:"Unit"
      ~input:n
      ~gas:hard_gas_limit_per_operation
      client
  in
  let rejected =
    let* () =
      Process.check_error
        ~msg:(rex "Gas limit exceeded|invalid for type chain_id")
        process
    in
    Lwt.return `Rejected
  in
  let watchdog =
    let* () = Lwt_unix.sleep watchdog_seconds in
    Lwt.return `Timeout
  in
  let* result = Lwt.pick [rejected; watchdog] in
  match result with
  | `Rejected ->
      Log.info
        "UNPACK chain_id on a 2^%s-byte string was rejected promptly -- the \
         Base58 decode bound short-circuited the quadratic decoder."
        n ;
      unit
  | `Timeout ->
      Process.kill process ;
      Test.fail
        "UNPACK chain_id on a 2^%s-byte string did not terminate within %.0fs: \
         the Base58 decode bound appears to have regressed and the unmetered \
         quadratic decoder is running (pre-fix DoS)."
        n
        watchdog_seconds

let register ~protocols =
  test_forged_undersized_payload protocols ;
  test_decode_bound protocols ;
  test_unpack_chain_id_decode_bound protocols ;
  test_forged_payload protocols ;
  test_forged_payload_real_node protocols
