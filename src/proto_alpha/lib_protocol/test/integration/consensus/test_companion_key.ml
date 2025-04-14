(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Testing
    -------
    Component:  Protocol (delegate_storage)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/integration/consensus/main.exe \
                  -- --file test_companion_key.ml
    Subject:    Companion key test: registration, usage, etc
 *)

open Scenario

let check_cks delegate_name : t -> unit tzresult Lwt.t =
 fun (block, state) ->
  let current_cycle = current_cycle block in
  Account_helpers.assert_CKs
    ~loc:__LOC__
    (B block)
    current_cycle
    delegate_name
    state.State.account_map

let check_all_cks : t -> unit tzresult Lwt.t =
 fun (block, state) ->
  let all_delegates =
    String.Map.filter
      (fun name _ -> State.is_self_delegate name state)
      state.State.account_map
    |> String.Map.bindings |> List.map fst
  in
  let current_cycle = current_cycle block in
  List.iter_es
    (fun delegate_name ->
      Account_helpers.assert_CKs
        ~loc:__LOC__
        (B block)
        current_cycle
        delegate_name
        state.State.account_map)
    all_delegates

type key_status = Active | Pending | Unregistered

type key_kind = Protocol.Operation_repr.consensus_key_kind =
  | Consensus
  | Companion

let check_ck_status ~loc ~ck ~registered_for (status : key_status)
    (kind : key_kind) : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let delegate = State.find_account registered_for state in
      let ck = State.find_account ck state in
      let* info = Context.Delegate.info (B block) delegate.pkh in
      let {
        Delegate_services.active_consensus_key;
        active_companion_key;
        pending_consensus_keys;
        pending_companion_keys;
        _;
      } =
        info
      in
      match (status, kind) with
      | Active, Consensus ->
          Assert.equal
            ~loc
            Signature.Public_key_hash.equal
            "Active consensus key"
            Signature.Public_key_hash.pp
            active_consensus_key
            ck.pkh
      | Active, Companion -> (
          match active_companion_key with
          | None -> failwith "Companion key is not yet active"
          | Some active_companion_key ->
              Assert.equal
                ~loc
                Signature.Public_key_hash.equal
                "Active companion key"
                Signature.Public_key_hash.pp
                (Bls active_companion_key)
                ck.pkh)
      | Pending, Consensus ->
          Assert.is_true
            ~loc
            (List.exists
               (fun (_, pending_key) ->
                 Signature.Public_key_hash.equal pending_key ck.pkh)
               pending_consensus_keys)
      | Pending, Companion ->
          Assert.is_true
            ~loc
            (List.exists
               (fun (_, pending_key) ->
                 Signature.Public_key_hash.equal (Bls pending_key) ck.pkh)
               pending_companion_keys)
      | Unregistered, Consensus ->
          let* () =
            Assert.not_equal
              ~loc
              Signature.Public_key_hash.equal
              "Active consensus key"
              Signature.Public_key_hash.pp
              active_consensus_key
              ck.pkh
          in
          Assert.is_true
            ~loc
            (List.for_all
               (fun (_, pending_key) ->
                 not (Signature.Public_key_hash.equal pending_key ck.pkh))
               pending_consensus_keys)
      | Unregistered, Companion ->
          let* () =
            match active_companion_key with
            | None -> return_unit
            | Some ack ->
                Assert.not_equal
                  ~loc
                  Signature.Public_key_hash.equal
                  "Active companion key"
                  Signature.Public_key_hash.pp
                  (Bls ack)
                  ck.pkh
          in
          Assert.is_true
            ~loc
            (List.for_all
               (fun (_, pending_key) ->
                 not (Signature.Public_key_hash.equal (Bls pending_key) ck.pkh))
               pending_companion_keys))

let check_no_active_companion ~loc ~delegate : (t, t) scenarios =
  let open Lwt_result_syntax in
  exec_unit (fun (block, state) ->
      let delegate = State.find_account delegate state in
      let* info = Context.Delegate.info (B block) delegate.pkh in
      Assert.is_none
        ~loc
        ~pp:Signature.Bls.Public_key_hash.pp
        info.active_companion_key)

let update_consensus_key_any_algo_with_fresh_key ~ck_name src =
  fold_tag
    (fun algo ->
      add_account ~algo ck_name --> update_consensus_key ~ck_name src)
    [
      ("Ed25519", Ed25519);
      ("Secp256k1", Secp256k1);
      ("P256", P256);
      ("BLS", Bls);
    ]

let update_companion_key_with_fresh_key ~ck_name src =
  add_account ~algo:Bls ck_name --> update_companion_key ~ck_name src

let test_simple_register_consensus_and_companion_keys =
  let bootstrap_accounts = ["bootstrap1"; "bootstrap2"] in
  let delegate = "delegate" in
  let new_ck = "ck" in
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let check_finalized_block = check_cks delegate in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> (Tag "is bootstrap"
       --> begin_test
             ~force_attest_all:true
             ~check_finalized_block
             (delegate :: bootstrap_accounts)
      |+ Tag "is created"
         --> begin_test
               ~force_attest_all:true
               ~check_finalized_block
               ~algo:Bls
               bootstrap_accounts
         --> add_account_with_funds
               delegate
               ~funder:(Stdlib.List.hd bootstrap_accounts)
               (Amount (Tez_helpers.of_mutez 2_000_000_000_000L))
         --> set_delegate delegate (Some delegate)
         --> (Tag "active" --> stake delegate Half
              --> wait_n_cycles consensus_rights_delay
             |+ Tag "inactive" --> noop))
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Active
        Consensus
  --> (Tag "update consensus key"
       --> update_consensus_key_any_algo_with_fresh_key ~ck_name:new_ck delegate
       (* It is not a companion key *)
       --> check_ck_status
             ~loc:__LOC__
             ~ck:new_ck
             ~registered_for:delegate
             Unregistered
             Companion
       --> loop
             (consensus_rights_delay + 1)
             (check_ck_status
                ~loc:__LOC__
                ~ck:delegate
                ~registered_for:delegate
                Active
                Consensus
             --> check_ck_status
                   ~loc:__LOC__
                   ~ck:new_ck
                   ~registered_for:delegate
                   Pending
                   Consensus
             --> next_cycle)
       (* After consensus_rights_delay + 1 cycles, new key becomes active, and old key
          becomes unregistered *)
       --> check_ck_status
             ~loc:__LOC__
             ~ck:delegate
             ~registered_for:delegate
             Unregistered
             Consensus
       --> check_ck_status
             ~loc:__LOC__
             ~ck:new_ck
             ~registered_for:delegate
             Active
             Consensus
      |+ Tag "update companion key"
         --> update_companion_key_with_fresh_key ~ck_name:new_ck delegate
             (* It is not a consensus key *)
         --> check_ck_status
               ~loc:__LOC__
               ~ck:new_ck
               ~registered_for:delegate
               Unregistered
               Consensus
         --> loop
               (consensus_rights_delay + 1)
               (check_ck_status
                  ~loc:__LOC__
                  ~ck:delegate
                  ~registered_for:delegate
                  Active
                  Consensus
               --> check_ck_status
                     ~loc:__LOC__
                     ~ck:new_ck
                     ~registered_for:delegate
                     Pending
                     Companion
               --> check_no_active_companion ~loc:__LOC__ ~delegate
               --> next_cycle)
         (* After consensus_rights_delay + 1 cycles, new key becomes active *)
         --> check_ck_status
               ~loc:__LOC__
               ~ck:delegate
               ~registered_for:delegate
               Active
               Consensus
         --> check_ck_status
               ~loc:__LOC__
               ~ck:new_ck
               ~registered_for:delegate
               Active
               Companion)

let test_register_other_accounts_as_ck =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~algo:Bls
        ~check_finalized_block:check_all_cks
        ~force_attest_all:true
        ["delegate"; "victim_1"; "victim_2"]
  (* Both victims start with themselves as their own consensus_keys *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ err ->
          Error_helpers.check_error_constructor_name
            ~loc:__LOC__
            ~expected:
              Protocol.Delegate_consensus_key
              .Invalid_consensus_key_update_active
            err)
        (update_consensus_key ~ck_name:"victim_1" "delegate")
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ err ->
          Error_helpers.check_error_constructor_name
            ~loc:__LOC__
            ~expected:
              Protocol.Delegate_consensus_key
              .Invalid_consensus_key_update_active
            err)
        (update_companion_key ~ck_name:"victim_2" "delegate")
  (* ... So we give them other consensus keys.
     Note that the algo is not defined, so it is chosen at random. *)
  --> add_account "consensus_key_1"
  --> update_consensus_key ~ck_name:"consensus_key_1" "victim_1"
  --> add_account "consensus_key_2"
  --> update_consensus_key ~ck_name:"consensus_key_2" "victim_2"
  (* We do not need to wait to steal the newly available keys *)
  --> update_consensus_key ~ck_name:"victim_1" "delegate"
  --> update_companion_key ~ck_name:"victim_2" "delegate"
  (* Check that everything is as expected:
     - both victims have themselves as active consensus keys,
       and have a pending companion key each
     - The delegate has two keys pending
     - After the consensus rights delay, all pendings become active,
       and the victims do not use their own keys as consensus keys. *)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_1"
        ~registered_for:"victim_1"
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_2"
        ~registered_for:"victim_2"
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key_1"
        ~registered_for:"victim_1"
        Pending
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key_2"
        ~registered_for:"victim_2"
        Pending
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_1"
        ~registered_for:"delegate"
        Pending
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_2"
        ~registered_for:"delegate"
        Pending
        Companion
  (* Activation *)
  --> wait_n_cycles (consensus_rights_delay + 1)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_1"
        ~registered_for:"victim_1"
        Unregistered
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_2"
        ~registered_for:"victim_2"
        Unregistered
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key_1"
        ~registered_for:"victim_1"
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key_2"
        ~registered_for:"victim_2"
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_1"
        ~registered_for:"delegate"
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"victim_2"
        ~registered_for:"delegate"
        Active
        Companion
  (* Bake a cycle, for fun. Note that for each block, the RPCs also check for the complete
     status of registration of consensus and companion keys, and all delegates attest
     with their active consensus key. In this new cycle, the newly activated keys are thus
     used for the first time. *)
  --> next_cycle

let test_self_register_as_companion =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let check_finalized_block = check_cks delegate in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~algo:Bls
        ~force_attest_all:true
        ~check_finalized_block
        [delegate]
  (* As expected, a delegate cannot register itself as a companion,
     if it is already itself its own consensus key *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ err ->
          Error_helpers.check_error_constructor_name
            ~loc:__LOC__
            ~expected:
              Protocol.Delegate_consensus_key
              .Invalid_consensus_key_update_active
            err)
        (update_companion_key ~ck_name:delegate delegate)
  (* We can change that *)
  --> add_account "consensus_key"
  --> update_consensus_key ~ck_name:"consensus_key" delegate
  (* Freed from the consensus obligations, delegate can now focus on
     its true calling: companionship. However, self-centered as ever,
     it decided that its companion would be... itself *)
  --> update_companion_key ~ck_name:delegate delegate
  (* Checks *)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key"
        ~registered_for:delegate
        Pending
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Pending
        Companion
  (* Activate *)
  --> wait_n_cycles (consensus_rights_delay + 1)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Unregistered
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key"
        ~registered_for:delegate
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Active
        Companion
  (* Bake a cycle for auto checks *)
  --> next_cycle
  (* After a while, delegate realised that companionship is better shared.
     So, in a surprising turn of events, it went back to the consensus, to make
     room for a new companion in its storage. *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun _ err ->
          Error_helpers.check_error_constructor_name
            ~loc:__LOC__
            ~expected:
              Protocol.Delegate_consensus_key
              .Invalid_consensus_key_update_active
            err)
        (update_consensus_key ~ck_name:delegate delegate)
  --> add_account ~algo:Bls "companion_key"
  --> update_companion_key ~ck_name:"companion_key" delegate
  --> update_consensus_key ~ck_name:delegate delegate
  (* Checks *)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key"
        ~registered_for:delegate
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Active
        Companion
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Pending
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"companion_key"
        ~registered_for:delegate
        Pending
        Companion
  (* Activation *)
  --> wait_n_cycles (consensus_rights_delay + 1)
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"consensus_key"
        ~registered_for:delegate
        Unregistered
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Unregistered
        Companion
  --> check_ck_status
        ~loc:__LOC__
        ~ck:delegate
        ~registered_for:delegate
        Active
        Consensus
  --> check_ck_status
        ~loc:__LOC__
        ~ck:"companion_key"
        ~registered_for:delegate
        Active
        Companion
  --> next_cycle

let tests =
  tests_of_scenarios
  @@ [
       ( "Simple update ck for delegate",
         test_simple_register_consensus_and_companion_keys );
       ("Register other accounts as ck", test_register_other_accounts_as_ck);
       ("Self register as companion", test_self_register_as_companion);
     ]

let () =
  register_tests
    ~__FILE__
    ~tags:
      ["protocol"; "scenario"; "consensus"; "bls"; "attestation"; "companion"]
    tests
