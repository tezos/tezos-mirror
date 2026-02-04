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

let check_error_invalid_consensus_key_update_active ~loc ~pkh ~kind errs =
  Assert.expect_error ~loc errs (function
    | [
        Protocol.Delegate_consensus_key.Invalid_consensus_key_update_active
          (err_pkh, err_kind);
      ]
      when Signature.Public_key_hash.equal pkh err_pkh && kind = err_kind ->
        true
    | _ -> false)

let check_error_invalid_consensus_key_update_another_delegate ~loc ~pkh ~kind
    errs =
  Assert.expect_error ~loc errs (function
    | [
        Protocol.Delegate_consensus_key
        .Invalid_consensus_key_update_another_delegate
          (err_pkh, err_kind);
      ]
      when Signature.Public_key_hash.equal pkh err_pkh && kind = err_kind ->
        true
    | _ -> false)

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

(* Should only be used with tz4 *)
let update_key ?proof_signer ?force_no_signer ~kind ~ck_name src =
  match kind with
  | Consensus ->
      update_consensus_key ?proof_signer ?force_no_signer ~ck_name src
  | Companion ->
      update_companion_key ?proof_signer ?force_no_signer ~ck_name src

let test_init_with_cks_for_bootstraps =
  let check_finalized_every_block = [(fun _ -> check_all_cks)] in
  init_constants ()
  --> begin_test
        ~check_finalized_every_block
        ~force_attest_all:true
        ~bootstrap_info_list:
          [
            make "no_ck";
            make "with_consensus_key" ~consensus_key:(Some Not_Mldsa44);
            make "with_companion_key" ~companion_key:true;
            make "with_both_tz4" ~consensus_key:(Some Bls) ~companion_key:true;
          ]
        []
  --> exec_unit check_all_cks
  (* Bake a bit, check at each block *)
  --> next_block
  --> next_block
  (* With some DAL, to test the companion key *)
  --> exec_state (fun (_block, state) ->
          Lwt_result.return {state with State.force_attest_all = false})
  --> attest_aggreg_with ~delegates_with_dal:[("with_both_tz4", Z.of_int 7)] []
  --> next_block

let test_simple_register_consensus_and_companion_keys =
  let bootstrap_accounts = ["bootstrap1"; "bootstrap2"] in
  let delegate = "delegate" in
  let new_ck = "ck" in
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> (Tag "is bootstrap"
       --> begin_test
             ~force_attest_all:true
             ~check_finalized_every_block
             (delegate :: bootstrap_accounts)
      |+ Tag "is created"
         --> begin_test
               ~force_attest_all:true
               ~check_finalized_every_block
               ~default_algo:Bls
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
        ~default_algo:Bls
        ~check_finalized_every_block:[(fun _ -> check_all_cks)]
        ~force_attest_all:true
        ["delegate"; "victim_1"; "victim_2"]
  (* Both victims start with themselves as their own consensus_keys *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account "victim_1" state).pkh in
          check_error_invalid_consensus_key_update_active
            ~loc:__LOC__
            ~pkh
            ~kind:Consensus
            err)
        (update_consensus_key ~ck_name:"victim_1" "delegate")
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account "victim_2" state).pkh in
          check_error_invalid_consensus_key_update_active
            ~loc:__LOC__
            ~pkh
            ~kind:Companion
            err)
        (update_companion_key ~ck_name:"victim_2" "delegate")
  (* ... So we give them other consensus keys.
     Note that the algo is not defined, so it is chosen at random. *)
  --> add_account "consensus_key_1"
  --> update_consensus_key ~ck_name:"consensus_key_1" "victim_1"
  --> add_account "consensus_key_2"
  --> update_consensus_key ~ck_name:"consensus_key_2" "victim_2"
  (* We cannot steal the newly available keys *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account "victim_1" state).pkh in
          check_error_invalid_consensus_key_update_another_delegate
            ~loc:__LOC__
            ~pkh
            ~kind:Consensus
            err)
        (update_consensus_key ~ck_name:"victim_1" "delegate")
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account "victim_2" state).pkh in
          check_error_invalid_consensus_key_update_another_delegate
            ~loc:__LOC__
            ~pkh
            ~kind:Companion
            err)
        (update_companion_key ~ck_name:"victim_2" "delegate")

let test_self_register_as_companion =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  (* As expected, a delegate cannot register itself as a companion,
     if it is already itself its own consensus key *)
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account delegate state).pkh in
          check_error_invalid_consensus_key_update_active
            ~loc:__LOC__
            ~pkh
            ~kind:Companion
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
        ~expected_error:(fun (_block, state) err ->
          let pkh = (State.find_account delegate state).pkh in
          check_error_invalid_consensus_key_update_active
            ~loc:__LOC__
            ~pkh
            ~kind:Consensus
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

let test_register_same_key_multiple_times =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let ck = "ck" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  let update_either_ck ~ck_name delegate =
    Tag "consensus" --> update_consensus_key ~ck_name delegate
    |+ Tag "companion" --> update_companion_key ~ck_name delegate
  in
  let check_ck_cannot_be_registered =
    assert_failure
      ~loc:__LOC__
      ~expected_error:(fun (_block, state) err ->
        let delegate = State.find_account delegate state in
        let ck = State.find_account ck state in
        let cycle, ck_pkh = Account_helpers.latest_consensus_key delegate in
        if Signature.Public_key_hash.equal ck_pkh ck.pkh then
          Assert.expect_error ~loc:__LOC__ err (function
            | [
                Protocol.Delegate_consensus_key.Invalid_consensus_key_update_noop
                  (err_cycle, err_kind);
              ] ->
                Int32.equal
                  (Protocol.Cycle_repr.to_int32 err_cycle)
                  (State_account.Cycle.to_int32 cycle)
                && err_kind = Consensus
            | _ -> false)
        else
          check_error_invalid_consensus_key_update_active
            ~loc:__LOC__
            ~pkh:ck.pkh
            ~kind:Consensus
            err)
      (update_consensus_key ~ck_name:ck delegate)
    --> assert_failure
          ~loc:__LOC__
          ~expected_error:(fun (_block, state) err ->
            let delegate = State.find_account delegate state in
            let ck = State.find_account ck state in
            let cycle, ck_pkh = Account_helpers.latest_companion_key delegate in
            if
              Option.fold
                ~none:false
                ~some:(fun x -> Signature.Public_key_hash.equal ck.pkh (Bls x))
                ck_pkh
            then
              Assert.expect_error ~loc:__LOC__ err (function
                | [
                    Protocol.Delegate_consensus_key
                    .Invalid_consensus_key_update_noop
                      (err_cycle, err_kind);
                  ] ->
                    Int32.equal
                      (Protocol.Cycle_repr.to_int32 err_cycle)
                      (State_account.Cycle.to_int32 cycle)
                    && err_kind = Companion
                | _ -> false)
            else
              check_error_invalid_consensus_key_update_active
                ~loc:__LOC__
                ~pkh:ck.pkh
                ~kind:Companion
                err)
          (update_companion_key ~ck_name:ck delegate)
  in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  --> add_account ~algo:Bls ck
  --> update_either_ck ~ck_name:ck delegate
  (* The key cannot be registered again *)
  --> check_ck_cannot_be_registered
  --> wait_n_cycles (consensus_rights_delay + 1)
  --> check_ck_cannot_be_registered
  (* We can now register other keys, and ck can be registered again after that *)
  --> add_account "consensus_key"
  --> update_consensus_key ~ck_name:"consensus_key" delegate
  --> add_account ~algo:Bls "companion_key"
  --> update_companion_key ~ck_name:"companion_key" delegate
  --> update_either_ck ~ck_name:ck delegate
  --> next_cycle

let test_register_new_key_every_cycle =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  let update_both_cks delegate =
    add_account "consensus_key"
    --> update_consensus_key ~ck_name:"consensus_key" delegate
    --> add_account ~algo:Bls "companion_key"
    --> update_companion_key ~ck_name:"companion_key" delegate
  in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  --> loop (consensus_rights_delay + 2) (update_both_cks delegate --> next_cycle)

let test_register_key_end_of_cycle =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  --> add_account ~algo:Bls "ck"
  --> exec bake_until_next_cycle_end_but_one
  --> fold_tag
        (fun kind ->
          update_key ~kind ~ck_name:"ck" delegate
          --> wait_n_cycles (consensus_rights_delay + 1)
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck"
                ~registered_for:delegate
                Active
                kind)
        [("update consensus", Consensus); ("update companion", Companion)]
  --> next_cycle

let test_registration_override =
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  --> add_account ~algo:Bls "ck1"
  --> add_account ~algo:Bls "ck2"
  --> fold_tag
        (fun kind ->
          update_key ~kind ~ck_name:"ck1" delegate
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                Pending
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck2"
                ~registered_for:delegate
                Unregistered
                kind
          --> update_key ~kind ~ck_name:"ck2" delegate
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                Unregistered
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck2"
                ~registered_for:delegate
                Pending
                kind
          --> update_key ~kind ~ck_name:"ck1" delegate
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                Pending
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck2"
                ~registered_for:delegate
                Unregistered
                kind)
        [("update consensus", Consensus); ("update companion", Companion)]
  --> next_block

let test_in_registration_table_twice =
  (* We manually set consensus_rights_delay,
     to ensure all update operations are made without any key being activated.
     This ensures that a key can be pending for two different cycles at the same time. *)
  let consensus_rights_delay = 4 in
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  let check_is_pending_twice ~loc ~ck ~registered_for kind =
    let open Lwt_result_syntax in
    exec_unit (fun (block, state) ->
        let delegate = State.find_account registered_for state in
        let ck = State.find_account ck state in
        let* info = Context.Delegate.info (B block) delegate.pkh in
        let {
          Delegate_services.pending_consensus_keys;
          pending_companion_keys;
          _;
        } =
          info
        in
        let pending_list =
          (match kind with
          | Consensus -> pending_consensus_keys
          | Companion ->
              pending_companion_keys
              |> List.map (fun (cycle, key) ->
                     (cycle, (Signature.Bls key : Signature.public_key_hash))))
          |> List.map snd
        in
        let occurrences =
          List.filter (Signature.Public_key_hash.equal ck.pkh) pending_list
        in
        match occurrences with
        | _ :: _ :: _ -> return_unit
        | _ ->
            failwith
              "Given key %a is not registered for two different cycles for %a \
               (%s)"
              Signature.Public_key_hash.pp
              ck.pkh
              Signature.Public_key_hash.pp
              delegate.pkh
              loc)
  in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> set S.cache_stake_distribution_cycles (consensus_rights_delay + 3)
  --> set S.cache_sampler_state_cycles (consensus_rights_delay + 3)
  --> set S.cache_stake_info_cycles (consensus_rights_delay + 3)
  --> begin_test
        ~default_algo:Bls
        ~force_attest_all:true
        ~check_finalized_every_block
        [delegate]
  --> add_account ~algo:Bls "ck1"
  --> add_account ~algo:Bls "ck2"
  --> fold_tag
        (fun kind ->
          update_key ~kind ~ck_name:"ck1" delegate
          --> next_cycle
          --> update_key ~kind ~ck_name:"ck2" delegate
          --> next_cycle
          --> update_key ~kind ~ck_name:"ck1" delegate
          --> check_is_pending_twice
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck2"
                ~registered_for:delegate
                Pending
                kind
          --> wait_n_cycles (consensus_rights_delay - 1)
          (* ck1 is both active AND pending *)
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck2"
                ~registered_for:delegate
                Pending
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                Active
                kind
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck1"
                ~registered_for:delegate
                Pending
                kind)
        [("update consensus", Consensus); ("update companion", Companion)]
  --> next_block

let test_unregistered =
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true ["bootstrap"]
  (* This account is not a delegate *)
  --> add_account_with_funds ~funder:"bootstrap" "account" Half
  --> add_account ~algo:Bls "ck"
  --> fold_tag
        (fun kind ->
          assert_failure
            ~loc:__LOC__
            ~expected_error:(fun (_block, state) err ->
              let account = State.find_account "account" state in
              Assert.expect_error ~loc:__LOC__ err (function
                | [
                    Protocol.Apply.Update_consensus_key_on_unregistered_delegate
                      (err_account_pkh, err_kind);
                  ] ->
                    Signature.Public_key_hash.equal err_account_pkh account.pkh
                    && err_kind = kind
                | _ -> false))
            (update_key ~kind ~ck_name:"ck" "account"))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_forbidden_tz4 =
  let open Lwt_result_syntax in
  init_constants ()
  (* tz4 forbidden *)
  --> set S.allow_tz4_delegate_enable false
  --> begin_test ~force_attest_all:true ["delegate"]
  --> add_account ~algo:Bls "ck"
  --> fold_tag
        (fun kind ->
          assert_failure
            ~loc:__LOC__
            ~expected_error:(fun (_block, state) err ->
              let ck = State.find_account "ck" state in
              let* ck_account = Account.find ck.pkh in
              Assert.expect_error ~loc:__LOC__ err (function
                | [
                    Protocol.Delegate_consensus_key
                    .Invalid_consensus_key_update_tz4
                      (err_ck_bls_pk, err_kind);
                  ] ->
                    kind = err_kind
                    && Signature.Public_key.equal
                         (Bls err_ck_bls_pk)
                         ck_account.pk
                | _ -> false))
            (update_key ~kind ~ck_name:"ck" "delegate"))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_fail_noop =
  let consensus_rights_delay =
    Default_parameters.constants_mainnet.consensus_rights_delay
  in
  let delegate = "delegate" in
  let check_finalized_every_block = [(fun _ -> check_cks delegate)] in
  let assert_fail_with_invalid_consensus_key_update_noop kind =
    assert_failure ~loc:__LOC__ ~expected_error:(fun (_block, state) err ->
        let delegate = State.find_account delegate state in
        let cycle =
          match kind with
          | Consensus -> Account_helpers.latest_consensus_key delegate |> fst
          | Companion -> Account_helpers.latest_companion_key delegate |> fst
        in
        Assert.expect_error ~loc:__LOC__ err (function
          | [
              Protocol.Delegate_consensus_key.Invalid_consensus_key_update_noop
                (err_cycle, err_kind);
            ] ->
              Int32.equal
                (Protocol.Cycle_repr.to_int32 err_cycle)
                (State_account.Cycle.to_int32 cycle)
              && err_kind = kind
          | _ -> false))
  in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> set S.consensus_rights_delay consensus_rights_delay
  --> begin_test ~force_attest_all:true ~check_finalized_every_block [delegate]
  --> add_account ~algo:Bls "ck"
  --> fold_tag
        (fun kind ->
          update_key ~kind ~ck_name:"ck" delegate
          (* Cannot register a pending key *)
          --> loop
                (consensus_rights_delay + 1)
                (check_ck_status
                   ~loc:__LOC__
                   ~ck:"ck"
                   ~registered_for:delegate
                   Pending
                   kind
                --> assert_fail_with_invalid_consensus_key_update_noop
                      kind
                      (update_key ~kind ~ck_name:"ck" delegate)
                --> next_cycle)
          (* Once it's active: same error *)
          --> check_ck_status
                ~loc:__LOC__
                ~ck:"ck"
                ~registered_for:delegate
                Active
                kind
          --> assert_fail_with_invalid_consensus_key_update_noop
                kind
                (update_key ~kind ~ck_name:"ck" delegate))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_fail_already_registered =
  let delegate = "delegate" in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true ~default_algo:Bls [delegate; "ck"]
  --> fold_tag
        (fun kind ->
          assert_failure
            ~loc:__LOC__
            ~expected_error:(fun (_block, state) err ->
              let pkh = (State.find_account "ck" state).pkh in
              check_error_invalid_consensus_key_update_active
                ~loc:__LOC__
                ~pkh
                ~kind
                err)
            (update_key ~kind ~ck_name:"ck" delegate))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_fail_no_signer =
  let open Lwt_result_syntax in
  let delegate = "delegate" in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true [delegate]
  --> add_account ~algo:Bls "ck"
  --> fold_tag
        (fun kind ->
          assert_failure
            ~loc:__LOC__
            ~expected_error:(fun (_block, state) err ->
              let delegate = State.find_account delegate state in
              let ck = State.find_account "ck" state in
              let* ck_account = Account.find ck.pkh in
              Error_helpers.expect_missing_bls_proof
                ~loc:__LOC__
                ~kind_pk:
                  (Protocol.Operation_repr.consensus_to_public_key_kind kind)
                ~pk:ck_account.pk
                ~source_pkh:delegate.pkh
                err)
            (update_key ~force_no_signer:true ~kind ~ck_name:"ck" delegate))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_fail_wrong_signer =
  let open Lwt_result_syntax in
  let delegate = "delegate" in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true ~default_algo:Bls [delegate; "signer"]
  --> add_account ~algo:Bls "ck"
  --> fold_tag
        (fun kind ->
          assert_failure
            ~loc:__LOC__
            ~expected_error:(fun (_block, state) err ->
              let ck = State.find_account "ck" state in
              let* ck_account = Account.find ck.pkh in
              Error_helpers.expect_incorrect_bls_proof
                ~loc:__LOC__
                ~kind_pk:
                  (Protocol.Operation_repr.consensus_to_public_key_kind kind)
                ~pk:ck_account.pk
                err)
            (update_key ~proof_signer:"signer" ~kind ~ck_name:"ck" delegate))
        [("update consensus", Consensus); ("update companion", Companion)]

let test_fail_companion_not_tz4 =
  let open Lwt_result_syntax in
  let delegate = "delegate" in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true ~default_algo:Bls [delegate; "signer"]
  --> fold_tag
        (fun algo -> add_account ~algo "ck")
        [("Ed25519", Ed25519); ("Secp256k1", Secp256k1); ("P256", P256)]
  --> assert_failure
        ~loc:__LOC__
        ~expected_error:(fun (_block, state) err ->
          let delegate = State.find_account delegate state in

          let ck = State.find_account "ck" state in
          let* ck_account = Account.find ck.pkh in
          Assert.expect_error ~loc:__LOC__ err (function
            | [
                Protocol.Validate_errors.Manager.Update_companion_key_not_tz4
                  {source = err_source; public_key = err_pk};
              ] ->
                Signature.Public_key_hash.equal err_source delegate.pkh
                && Signature.Public_key.equal err_pk ck_account.pk
            | _ -> false))
        (update_key ~kind:Companion ~ck_name:"ck" delegate)

let test_batch =
  let delegate = "delegate" in
  init_constants ()
  --> set S.allow_tz4_delegate_enable true
  --> begin_test ~force_attest_all:true ["bootstrap"]
  --> add_account ~algo:Bls delegate
  --> add_account ~algo:Bls "ck"
  --> transfer "bootstrap" delegate (Amount (Tez_helpers.of_mutez 100_000_000L))
  --> fold_tag
        (fun kind ->
          batch ~source:delegate --> reveal delegate
          --> set_delegate delegate (Some delegate)
          --> update_key ~kind ~ck_name:"ck" delegate
          --> end_batch ~bake_after:true ())
        [("update consensus", Consensus); ("update companion", Companion)]

let tests =
  tests_of_scenarios
  @@ [
       ( "Test bootstrap accounts with initial consensus and companion keys",
         test_init_with_cks_for_bootstraps );
       ( "Simple update ck for delegate",
         test_simple_register_consensus_and_companion_keys );
       ("Register other accounts as ck", test_register_other_accounts_as_ck);
       ("Self register as companion", test_self_register_as_companion);
       ( "Register same key multiple times",
         test_register_same_key_multiple_times );
       ("Test register new key every cycle", test_register_new_key_every_cycle);
       ("Test register key at end of cycle", test_register_key_end_of_cycle);
       ("Test registration override", test_registration_override);
       ("Test double registration", test_in_registration_table_twice);
       ("Test fail on unregistered delegate", test_unregistered);
       ("Test fail forbidden tz4", test_forbidden_tz4);
       ("Test fail noop", test_fail_noop);
       ("Test already registered", test_fail_already_registered);
       ("Test fail if no signer", test_fail_no_signer);
       ("Test fail if wrong signer", test_fail_wrong_signer);
       ("Test fail companion not tz4", test_fail_companion_not_tz4);
       ("Test batch", test_batch);
     ]

let () =
  register_tests
    ~__FILE__
    ~tags:
      ["protocol"; "scenario"; "consensus"; "bls"; "attestation"; "companion"]
    tests
