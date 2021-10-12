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

module StateRecord = struct
  type t = {
    node : Node.t;
    client : Client.t;
    keys : Account.key Array.t;
    vesting_contract : string;
    storage : Contract_storage.t;
  }

  let get_client {client; _} = client

  let get_keys {keys; _} = keys

  let key i {keys; _} = Array.get keys i
end

module StateMonad = struct
  include State.Monad (StateRecord) (Lwt)

  let parameter_file = "src/proto_alpha/parameters/sandbox-parameters.json"

  let client = getf StateRecord.get_client

  let rec update_storage = function
    | [] -> return ()
    | f :: fs ->
        let* () =
          update (fun s -> StateRecord.{s with storage = f s.storage})
        in
        update_storage fs

  let alias = function
    | `Alias alias -> return alias
    | `User i ->
        let+ k = getf (StateRecord.key i) in
        k.alias

  let user_address i =
    let+ k = getf (StateRecord.key i) in
    k.public_key_hash

  let user_pk i =
    let+ k = getf (StateRecord.key i) in
    k.public_key

  let initialise_vesting_state ?overall_treshold ?vesting_increment ?next_payout
      ?payout_interval ?pour_info key_indices =
    let* keys = getf StateRecord.get_keys in
    let storage =
      Contract_storage.initial
        ?overall_treshold
        ?vesting_increment
        ?next_payout
        ?payout_interval
        ?pour_info
      @@ List.map
           Account.(
             fun (idx, treshold) ->
               let ks = List.map (fun i -> (Array.get keys i).public_key) idx in
               (ks, treshold))
           key_indices
    in
    update (fun s -> StateRecord.{s with storage})

  let bake =
    let* c = client in
    lift @@ Client.bake_for c

  let activate_alpha =
    let* c = client in
    let* () =
      lift
      @@ Client.activate_protocol ~protocol:Protocol.Alpha ~parameter_file c
    in
    bake

  let transfer ?endpoint ?wait ?burn_cap ?fee ?gas_limit ?storage_limit ?counter
      ?arg ?expect_failure ~source ~target amount =
    let* c = client in
    let* () =
      lift
      @@ Client.transfer
           ?endpoint
           ?wait
           ?burn_cap
           ?fee
           ?gas_limit
           ?storage_limit
           ?counter
           ?arg:(Option.map Test_michelson.to_string arg)
           ?expect_failure
           ~amount
           ~giver:source
           ~receiver:target
           c
    in
    bake

  let originate ~storage ~alias contract =
    let* c = client in
    let* hash =
      lift
      @@ Client.originate_contract
           ~alias
           ~amount:Tez.zero
           ~src:"bootstrap1"
           ~prg:contract
           ~init:(Test_michelson.to_string storage)
           ~burn_cap:Tez.one
           c
    in
    let* () = bake in
    return hash

  let originate_vesting alias amount =
    let* StateRecord.{client; vesting_contract; storage; _} = get in
    let* hash =
      lift
      @@ Client.originate_contract
           ~alias
           ~amount
           ~src:"bootstrap1"
           ~prg:vesting_contract
           ~init:
             (Test_michelson.to_string @@ Contract_storage.to_micheline storage)
           ~burn_cap:(Tez.of_int 2)
           client
    in
    let* () = bake in
    return hash

  let make_delegate u =
    let* c = client in
    let* target = alias (`User u) in
    let* () =
      transfer
        ~source:"bootstrap1"
        ~target
        ~burn_cap:(Tez.of_int 1)
        (Tez.of_int 100)
    in
    let* () = bake in
    let* () = lift @@ Client.register_key target c in
    bake

  let assert_balance account expected =
    let* c = client in
    let* actual = lift @@ Client.get_balance_for ~account c in
    if actual = expected then return ()
    else
      Test.fail
        "Balance for %s is ꜩ %s instead of expected ꜩ %s."
        account
        (Tez.to_string actual)
        (Tez.to_string expected)

  (* Assert that the actual storage is identical to the one maintained
     by the test state. *)
  let assert_storage account =
    let open Tezos_micheline in
    let* StateRecord.{client; storage; _} = get in
    let expected = Contract_storage.to_micheline storage in
    let* storage_code = lift @@ Client.contract_storage account client in
    let* actual = lift @@ Test_michelson.parse storage_code in
    match Micheline_diff.diff ~current:expected ~prev:actual () with
    | Some diff ->
        Test.fail
          "Storage for %s is different than expected:\n%a"
          account
          (fun fmt e ->
            Format.pp_print_string fmt @@ Test_michelson.to_string e)
          diff
    | None ->
        Log.info
          "Current storage:\n%a"
          Micheline_printer.print_expr
          (Micheline.map_node
             (fun _ -> Micheline_printer.{comment = None})
             Fun.id
             actual) ;
        return ()

  let assert_updated_storage updates account =
    let* () = update_storage updates in
    assert_storage account

  let get_signature ~typ ~data user =
    let* c = client in
    let* hash_data_output =
      lift
      @@ Client.hash_data
           ~data:(Test_michelson.to_string data)
           ~typ:(Test_michelson.to_string typ)
           c
    in
    let packed_data = List.assoc "Raw packed data" hash_data_output in
    let* signer = alias (`User user) in
    lift @@ Client.sign_bytes ~signer ~data:packed_data c

  let signatures ~typ ~data signers =
    list_map (list_map (fun u -> opt_map (get_signature ~typ ~data) u)) signers

  let assert_delegate src expected =
    let* c = client in
    let pp fmt =
      let open Format in
      fprintf fmt "%a"
      @@ pp_print_option
           ~none:(fun fmt () -> pp_print_string fmt "—")
           (fun fmt s -> pp_print_string fmt s)
    in
    let* actual = lift @@ Client.get_delegate ~src c in
    if expected = actual then return ()
    else
      lift
      @@ Test.fail
           "Delegate for %s is %a, but %a was expected."
           src
           pp
           actual
           pp
           expected

  let head_timestamp : Ptime.t t =
    let open JSON in
    let* c = client in
    let* json = lift @@ Client.rpc GET ["chains"; "main"; "blocks"; "head"] c in
    let timestamp_or_error =
      json |-> "header" |-> "timestamp" |> as_string |> Ptime.of_rfc3339
    in
    match Ptime.rfc3339_error_to_msg timestamp_or_error with
    | Ok (timestamp, _, _) -> return timestamp
    | Error (`Msg e) -> Test.fail "Couldn't parse last block's timestamp: %s" e
end

let path_to file = Filename.(dirname __FILE__ ^ dir_sep ^ file)

let empty_contract = path_to "empty.tz"

let sigs_michelson sigs =
  let open Test_michelson in
  list @@ List.map (fun grp -> list @@ List.map (optional str) grp) sigs

let vesting_arg_type =
  let open Test_michelson.Types in
  pair
    (either
       (either (pair (contract unit) mutez) (option (pair (contract unit) key)))
       (either (pair (list (pair (list key) nat)) nat) (option key_hash)))
    (pair address nat)

let gen_keys count client =
  let ( let+ ) = Fun.flip Lwt.map in
  let gen alias = Client.gen_and_show_keys ~alias client in
  let+ keys =
    Lwt_list.map_s gen @@ List.init count (fun i -> "user" ^ string_of_int i)
  in
  Array.of_list keys

let vest ?(expect_failure = false) ?(amount = Tez.zero) vesting_contract =
  let open StateMonad in
  let* () =
    transfer
      ~source:"bootstrap1"
      ~target:vesting_contract
      ~arg:Test_michelson.(right none)
      ~burn_cap:Tez.one
      ~expect_failure
      amount
  in
  assert_updated_storage
    (if expect_failure then []
    else Contract_storage.[increment_vested_balance; next_payout])
    vesting_contract

let sign_transfer ?(expect_failure = false) ~contract ~replay ~receiver ~signers
    amount =
  let open StateMonad in
  let data =
    let open Test_michelson in
    pair
      (left @@ left @@ pair (str receiver) (tez amount))
      (pair (str contract) (num replay))
  in
  let* signatures = signatures ~typ:vesting_arg_type ~data signers in
  let arg =
    let open Test_michelson in
    left
    @@ pair
         (left @@ left @@ pair (str receiver) (tez amount))
         (sigs_michelson signatures)
  in
  let* () =
    transfer
      ~expect_failure
      ~source:"bootstrap1"
      ~target:contract
      ~burn_cap:Tez.(of_mutez_int 100000)
      ~arg
      Tez.zero
  in
  assert_updated_storage
    (if expect_failure then [] else Contract_storage.[pay_out amount])
    contract

let set_pour ~replay ~signers info contract =
  let open StateMonad in
  let* keys = getf StateRecord.get_keys in
  let pour_info (recipient, authorizer) =
    let open Contract_storage in
    let recp_pk_hash = (Array.get keys recipient).public_key_hash in
    let auth = Array.get keys authorizer in
    {pour_dest = recp_pk_hash; pour_authorizer = auth.public_key}
  in
  let new_pour_info = Option.map pour_info info in
  let pour_info_micheline =
    Contract_storage.pour_info_micheline new_pour_info
  in
  let to_sign =
    let open Test_michelson in
    pair (left @@ right pour_info_micheline) (pair (str contract) (num replay))
  in
  let* signatures = signatures ~typ:vesting_arg_type ~data:to_sign signers in
  let arg =
    let open Test_michelson in
    left @@ pair (left @@ right pour_info_micheline) (sigs_michelson signatures)
  in
  let* () =
    transfer
      ~arg
      ~source:"bootstrap1"
      ~target:contract
      ~burn_cap:(Tez.of_mutez_int 100000)
      Tez.zero
  in
  assert_updated_storage Contract_storage.[set_pour_info new_pour_info] contract

let set_pour_for ~replay ~signers ~authorizer ~recipient contract =
  set_pour ~replay ~signers (Some (recipient, authorizer)) contract

let disable_pour ~replay ~signers contract =
  set_pour ~replay ~signers None contract

let execute_pour ?(expect_failure = false) ~authorizer ~recipient ~amount
    ~replay contract =
  let open StateMonad in
  let* keys = getf StateRecord.get_keys in
  let recp_pk_hash = (Array.get keys recipient).public_key_hash in
  let to_sign =
    let open Test_michelson in
    pair
      (pair (str recp_pk_hash) (tez amount))
      (pair (str contract) (num replay))
  in
  let typ =
    let open Test_michelson.Types in
    pair (pair (contract unit) mutez) (pair address nat)
  in
  let* signature = get_signature ~typ ~data:to_sign authorizer in
  let arg =
    let open Test_michelson in
    right @@ some @@ pair (str signature) (tez amount)
  in
  let* () =
    transfer
      ~source:"bootstrap1"
      ~target:contract
      ~arg
      ~burn_cap:Tez.(of_mutez_int 100000)
      ~expect_failure
      Tez.zero
  in
  assert_updated_storage
    (if expect_failure then [] else Contract_storage.[pay_out amount])
    contract

let set_delegate ~delegate ~signers ~replay contract =
  let open StateMonad in
  let data =
    let open Test_michelson in
    pair
      (right @@ right @@ optional str delegate)
      (pair (str contract) (num replay))
  in
  let* signatures = signatures ~typ:vesting_arg_type ~data signers in
  let arg =
    let open Test_michelson in
    left
    @@ pair
         (right @@ right @@ optional str delegate)
         (sigs_michelson signatures)
  in
  let* () = transfer ~source:"bootstrap1" ~target:contract ~arg Tez.zero in
  assert_updated_storage Contract_storage.[bump_replay_counter] contract

let set_keys ?(expect_failure = false) ~signers ~key_groups ~overall_treshold
    ~replay contract =
  let open StateMonad in
  let* keys = getf StateRecord.get_keys in
  let key_groups =
    List.map
      (fun (ks, t) ->
        Contract_storage.
          {
            signatories = List.map (fun i -> (Array.get keys i).public_key) ks;
            group_treshold = t;
          })
      key_groups
  in
  let key_groups_micheline =
    let open Test_michelson in
    right @@ left
    @@ Contract_storage.key_info_micheline {key_groups; overall_treshold}
  in
  let typ =
    let open Test_michelson.Types in
    pair
      (either unit (either (pair (list (pair (list key) nat)) nat) unit))
      (pair address nat)
  in
  let data =
    let open Test_michelson in
    pair key_groups_micheline (pair (str contract) (num replay))
  in
  let* signatures = signatures ~typ ~data signers in
  let arg =
    let open Test_michelson in
    left @@ pair key_groups_micheline (sigs_michelson signatures)
  in
  let* () =
    transfer
      ~expect_failure
      ~source:"bootstrap1"
      ~target:contract
      ~burn_cap:Tez.(of_mutez_int 10000)
      ~arg
      Tez.zero
  in
  assert_updated_storage
    (if expect_failure then []
    else Contract_storage.[update_keys key_groups overall_treshold])
    contract

let transfer_and_pour_happy_path =
  let open StateMonad in
  let* () = activate_alpha in
  let* () = initialise_vesting_state [([0], 1); ([1], 1); ([2], 1)] in
  let* contract = originate_vesting "vesting_3_keys_60s" (Tez.of_int 500) in
  let* () = assert_balance contract @@ Tez.of_int 500 in
  let* () = assert_storage contract in

  (* Make 5 consecutive vest operations from the vesting contract. After each
     of them [vested_balance] should increase by ꜩ100 and [next_payout] by 60s.
     Appropriate assertions are performed automatically. *)
  let* () = vest contract in
  let* () = vest contract in
  let* () = vest contract in
  let* () = vest contract in
  let* () = vest contract in

  let* receiver = user_address 3 in
  let* () = assert_balance receiver Tez.zero in
  let* () =
    sign_transfer
      ~contract
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~receiver
      ~replay:0
      Tez.(of_int 400)
  in
  let* () = assert_balance receiver @@ Tez.of_int 400 in
  let* () = assert_balance contract @@ Tez.of_int 100 in
  let* () = vest contract in
  (* This is expected to fail, as we order transfer of more funds than is available.
     ꜩ400 out of ꜩ500 transferred initially to the contract are already spent. *)
  let* () =
    sign_transfer
      ~contract
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~receiver
      ~replay:1
      ~expect_failure:true
      Tez.(of_int 100 + of_mutez_int 1)
  in
  (* Hence transferring exactly ꜩ100 is still possible. *)
  let* () =
    sign_transfer
      ~contract
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~receiver
      ~replay:1
      Tez.(of_int 100)
  in
  let* () = assert_balance receiver @@ Tez.of_int 500 in
  let* () = assert_balance contract @@ Tez.of_int 0 in
  let* () = vest ~amount:(Tez.of_int 200) contract in
  let* () = assert_balance contract @@ Tez.of_int 200 in

  (* Decide to pour funds from the vesting contract to a previously agreed
     account by a single signature. *)
  let* () =
    set_pour_for
      ~authorizer:5
      ~recipient:4
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~replay:2
      contract
  in
  let* pour_receiver = user_address 4 in
  let* () = assert_balance pour_receiver @@ Tez.of_int 0 in
  let* () =
    execute_pour
      ~authorizer:5
      ~recipient:4
      ~replay:3
      ~amount:Tez.(of_int 100)
      contract
  in
  let* () = assert_balance contract @@ Tez.of_int 100 in
  let* () = assert_balance pour_receiver @@ Tez.of_int 100 in
  (* Pour cannot exceed the available balance. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:5
      ~recipient:4
      ~replay:4
      ~amount:Tez.(of_int 100 + of_mutez_int 1)
      contract
  in
  (* Pour can equal the available balance, though. *)
  let* () =
    execute_pour
      ~authorizer:5
      ~recipient:4
      ~replay:4
      ~amount:Tez.(of_int 100)
      contract
  in
  let* () = assert_balance pour_receiver @@ Tez.of_int 200 in
  let* () = assert_balance contract @@ Tez.of_int 0 in
  (* Transfer of ꜩ0 to a smart contract is always possible. *)
  let* empty_contract =
    originate ~alias:"empty" ~storage:Test_michelson.unit empty_contract
  in
  let* () =
    sign_transfer
      ~contract
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~receiver:empty_contract
      ~replay:5
      Tez.zero
  in
  (* Nothing has changed. *)
  let* () = assert_balance receiver @@ Tez.of_int 500 in
  assert_balance contract @@ Tez.of_int 0

let vesting_3_keys_2s =
  let open StateMonad in
  let* () = activate_alpha in
  (* For 4 first users (ids 0-3) give each ꜩ100 and register
     him as a delegate. *)
  let* () = iter_int make_delegate 4 in

  let* _empty_contract_hash =
    originate ~alias:"empty" ~storage:Test_michelson.unit empty_contract
  in
  let* head_ts = head_timestamp in
  let next_payout =
    let open Ptime in
    Option.value ~default:epoch @@ add_span head_ts @@ Span.of_int_s 30
  in
  let* () =
    initialise_vesting_state
      ~next_payout
      ~payout_interval:Ptime.Span.(of_int_s 2)
      [([0], 1); ([1], 1); ([2], 1)]
  in
  let* contract = originate_vesting "vesting_3_keys_2s" (Tez.of_int 1000) in
  let* () = vest ~expect_failure:true contract in
  (* A block later, vest succeeds. *)
  let* () = bake in
  let* () = vest contract in
  let* () = vest contract in
  return contract

let test_delegation =
  let open StateMonad in
  let* contract = vesting_3_keys_2s in
  let* () = assert_delegate contract None in
  let* deleg = user_address 0 in
  let* () =
    set_delegate
      ~delegate:(Some deleg)
      ~signers:[[Some 0]; [Some 1]; [Some 2]]
      ~replay:0
      contract
  in
  let* () = assert_delegate contract (Some deleg) in
  let* deleg1 = user_address 1 in
  let* () =
    set_delegate
      ~delegate:(Some deleg1)
      ~signers:[[None]; [Some 1]; [Some 2]]
      ~replay:1
      contract
  in
  let* () = assert_delegate contract (Some deleg1) in
  let* deleg2 = user_address 2 in
  let* () =
    set_delegate
      ~delegate:(Some deleg2)
      ~signers:[[Some 0]; [None]; [Some 2]]
      ~replay:2
      contract
  in
  let* () = assert_delegate contract (Some deleg2) in
  (* Now remove the delegate. *)
  let* () =
    set_delegate
      ~delegate:None
      ~signers:[[Some 0]; [None]; [Some 2]]
      ~replay:3
      contract
  in
  let* () = assert_delegate contract None in
  let* () = assert_balance deleg @@ Tez.of_mutez_int 99999393 in
  let* () = assert_balance contract @@ Tez.of_int 1000 in
  let* () =
    sign_transfer
      ~contract
      ~replay:4
      ~receiver:deleg
      ~signers:[[Some 0]; [None]; [Some 2]]
      Tez.(of_int 100)
  in
  let* () = assert_balance deleg @@ Tez.of_mutez_int 199999393 in
  assert_balance contract @@ Tez.of_int 900

let test_invalid_transfers =
  let open StateMonad in
  let* contract = vesting_3_keys_2s in
  let* receiver = user_address 0 in
  (* Transfer fails without enough signatures. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:0
      ~receiver
      ~signers:[[None]; [None]; [None]]
      Tez.zero
  in
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:0
      ~receiver
      ~signers:[[Some 0]; [None]; [None]]
      Tez.zero
  in
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:0
      ~receiver
      ~signers:[[None]; [Some 1]; [None]]
      Tez.zero
  in
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:0
      ~receiver
      ~signers:[[None]; [None]; [Some 2]]
      Tez.zero
  in

  let* receiver = user_address 5 in
  let* () =
    set_pour_for
      ~replay:0
      ~authorizer:4
      ~recipient:5
      ~signers:[[Some 0]; [None]; [Some 2]]
      contract
  in
  (* Transaction exceeds available balance. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:1
      ~receiver
      ~signers:[[Some 0]; [Some 1]; [Some 2]]
      Tez.(of_int 1000)
  in
  let* () = assert_balance receiver @@ Tez.of_int 0 in
  let* () = assert_balance contract @@ Tez.of_int 1000 in
  let* () =
    sign_transfer
      ~contract
      ~replay:1
      ~receiver
      ~signers:[[Some 0]; [None]; [Some 2]]
      Tez.(of_int 75)
  in
  let* () = assert_balance receiver @@ Tez.of_int 75 in
  let* () = assert_balance contract @@ Tez.of_int 925 in
  let* () =
    sign_transfer
      ~contract
      ~replay:2
      ~receiver
      ~signers:[[None]; [Some 1]; [Some 2]]
      Tez.(of_int 25)
  in
  let* () = assert_balance receiver @@ Tez.of_int 100 in
  let* () = assert_balance contract @@ Tez.of_int 900 in

  let* () = vest contract in
  let* () =
    execute_pour
      ~authorizer:4
      ~recipient:5
      ~amount:Tez.(of_int 50)
      ~replay:3
      contract
  in
  let* () = assert_balance receiver @@ Tez.of_int 150 in
  let* () = assert_balance contract @@ Tez.of_int 850 in

  (* Pour fails if amount exceeds available funds. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:4
      ~recipient:5
      ~amount:Tez.(of_int 751)
      ~replay:4
      contract
  in
  let* () = assert_balance receiver @@ Tez.of_int 150 in
  let* () = assert_balance contract @@ Tez.of_int 850 in
  (* Pour fails if replay counter is too large. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:4
      ~recipient:5
      ~amount:Tez.(of_int 20)
      ~replay:5
      contract
  in
  let* () = assert_balance receiver @@ Tez.of_int 150 in
  let* () = assert_balance contract @@ Tez.of_int 850 in
  (* Pour fails if replay counter is too small. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:4
      ~recipient:5
      ~amount:Tez.(of_int 20)
      ~replay:3
      contract
  in
  let* () = assert_balance receiver @@ Tez.of_int 150 in
  let* () = assert_balance contract @@ Tez.of_int 850 in
  (* Pour fails if signature is incorrect. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:5
      ~recipient:5
      ~amount:Tez.(of_int 20)
      ~replay:4
      contract
  in
  let* () = assert_balance receiver @@ Tez.of_int 150 in
  let* () = assert_balance contract @@ Tez.of_int 850 in
  let* () =
    disable_pour ~replay:4 ~signers:[[None]; [Some 1]; [Some 2]] contract
  in

  let* () = vest contract in
  (* Pour fails when no pour info is set. *)
  execute_pour
    ~expect_failure:true
    ~replay:5
    ~recipient:5
    ~authorizer:4
    ~amount:Tez.zero
    contract

let test_update_keys : unit StateMonad.t =
  let open StateMonad in
  let* contract = vesting_3_keys_2s in
  let* receiver = user_address 4 in
  let* () =
    sign_transfer
      ~contract
      ~replay:0
      ~receiver
      ~signers:[[Some 0]; [Some 1]; [None]]
      Tez.(of_int 100)
  in
  let* () = assert_balance contract @@ Tez.of_int 900 in
  let* () = assert_balance receiver @@ Tez.of_int 100 in
  let* () =
    set_keys
      ~expect_failure:true (* Overall treshold can't be 0. *)
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 1); ([5], 1); ([6], 1)]
      ~overall_treshold:0
      ~replay:1
      contract
  in
  let* () =
    set_keys
      ~expect_failure:
        true (* Overall treshold can't be greater than number of keys. *)
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 1); ([5], 1); ([6], 1)]
      ~overall_treshold:4
      ~replay:1
      contract
  in
  let* () =
    set_keys
      ~expect_failure:true (* Group threshold can't be 0. *)
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 0); ([5], 1); ([6], 1)]
      ~overall_treshold:2
      ~replay:1
      contract
  in
  let* () =
    set_keys
      ~expect_failure:true (* Group cannot be empty. *)
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 1); ([5], 1); ([], 1)]
      ~overall_treshold:2
      ~replay:1
      contract
  in
  let* () =
    set_keys
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 1); ([5], 1); ([6], 1)]
      ~overall_treshold:2
      ~replay:1
      contract
  in
  (* unchanged *)
  let* () = assert_balance contract @@ Tez.of_int 900 in
  let* receiver = user_address 1 in
  (* Old signatures don't work anymore. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:2
      ~receiver
      ~signers:[[Some 0]; [Some 1]; [Some 2]]
      Tez.(of_int 200)
  in
  (* unchanged *)
  let* () = assert_balance contract @@ Tez.of_int 900 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 99999393 in
  let* () =
    sign_transfer
      ~contract
      ~replay:2
      ~receiver
      ~signers:[[Some 3; None]; [Some 5]; [Some 6]]
      Tez.(of_int 100)
  in
  let* () = assert_balance contract @@ Tez.of_int 800 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 199999393 in
  let* () = vest contract in
  let* () =
    sign_transfer
      ~contract
      ~replay:3
      ~receiver
      ~signers:[[Some 3; None]; [None]; [Some 6]]
      Tez.(of_int 100)
  in
  let* () = assert_balance contract @@ Tez.of_int 700 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 299999393 in
  let* () = vest contract in
  let* () =
    sign_transfer
      ~contract
      ~replay:4
      ~receiver
      ~signers:[[None; None]; [Some 5]; [Some 6]]
      Tez.(of_int 100)
  in
  let* () = assert_balance contract @@ Tez.of_int 600 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 399999393 in
  let* () = vest contract in
  let* () =
    sign_transfer
      ~contract
      ~replay:5
      ~receiver
      ~signers:[[None; Some 4]; [Some 5]; [None]]
      Tez.(of_int 10)
  in
  let* () = assert_balance contract @@ Tez.of_int 590 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 409999393 in
  (* Group treshold must be met. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:6
      ~receiver
      ~signers:[[Some 3; Some 4]; [None]; [None]]
      Tez.(of_int 10)
  in
  let* () = assert_balance contract @@ Tez.of_int 590 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 409999393 in
  return ()

let test_all_sigs_required =
  let open StateMonad in
  let* contract = vesting_3_keys_2s in
  let* () =
    set_keys
      ~signers:[[Some 0]; [Some 1]; [None]]
      ~key_groups:[([3; 4], 2); ([5], 1); ([6], 1)]
      ~overall_treshold:3
      ~replay:0
      contract
  in
  let* receiver = user_address 1 in
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:1
      ~receiver
      ~signers:[[Some 3; Some 4]; [Some 5]; [None]]
      Tez.(of_int 10)
  in
  let* () = assert_balance contract @@ Tez.of_int 1000 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 99999393 in
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:1
      ~receiver
      ~signers:[[None; None]; [Some 5]; [Some 6]]
      Tez.(of_int 10)
  in
  let* () = assert_balance contract @@ Tez.of_int 1000 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 99999393 in
  let* () =
    sign_transfer
      ~contract
      ~replay:1
      ~receiver
      ~signers:[[Some 3; Some 4]; [Some 5]; [Some 6]]
      Tez.(of_int 10)
  in
  let* () = assert_balance contract @@ Tez.of_int 990 in
  let* () = assert_balance receiver @@ Tez.of_mutez_int 109999393 in
  return ()

let test_full_contract =
  let open StateMonad in
  let* () = activate_alpha in
  let* _empty_contract_hash =
    originate ~alias:"empty" ~storage:Test_michelson.unit empty_contract
  in
  let* () =
    initialise_vesting_state
      ~vesting_increment:
        (Tez.of_mutez_int 636089108075)
        (* 1/12th of the total initial balance. *)
      ~payout_interval:
        Ptime.Span.(of_int_s (60 * 60 * 24 * 365 / 12))
        (* Approximately one month. *)
      ~overall_treshold:4
      [
        ([0; 1; 2; 3], 2);
        ([4; 5; 6; 7], 2);
        ([8; 9; 10; 11], 2);
        ([12; 13; 14; 15], 2);
        ([16; 17; 18; 19], 2);
        ([20; 21; 22; 23], 2);
        ([24; 25; 26; 27], 2);
      ]
  in
  (* 10% of the total token supply. *)
  let initial_balance = Tez.of_mutez_int 7633069296900 in
  (* Ensure bootstrap1 has enough funds to cover the initial balance. *)
  let* () =
    transfer ~source:"bootstrap2" ~target:"bootstrap1" Tez.(of_int 3000000)
  in
  let* () =
    transfer ~source:"bootstrap3" ~target:"bootstrap1" Tez.(of_int 3000000)
  in
  let* contract = originate_vesting "full_vesting_contract" initial_balance in
  let* () = vest contract (* 1 / 12 *) in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7633069296900 in
  let* () = vest contract (* 2 / 12 *) in

  let* () = make_delegate 0 in
  let* delegate = user_address 0 in
  let* () =
    set_delegate
      ~delegate:(Some delegate)
      ~signers:
        [
          [Some 0; Some 1; None; None];
          [Some 4; Some 5; None; None];
          [Some 8; Some 9; None; None];
          [Some 12; Some 13; None; None];
          [None; None; None; None];
          [None; None; None; None];
          [None; None; None; None];
        ]
      ~replay:0
      contract
  in
  let* () = assert_delegate contract (Some delegate) in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7633069296900 in
  let* () =
    set_delegate
      ~delegate:None
      ~signers:
        [
          [None; None; Some 2; Some 3];
          [None; None; Some 6; Some 7];
          [None; None; Some 10; Some 11];
          [None; Some 13; Some 14; Some 15];
          [None; None; None; None];
          [None; None; None; None];
          [None; None; None; None];
        ]
      ~replay:1
      contract
  in
  let* u27 = user_address 27 in
  let* () = assert_delegate contract None in
  let* () = assert_balance u27 @@ Tez.of_int 0 in
  let* () =
    sign_transfer
      ~contract
      ~replay:2
      ~receiver:u27
      ~signers:
        [
          [None; None; None; None];
          [None; None; None; None];
          [None; None; None; None];
          [None; Some 13; None; Some 15];
          [Some 16; None; None; Some 19];
          [Some 20; None; None; Some 23];
          [None; Some 25; None; Some 27];
        ]
      Tez.(of_int 100)
  in
  let* () = assert_balance u27 @@ Tez.of_int 100 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632969296900 in
  let* () =
    sign_transfer
      ~contract
      ~replay:3
      ~receiver:u27
      ~signers:
        [
          [Some 0; Some 1; Some 2; Some 3];
          [Some 4; Some 5; Some 6; Some 7];
          [Some 8; Some 9; Some 10; Some 11];
          [Some 12; Some 13; Some 14; Some 15];
          [Some 16; Some 17; Some 18; Some 19];
          [Some 20; Some 21; Some 22; Some 23];
          [Some 24; Some 25; Some 26; Some 27];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 300 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632769296900 in
  (* Group thresholds must be met. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:4
      ~receiver:u27
      ~signers:
        [
          [None; None; Some 2; None];
          [None; Some 5; None; None];
          [None; None; Some 10; None];
          [None; Some 13; None; None];
          [Some 16; None; None; None];
          [None; Some 21; None; None];
          [None; None; Some 26; None];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 300 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632769296900 in
  (* Overall threshold must be met. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:4
      ~receiver:u27
      ~signers:
        [
          [Some 0; None; Some 2; None];
          [None; Some 5; None; None];
          [None; None; Some 10; Some 11];
          [None; Some 13; None; None];
          [Some 16; None; None; None];
          [None; Some 21; None; None];
          [None; None; Some 26; Some 27];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 300 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632769296900 in
  (* All signatures must be valid key (#0 signed in the wrong slot). *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:4
      ~receiver:u27
      ~signers:
        [
          [None; Some 1; Some 2; Some 0];
          [None; Some 5; Some 6; None];
          [None; None; Some 10; Some 11];
          [None; Some 13; None; Some 15];
          [Some 16; None; None; None];
          [None; Some 21; None; None];
          [None; None; Some 26; None];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 300 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632769296900 in

  (* Change keys: *)
  let* () =
    set_keys
      ~signers:
        [
          [None; Some 1; Some 2; None];
          [None; Some 5; Some 6; None];
          [None; None; Some 10; Some 11];
          [None; Some 13; None; Some 15];
          [Some 16; None; None; None];
          [None; Some 21; None; None];
          [None; None; Some 26; None];
        ]
      ~key_groups:
        [
          ([0; 7; 14; 21], 1);
          ([1; 8; 15; 22], 1);
          ([2; 9; 16; 23], 1);
          ([3; 10; 17; 24], 1);
          ([4; 11; 18; 25], 1);
          ([5; 12; 19; 26], 1);
          ([6; 13; 20; 27], 1);
        ]
      ~overall_treshold:4
      ~replay:4
      contract
  in
  (* Old keys no longer work. *)
  let* () =
    sign_transfer
      ~expect_failure:true
      ~contract
      ~replay:5
      ~receiver:u27
      ~signers:
        [
          [None; Some 1; Some 2; None];
          [None; Some 5; Some 6; None];
          [None; None; Some 10; Some 11];
          [None; Some 13; None; Some 15];
          [Some 16; None; None; None];
          [None; Some 21; None; None];
          [None; None; Some 26; None];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 300 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632769296900 in
  (* New keys do work. *)
  let* () =
    sign_transfer
      ~contract
      ~replay:5
      ~receiver:u27
      ~signers:
        [
          [None; Some 7; None; None];
          [None; None; Some 15; None];
          [None; None; None; Some 23];
          [None; None; None; None];
          [Some 4; None; None; None];
          [None; None; None; None];
          [Some 6; None; None; None];
        ]
      Tez.(of_int 200)
  in
  let* () = assert_balance u27 @@ Tez.of_int 500 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632569296900 in

  (* Pour  must be set up first. *)
  let* () =
    execute_pour
      ~expect_failure:true
      ~authorizer:0
      ~recipient:27
      ~amount:Tez.(of_int 500)
      ~replay:6
      contract
  in
  let* () = assert_balance u27 @@ Tez.of_int 500 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632569296900 in
  let* () =
    set_pour_for
      ~replay:6
      ~signers:
        [
          [None; Some 7; None; None];
          [None; None; Some 15; None];
          [None; None; None; Some 23];
          [None; None; None; None];
          [Some 4; None; None; None];
          [None; None; None; None];
          [Some 6; None; None; None];
        ]
      ~authorizer:0
      ~recipient:27
      contract
  in
  let* () =
    execute_pour
      ~authorizer:0
      ~recipient:27
      ~amount:Tez.(of_int 500)
      ~replay:7
      contract
  in
  let* () = assert_balance u27 @@ Tez.of_int 1000 in
  let* () = assert_balance contract @@ Tez.of_mutez_int 7632069296900 in
  return ()

let execute ~user_count ~contract test () =
  let* node = Node.init [Connections 0; Synchronisation_threshold 0] in
  let* client = Client.init ~endpoint:(Node node) () in
  let* keys = gen_keys user_count client in
  let vesting_contract = path_to contract in
  let storage = Contract_storage.initial [] in
  StateMonad.eval
    StateRecord.{node; client; keys; vesting_contract; storage}
    test
