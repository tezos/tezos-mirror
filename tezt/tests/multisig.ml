(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client/Multisig
   Invocation:   dune exec tezt/tests/main.exe -- --file multisig.ml
   Subject:      Tests octez-client multisig interaction
*)

(* octez-client has builtin support for multisig smart contracts. See
   docs/user/multisig.rst for more details about it.

   This file tests the client multisig support; more precisely it tests
   that both the generic and the legacy versions of the multisig smart
   contract behave as intended. For all commands, we check that we can
   interact with the multisig contract when invoking it by its address or
   by its alias. *)

type msig_version = Legacy | Generic

let show_msig_version = function Legacy -> "legacy" | Generic -> "generic"

(* Generate 3 pairs of keys using various schemes and return the list
   of keys. *)
let get_keys client (sig_alg0, sig_alg1, sig_alg2) =
  let ((key0, key1, key2) as keys) = ("foo", "bar", "boo") in
  let* (_ : string) = Client.gen_keys ~alias:key0 ?sig_alg:sig_alg0 client in
  let* (_ : string) = Client.gen_keys ~alias:key1 ?sig_alg:sig_alg1 client in
  let* (_ : string) = Client.gen_keys ~alias:key2 ?sig_alg:sig_alg2 client in
  return keys

type msig_storage = {counter : int; threshold : int; keys : string list}

let msig_storage_typ =
  Check.(
    convert
      (fun {counter; threshold; keys} -> (counter, threshold, keys))
      (tuple3 int int (list string)))

module Msig_state = struct
  type t = {balance : Tez.t; delegate : string option; storage : msig_storage}

  let typ =
    Check.(
      convert
        (fun {balance; delegate; storage} -> (balance, delegate, storage))
        (tuple3 Tez.typ (option string) msig_storage_typ))

  let incr_counter {balance; delegate; storage} =
    {balance; delegate; storage = {storage with counter = storage.counter + 1}}

  let incr_balance diff state =
    {state with balance = Tez.(state.balance + diff)}

  let decr_balance diff state =
    {state with balance = Tez.(state.balance - diff)}

  let set_delegate delegate state = {state with delegate = Some delegate}

  let withdraw_delegate state = {state with delegate = None}

  let set_threshold_and_public_keys threshold keys state =
    let storage = {state.storage with threshold; keys} in
    {state with storage}

  let ( <$> ) f1 f2 st = f2 (f1 st)
end

(* Parse the storage of a multisig contract to get its counter (as a
   number), threshold (as a number), and the keys of the signers (as
   Micheline sequence in a string). *)
let parse_msig_storage storage =
  let storage = replace_string ~all:true (rex "\\s+") ~by:" " storage in
  let storage_regexp = rex "Pair ([0-9]+) ([0-9]+) \\{ (.*) \\}" in
  match storage =~*** storage_regexp with
  | Some (counter, threshold, keys) ->
      {
        counter = int_of_string counter;
        threshold = int_of_string threshold;
        keys =
          keys |> String.split_on_char ';'
          |> List.map (fun s ->
                 match String.split_on_char '"' s with
                 | [_; key; _] -> key
                 | _ ->
                     Test.fail
                       "[parse_msig_storage] Could not parse keys in multisig \
                        storage %S"
                       storage);
      }
  | None ->
      Test.fail
        "[parse_msig_storage] Could not parse multisig storage: %S"
        storage

(* Build a multisig storage from its components: a counter, a
   threshold and the list of signer public keys. *)
let build_msig_storage client {counter; threshold; keys} =
  let* keys =
    let* keys =
      Lwt_list.map_s (fun key -> Client.show_address ~alias:key client) keys
    in
    return
      ("{"
      ^ String.concat
          ";"
          (List.map (fun key -> sf "%S" key.Account.public_key) keys)
      ^ "}")
  in
  return (sf "Pair %d %d %s" counter threshold keys)

type multisig = {handle : string; keys : string list; version : msig_version}

(* [deploy_msig protocol client ~keys ~version ~by_address] originates a
   multisig contract with a threshold of 2 and the [keys] given as parameter.
   The version of the script is given by the [version] parameter, it can be
   either {Generic} or {Legacy}. This function returns a dictionary
   containing:
   - a [handle] that can be used to interact with the contract:
   either the address of the script or an alias, depending on the
   [by_address] parameter
   - the list of [keys] that are stored in the contract which is a
   copy of the [keys] parameter
   - the [version], which is a copy of the [msig_version] parameter *)
let deploy_msig protocol client ~keys ~version ~by_address =
  let msig_alias = "msig" in
  let* initial_storage =
    build_msig_storage client {counter = 0; threshold = 2; keys}
  in
  let* _alias, deployment =
    let msig_script_name =
      ["mini_scenarios"; show_msig_version version ^ "_multisig"]
    in
    Client.originate_contract_at
      ~alias:msig_alias
      ~src:"bootstrap1"
      ~amount:(Tez.of_int 100)
      ~init:initial_storage
      ~burn_cap:(Tez.of_int 100)
      client
      msig_script_name
      protocol
  in
  let handle = if by_address then deployment else msig_alias in
  return {handle; keys; version}

let test_multisig ~sig_algs ~supports protocols =
  let iter xs f = List.iter f xs in
  iter [true; false] @@ fun by_address ->
  iter [Generic; Legacy] @@ fun version ->
  Protocol.register_test
    ~__FILE__
    ~supports
    ~title:
      (sf
         "Multisig [by_address:%b, %s, sig_algs:%s]"
         by_address
         (show_msig_version version)
         (String.concat ","
         @@ List.map
              (function None -> "None" | Some s -> s)
              (let sig0, sig1, sig2 = sig_algs in
               [sig0; sig1; sig2])))
    ~tags:
      [
        "multisig";
        (if by_address then "by_address" else "by_alias");
        show_msig_version version;
      ]
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in
      let bootstrap1 = Constant.bootstrap1.alias in
      let bootstrap2 = Constant.bootstrap2.alias in
      let bootstrap5 = Constant.bootstrap5.alias in
      let qty = Tez.of_int 10 in
      let color = Log.Color.(bold ++ FG.cyan) in
      (* Test that:
         - the script originated by the "deploy multisig" command,
         - the generic_multisig.tz script found in the mini_scenarios
         directory, and
         - the script printed by the "show multisig script"
         are the same. *)
      Log.info ~color "Deploy multisig" ;
      let* key0, key1, key2 = get_keys client sig_algs in
      let keys = [key0; key1; key2] in
      let* msig = deploy_msig protocol ~version ~keys ~by_address client in
      let multisig = msig.handle in
      let get_state ?(contract = msig.handle) () =
        let* balance = Client.get_balance_for ~account:contract client
        and* storage = Client.contract_storage contract client
        and* delegate = Client.get_delegate ~src:contract client in
        return
          Msig_state.{balance; storage = parse_msig_storage storage; delegate}
      in
      let check_state_change ?__LOC__ ?(error_msg = "Expected state %R, got %L")
          f_state f_body =
        let* current_state = get_state () in
        let* () = f_body () in
        let* new_state = get_state () in
        Check.(
          (new_state = f_state current_state) Msig_state.typ ?__LOC__ ~error_msg) ;
        unit
      in
      let* () =
        match version with
        | Legacy -> unit
        | Generic ->
            let qty = Tez.of_int 100 in
            let* () =
              Client.deploy_multisig
                ~burn_cap:Tez.one
                ~new_multisig:"dummy_msig"
                ~src:bootstrap1
                ~threshold:2
                ~keys:msig.keys
                ~qty
                client
            in
            let expected_hash =
              "exprub9UzpxmhedNQnsv1J1DazWGJnj1dLhtG1fxkUoWSdFLBGLqJ4"
            in
            let* handle_hash =
              Client.get_contract_hash ~contract:msig.handle client
            in
            Check.(
              (handle_hash = expected_hash)
                string
                ~__LOC__
                ~error_msg:"Expected hash of handle to be %R, got %L") ;
            let* alias_hash =
              Client.get_contract_hash ~contract:"dummy_msig" client
            in
            Check.(
              (alias_hash = expected_hash)
                string
                ~__LOC__
                ~error_msg:"Expected hash of handle to be %R, got %L") ;
            let* balance =
              Client.get_balance_for ~account:"dummy_msig" client
            in
            Check.(
              (balance = qty)
                Tez.typ
                ~__LOC__
                ~error_msg:"Expected balance of dummy_sig to be %R, got %L.") ;
            unit
      in
      (* Test the client command for signing a multisig transfer from key
         number 0. *)
      Log.info ~color "Transfer" ;
      let* sig0 =
        Client.sign_multisig_transaction_transfer
          ~multisig
          ~qty
          ~dst:bootstrap2
          ~key:key0
          client
      in
      (* Test the client command for preparing a transfer. The result of the
         command is ignored in this test, we only test that the command
         succeeds. *)
      Log.info ~color "Prepare msig transfer" ;
      let* _data =
        Client.prepare_multisig_transaction
          ~multisig
          ~qty
          ~dst:bootstrap2
          client
      in
      (* Produce signatures for keys number 1 and 2 using the the
         preparation command together with the sign_bytes client command. *)
      Log.info ~color "Prepare msig sign" ;
      let* data =
        Client.prepare_multisig_transaction
          ~multisig
          ~qty
          ~dst:bootstrap2
          ~bytes_only:true
          client
      in
      let* (_ : string) = Client.sign_bytes ~signer:key1 ~data client
      and* sig2 = Client.sign_bytes ~signer:key2 ~data client in
      (* Test transfer failure when there are too few signatures. *)
      Log.info ~color "Transfer failure" ;
      let* () =
        Client.spawn_from_multisig_transfer
          ~multisig
          ~qty
          ~src:bootstrap1
          ~dst:bootstrap2
          ~signatures:[sig2]
          client
        |> Process.check_error
             ~msg:
               (rex
                  "Not enough signatures: only 1 signatures were given but the \
                   threshold is currently 2")
      in
      (* Test a successful transfer using signatures obtained by different
         methods. *)
      Log.info ~color "Transfer success" ;
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(incr_counter <$> decr_balance qty)
        @@ fun () ->
        Client.from_multisig_transfer
          ~multisig
          ~qty
          ~src:bootstrap1
          ~dst:bootstrap2
          ~signatures:[sig0; sig2]
          client
      in
      (* The generic multisig contract features an unauthorized default
         entrypoint to receive donations but the legacy one does not. *)
      Log.info ~color "Default entrypoint" ;
      let amount = Tez.of_int 100 in
      let spawn_transfer () =
        Client.spawn_transfer
          ~amount
          ~giver:bootstrap1
          ~receiver:msig.handle
          client
      in
      let* () =
        match msig.version with
        | Legacy ->
            spawn_transfer ()
            |> Process.check_error
                 ~msg:(rex "Invalid argument passed to contract")
        | Generic ->
            check_state_change ~__LOC__ Msig_state.(incr_balance amount)
            @@ fun () -> spawn_transfer () |> Process.check
      in
      Log.info ~color "Transfer with entrypoint" ;
      (* Both versions of the contract can call arbitrary entrypoints of
         type unit. This test uses the two possible methods to produce the
         signatures. *)
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(incr_counter <$> decr_balance qty)
        @@ fun () ->
        let alias = "multisig_dest_entrypoint" in
        let entrypoint = "a" in
        let* _address =
          let path =
            Michelson_script.(find ["mini_scenarios"; alias] protocol |> path)
          in
          Client.originate_contract
            ~alias
            ~amount:Tez.zero
            ~burn_cap:(Tez.of_int 10)
            ~prg:path
            ~src:bootstrap1
            client
        in
        let* data =
          Client.prepare_multisig_transaction
            ~multisig
            ~qty
            ~dst:alias
            ~bytes_only:true
            ~entrypoint
            client
        in
        let* sig0 = Client.sign_bytes ~signer:key0 ~data client
        and* sig2 =
          Client.sign_multisig_transaction_transfer
            ~multisig
            ~qty
            ~dst:alias
            ~key:key2
            ~entrypoint
            client
        in
        Client.from_multisig_transfer
          ~multisig
          ~qty
          ~src:bootstrap1
          ~dst:alias
          ~signatures:[sig0; sig2]
          ~entrypoint
          client
      in
      (* The generic multisig contract can call other contracts with
         arbitrary parameters but the legacy one can only send Unit. *)
      Log.info ~color "Transfer with arg" ;
      (* Test that the multisig transfer preparation command type checks the
         parameter. *)
      let alias = "multisig_dest_entrypoint_arg" in
      let* _address =
        let path =
          Michelson_script.(find ["mini_scenarios"; alias] protocol |> path)
        in
        Client.originate_contract
          ~alias
          ~amount:Tez.zero
          ~burn_cap:(Tez.of_int 10)
          ~prg:path
          ~src:bootstrap1
          client
      in
      let entrypoint = "a" in
      let arg = "42" in
      let prepare_transfer () =
        Client.spawn_prepare_multisig_transaction
          ~multisig
          ~qty
          ~dst:alias
          ~bytes_only:true
          ~entrypoint
          ~arg
          client
      in
      let* () =
        match msig.version with
        | Legacy ->
            prepare_transfer ()
            |> Process.check_error
                 ~msg:
                   (rex
                      "This multisig contract can only transfer tokens to \
                       contracts of type unit; calling a contract with \
                       argument 42 is not supported.")
        | Generic ->
            check_state_change
              ~__LOC__
              Msig_state.(incr_counter <$> decr_balance qty)
            @@ fun () ->
            let* data = prepare_transfer () |> Process.check_and_read_stdout in
            let data = String.trim data in
            let* sig0 = Client.sign_bytes ~signer:key0 ~data client
            and* sig2 =
              Client.sign_multisig_transaction_transfer
                ~multisig
                ~qty
                ~dst:alias
                ~key:key2
                ~entrypoint
                ~arg
                client
            in
            Client.from_multisig_transfer
              ~multisig
              ~qty
              ~src:bootstrap1
              ~dst:alias
              ~signatures:[sig0; sig2]
              ~entrypoint
              ~arg
              client
      in
      (* Test that the multisig transfer preparation command type
         checks the parameter. *)
      Log.info ~color "Transfer ill-typed" ;
      let* () =
        let msg =
          rex
          @@
          match msig.version with
          | Generic ->
              "The entrypoint b of contract .* called from a multisig contract \
               is of type string; the provided parameter 42 is ill-typed."
          | Legacy ->
              "This multisig contract can only transfer tokens to contracts of \
               type unit; calling a contract with argument 42 is not \
               supported."
        in
        Client.spawn_prepare_multisig_transaction
          ~multisig
          ~qty
          ~dst:alias
          ~bytes_only:true
          ~entrypoint:"b"
          ~arg:"42"
          client
        |> Process.check_error ~msg
      in
      (* Test that the multisig transfer preparation command checks the
         balance. *)
      Log.info ~color "Transfer too high" ;
      let* output =
        Client.spawn_prepare_multisig_transaction
          ~multisig
          ~qty:(Tez.of_int 1000)
          ~dst:bootstrap1
          ~bytes_only:true
          client
        |> Process.check_and_read_stderr
      in
      Check.(
        (output
        =~ rex "Transferred amount is bigger than current multisig balance")
          ~error_msg:"Expected output %L to match expression %R.") ;
      (* The generic multisig client can run lambdas, this can be used to
         atomically run several operations. *)
      Log.info ~color "Multiple operations" ;
      let bootstrap1_address = Constant.bootstrap1.public_key_hash in
      let bootstrap2_address = Constant.bootstrap2.public_key_hash in
      let bootstrap3_address = Constant.bootstrap3.public_key_hash in
      let lambda =
        sf
          {|{ DROP; NIL operation; PUSH key_hash "%s"; IMPLICIT_ACCOUNT; PUSH mutez 1000000; UNIT; TRANSFER_TOKENS; CONS; PUSH key_hash "%s"; IMPLICIT_ACCOUNT; PUSH mutez 2000000; UNIT; TRANSFER_TOKENS; CONS; PUSH key_hash "%s"; SOME; SET_DELEGATE; CONS }|}
          bootstrap1_address
          bootstrap2_address
          bootstrap3_address
      in
      let* lambda =
        Client.normalize_data
          ~data:("text:" ^ lambda)
            (* Force 'text' to avoid the client attempting read the lambda as a path *)
          ~typ:"lambda unit (list operation)"
          ~mode:Client.Optimized
          client
      in
      let prepare_lambda () =
        Client.spawn_prepare_multisig_transaction_run_lambda
          ~multisig
          ~lambda
          ~bytes_only:true
          client
      in
      let* () =
        match msig.version with
        | Legacy ->
            prepare_lambda ()
            |> Process.check_error
                 ~msg:(rex "This multisig contract has a fixed set of actions")
        | Generic ->
            check_state_change
              ~__LOC__
              Msig_state.(
                incr_counter
                <$> decr_balance (Tez.of_int 3)
                <$> set_delegate bootstrap3_address)
            @@ fun () ->
            let* data = prepare_lambda () |> Process.check_and_read_stdout in
            let data = String.trim data in
            let* sig0 = Client.sign_bytes ~signer:key0 ~data client
            and* sig2 =
              Client.sign_multisig_transaction_run_lambda
                ~multisig
                ~lambda
                ~key:key2
                client
            in
            Client.from_multisig_run_lambda
              ~multisig
              ~lambda
              ~src:bootstrap1
              ~signatures:[sig0; sig2]
              client
      in
      (* Test for the error message for ill-typed lambdas. *)
      Log.info ~color "Multiple operations failure" ;
      let* () =
        let msg =
          rex
          @@
          match msig.version with
          | Generic ->
              "The provided lambda .* for multisig contract is ill-typed; .* \
               is expected."
          | Legacy -> "This multisig contract has a fixed set of actions."
        in
        Client.spawn_prepare_multisig_transaction_run_lambda
          ~multisig
          ~lambda:"{ DROP }"
          ~bytes_only:true
          client
        |> Process.check_error ~msg
      in
      (* Test the multisig command for changing delegate. *)
      Log.info ~color "Delegate change" ;
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(
            incr_counter <$> set_delegate Constant.bootstrap5.public_key_hash)
        @@ fun () ->
        let* data =
          Client.prepare_multisig_transaction_set_delegate
            ~multisig
            ~dlgt:bootstrap5
            ~bytes_only:true
            client
        in
        let* sig0 =
          Client.sign_multisig_transaction_set_delegate
            ~multisig
            ~dlgt:bootstrap5
            ~key:key0
            client
        and* sig2 = Client.sign_bytes ~signer:key2 ~data client in
        Client.set_delegate_of_multisig
          ~multisig
          ~dlgt:bootstrap5
          ~src:bootstrap1
          ~signatures:[sig0; sig2]
          client
      in
      (* Test the multisig command for removing delegation. *)
      Log.info ~color "Delegate withdraw" ;
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(incr_counter <$> withdraw_delegate)
        @@ fun () ->
        let* data =
          Client.prepare_multisig_transaction_withdraw_delegate
            ~multisig
            ~bytes_only:true
            client
        in
        let* sig0 =
          Client.sign_multisig_transaction_withdraw_delegate
            ~multisig
            ~key:key0
            client
        and* sig1 = Client.sign_bytes ~signer:key1 ~data client in
        Client.withdraw_delegate_of_multisig
          ~multisig
          ~src:bootstrap1
          ~signatures:[sig0; sig1]
          client
      in
      (* Test changing the keys and threshold with the `run transaction`
         command. *)
      Log.info ~color "Run transaction change keys and threshold" ;
      let* key0_key = Client.show_address ~alias:key0 client
      and* key2_key = Client.show_address ~alias:key2 client in
      let threshold = 2 in
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(
            incr_counter
            <$> set_threshold_and_public_keys
                  threshold
                  [key0_key.public_key; key2_key.public_key])
        @@ fun () ->
        let public_keys = [key0; key2] in
        let* data =
          Client.prepare_multisig_transaction_set_threshold_and_public_keys
            ~multisig
            ~threshold
            ~public_keys
            ~bytes_only:true
            client
        in
        let* sig0 =
          Client.sign_multisig_transaction_set_threshold_and_public_keys
            ~multisig
            ~signing_key:key0
            ~threshold
            ~public_keys
            client
        and* sig2 = Client.sign_bytes ~signer:key2 ~data client in
        Client.run_transaction_on_multisig
          ~multisig
          ~bytes:data
          ~src:bootstrap1
          ~signatures:[sig0; sig2]
          client
      in
      (* Test changing the keys and threshold with `set threshold of
         multisig` command. *)
      Log.info ~color "Change keys and threshold" ;
      let threshold = 2 in
      let* () =
        check_state_change
          ~__LOC__
          Msig_state.(
            incr_counter
            <$> set_threshold_and_public_keys
                  threshold
                  [key0_key.public_key; key2_key.public_key])
        @@ fun () ->
        let public_keys = [key0; key2] in
        let* data =
          Client.prepare_multisig_transaction_set_threshold_and_public_keys
            ~multisig
            ~threshold
            ~public_keys
            ~bytes_only:true
            client
        in
        let* sig0 =
          Client.sign_multisig_transaction_set_threshold_and_public_keys
            ~multisig
            ~signing_key:key0
            ~threshold
            ~public_keys
            client
        and* sig2 = Client.sign_bytes ~signer:key2 ~data client in
        Client.set_threshold_of_multisig
          ~multisig
          ~threshold
          ~public_keys
          ~src:bootstrap1
          ~signatures:[sig0; sig2]
          client
      in

      unit)
    protocols

(* Verify that non-multisig contracts are rejected *)
let test_unsupported_multisig =
  Protocol.register_test
    ~__FILE__
    ~title:"Unsupported multisig"
    ~tags:["unsupported"; "multisig"]
  @@ fun protocol ->
  Log.info "Deploy nonmultisig" ;
  let* client = Client.init_mockup ~protocol () in
  let* _alias, _address =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~alias:"id"
      ~src:"bootstrap1"
      ~burn_cap:(Tez.of_int 10)
      ~init:{|""|}
      client
      ["attic"; "id"]
      protocol
  in
  Client.spawn_from_multisig_transfer
    ~multisig:"id"
    ~qty:(Tez.of_int 10)
    ~src:"bootstrap1"
    ~dst:"bootstrap2"
    ~signatures:[]
    client
  |> Process.check_error
       ~msg:
         (rex
            "The hash of this script is \
             exprv8K6ceBpFH5SFjQm4BRYSLJCHQBFeQU6BFTdvQSRPaPkzdLyAL, it was \
             not found among in the list of known multisig script hashes.")

let register ~protocols =
  test_unsupported_multisig protocols ;
  test_multisig
    ~supports:Protocol.Any_protocol
    ~sig_algs:(None, Some "secp256k1", Some "ed25519")
    protocols ;
  test_multisig
    ~supports:Protocol.Any_protocol
    ~sig_algs:(Some "p256", Some "ed25519", None)
    protocols ;
  test_multisig
    ~supports:(Protocol.From_protocol 16)
    ~sig_algs:(None, Some "bls", None)
    protocols
