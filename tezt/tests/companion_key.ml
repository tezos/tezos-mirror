(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Companion key client-baker integration
    Invocation:   dune exec tezt/tests/main.exe -- --file companion_key.ml
    Subject:      Test companion key registration and usage
*)

let team = Tag.layer1

let hooks =
  let replacements =
    ("edsig\\w{94}", "[SIGNATURE]")
    :: ("BLsig\\w{137}", "[BLS_SIGNATURE]")
    :: Tezos_regression.replacements
  in
  Tezos_regression.hooks_custom
    ~replace_variables:(fun s ->
      Tezos_regression.replace_variables ~replacements s)
    ()

let blocks_per_cycle = 4

let consensus_rights_delay = 1

open Consensus_key.Helpers

type companion_key = {
  active_companion_key : string option;
  (* Pending companion keys per cycle *)
  pending_companion_keys : (int * string) list;
}

let companion_key_typ : companion_key Check.typ =
  Check.(
    convert
      (fun {active_companion_key; pending_companion_keys} ->
        (active_companion_key, pending_companion_keys))
      (tuple2 (option string) (list (tuple2 int string))))

let get_companion_key ?block client (delegate : Account.key) :
    companion_key Lwt.t =
  let* json =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_delegate_companion_key
         ?block
         delegate.public_key_hash
  in
  return
    JSON.
      {
        active_companion_key =
          json |-> "active" |> as_opt
          |> Option.map (fun x -> x |-> "pkh" |> as_string);
        pending_companion_keys =
          json |-> "pendings" |> as_list
          |> List.map (fun pending_key ->
                 ( pending_key |-> "cycle" |> as_int,
                   pending_key |-> "pkh" |> as_string ));
      }

let check_companion_key ~__LOC__ delegate ?expected_active
    ?(expected_pending = []) client =
  let* companion_key = get_companion_key client delegate in
  let expected_active_pkh =
    Option.map (fun (x : Account.key) -> x.public_key_hash) expected_active
  in
  let expected_companion_key =
    {
      active_companion_key = expected_active_pkh;
      pending_companion_keys =
        List.map
          (fun (cycle, (account : Account.key)) ->
            (cycle, account.public_key_hash))
          expected_pending;
    }
  in
  Check.(
    (companion_key = expected_companion_key)
      companion_key_typ
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let test_update_companion_key =
  Protocol.register_regression_test
    ~__FILE__
    ~title:"update companion key"
    ~tags:[team; "companion_key"]
    ~supports:(Protocol.From_protocol 023)
  @@ fun protocol ->
  let parameters =
    (* we update parameters for faster testing: no need to wait
       2 cycles for the consensus key to activate. *)
    [
      (["blocks_per_cycle"], `Int blocks_per_cycle);
      (["nonce_revelation_threshold"], `Int 2);
      (["consensus_rights_delay"], `Int consensus_rights_delay);
      (["cache_sampler_state_cycles"], `Int (consensus_rights_delay + 3));
      (["cache_stake_distribution_cycles"], `Int (consensus_rights_delay + 3));
      (["allow_tz4_delegate_enable"], `Bool true);
    ]
  in
  let* parameter_file =
    Protocol.write_parameter_file ~base:(Right (protocol, None)) parameters
  in
  let* _, client =
    Client.init_with_protocol ~parameter_file ~protocol `Client ()
  in

  let delegate = Constant.bootstrap1 in
  let* consensus_key_bls =
    Client.gen_and_show_keys ~alias:"consensus_key" ~sig_alg:"bls" client
  in
  let* companion_key_bls =
    Client.gen_and_show_keys ~alias:"companion_key" ~sig_alg:"bls" client
  in

  Log.info "Updating consensus key and companion key." ;
  let* () =
    Client.update_consensus_key
      ~hooks
      ~src:delegate.alias
      ~pk:consensus_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* () =
    Client.update_companion_key
      ~hooks
      ~src:delegate.alias
      ~pk:companion_key_bls.alias
      client
  in
  let* () = Client.bake_for_and_wait client in

  Log.info "Waiting for consensus and companion keys activation" ;
  let* () = bake_n_cycles (consensus_rights_delay + 1) client in

  Log.info "Checking keys are activated" ;
  let* () =
    Consensus_key.check_consensus_key
      ~__LOC__
      delegate
      ~expected_active:consensus_key_bls
      client
  in
  let* () =
    check_companion_key
      ~__LOC__
      delegate
      ~expected_active:companion_key_bls
      client
  in
  unit

let register ~protocols =
  let () = test_update_companion_key protocols in
  ()
