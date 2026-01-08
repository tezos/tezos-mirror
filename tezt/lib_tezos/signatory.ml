(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type generic =
  [ `Activate_account
  | `Attestation
  | `Attestation_with_dal
  | `Ballot
  | `Dal_publish_commitment
  | `Delegation
  | `Double_attestation_evidence
  | `Double_baking_evidence
  | `Double_preattestation_evidence
  | `Drain_delegate
  | `Failing_noop
  | `Finalize_unstake
  | `Increase_paid_storage
  | `Origination
  | `Preattestation
  | `Proposals
  | `Register_global_constant
  | `Reveal
  | `Seed_nonce_revelation
  | `Set_delegate_parameters
  | `Set_deposits_limit
  | `Signature_prefix
  | `Smart_rollup_add_messages
  | `Smart_rollup_cement
  | `Smart_rollup_execute_outbox_message
  | `Smart_rollup_originate
  | `Smart_rollup_publish
  | `Smart_rollup_recover_bond
  | `Smart_rollup_refute
  | `Smart_rollup_timeout
  | `Stake
  | `Transaction
  | `Transfer_ticket
  | `Unstake
  | `Update_consensus_key
  | `Vdf_revelation
  | `Zk_rollup_origination
  | `Zk_rollup_publish
  | `Zk_rollup_update ]

type restriction =
  [ `Block
  | `Endorsement
  | `Failing_noop
  | `Preendorsement
  | `Generic of generic list ]

type key = {
  account : Account.key;
  active : bool;
  log_payloads : bool;
  restrictions : restriction list;
}

module Parameters = struct
  type persistent_state = {
    runner : Runner.t option;
    base_dir : string;
    config_file : string;
    port : int;
    vault_name : string;
    keys : key list;
    mutable pending_ready : unit option Lwt.u list;
  }

  type session_state = {mutable ready : bool}

  let base_default_name = "signatory"

  let default_colors =
    Log.Color.
      [|BG.blue ++ FG.magenta; BG.blue ++ FG.gray; BG.blue ++ FG.bright_white|]
end

open Parameters

type session_status = {
  process : Process.t;
  stdin : Lwt_io.output_channel;
  session_state : session_state;
}

type status = Not_running | Running of session_status

type t = {
  name : string;
  color : Tezt.Log.Color.t;
  path : string;
  persistent_state : persistent_state;
  mutable status : status;
}

let port signer = signer.persistent_state.port

let trigger_ready signer value =
  let pending = signer.persistent_state.pending_ready in
  signer.persistent_state.pending_ready <- [] ;
  List.iter (fun pending -> Lwt.wakeup_later pending value) pending

let set_ready signer =
  (match signer.status with
  | Not_running -> ()
  | Running status -> status.session_state.ready <- true) ;
  trigger_ready signer (Some ())

let check_ready signer =
  let uri =
    Uri.of_string
    @@ Format.sprintf
         "http://localhost:%d/authorized_keys"
         signer.persistent_state.port
  in
  Lwt.catch
    (fun () ->
      let* resp, _body = Cohttp_lwt_unix.Client.call `GET uri in
      return (resp.status = `OK))
    (fun _ -> return false)

let rec poll_ready signer =
  let* ready = check_ready signer in
  if ready then (
    set_ready signer ;
    unit)
  else
    let* () = Lwt_unix.sleep 0.5 in
    poll_ready signer

let config_arg signer = ["--config"; signer.config_file]

let spawn_command ?env ?hooks signatory command =
  Process.spawn_with_stdin
    ~name:signatory.name
    ~color:signatory.color
    ?runner:signatory.persistent_state.runner
    ?env
    ?hooks
    signatory.path
  @@ config_arg signatory.persistent_state
  @ command

let fresh_name =
  let next_name = ref 1 in
  fun () ->
    let index = !next_name in
    incr next_name ;
    base_default_name ^ string_of_int index

let get_next_color =
  let next_color = ref 0 in
  fun () ->
    let color = default_colors.(!next_color mod Array.length default_colors) in
    incr next_color ;
    color

let secret_file {base_dir; _} =
  Filename.concat
    base_dir
    "secret_keys" (* To be compatible with Account.write *)

let json_of_generic = function
  | `Activate_account -> `String "activate_account"
  | `Attestation -> `String "attestation"
  | `Attestation_with_dal -> `String "attestation_with_dal"
  | `Ballot -> `String "ballot"
  | `Dal_publish_commitment -> `String "dal_publish_commitment"
  | `Delegation -> `String "delegation"
  | `Double_attestation_evidence -> `String "double_attestation_evidence"
  | `Double_baking_evidence -> `String "double_baking_evidence"
  | `Double_preattestation_evidence -> `String "double_preattestation_evidence"
  | `Drain_delegate -> `String "drain_delegate"
  | `Failing_noop -> `String "failing_noop"
  | `Finalize_unstake -> `String "finalize_unstake"
  | `Increase_paid_storage -> `String "increase_paid_storage"
  | `Origination -> `String "origination"
  | `Preattestation -> `String "preattestation"
  | `Proposals -> `String "proposals"
  | `Register_global_constant -> `String "register_global_constant"
  | `Reveal -> `String "reveal"
  | `Seed_nonce_revelation -> `String "seed_nonce_revelation"
  | `Set_delegate_parameters -> `String "set_delegate_parameters"
  | `Set_deposits_limit -> `String "set_deposits_limit"
  | `Signature_prefix -> `String "signature_prefix"
  | `Smart_rollup_add_messages -> `String "smart_rollup_add_messages"
  | `Smart_rollup_cement -> `String "smart_rollup_cement"
  | `Smart_rollup_execute_outbox_message ->
      `String "smart_rollup_execute_outbox_message"
  | `Smart_rollup_originate -> `String "smart_rollup_originate"
  | `Smart_rollup_publish -> `String "smart_rollup_publish"
  | `Smart_rollup_recover_bond -> `String "smart_rollup_recover_bond"
  | `Smart_rollup_refute -> `String "smart_rollup_refute"
  | `Smart_rollup_timeout -> `String "smart_rollup_timeout"
  | `Stake -> `String "stake"
  | `Transaction -> `String "transaction"
  | `Transfer_ticket -> `String "transfer_ticket"
  | `Unstake -> `String "unstake"
  | `Update_consensus_key -> `String "update_consensus_key"
  | `Vdf_revelation -> `String "vdf_revelation"
  | `Zk_rollup_origination -> `String "zk_rollup_origination"
  | `Zk_rollup_publish -> `String "zk_rollup_publish"
  | `Zk_rollup_update -> `String "zk_rollup_update "

let json_of_restriction = function
  | `Block -> ("block", `A [])
  | `Endorsement -> ("endorsement", `A [])
  | `Failing_noop -> ("failing_noop", `A [])
  | `Preendorsement -> ("preendorsement", `A [])
  | `Generic generics -> ("generic", `A (List.map json_of_generic generics))

let json_of_key {account; active; log_payloads; restrictions} :
    (string * Yaml.value) option =
  if not active then None
  else
    Some
      ( account.public_key_hash,
        `O
          ([("log_payloads", `Bool log_payloads)]
          @
          if restrictions = [] then []
          else [("allow", `O (List.map json_of_restriction restrictions))]) )

let json_of_keys = List.filter_map json_of_key

let yaml_config
    {persistent_state = {base_dir; port; vault_name; keys; _} as state; _} =
  let members =
    [
      ("base_dir", `String base_dir);
      ("server", `O [("address", `String (":" ^ string_of_int port))]);
      ( "vaults",
        `O
          [
            ( vault_name,
              `O
                [
                  ("driver", `String "file");
                  ("config", `O [("file", `String (secret_file state))]);
                ] );
          ] );
      ("tezos", `O (json_of_keys keys));
    ]
  in
  match Yaml.of_json (`O members) with
  | Ok y -> y
  | Error (`Msg e) -> failwith ("Could not generate yaml signatory config: " ^ e)

let write_config signer =
  let yaml = yaml_config signer in
  let contents =
    match Yaml.yaml_to_string yaml with
    | Ok s -> s
    | Error (`Msg e) -> failwith ("Could not write yaml signatory config: " ^ e)
  in
  Base.write_file signer.persistent_state.config_file ~contents

let create ?name ?color ?base_dir ?port ?runner ?config_file ?vault_name keys =
  let name = match name with None -> fresh_name () | Some name -> name in
  let color =
    match color with None -> get_next_color () | Some color -> color
  in
  let base_dir =
    match base_dir with None -> Temp.dir name | Some dir -> dir
  in
  let port = match port with Some p -> p | None -> Port.fresh () in
  let config_file =
    match config_file with
    | Some c -> c
    | None -> Temp.file ?runner "signatory.yml"
  in
  let vault_name =
    match vault_name with None -> name ^ "_keys" | Some vn -> vn
  in
  let signer =
    {
      name;
      color;
      path = "signatory";
      persistent_state =
        {
          runner;
          base_dir;
          port;
          keys;
          config_file;
          vault_name;
          pending_ready = [];
        };
      status = Not_running;
    }
  in
  write_config signer ;
  Account.write (List.map (fun k -> k.account) keys) ~base_dir ;
  return signer

let wait_ready_promise promise =
  let* result = promise in
  match result with
  | None -> failwith "Signatory terminated before ready"
  | Some x -> return x

let wait_for_ready signer =
  match signer.status with
  | Running {session_state = {ready = true; _}; _} -> unit
  | Not_running | Running {session_state = {ready = false; _}; _} ->
      let promise, resolver = Lwt.task () in
      signer.persistent_state.pending_ready <-
        resolver :: signer.persistent_state.pending_ready ;
      wait_ready_promise promise

let run signer =
  (match signer.status with
  | Not_running -> ()
  | Running _ -> Test.fail "Signatory server %s is already running" signer.name) ;
  let process, stdin = spawn_command signer ["serve"] in
  let running_status = {process; session_state = {ready = false}; stdin} in
  signer.status <- Running running_status ;
  let _ =
    let* _process_status = Process.wait process in
    signer.status <- Not_running ;
    trigger_ready signer None ;
    unit
  in
  let _ = poll_ready signer in
  wait_for_ready signer

let terminate ?(timeout = 30.) signer =
  match signer.status with
  | Not_running -> ()
  | Running {process; _} -> Process.terminate ~timeout process

let restart signer =
  terminate signer ;
  run signer
