(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2024 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Runnable.Syntax

type consensus_kind = Attestation of {with_dal : bool} | Preattestation

type kind =
  | Consensus of {kind : consensus_kind; chain_id : string}
  | Anonymous
  | Voting
  | Manager

type t = {
  branch : string;
  contents : JSON.u;
  kind : kind;
  signer : Account.key option;
  mutable raw : Hex.t option;
      (* This is mutable to avoid computing the raw representation several times. *)
}

let get_branch ?(offset = 2) client =
  let block = sf "head~%d" offset in
  Client.RPC.call client @@ RPC.get_chain_block_hash ~block ()

let make ~branch ?signer ~kind contents =
  {branch; contents; kind; signer; raw = None}

let json t = `O [("branch", Ezjsonm.string t.branch); ("contents", t.contents)]

let raw ?protocol t client =
  match t.raw with
  | None -> (
      match protocol with
      | None ->
          let* raw =
            Client.RPC.call client
            @@ RPC.post_chain_block_helpers_forge_operations
                 ~data:(Data (json t))
                 ()
            |> Lwt.map JSON.as_string
          in
          t.raw <- Some (`Hex raw) ;
          return (`Hex raw)
      | Some p -> (
          let name = Protocol.daemon_name p ^ ".operation.unsigned" in
          match Data_encoding.Registration.find name with
          | None -> Test.fail "%s encoding was not found" name
          | Some registered -> (
              match
                Data_encoding.Registration.bytes_of_json registered (json t)
              with
              | None ->
                  Test.fail
                    "encoding of %s with %s failed"
                    (Ezjsonm.to_string (json t))
                    name
              | Some bytes -> return (Hex.of_bytes bytes))))
  | Some raw -> return raw

let hex ?protocol ?signature t client =
  let* (`Hex raw) = raw ?protocol t client in
  match signature with
  | None -> return (`Hex raw)
  | Some signature ->
      let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
      return (`Hex (raw ^ signature))

let sign ?protocol ({kind; signer; _} as t) client =
  match signer with
  | None -> return Tezos_crypto.Signature.zero
  | Some signer ->
      let watermark =
        match kind with
        | Consensus {kind; chain_id} ->
            let chain_id =
              Tezos_crypto.Hashed.Chain_id.to_string
                (Tezos_crypto.Hashed.Chain_id.of_b58check_exn chain_id)
            in
            let prefix =
              match kind with
              | Preattestation -> "\x12"
              | Attestation _ -> "\x13"
            in
            Tezos_crypto.Signature.Custom
              (Bytes.cat (Bytes.of_string prefix) (Bytes.of_string chain_id))
        | Anonymous | Voting | Manager ->
            Tezos_crypto.Signature.Generic_operation
      in
      let* hex = hex ?protocol t client in
      let bytes = Hex.to_bytes hex in
      return (Account.sign_bytes ~watermark ~signer bytes)

let signed_hex ?protocol ?signature t client =
  let* signature =
    match signature with
    | None -> sign t client
    | Some signature -> return signature
  in
  hex ?protocol ~signature t client

let byte_size ?protocol ?signature t client =
  let* hex = signed_hex ?protocol ?signature t client in
  return (Bytes.length (Hex.to_bytes hex))

module Tezos_operation = Tezos_base.TzPervasives.Operation

let hash t client : [`OpHash of string] Lwt.t =
  let* signature = sign t client in
  let* (`Hex hex) = hex ~signature t client in
  let bytes = Hex.to_bytes (`Hex hex) in
  let op =
    Data_encoding.Binary.of_bytes_exn Tezos_base.Operation.encoding bytes
  in
  let hash = Tezos_base.Operation.hash op in
  return (`OpHash (Tezos_crypto.Hashed.Operation_hash.to_b58check hash))

let spawn_inject ?(force = false) ?protocol ?signature t client =
  let* (`Hex op) = signed_hex ?protocol ?signature t client in
  let inject_rpc =
    if force then RPC.post_private_injection_operation
    else RPC.post_injection_operation
  in
  return (Client.RPC.spawn client @@ inject_rpc (Data (`String op)))

let inject ?(dont_wait = false) ?(request = `Inject) ?force ?protocol ?signature
    ?error t client : [`OpHash of string] Lwt.t =
  let waiter =
    if dont_wait then Lwt.return_unit
    else
      let mode = Client.get_mode client in
      match Client.mode_to_endpoint mode with
      | None -> Test.fail "Operation.inject: Endpoint expected"
      | Some (Foreign_endpoint _) ->
          Test.fail
            "Operation.inject: Node endpoint expected instead of foreign \
             endpoint"
      | Some (Node node) -> Node.wait_for_request ~request node
  in
  let* runnable = spawn_inject ?force ?protocol ?signature t client in
  match error with
  | None ->
      let* () = waiter in
      let*! oph_json = runnable in
      return (`OpHash (JSON.as_string oph_json))
  | Some msg ->
      let*? process = runnable in
      let* () = Process.check_error ~msg process in
      hash t client

let inject_and_capture1_stderr ~rex ?force ?protocol ?signature t client =
  let* runnable = spawn_inject ?force ?protocol ?signature t client in
  let*? process = runnable in
  let* stderr = Process.check_and_read_stderr ~expect_failure:true process in
  match stderr =~* rex with
  | None ->
      Test.fail
        "Injection was expected to fail with:\n%s\nbut instead failed with:\n%s"
        (show_rex rex)
        stderr
  | Some groups -> return groups

let inject_and_capture2_stderr ~rex ?force ?protocol ?signature t client =
  let* runnable = spawn_inject ?force ?protocol ?signature t client in
  let*? process = runnable in
  let* stderr = Process.check_and_read_stderr ~expect_failure:true process in
  match stderr =~** rex with
  | None ->
      Test.fail
        "Injection was expected to fail with:\n%s\nbut instead failed with:\n%s"
        (show_rex rex)
        stderr
  | Some groups -> return groups

let inject_operations ?protocol ?(request = `Inject) ?(force = false) ?error
    ?use_tmp_file t client : [`OpHash of string] list Lwt.t =
  let forge op =
    let* signature = sign ?protocol op client in
    hex ?protocol ~signature op client
  in
  let* ops = Lwt_list.map_s forge t in
  let waiter =
    let mode = Client.get_mode client in
    match Client.mode_to_endpoint mode with
    | None -> Test.fail "Operation.inject: Endpoint expected"
    | Some (Foreign_endpoint _) ->
        Test.fail
          "Operation.inject: Node endpoint expected instead of foreign endpoint"
    | Some (Node node) -> Node.wait_for_request ~request node
  in
  let rpc =
    RPC.post_private_injection_operations ?use_tmp_file ~force ~ops ()
  in
  match error with
  | None ->
      let* ophs = Client.RPC.call client rpc in
      let* () = waiter in
      return ophs
  | Some msg ->
      let*? process = Client.RPC.spawn client rpc in
      let* () = Process.check_error ~msg process in
      Lwt_list.map_s (fun op -> hash op client) t

let make_run_operation_input ?chain_id t client =
  let* chain_id =
    match chain_id with
    | Some chain_id -> return chain_id
    | None -> Client.RPC.call client (RPC.get_chain_chain_id ())
  in
  (* The [run_operation] RPC does not check the signature. *)
  let signature = Tezos_crypto.Signature.zero in
  return
    (`O
      [
        ( "operation",
          `O
            [
              ("branch", `String t.branch);
              ("contents", t.contents);
              ( "signature",
                `String (Tezos_crypto.Signature.to_b58check signature) );
            ] );
        ("chain_id", `String chain_id);
      ])

let make_preapply_operation_input ~protocol ~signature t =
  let protocol = Protocol.hash protocol in
  `O
    [
      ("protocol", `String protocol);
      ("branch", `String t.branch);
      ("contents", t.contents);
      ("signature", `String (Tezos_crypto.Signature.to_b58check signature));
    ]

module Consensus = struct
  type consensus_content = {
    slot : int;
    level : int;
    round : int;
    block_payload_hash : string;
  }

  type dal_content = {attestation : bool array}

  type t =
    | CPreattestation of {consensus : consensus_content}
    | CAttestation of {consensus : consensus_content; dal : dal_content option}

  let consensus ~kind ~slot ~level ~round ~block_payload_hash =
    let consensus = {slot; level; round; block_payload_hash} in
    match kind with
    | Preattestation -> CPreattestation {consensus}
    | Attestation {with_dal} ->
        assert (with_dal = false) ;
        CAttestation {consensus; dal = None}

  let attestation ~slot ~level ~round ~block_payload_hash ?dal_attestation () =
    let consensus = {slot; level; round; block_payload_hash} in
    CAttestation
      {
        consensus;
        dal = Option.map (fun attestation -> {attestation}) dal_attestation;
      }

  let preattestation ~slot ~level ~round ~block_payload_hash =
    let consensus = {slot; level; round; block_payload_hash} in
    CPreattestation {consensus}

  let string_of_bool_vector dal_attestation =
    let aux (acc, n) b =
      let bit = if b then 1 else 0 in
      (acc lor (bit lsl n), n + 1)
    in
    Array.fold_left aux (0, 0) dal_attestation |> fst |> string_of_int

  let kind_to_string kind =
    match kind with
    | Attestation {with_dal} ->
        "attestation" ^ if with_dal then "_with_dal" else ""
    | Preattestation -> Format.sprintf "preattestation"

  let json = function
    | CAttestation {consensus; dal} ->
        let with_dal = Option.is_some dal in
        `O
          ([
             ("kind", Ezjsonm.string (kind_to_string (Attestation {with_dal})));
             ("slot", Ezjsonm.int consensus.slot);
             ("level", Ezjsonm.int consensus.level);
             ("round", Ezjsonm.int consensus.round);
             ("block_payload_hash", Ezjsonm.string consensus.block_payload_hash);
           ]
          @
          match dal with
          | None -> []
          | Some {attestation} ->
              [
                ( "dal_attestation",
                  Ezjsonm.string (string_of_bool_vector attestation) );
              ])
    | CPreattestation {consensus} ->
        `O
          [
            ("kind", Ezjsonm.string (kind_to_string Preattestation));
            ("slot", Ezjsonm.int consensus.slot);
            ("level", Ezjsonm.int consensus.level);
            ("round", Ezjsonm.int consensus.round);
            ("block_payload_hash", Ezjsonm.string consensus.block_payload_hash);
          ]

  let operation ?branch ?chain_id ~signer consensus_operation client =
    let json = `A [json consensus_operation] in
    let* branch =
      match branch with
      | None -> get_branch ~offset:0 client
      | Some branch -> return branch
    in
    let* chain_id =
      match chain_id with
      | None -> Client.RPC.call client @@ RPC.get_chain_chain_id ()
      | Some branch -> return branch
    in
    let kind =
      match consensus_operation with
      | CPreattestation _ -> Preattestation
      | CAttestation _ -> Attestation {with_dal = false}
    in
    return (make ~branch ~signer ~kind:(Consensus {kind; chain_id}) json)

  let inject ?request ?force ?branch ?chain_id ?error ~signer consensus client =
    let* op = operation ?branch ?chain_id ~signer consensus client in
    inject ?request ?force ?error op client

  let get_slots ~level client =
    Client.RPC.call client @@ RPC.get_chain_block_helper_validators ~level ()

  let first_slot ~slots_json (delegate : Account.key) =
    let open JSON in
    match
      List.find_opt
        (fun slots ->
          String.equal
            (slots |-> "delegate" |> as_string)
            delegate.public_key_hash)
        (as_list slots_json)
    with
    | Some slots -> List.hd (slots |-> "slots" |> as_list) |> as_int
    | None ->
        Test.fail
          "No slots found for %s in: %s"
          delegate.public_key_hash
          (JSON.encode slots_json)

  let get_block_payload_hash ?block client =
    let* block_header =
      Client.RPC.call client @@ RPC.get_chain_block_header ?block ()
    in
    return JSON.(block_header |-> "payload_hash" |> as_string)
end

module Anonymous = struct
  type double_consensus_evidence_kind =
    | Double_attestation_evidence
    | Double_preattestation_evidence

  type nonrec t =
    | Double_consensus_evidence of {
        kind : double_consensus_evidence_kind;
        op1 : t * Tezos_crypto.Signature.t;
        op2 : t * Tezos_crypto.Signature.t;
      }

  let double_consensus_evidence ~kind (({kind = op1_kind; _}, _) as op1)
      (({kind = op2_kind; _}, _) as op2) =
    match (kind, op1_kind, op2_kind) with
    | ( Double_attestation_evidence,
        Consensus {kind = Attestation {with_dal = false}; _},
        Consensus {kind = Attestation {with_dal = false}; _} ) ->
        Double_consensus_evidence {kind; op1; op2}
    | ( Double_preattestation_evidence,
        Consensus {kind = Preattestation; _},
        Consensus {kind = Preattestation; _} ) ->
        Double_consensus_evidence {kind; op1; op2}
    | _, _, _ ->
        Test.fail "Invalid arguments to create a double_consensus_evidence"

  let double_attestation_evidence =
    double_consensus_evidence ~kind:Double_attestation_evidence

  let double_preattestation_evidence =
    double_consensus_evidence ~kind:Double_preattestation_evidence

  let kind_to_string kind =
    sf
      "double_%s_evidence"
      (Consensus.kind_to_string
         (match kind with
         | Double_attestation_evidence -> Attestation {with_dal = false}
         | Double_preattestation_evidence -> Preattestation))

  let denunced_op_json (op, signature) =
    `O
      [
        ("branch", `String op.branch);
        ("operations", List.hd (Ezjsonm.get_list Fun.id op.contents));
        ("signature", `String (Tezos_crypto.Signature.to_b58check signature));
      ]

  let json = function
    | Double_consensus_evidence {kind; op1; op2} ->
        let op1 = denunced_op_json op1 in
        let op2 = denunced_op_json op2 in
        `O
          [
            ("kind", Ezjsonm.string (kind_to_string kind));
            ("op1", op1);
            ("op2", op2);
          ]

  let operation ?branch anonymous_operation client =
    let json = `A [json anonymous_operation] in
    let* branch =
      match branch with
      | None -> get_branch ~offset:0 client
      | Some branch -> return branch
    in
    return (make ~branch ~kind:Anonymous json)

  let inject ?request ?force ?branch ?error consensus client =
    let* op = operation ?branch consensus client in
    inject ?request ?force ?error op client

  let as_consensus_kind = function
    | Double_attestation_evidence -> Attestation {with_dal = false}
    | Double_preattestation_evidence -> Preattestation

  let arbitrary_block_payload_hash1 =
    "vh1g87ZG6scSYxKhspAUzprQVuLAyoa5qMBKcUfjgnQGnFb3dJcG"

  let arbitrary_block_payload_hash2 =
    "vh3cjL2UL3p73CHhSLpAcLvB9obU9jSrRsu1Y9tg85os3i3akAig"

  let make_double_consensus_evidence_with_distinct_bph ~kind ~misbehaviour_level
      ~misbehaviour_round ~culprit client =
    let* slots =
      Client.RPC.call client
      @@ RPC.get_chain_block_helper_validators
           ~delegate:culprit.Account.public_key_hash
           ~level:misbehaviour_level
           ()
    in
    let slot =
      JSON.(
        slots |> as_list |> List.hd |-> "slots" |> as_list |> List.hd |> as_int)
    in
    let mk_consensus_op block_payload_hash =
      let consensus =
        Consensus.consensus
          ~kind:(as_consensus_kind kind)
          ~slot
          ~level:misbehaviour_level
          ~round:misbehaviour_round
          ~block_payload_hash
      in
      Consensus.operation ~signer:culprit consensus client
    in
    let* op1 = mk_consensus_op arbitrary_block_payload_hash1
    and* op2 = mk_consensus_op arbitrary_block_payload_hash2 in
    let* (`OpHash oph1) = hash op1 client
    and* (`OpHash oph2) = hash op2 client in
    let op1, op2 =
      if String.compare oph1 oph2 < 1 then (op1, op2) else (op2, op1)
    in
    let* op1_sign = sign op1 client and* op2_sign = sign op2 client in
    return (double_consensus_evidence ~kind (op1, op1_sign) (op2, op2_sign))
end

module Voting = struct
  type t =
    | Proposals of {
        source : Account.key;
        period : int;
        proposals : string list;  (** Candidate protocol hashes *)
      }

  let proposals source period proposals = Proposals {source; period; proposals}

  let get_source_and_make_contents = function
    | Proposals {source; period; proposals} ->
        let proposals_contents =
          [
            ("kind", `String "proposals");
            ("source", `String source.Account.public_key_hash);
            ("period", Ezjsonm.int period);
            ("proposals", Ezjsonm.list Ezjsonm.string proposals);
          ]
        in
        let contents = `A [`O proposals_contents] in
        (source, contents)

  let operation ?branch ?client ?signer t =
    let* branch =
      match branch with
      | Some branch -> return branch
      | None -> (
          match client with
          | Some client -> get_branch ~offset:0 client
          | None ->
              raise
                (Invalid_argument
                   "At least one of arguments [branch] and [client] must be \
                    provided."))
    in
    let source, contents = get_source_and_make_contents t in
    let signer = Option.value signer ~default:source in
    return (make ~branch ~signer ~kind:Voting contents)

  let inject ?request ?force ?signature ?error ?branch ?signer t client =
    let* op = operation ?branch ?signer ~client t in
    inject ?request ?force ?signature ?error op client
end

module Manager = struct
  let json_of_account account = Ezjsonm.string account.Account.public_key_hash

  let json_of_tez n = string_of_int n |> Ezjsonm.string

  let json_of_int_as_string n = string_of_int n |> Ezjsonm.string

  let json_of_int n = float_of_int n |> Ezjsonm.float

  let json_of_commitment commitment =
    Data_encoding.Json.construct
      Tezos_crypto_dal.Cryptobox.Commitment.encoding
      commitment

  let json_of_commitment_proof proof =
    Data_encoding.Json.construct
      Tezos_crypto_dal.Cryptobox.Commitment_proof.encoding
      proof

  let get_next_counter ?(source = Constant.bootstrap1) client =
    let* counter_json =
      Client.RPC.call client
      @@ RPC.get_chain_block_context_contract_counter
           ~id:source.Account.public_key_hash
           ()
    in
    return (1 + JSON.as_int counter_json)

  let json_of_option f v = Option.fold ~none:`Null ~some:f v

  let strip_null_fields l =
    List.fold_left
      (fun acc e -> match e with _, `Null -> acc | e -> e :: acc)
      []
      l
    |> List.rev

  type sc_rollup_dissection_chunk = {state_hash : string option; tick : int}

  type sc_rollup_proof = Ezjsonm.value

  type sc_rollup_game_refutation_step =
    | Proof of sc_rollup_proof
    | Dissection of sc_rollup_dissection_chunk list

  type sc_rollup_refutation =
    | Start of {
        player_commitment_hash : string;
        opponent_commitment_hash : string;
      }
    | Move of {
        choice_tick : int;
        refutation_step : sc_rollup_game_refutation_step;
      }

  let json_of_sc_rollup_dissection ~dissection =
    Ezjsonm.list
      (fun {state_hash; tick} ->
        let sh = json_of_option Ezjsonm.string state_hash in
        `O
          (strip_null_fields
             [("state", sh); ("tick", json_of_int_as_string tick)]))
      dissection

  let json_payload_binding_of_sc_rollup_refutation sc_rollup opponent refutation
      =
    let json_of_refutation_step = function
      | Start {player_commitment_hash; opponent_commitment_hash} ->
          `O
            [
              ("refutation_kind", `String "start");
              ("player_commitment_hash", `String player_commitment_hash);
              ("opponent_commitment_hash", `String opponent_commitment_hash);
            ]
      | Move {choice_tick; refutation_step} ->
          let step =
            match refutation_step with
            | Proof proof -> proof
            | Dissection dissection -> json_of_sc_rollup_dissection ~dissection
          in
          `O
            [
              ("refutation_kind", `String "move");
              ("choice", json_of_int_as_string choice_tick);
              ("step", step);
            ]
    in
    let refutation = json_of_refutation_step refutation in
    strip_null_fields
      [
        ("kind", `String "smart_rollup_refute");
        ("rollup", `String sc_rollup);
        ("opponent", `String opponent);
        ("refutation", refutation);
      ]

  type transfer_parameters = {entrypoint : string; arg : JSON.u}

  type payload =
    | Reveal of Account.key
    | Transfer of {
        amount : int;
        dest : string;
        parameters : transfer_parameters option;
      }
    | Origination of {code : JSON.u; storage : JSON.u; balance : int}
    | Dal_publish_commitment of {
        index : int;
        commitment : Tezos_crypto_dal.Cryptobox.commitment;
        proof : Tezos_crypto_dal.Cryptobox.commitment_proof;
      }
    | Delegation of {delegate : Account.key}
    | Sc_rollup_refute of {
        (* See details in {!Operation_repr} module. *)
        sc_rollup : string;
        opponent : string;
        refutation : sc_rollup_refutation;
      }

  let reveal account = Reveal account

  let transfer ?(dest = Constant.bootstrap2) ?(amount = 1_000_000) () =
    Transfer {amount; dest = dest.public_key_hash; parameters = None}

  let call ?(dest = "KT1LfQjDNgPpdwMHbhzyQcD8GTE2L4rwxxpN") ?(amount = 0)
      ?(entrypoint = "default") ?(arg = `O [("prim", `String "Unit")]) () =
    Transfer {amount; dest; parameters = Some {entrypoint; arg}}

  let dal_publish_commitment ~index ~commitment ~proof =
    Dal_publish_commitment {index; commitment; proof}

  let origination ?(init_balance = 0) ~code ~init_storage () =
    Origination {code; storage = init_storage; balance = init_balance}

  let delegation ?(delegate = Constant.bootstrap2) () = Delegation {delegate}

  let sc_rollup_refute ~refutation ~sc_rollup ~opponent () =
    Sc_rollup_refute {sc_rollup; opponent; refutation}

  type t = {
    source : Account.key;
    counter : int option;
    fee : int;
    gas_limit : int;
    storage_limit : int;
    payload : payload;
  }

  let json_payload_binding = function
    | Reveal account ->
        [("kind", `String "reveal"); ("public_key", `String account.public_key)]
    | Transfer {amount; dest; parameters} ->
        let parameters =
          match parameters with
          | None -> []
          | Some {entrypoint; arg} ->
              [
                ( "parameters",
                  `O [("entrypoint", `String entrypoint); ("value", arg)] );
              ]
        in
        [
          ("kind", `String "transaction");
          ("amount", json_of_tez amount);
          ("destination", `String dest);
        ]
        @ parameters
    | Origination {code; storage; balance} ->
        let script = `O [("code", code); ("storage", storage)] in
        [
          ("kind", `String "origination");
          ("balance", json_of_tez balance);
          ("script", script);
        ]
    | Dal_publish_commitment {index; commitment; proof} ->
        let slot_header =
          `O
            [
              ("slot_index", json_of_int index);
              ("commitment", json_of_commitment commitment);
              ("commitment_proof", json_of_commitment_proof proof);
            ]
        in
        [
          ("kind", `String "dal_publish_commitment");
          ("slot_header", slot_header);
        ]
    | Delegation {delegate} ->
        [("kind", `String "delegation"); ("delegate", json_of_account delegate)]
    | Sc_rollup_refute {sc_rollup; opponent; refutation} ->
        json_payload_binding_of_sc_rollup_refutation
          sc_rollup
          opponent
          refutation

  let json client {source; counter; fee; gas_limit; storage_limit; payload} =
    let* counter =
      match counter with
      | None -> get_next_counter ~source client
      | Some counter -> return counter
    in
    let json_bindings =
      [
        ("source", json_of_account source);
        ("fee", json_of_tez fee);
        ("counter", json_of_int_as_string counter);
        ("gas_limit", json_of_int_as_string gas_limit);
        ("storage_limit", json_of_int_as_string storage_limit);
      ]
      @ json_payload_binding payload
    in
    return (`O json_bindings)

  let operation ?branch ?signer manager_operations client =
    let* json_list = Lwt_list.map_s (json client) manager_operations in
    let json = `A json_list in
    let signer =
      match signer with
      | None -> (
          match manager_operations with
          | [] ->
              Test.fail
                "Operation_core: Cannot sign the operation because no signer \
                 was provided and there is no manager to sign."
          | {source; _} :: _ -> source)
      | Some signer -> signer
    in
    let* branch =
      match branch with
      | None -> get_branch client
      | Some branch -> return branch
    in
    return (make ~branch ~signer ~kind:Manager json)

  let make ?(source = Constant.bootstrap1) ?counter ?fee ?gas_limit
      ?storage_limit payload =
    (* Default value are set individually for each manager
       operation. They are close from the default values set by the
       client. *)
    match payload with
    | Transfer _ ->
        let fee = Option.value fee ~default:1_000 in
        let gas_limit = Option.value gas_limit ~default:1_040 in
        let storage_limit = Option.value storage_limit ~default:257 in
        {source; counter; fee; gas_limit; storage_limit; payload}
    | Dal_publish_commitment _ ->
        let fee = Option.value fee ~default:2_100 in
        let gas_limit = Option.value gas_limit ~default:17_000 in
        let storage_limit = Option.value storage_limit ~default:0 in
        {source; counter; fee; gas_limit; storage_limit; payload}
    | Reveal _ | Origination _ | Delegation _ ->
        let fee = Option.value fee ~default:1_450 in
        let gas_limit = Option.value gas_limit ~default:1_490 in
        let storage_limit = Option.value storage_limit ~default:0 in
        {source; counter; fee; gas_limit; storage_limit; payload}
    | Sc_rollup_refute _ ->
        let fee = Option.value fee ~default:12_000 in
        let gas_limit = Option.value gas_limit ~default:12_000 in
        let storage_limit = Option.value storage_limit ~default:1028 in
        {source; counter; fee; gas_limit; storage_limit; payload}

  let make_batch ?source ?fee ?gas_limit ?storage_limit ~counter payloads =
    List.mapi
      (fun i payload ->
        let counter = counter + i in
        make ?source ?fee ?gas_limit ?storage_limit ~counter payload)
      payloads

  let inject ?dont_wait ?request ?force ?branch ?signer ?error managers client =
    let* op = operation ?branch ?signer managers client in
    inject ?dont_wait ?request ?force ?error op client

  let get_branch ?chain ?(offset = 2) client =
    let block = sf "head~%d" offset in
    Client.RPC.call client @@ RPC.get_chain_block_hash ?chain ~block ()

  let mk_single_transfer ?source ?counter ?fee ?gas_limit ?storage_limit ?dest
      ?amount ?branch ?signer client =
    let payload =
      make
        ?source
        ?counter
        ?fee
        ?gas_limit
        ?storage_limit
        (transfer ?dest ?amount ())
    in
    operation ?branch ?signer [payload] client

  let inject_single_transfer ?source ?counter ?fee ?gas_limit ?storage_limit
      ?dest ?amount ?request ?force ?branch ?signer ?error client =
    let payload =
      make
        ?source
        ?counter
        ?fee
        ?gas_limit
        ?storage_limit
        (transfer ?dest ?amount ())
    in
    inject ?request ?force ?branch ?signer ?error [payload] client
end

let gas_limit_exceeded =
  rex
    "Gas limit exceeded during typechecking or execution.\n\
     Try again with a higher gas limit."

let conflict_error_with_needed_fee =
  rex
    {|The operation ([\w\d]+) cannot be added because the mempool already contains a conflicting operation\. To replace the latter, this particular operation would need a total fee of at least ([\d]+) mutez\.|}

let conflict_error_no_possible_fee =
  rex
    {|The operation ([\w\d]+) cannot be added because the mempool already contains a conflicting operation\. The pre-existing operation cannot be replaced with the new one, even if fees were increased\. Try again after the next block has been baked\.|}

let rejected_by_full_mempool_with_needed_fee =
  rex
    {|Operation ([\w\d]+) has been rejected because the mempool is full\. This specific operation would need a total fee of at least ([\d]+) mutez to be considered and propagated by the mempool of this particular node right now\. Note that if the node receives operations with a better fee over gas limit ratio in the future, the operation may be rejected even with the indicated fee, or it may be successfully injected but removed at a later date\.|}

let rejected_by_full_mempool_no_possible_fee =
  rex
    {|Operation ([\w\d]+) has been rejected because the mempool is full\. The operation cannot be accepted by this node at the moment, regardless of its fee\. Try again after the next block has been baked\.|}

let inject_error_check_recommended_fee ~loc ~rex ~expected_fee op client =
  let* _oph, needed_fee = inject_and_capture2_stderr ~rex op client in
  Check.(
    (int_of_string needed_fee = expected_fee)
      int
      ~error_msg:("The recommended fee is %L but expected %R at " ^ loc)) ;
  unit

let dal_data_availibility_attester_not_in_committee =
  rex
    {|The attester (tz[\w\d]+), with slot ([\d]+), is not part of the DAL committee for the level ([\d]+)\.|}

let already_denounced =
  rex
    {|Delegate ([\w\d]+) at level ([\d]+) has already been denounced for a double ([\w]+).|}

let outdated_denunciation =
  rex
    {|A double-([\w]+) denunciation is outdated \(last acceptable cycle: ([\d]+), given level: ([\d]+)\).|}

let injection_error_unknown_branch =
  rex {|Operation ([\w\d]+) is branched on either:|}
