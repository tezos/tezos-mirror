(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type consensus_kind = Attestation | Preattestation

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
  RPC.Client.call client @@ RPC.get_chain_block_hash ~block ()

let make ~branch ?signer ~kind contents =
  {branch; contents; kind; signer; raw = None}

let json t = `O [("branch", Ezjsonm.string t.branch); ("contents", t.contents)]

let raw ?protocol t client =
  match t.raw with
  | None -> (
      match protocol with
      | None ->
          let* raw =
            RPC.Client.call client
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
              match kind with Preattestation -> "\x12" | Attestation -> "\x13"
            in
            Tezos_crypto.Signature.Custom
              (Bytes.cat (Bytes.of_string prefix) (Bytes.of_string chain_id))
        | Anonymous | Voting | Manager ->
            Tezos_crypto.Signature.Generic_operation
      in
      let* hex = hex ?protocol t client in
      let bytes = Hex.to_bytes hex in
      return (Account.sign_bytes ~watermark ~signer bytes)

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

let inject ?(request = `Inject) ?(force = false) ?protocol ?signature ?error t
    client : [`OpHash of string] Lwt.t =
  let* signature =
    match signature with
    | None -> sign t client
    | Some signature -> return signature
  in
  let* (`Hex op) = hex ?protocol ~signature t client in
  let inject_rpc =
    if force then RPC.post_private_injection_operation
    else RPC.post_injection_operation
  in
  let waiter =
    let mode = Client.get_mode client in
    match Client.mode_to_endpoint mode with
    | None -> Test.fail "Operation.inject: Endpoint expected"
    | Some (Proxy_server _ | Foreign_endpoint _) ->
        Test.fail
          "Operation.inject: Node endpoint expected instead of proxy server or \
           foreign endpoint"
    | Some (Node node) -> Node.wait_for_request ~request node
  in
  let runnable = RPC.Client.spawn client @@ inject_rpc (Data (`String op)) in
  match error with
  | None ->
      let* () = waiter in
      let*! oph_json = runnable in
      return (`OpHash (JSON.as_string oph_json))
  | Some msg ->
      let*? process = runnable in
      let* () = Process.check_error ~msg process in
      hash t client

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
    | Some (Proxy_server _ | Foreign_endpoint _) ->
        Test.fail
          "Operation.inject: Node endpoint expected instead of proxy server or \
           foreign endpoint"
    | Some (Node node) -> Node.wait_for_request ~request node
  in
  let rpc =
    RPC.post_private_injection_operations ?use_tmp_file ~force ~ops ()
  in
  match error with
  | None ->
      let* ophs = RPC.Client.call client rpc in
      let* () = waiter in
      return ophs
  | Some msg ->
      let*? process = RPC.Client.spawn client rpc in
      let* () = Process.check_error ~msg process in
      Lwt_list.map_s (fun op -> hash op client) t

let make_run_operation_input ?chain_id t client =
  let* chain_id =
    match chain_id with
    | Some chain_id -> return chain_id
    | None -> RPC.(Client.call client (get_chain_chain_id ()))
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

module Consensus = struct
  type t =
    | Consensus of {
        kind : consensus_kind;
        use_legacy_name : bool;
        slot : int;
        level : int;
        round : int;
        block_payload_hash : string;
      }
    | Dal_attestation of {attestation : bool array; level : int}

  let consensus ~use_legacy_name ~kind ~slot ~level ~round ~block_payload_hash =
    Consensus {kind; use_legacy_name; slot; level; round; block_payload_hash}

  let attestation = consensus ~kind:Attestation

  let preattestation = consensus ~kind:Preattestation

  let dal_attestation ~attestation ~level = Dal_attestation {attestation; level}

  let kind_to_string kind use_legacy_name =
    let name = function true -> "endorsement" | false -> "attestation" in
    match kind with
    | Attestation -> name use_legacy_name
    | Preattestation -> Format.sprintf "pre%s" (name use_legacy_name)

  let json signer = function
    | Consensus {kind; use_legacy_name; slot; level; round; block_payload_hash}
      ->
        `O
          [
            ("kind", Ezjsonm.string (kind_to_string kind use_legacy_name));
            ("slot", Ezjsonm.int slot);
            ("level", Ezjsonm.int level);
            ("round", Ezjsonm.int round);
            ("block_payload_hash", Ezjsonm.string block_payload_hash);
          ]
    | Dal_attestation {attestation; level} ->
        let string_of_bool_vector attestation =
          let aux (acc, n) b =
            let bit = if b then 1 else 0 in
            (acc lor (bit lsl n), n + 1)
          in
          Array.fold_left aux (0, 0) attestation |> fst |> string_of_int
        in
        `O
          [
            ("kind", Ezjsonm.string "dal_attestation");
            ("attestor", Ezjsonm.string signer.Account.public_key_hash);
            ("attestation", Ezjsonm.string (string_of_bool_vector attestation));
            ("level", Ezjsonm.int level);
          ]

  let operation ?branch ?chain_id ~signer consensus_operation client =
    let json = `A [json signer consensus_operation] in
    let* branch =
      match branch with
      | None -> get_branch ~offset:0 client
      | Some branch -> return branch
    in
    let* chain_id =
      match chain_id with
      | None -> RPC.Client.call client @@ RPC.get_chain_chain_id ()
      | Some branch -> return branch
    in
    let kind =
      match consensus_operation with
      | Consensus {kind; _} -> kind
      | Dal_attestation _ -> Attestation
    in
    return (make ~branch ~signer ~kind:(Consensus {kind; chain_id}) json)

  let inject ?request ?force ?branch ?chain_id ?error ~signer consensus client =
    let* op = operation ?branch ?chain_id ~signer consensus client in
    inject ?request ?force ?error op client
end

module Anonymous = struct
  type double_consensus_evidence_kind =
    | Double_attestation_evidence
    | Double_preattestation_evidence

  type nonrec t =
    | Double_consensus_evidence of {
        kind : double_consensus_evidence_kind;
        use_legacy_name : bool;
        op1 : t * Tezos_crypto.Signature.t;
        op2 : t * Tezos_crypto.Signature.t;
      }

  let double_consensus_evidence ~kind ~use_legacy_name
      (({kind = op1_kind; _}, _) as op1) (({kind = op2_kind; _}, _) as op2) =
    match (kind, op1_kind, op2_kind) with
    | ( Double_attestation_evidence,
        Consensus {kind = Attestation; _},
        Consensus {kind = Attestation; _} ) ->
        Double_consensus_evidence {kind; use_legacy_name; op1; op2}
    | ( Double_preattestation_evidence,
        Consensus {kind = Preattestation; _},
        Consensus {kind = Preattestation; _} ) ->
        Double_consensus_evidence {kind; use_legacy_name; op1; op2}
    | _, _, _ ->
        Test.fail "Invalid arguments to create a double_consensus_evidence"

  let double_attestation_evidence =
    double_consensus_evidence ~kind:Double_attestation_evidence

  let double_preattestation_evidence =
    double_consensus_evidence ~kind:Double_preattestation_evidence

  let kind_to_string kind use_legacy_name =
    sf
      "double_%s_evidence"
      (Consensus.kind_to_string
         (match kind with
         | Double_attestation_evidence -> Attestation
         | Double_preattestation_evidence -> Preattestation)
         use_legacy_name)

  let denunced_op_json (op, signature) =
    `O
      [
        ("branch", `String op.branch);
        ("operations", List.hd (Ezjsonm.get_list Fun.id op.contents));
        ("signature", `String (Tezos_crypto.Signature.to_b58check signature));
      ]

  let json = function
    | Double_consensus_evidence {kind; use_legacy_name; op1; op2} ->
        let op1 = denunced_op_json op1 in
        let op2 = denunced_op_json op2 in
        `O
          [
            ("kind", Ezjsonm.string (kind_to_string kind use_legacy_name));
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
      RPC.Client.call client
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
    | Dal_publish_slot_header of {
        level : int;
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

  let dal_publish_slot_header ~level ~index ~commitment ~proof =
    Dal_publish_slot_header {level; index; commitment; proof}

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
    | Dal_publish_slot_header {level; index; commitment; proof} ->
        let slot_header =
          `O
            [
              ("slot_index", json_of_int index);
              ("published_level", json_of_int level);
              ("commitment", json_of_commitment commitment);
              ("commitment_proof", json_of_commitment_proof proof);
            ]
        in
        [
          ("kind", `String "dal_publish_slot_header");
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
    | Reveal _ | Origination _ | Dal_publish_slot_header _ | Delegation _ ->
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

  let inject ?request ?force ?branch ?signer ?error managers client =
    let* op = operation ?branch ?signer managers client in
    inject ?request ?force ?error op client

  let get_branch ?chain ?(offset = 2) client =
    let block = sf "head~%d" offset in
    RPC.Client.call client @@ RPC.get_chain_block_hash ?chain ~block ()
end

let conflict_error =
  rex
    {|The operation [\w\d]+ cannot be added because the mempool already contains a conflicting operation that should not be replaced \(e\.g\. an operation from the same manager with better fees\)\.|}
