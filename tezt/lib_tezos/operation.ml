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

open Lwt.Infix

(* For Smart contracts' script, initial storage and arguments, we offer
   several syntaxes, depending on the test context *)
type micheline =
  [ `Json of Ezjsonm.value (* EzJsonm value *)
  | `Michelson of string (* Michelson string *)
  | `File of string (* file with ext .tz or .json for Ezjsonm *) ]

type manager_op_param = {entrypoint : string; value : micheline}

type manager_op_kind =
  | Transaction of {
      dest : string;
      (* public key hash *)
      amount : int;
      (* in mutez *)
      parameter : manager_op_param option;
    }
  | Reveal of string (* public key *)
  | Origination of {code : micheline; storage : micheline; balance : int}
  | Rejection of {
      proof : string;
      tx_rollup : string;
      level : int;
      message : Rollup.Tx_rollup.message;
      message_position : int;
      message_path : string list;
      message_result_hash : string;
      message_result_path : JSON.u;
      previous_message_result_path : JSON.u;
      previous_message_context_hash : string;
      previous_message_withdraw_list_hash : string;
    }
  | Delegation of (* public key hash *) string

(* This is the manager operations' content type *)
type manager_operation_content = {
  source : string;
  (* public key hash *)
  op_kind : manager_op_kind;
  counter : int;
  fee : int;
  gas_limit : int;
  storage_limit : int;
}

let micheline_to_json convert client = function
  | `Json json -> return json
  | `Michelson data -> convert data client
  | `File file -> (
      match Filename.extension file with
      | ".tz" | ".tez" | ".mic" -> convert file client
      | ".json" -> JSON.parse_file file |> JSON.unannotate |> Lwt.return
      | s -> Test.fail "Unknown file extension %S in %s" s file)

let script_to_json =
  micheline_to_json (fun script client ->
      Client.convert_script_to_json ~script client)

let data_to_json =
  micheline_to_json (fun data client ->
      Client.convert_data_to_json ~data client)

(* Some basic auxiliary functions *)
let get_counter ~source client =
  let*! json =
    RPC.Contracts.get_counter ~contract_id:source.Account.public_key_hash client
  in
  return (JSON.as_int json)

let get_next_counter ~source client = get_counter client ~source >|= succ

let get_injection_branch ?branch client =
  match branch with
  | Some b -> Lwt.return b
  | None -> RPC.get_branch client >|= JSON.as_string

(* Smart constructors *)

let mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client op_kind
    =
  let* counter =
    match counter with
    | None -> get_next_counter ~source client
    | Some counter -> Lwt.return counter
  in
  Lwt.return
    {
      op_kind;
      source = source.Account.public_key_hash;
      counter;
      fee;
      gas_limit;
      storage_limit;
    }

let mk_call ~source ?counter ?(fee = 30_000) ?(gas_limit = 30_000)
    ?(storage_limit = 1_500) ~dest ?(amount = 0) ~entrypoint ~arg client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Transaction {dest; amount; parameter = Some {entrypoint; value = arg}}

let mk_transfer ~source ?counter ?(fee = 1_000) ?(gas_limit = 1040)
    ?(storage_limit = 257) ~dest ?(amount = 1_000_000) client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Transaction {dest = dest.Account.public_key_hash; amount; parameter = None}

let mk_reveal ~source ?counter ?(fee = 1_000) ?(gas_limit = 1040)
    ?(storage_limit = 0) client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Reveal source.Account.public_key

let mk_delegation ~source ?counter ?(fee = 1_000) ?(gas_limit = 1040)
    ?(storage_limit = 0) ~delegate client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Delegation delegate

let mk_rejection ~source ?counter ?(fee = 1_000_000) ?(gas_limit = 1_000_000)
    ?(storage_limit = 0) ~tx_rollup ~proof ~level ~message ~message_position
    ~message_path ~message_result_hash ~message_result_path
    ~previous_message_result_path ~previous_message_context_hash
    ~previous_message_withdraw_list_hash client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Rejection
       {
         tx_rollup;
         proof;
         level;
         message;
         message_position;
         message_path;
         message_result_hash;
         message_result_path;
         previous_message_result_path;
         previous_message_context_hash;
         previous_message_withdraw_list_hash;
       }

let mk_origination ~source ?counter ?(fee = 1_000_000) ?(gas_limit = 100_000)
    ?(storage_limit = 10_000) ~code ~init_storage ?(init_balance = 0) client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Origination {code; storage = init_storage; balance = init_balance}

(* encodes the given manager operation as a JSON string *)
let manager_op_content_to_json_string
    {op_kind; fee; gas_limit; storage_limit; source; counter} client =
  let jz_string_of_int n = Ezjsonm.string @@ string_of_int n in
  let mk_jsonm ?(amount = `Null) ?(destination = `Null) ?(parameter = `Null)
      ?(public_key = `Null) ?(delegate = `Null) ?(balance = `Null)
      ?(script = `Null) ?(proof = `Null) ?(rollup = `Null) ?(message = `Null)
      ?(message_position = `Null) ?(message_path = `Null) ?(level = `Null)
      ?(previous_message_result = `Null) ?(message_result_hash = `Null)
      ?(message_result_path = `Null) ?(previous_message_result_path = `Null)
      kind =
    let filter = List.filter (fun (_k, v) -> v <> `Null) in
    return
    @@ `O
         (filter
            [
              (* Common parts *)
              ("source", Ezjsonm.string source);
              ("fee", jz_string_of_int fee);
              ("counter", jz_string_of_int counter);
              ("gas_limit", jz_string_of_int gas_limit);
              ("storage_limit", jz_string_of_int storage_limit);
              ("kind", Ezjsonm.string kind);
              (* Simple transfer, or SC call *)
              ("amount", amount);
              ("destination", destination);
              ("parameters", parameter);
              (* Pk reveal *)
              ("public_key", public_key);
              (* Delegation *)
              ("delegate", delegate);
              (* Smart Contract origination *)
              ("balance", balance);
              ("script", script);
              ("proof", proof);
              ("rollup", rollup);
              ("message", message);
              ("message_position", message_position);
              ("message_path", message_path);
              ("previous_message_result", previous_message_result);
              ("level", level);
              ("message_result_hash", message_result_hash);
              ("message_result_path", message_result_path);
              ("previous_message_result_path", previous_message_result_path);
            ])
  in
  match op_kind with
  | Transaction {dest; amount; parameter = None} ->
      mk_jsonm
        ~amount:(jz_string_of_int amount)
        ~destination:(Ezjsonm.string dest)
        "transaction"
  | Transaction {dest; amount; parameter = Some {entrypoint; value}} ->
      let* value = data_to_json client value in
      let parameter =
        `O [("entrypoint", Ezjsonm.string entrypoint); ("value", value)]
      in
      mk_jsonm
        ~amount:(jz_string_of_int amount)
        ~destination:(Ezjsonm.string dest)
        ~parameter
        "transaction"
  | Reveal pk -> mk_jsonm ~public_key:(Ezjsonm.string pk) "reveal"
  | Delegation delegate ->
      mk_jsonm ~delegate:Ezjsonm.(string delegate) "delegation"
  | Origination {code; storage; balance} ->
      let* code = script_to_json client code in
      let* storage = data_to_json client storage in
      let script : Ezjsonm.value = `O [("code", code); ("storage", storage)] in
      mk_jsonm ~balance:(jz_string_of_int balance) ~script "origination"
  | Rejection
      {
        proof;
        tx_rollup;
        level;
        message;
        message_position;
        message_path;
        previous_message_context_hash;
        message_result_hash;
        message_result_path;
        previous_message_result_path;
        previous_message_withdraw_list_hash;
      } ->
      let rollup = `String tx_rollup in
      let proof = Ezjsonm.value_from_string proof in
      let level = `Float (float_of_int level) in
      let message =
        match message with `Batch (`Hex str) -> `O [("batch", `String str)]
      in
      let message_position = `String (string_of_int message_position) in
      let message_path = `A (List.map (fun x -> `String x) message_path) in
      let previous_message_result =
        `O
          [
            ("context_hash", `String previous_message_context_hash);
            ("withdraw_list_hash", `String previous_message_withdraw_list_hash);
          ]
      in
      let message_result_hash = `String message_result_hash in
      mk_jsonm
        ~rollup
        ~proof
        ~level
        ~message
        ~message_position
        ~message_path
        ~previous_message_result
        ~message_result_hash
        ~message_result_path
        ~previous_message_result_path
        "tx_rollup_rejection"

(* construct a JSON operations with contents and branch *)
let manager_op_to_json_string ~branch operations_json =
  `O [("branch", Ezjsonm.string branch); ("contents", operations_json)]

(* Forging, signing and injection operations *)

let forge_operation ?protocol ~branch ~batch client =
  let* json_batch =
    Lwt_list.map_p (fun op -> manager_op_content_to_json_string op client) batch
  in
  let op_json = manager_op_to_json_string ~branch (`A json_batch) in
  let* hex =
    match protocol with
    | None -> RPC.post_forge_operations ~data:op_json client >|= JSON.as_string
    | Some p ->
        let name = Protocol.daemon_name p ^ ".operation.unsigned" in
        Codec.encode ~name op_json
  in
  return (`Hex hex)

let sign_manager_op_bytes ~(signer : Account.key) (op_bytes : Bytes.t) =
  Account.sign_bytes ~watermark:Generic_operation ~signer op_bytes

let sign_manager_op_hex ~signer op_hex =
  let op_bytes = Hex.to_bytes op_hex in
  let signature = sign_manager_op_bytes ~signer op_bytes in
  Tezos_crypto.Signature.to_hex signature

let inject_operation ?(async = false) ?(force = false) ?wait_for_injection
    ~unsigned_op ~signature client =
  let (`Hex unsigned_op) = unsigned_op in
  let (`Hex signature) = signature in
  let signed_op = unsigned_op ^ signature in
  let inject =
    if force then RPC.private_inject_operation else RPC.inject_operation
  in
  let waiter =
    match wait_for_injection with
    | None -> Lwt.return_unit
    | Some node -> Node.wait_for_request ~request:`Inject node
  in
  let*! oph_json = inject ~async ~data:(`String signed_op) client in
  let* () = waiter in
  return (`OpHash (JSON.as_string oph_json))

let runnable_inject_operation ?(async = false) ?(force = false) ~unsigned_op
    ~signature client =
  let (`Hex unsigned_op) = unsigned_op in
  let (`Hex signature) = signature in
  let signed_op = unsigned_op ^ signature in
  let inject =
    if force then RPC.private_inject_operation else RPC.inject_operation
  in
  inject ~async ~data:(`String signed_op) client

let forge_and_inject_operation ?protocol ?branch ?async ?force
    ?wait_for_injection ~batch ~signer client =
  let* branch = get_injection_branch ?branch client in
  let* unsigned_op = forge_operation ?protocol ~batch ~branch client in
  let signature = sign_manager_op_hex ~signer unsigned_op in
  inject_operation
    ?async
    ?force
    ?wait_for_injection
    ~unsigned_op
    ~signature
    client

let runnable_forge_and_inject_operation ?protocol ?branch ?async ?force ~batch
    ~signer client =
  let* branch = get_injection_branch ?branch client in
  let* unsigned_op = forge_operation ?protocol ~batch ~branch client in
  let signature = sign_manager_op_hex ~signer unsigned_op in
  return
    (runnable_inject_operation ?async ?force ~unsigned_op ~signature client)

(** High level operations injection wrappers *)

let inject_origination ?protocol ?async ?force ?wait_for_injection ?branch
    ~source ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit ~code
    ~init_storage ?init_balance client =
  let* op =
    mk_origination
      ~source
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~code
      ~init_storage
      ?init_balance
      client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ~batch:[op]
    ?branch
    ~signer
    client

let inject_public_key_revelation ?protocol ?async ?force ?wait_for_injection
    ?branch ~source ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit
    ?public_key client =
  let fake_source =
    match public_key with
    | None -> source
    | Some public_key -> {source with Account.public_key}
    (* to reveal wrong pk *)
  in
  let* op =
    mk_reveal ~source:fake_source ?counter ?fee ?gas_limit ?storage_limit client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ?branch
    ~batch:[op]
    ~signer
    client

let inject_delegation ?protocol ?async ?force ?wait_for_injection ?branch
    ~source ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit ~delegate
    client =
  let* op =
    mk_delegation
      ~source
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~delegate
      client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ?branch
    ~batch:[op]
    ~signer
    client

let inject_transfer ?protocol ?async ?force ?wait_for_injection ?branch ~source
    ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit ~dest ?amount
    client =
  let* op =
    mk_transfer
      ~source
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~dest
      ?amount
      client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ?branch
    ~batch:[op]
    ~signer
    client

let inject_rejection ?protocol ?async ?force ?wait_for_injection ?branch ~source
    ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit ~tx_rollup ~proof
    ~level ~message ~message_position ~message_path ~message_result_hash
    ~message_result_path ~previous_message_result_path
    ~previous_message_context_hash ~previous_message_withdraw_list_hash client =
  let* op =
    mk_rejection
      ~source
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~tx_rollup
      ~proof
      ~level
      ~message
      ~message_position
      ~message_path
      ~message_result_hash
      ~message_result_path
      ~previous_message_result_path
      ~previous_message_context_hash
      ~previous_message_withdraw_list_hash
      client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ?branch
    ~batch:[op]
    ~signer
    client

let inject_contract_call ?protocol ?async ?force ?wait_for_injection ?branch
    ~source ?(signer = source) ?counter ?fee ?gas_limit ?storage_limit ~dest
    ?amount ~entrypoint ~arg client =
  let* op =
    mk_call
      ~source
      ?counter
      ?fee
      ?gas_limit
      ?storage_limit
      ~dest
      ?amount
      ~entrypoint
      ~arg
      client
  in
  forge_and_inject_operation
    ?protocol
    ?async
    ?force
    ?wait_for_injection
    ?branch
    ~batch:[op]
    ~signer
    client

let inject_transfers ?protocol ?async ?force ?wait_for_injection ?amount ?fee
    ?gas_limit ?(source = Constant.bootstrap1)
    ?(destination = Constant.bootstrap2) ~node ~number_of_operations client =
  let* branch = get_injection_branch client in
  (* Counter needs to be computed manually to ensure several
     operations of the same manager can be included in the same block.
  *)
  let* counter = get_next_counter ~source client in
  let counter = ref counter in
  let rec loop oph_list = function
    | 0 -> return oph_list
    | n ->
        let transfer_1 = Node.wait_for_request ~request:`Inject node in
        let* oph =
          inject_transfer
            ?force
            ?wait_for_injection
            ?protocol
            ?async
            ?fee
            ?gas_limit
            ?amount
            ~signer:source
            ~branch
            ~counter:!counter
            ~source
            ~dest:destination
            client
        in
        let* () = transfer_1 in
        let oph_list = oph :: oph_list in
        incr counter ;
        loop oph_list (pred n)
  in
  loop [] number_of_operations
