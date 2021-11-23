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

type manager_op_param = {entrypoint : string; value : Ezjsonm.value}

type manager_op_kind =
  | Transaction of {
      dest : string;
      (* public key hash *)
      amount : int;
      parameter : manager_op_param option;
    }
  | Reveal of string (* public key *)
  | Origination of {
      code : Ezjsonm.value;
      storage : Ezjsonm.value;
      balance : int;
    }

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

(* Some basic auxiliary functions *)
let get_counter ~source client =
  RPC.Contracts.get_counter ~contract_id:source.Account.public_key_hash client
  >|= JSON.as_int

let get_next_counter ~source client = get_counter client ~source >|= succ

let get_injection_branch ~branch client =
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

let mk_origination ~source ?counter ?(fee = 1_000_000) ?(gas_limit = 100_000)
    ?(storage_limit = 10_000) ~code ~init_storage ?(init_balance = 0) client =
  mk_manager_op ~source ?counter ~fee ~gas_limit ~storage_limit client
  @@ Origination {code; storage = init_storage; balance = init_balance}

(* encodes the given manager operation as a JSON string *)
let manager_op_content_to_json_string
    {op_kind; fee; gas_limit; storage_limit; source; counter} =
  let jz_string_of_int n = Ezjsonm.string @@ string_of_int n in
  let mk_jsonm ?(amount = `Null) ?(destination = `Null) ?(parameter = `Null)
      ?(public_key = `Null) ?(balance = `Null) ?(script = `Null) kind =
    let filter = List.filter (fun (_k, v) -> v <> `Null) in
    `O
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
           (* Smart Contract origination *)
           ("balance", balance);
           ("script", script);
         ])
  in
  match op_kind with
  | Transaction {dest; amount; parameter = None} ->
      mk_jsonm
        ~amount:(jz_string_of_int amount)
        ~destination:(Ezjsonm.string dest)
        "transaction"
  | Transaction {dest; amount; parameter = Some {entrypoint; value}} ->
      let parameter =
        `O [("entrypoint", Ezjsonm.string entrypoint); ("value", value)]
      in
      mk_jsonm
        ~amount:(jz_string_of_int amount)
        ~destination:(Ezjsonm.string dest)
        ~parameter
        "transaction"
  | Reveal pk -> mk_jsonm ~public_key:(Ezjsonm.string pk) "reveal"
  | Origination {code; storage; balance} ->
      let script : Ezjsonm.value = `O [("code", code); ("storage", storage)] in
      mk_jsonm ~balance:(jz_string_of_int balance) ~script "origination"

(* construct a JSON operations with contents and branch *)
let manager_op_to_json_string ~branch operations_json =
  `O [("branch", Ezjsonm.string branch); ("contents", operations_json)]

(* Forging, signing and injection operations *)

let forge_operation ?protocol client ~branch ~batch =
  let json_batch = `A (List.map manager_op_content_to_json_string batch) in
  let op_json = manager_op_to_json_string ~branch json_batch in
  match protocol with
  | None -> RPC.post_forge_operations ~data:op_json client >|= JSON.as_string
  | Some p ->
      let name = Protocol.daemon_name p ^ ".operation.unsigned" in
      Codec.encode ~name op_json

let sign_bytes ~watermark (signer : Account.key) (msg : Bytes.t) =
  let open Tezos_crypto in
  let (Unencrypted b58_secret_key) = signer.secret_key in
  let sk = Signature.Secret_key.of_b58check_exn b58_secret_key in
  Signature.(sign ~watermark sk msg)

let sign_hex ~signer ~watermark str_hex =
  sign_bytes ~watermark signer (Hex.to_bytes (`Hex str_hex))
  |> Tezos_crypto.Signature.to_hex
  |> fun (`Hex signature) -> signature

let inject_operation ?(async = false) ?(force = false) ~signature op_str_hex
    client =
  let signed_op = op_str_hex ^ signature in
  if force then
    RPC.private_inject_operation ~async ~data:(`String signed_op) client
  else RPC.inject_operation ~async ~data:(`String signed_op) client

let forge_and_inject_operation ?protocol ?branch ?async ?force ~batch ~signer
    client =
  let* branch = get_injection_branch ~branch client in
  let* op_str_hex = forge_operation ?protocol ~batch ~branch client in
  let signature = sign_hex ~signer ~watermark:Generic_operation op_str_hex in
  inject_operation ?async ?force ~signature op_str_hex client >|= JSON.as_string

(** Two high level helpers *)

let inject_transfer ?protocol ?branch ?counter ?amount ?fee ?gas_limit
    ?(source = Constant.bootstrap1) ?(destination = Constant.bootstrap2) ?force
    client =
  let* op =
    mk_transfer
      ~source
      ?counter
      ?fee
      ?gas_limit
      ~dest:destination
      ?amount
      client
  in
  forge_and_inject_operation
    ?force
    ?protocol
    ?branch
    ~batch:[op]
    ~signer:source
    client

let inject_transfers ?protocol ?amount ?fee ?gas_limit
    ?(source = Constant.bootstrap1) ?(destination = Constant.bootstrap2) ?force
    ~node ~number_of_operations client =
  let* branch = get_injection_branch ~branch:None client in
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
            ?protocol
            ?fee
            ?gas_limit
            ?amount
            ~branch
            ~counter:!counter
            ~source
            ~destination
            client
        in
        let* () = transfer_1 in
        let oph_list = oph :: oph_list in
        incr counter ;
        loop oph_list (pred n)
  in
  loop [] number_of_operations
