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

let get_next_counter ~source client =
  let open Lwt.Infix in
  RPC.Contracts.get_counter ~contract_id:source.Constant.identity client
  >|= JSON.as_int >|= succ

let get_branch client =
  let open Lwt.Infix in
  RPC.get_branch client >|= JSON.as_string

let json_of_transfer_operation_content ~amount ~fee ~gas_limit ~counter ~source
    ~destination =
  `O
    [
      ("kind", `String "transaction");
      ("source", `String source.Constant.identity);
      ("fee", `String (string_of_int fee));
      ("counter", `String (string_of_int counter));
      ("gas_limit", `String (string_of_int gas_limit));
      ("storage_limit", `String "0");
      ("amount", `String (string_of_int amount));
      ("destination", `String destination.Constant.identity);
    ]

let json_of_operation ~branch operation_content_json =
  `O [("branch", `String branch); ("contents", `A [operation_content_json])]

let json_of_transfer_operation ~amount ~fee ~gas_limit ~branch ~counter ~source
    ~destination =
  json_of_transfer_operation_content
    ~amount
    ~fee
    ~gas_limit
    ~counter
    ~source
    ~destination
  |> json_of_operation ~branch

let forge_transfer ?(amount = 1) ?(fee = 1000) ?(gas_limit = 1040)
    ?(source = Constant.bootstrap1) ?(destination = Constant.bootstrap2) ?branch
    ?counter client =
  let open Lwt.Infix in
  let* branch =
    match branch with None -> get_branch client | Some b -> return b
  in
  let* counter =
    match counter with
    | None -> get_next_counter ~source client
    | Some c -> return c
  in
  let op_json =
    json_of_transfer_operation
      ~amount
      ~fee
      ~gas_limit
      ~branch
      ~counter
      ~source
      ~destination
  in
  RPC.post_forge_operations ~data:op_json client >|= JSON.as_string

let bytes_of_hex hex = Hex.to_bytes (`Hex hex)

let sign_operation ~watermark ~signer op_hex =
  let open Tezos_crypto in
  let sk =
    match String.split_on_char ':' signer.Constant.secret with
    | ["unencrypted"; b58_secret_key] ->
        Signature.Secret_key.of_b58check_exn b58_secret_key
    | _ -> Test.fail "Could not parse secret key: '%s'" signer.Constant.secret
  in
  let bytes = bytes_of_hex op_hex in
  let signature = Signature.(sign ~watermark sk bytes) in
  let (`Hex signature) = Tezos_crypto.Signature.to_hex signature in
  signature

let inject_operation ~signature op_str_hex client =
  let signed_op = op_str_hex ^ signature in
  let* res = RPC.inject_operation ~data:(`String signed_op) client in
  return res

let inject_transfer ?branch ?counter ?amount ?fee ?gas_limit
    ?(source = Constant.bootstrap1) ?(destination = Constant.bootstrap2) client
    =
  let* op_str_hex =
    forge_transfer
      ?branch
      ?counter
      ?amount
      ?fee
      ?gas_limit
      ~source
      ~destination
      client
  in
  let signature =
    sign_operation
      ~watermark:Tezos_crypto.Signature.Generic_operation
      ~signer:source
      op_str_hex
  in
  let* oph = inject_operation ~signature op_str_hex client in
  return (JSON.as_string oph)

let inject_transfers ?amount ?fee ?gas_limit ?(source = Constant.bootstrap1)
    ?(destination = Constant.bootstrap2) ~node ~number_of_operations client =
  let* branch = get_branch client in
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
