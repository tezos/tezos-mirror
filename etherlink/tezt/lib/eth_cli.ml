(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023-2024 Functori <contact@functori.com>                   *)
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

let spawn ?runner command =
  if Option.is_some (Sys.getenv_opt "TEZT_NO_NPX") then
    Process.spawn ?runner "/usr/local/lib/node_modules/eth-cli/bin/run" command
  else Process.spawn ?runner "npx" ("eth-cli" :: command)

let spawn_command_and_read_string ?runner ?expect_failure command =
  let process = spawn ?runner command in
  if expect_failure = Some true then
    Process.check_and_read_stderr ?expect_failure process
  else Process.check_and_read_stdout ?expect_failure process

let spawn_command ?runner ?expect_failure command =
  let process = spawn ?runner command in
  let* output = Process.check ?expect_failure process in
  return output

let spawn_command_and_read_json_opt ?runner command decode =
  let* output = spawn_command_and_read_string ?runner command in
  let json = JSON.parse ~origin:"eth-cli RPC" output in
  if JSON.is_null json then return None else json |> decode |> Lwt.return

let spawn_command_and_read_json ?runner command decode =
  let* opt = spawn_command_and_read_json_opt ?runner command decode in
  match opt with Some v -> return v | None -> failwith "No value to decode"

let balance ?runner ~account ~endpoint () =
  let* balance =
    spawn_command_and_read_string
      ?runner
      ["address:balance"; account; "--network"; endpoint]
  in
  return @@ Wei.of_eth_string balance

let transaction_send ~source_private_key ~to_public_key ~value ~endpoint ?runner
    ?data ?gas_limit ?gas_price () =
  spawn_command_and_read_json
    ?runner
    ([
       "transaction:send";
       "--pk";
       source_private_key;
       "--to";
       to_public_key;
       "--value";
       Wei.to_string value;
       "--network";
       endpoint;
     ]
    @ Cli_arg.optional_arg "data" Fun.id data
    @ Cli_arg.optional_arg "gas" Z.to_string gas_limit
    @ Cli_arg.optional_arg "gasPrice" Wei.to_string gas_price)
    JSON.as_string_opt

let transaction_get ?runner ~endpoint ~tx_hash () =
  spawn_command_and_read_json_opt
    ?runner
    ["transaction:get"; tx_hash; "--network"; endpoint]
    (fun json -> Some (Transaction.transaction_object_of_json json))

let get_block ?runner ~block_id ~endpoint () =
  spawn_command_and_read_json
    ?runner
    ["block:get"; block_id; "--network"; endpoint]
    (fun json -> Some (Block.of_json json))

let block_number ?runner ~endpoint () =
  spawn_command_and_read_json
    ?runner
    ["block:number"; "--network"; endpoint]
    JSON.as_int_opt

let add_abi ?runner ~label ~abi () =
  spawn_command ?runner ["abi:add"; label; abi]

let update_abi ?runner ~label ~abi () =
  spawn_command ?runner ["abi:update"; label; abi]

let show_abi ?runner ~label () =
  let* abi = spawn_command_and_read_string ?runner ["abi:show"; label] in
  return abi

let check_abi ?runner ~label () =
  let* abi_list = spawn_command_and_read_string ?runner ["abi:list"] in
  return (String.split_on_char '\n' abi_list |> List.mem label)

let deploy ?runner ?args ~source_private_key ~endpoint ~abi ~bin () =
  let decode json =
    let open JSON in
    let contract_address =
      json |-> "receipt" |-> "contractAddress" |> as_string
    in
    let tx_hash = json |-> "receipt" |-> "transactionHash" |> as_string in
    Some (contract_address, tx_hash)
  in
  spawn_command_and_read_json
    ?runner
    ([
       "contract:deploy";
       "--pk";
       source_private_key;
       "--abi";
       abi;
       "--network";
       endpoint;
       bin;
     ]
    @ Cli_arg.optional_arg "args" Fun.id args)
    decode

let contract_send ?runner ?(expect_failure = false) ?value ?gas ?gas_price
    ~source_private_key ~endpoint ~abi_label ~address ~method_call () =
  let command =
    [
      "contract:send";
      "--pk";
      source_private_key;
      "--network";
      endpoint;
      Format.sprintf "%s@%s" abi_label address;
      method_call;
    ]
    @ Cli_arg.optional_arg "value" Wei.to_string value
    @ Cli_arg.optional_arg "gas" Int.to_string gas
    @ Cli_arg.optional_arg "gasPrice" Int.to_string gas_price
  in
  if expect_failure then
    spawn_command_and_read_string ?runner ~expect_failure command
  else spawn_command_and_read_json ?runner command JSON.as_string_opt

let contract_call ?runner ?(expect_failure = false) ~endpoint ~abi_label
    ~address ~method_call () =
  let command =
    [
      "contract:call";
      "--network";
      endpoint;
      Format.sprintf "%s@%s" abi_label address;
      method_call;
    ]
  in
  spawn_command_and_read_string ?runner ~expect_failure command

let get_receipt ?runner ~endpoint ~tx () =
  spawn_command_and_read_json_opt
    ?runner
    ["transaction:get"; tx; "--network"; endpoint]
    JSON.(
      fun j ->
        j |-> "receipt" |> fun json ->
        if JSON.is_null json then None
        else Some (Transaction.transaction_receipt_of_json json))

let encode_method ?runner ~abi_label ~method_ () =
  let* data =
    spawn_command_and_read_string ?runner ["method:encode"; abi_label; method_]
  in
  return (String.trim data)

let decode_method ?runner ~abi_label ~method_ () =
  let* data =
    spawn_command_and_read_string ?runner ["method:decode"; abi_label; method_]
  in
  return (String.trim data)

let gen_eth_account ?runner () =
  spawn_command_and_read_json ?runner ["address:random"] (fun json ->
      Some (Eth_account.of_json json))
