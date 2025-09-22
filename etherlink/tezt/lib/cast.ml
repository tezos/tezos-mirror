(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024-2025 Functori <contact@functori.com>                   *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines how tezt tests will interact with cast client.
   Cast is an ethereum client to interact with the node provided by Foundry
   https://book.getfoundry.sh/cast *)

type tx =
  | CallTo of {
      signature : string option;
      arguments : string list;
      address : string;
    }
  | Create of {data : string}

let spawn arguments = Process.spawn "cast" arguments

let spawn_command_and_read_string ?expect_failure arguments =
  let process = spawn arguments in
  let* data = Process.check_and_read_stdout ?expect_failure process in
  return (String.trim data)

let version () = spawn_command_and_read_string ["--version"]

let cast_transaction ?expect_failure ~source_private_key ?endpoint ?chain_id
    ?nonce ?value ?gas ?gas_price ?priority_fee ?access_list ?authorization tx =
  let arguments =
    match tx with
    | CallTo {signature; arguments; address} ->
        address :: (Option.to_list signature @ arguments)
    | Create {data} -> ["--create"; data]
  in
  let access_list_to_json l : Ezjsonm.value =
    `A
      (List.map
         (fun (address, keys) ->
           `O
             [
               ("address", `String address);
               ("storageKeys", `A (List.map Ezjsonm.string keys));
             ])
         l)
  in
  let options =
    ["--private-key"; source_private_key]
    @ Cli_arg.optional_switch "legacy" (Option.is_none priority_fee)
    @ Cli_arg.optional_arg "rpc-url" Fun.id endpoint
    @ Cli_arg.optional_arg "chain" Int.to_string chain_id
    @ Cli_arg.optional_arg "nonce" Int.to_string nonce
    @ Cli_arg.optional_arg "value" Wei.to_string value
    @ Cli_arg.optional_arg "gas-limit" Int.to_string gas
    @ Cli_arg.optional_arg "priority-gas-price" Int.to_string priority_fee
    @ Cli_arg.optional_arg "gas-price" Int.to_string gas_price
    @ Cli_arg.optional_arg
        "access-list"
        (fun al ->
          Ezjsonm.value_to_string ~minify:true (access_list_to_json al))
        access_list
    @ Cli_arg.optional_arg "auth" Fun.id authorization
    @ arguments
  in
  spawn_command_and_read_string ?expect_failure ("mktx" :: options)

let craft_tx ~source_private_key ~chain_id ~nonce ~value ~gas ~gas_price
    ?(legacy = true) ?access_list ?authorization ~address ?signature
    ?(arguments = []) () =
  let priority_fee = if legacy then None else Some 1 in
  let tx = CallTo {signature; arguments; address} in
  let* encoded_tx =
    cast_transaction
      ~source_private_key
      ~chain_id
      ~nonce
      ~value
      ~gas
      ~gas_price
      ?priority_fee
      ?access_list
      ?authorization
      tx
  in
  return (String.sub encoded_tx 2 (String.length encoded_tx - 2))

let craft_deploy_tx ~source_private_key ~chain_id ~nonce ?value ~gas ~gas_price
    ?(legacy = true) ?access_list ?authorization ~data () =
  let priority_fee = if legacy then None else Some 1 in
  let tx = Create {data} in
  let* encoded_tx =
    cast_transaction
      ~source_private_key
      ~chain_id
      ~nonce
      ?value
      ~gas
      ~gas_price
      ?priority_fee
      ?access_list
      ?authorization
      tx
  in
  return (String.sub encoded_tx 2 (String.length encoded_tx - 2))

type wallet = {address : string; private_key : string}

let wallet_of_json json =
  let open JSON in
  {
    address = json |-> "address" |> as_string;
    private_key = json |-> "private_key" |> as_string;
  }

let gen_wallets ~number () =
  let* output =
    spawn_command_and_read_string
      ["wallet"; "new"; "--json"; "--number"; string_of_int number]
  in
  let wallets =
    JSON.parse ~origin:"cast" output |> JSON.as_list_opt |> function
    | Some l -> List.map wallet_of_json l
    | None -> []
  in
  return wallets

let calldata ?(args = []) signature =
  spawn_command_and_read_string ("calldata" :: signature :: args)

let call ?(args = []) signature ~endpoint ~address =
  spawn_command_and_read_string
    (("call" :: address :: signature :: args) @ ["--rpc-url"; endpoint])

let wallet_sign_auth ?nonce ~authorization ~private_key ~endpoint () =
  spawn_command_and_read_string
    ([
       "wallet";
       "sign-auth";
       authorization;
       "--private-key";
       private_key;
       "--rpc-url";
       endpoint;
     ]
    @ Cli_arg.optional_arg "nonce" Int.to_string nonce)

let raw_call ~endpoint ~address ~arg =
  spawn_command_and_read_string ["call"; address; arg; "--rpc-url"; endpoint]
