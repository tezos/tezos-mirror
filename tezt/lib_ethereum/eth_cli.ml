(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

let path = "eth"

let spawn_command_and_read command decode =
  let process = Process.spawn path command in
  let* output = Process.check_and_read_stdout process in
  return (JSON.parse ~origin:"eth_spawn_command" output |> decode)

let spawn_command command =
  let process = Process.spawn path command in
  let* output = Process.check process in
  return output

let balance ~account ~endpoint =
  let* balance =
    spawn_command_and_read
      ["address:balance"; account; "--network"; endpoint]
      JSON.as_int
  in
  return @@ Wei.of_eth_int balance

let transaction_send ~source_private_key ~to_public_key ~value ~endpoint ?data
    () =
  spawn_command_and_read
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
    @ match data with Some data -> ["--data"; data] | None -> [])
    JSON.as_string

let get_block ~block_id ~endpoint =
  spawn_command_and_read
    ["block:get"; block_id; "--network"; endpoint]
    Block.of_json

let block_number ~endpoint =
  spawn_command_and_read ["block:number"; "--network"; endpoint] JSON.as_int

let add_abi ~label ~abi () = spawn_command ["abi:add"; label; abi]

let deploy ~source_private_key ~endpoint ~abi ~bin () =
  let decode json =
    let open JSON in
    json |-> "address" |> as_string
  in
  spawn_command_and_read
    [
      "contract:deploy";
      "--pk";
      source_private_key;
      "--abi";
      abi;
      "--network";
      endpoint;
      bin;
    ]
    decode
