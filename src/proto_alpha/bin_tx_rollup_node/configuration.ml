(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

type t = {
  data_dir : string;
  client_keys : Client_keys.Public_key_hash.t;
  rollup_id : Protocol.Alpha_context.Tx_rollup.t;
  rollup_genesis : Block_hash.t option;
  rpc_addr : string;
  rpc_port : int;
  reconnection_delay : float;
}

let default_data_dir =
  let home = Sys.getenv "HOME" and dir = ".tezos-tx-rollup-node" in
  Filename.concat home dir

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 9999

let default_reconnection_delay = 2.0

let get_configuration_filename data_dir =
  Filename.Infix.(data_dir // "config.json")

let project
    {
      data_dir;
      client_keys;
      rollup_id;
      rollup_genesis;
      rpc_addr;
      rpc_port;
      reconnection_delay;
    } =
  ( data_dir,
    client_keys,
    rollup_id,
    rollup_genesis,
    rpc_addr,
    rpc_port,
    reconnection_delay )

let inject
    ( data_dir,
      client_keys,
      rollup_id,
      rollup_genesis,
      rpc_addr,
      rpc_port,
      reconnection_delay ) =
  {
    data_dir;
    client_keys;
    rollup_id;
    rollup_genesis;
    rpc_addr;
    rpc_port;
    reconnection_delay;
  }

let encoding =
  let open Data_encoding in
  conv project inject
  @@ obj7
       (dft
          ~description:"Location of the data dir"
          "data-dir"
          string
          default_data_dir)
       (req
          ~description:"Client keys"
          "client-keys"
          Client_keys.Public_key_hash.encoding)
       (req
          ~description:"Rollup id of the rollup to target"
          "rollup-id"
          Protocol.Alpha_context.Tx_rollup.encoding)
       (opt
          ~description:"Hash of the block where the rollup was created"
          "block-hash"
          Block_hash.encoding)
       (dft
          ~description:"RPC address listens by the node"
          "rpc-addr"
          string
          default_rpc_addr)
       (dft
          ~description:"RPC port listens by the node"
          "rpc-port"
          int16
          default_rpc_port)
       (dft
          ~description:"The reconnection delay when the connection is lost"
          "reconnection-delay"
          float
          default_reconnection_delay)

let save configuration =
  let open Lwt_result_syntax in
  let json = Data_encoding.Json.construct encoding configuration in
  let*! () = Lwt_utils_unix.create_dir configuration.data_dir in
  let file = get_configuration_filename configuration.data_dir in
  let*! v =
    Lwt_utils_unix.with_atomic_open_out file (fun chan ->
        let content = Data_encoding.Json.to_string json in
        Lwt_utils_unix.write_string chan content)
  in
  Lwt.return
    (Result.map_error
       (fun _ -> [Error.Tx_rollup_unable_to_write_configuration_file file])
       v)

let load ~data_dir =
  let open Lwt_result_syntax in
  let file = get_configuration_filename data_dir in
  let*! exists = Lwt_unix.file_exists file in
  let* () =
    fail_unless exists (Error.Tx_rollup_configuration_file_does_not_exists file)
  in
  let+ json = Lwt_utils_unix.Json.read_file file in
  Data_encoding.Json.destruct encoding json
