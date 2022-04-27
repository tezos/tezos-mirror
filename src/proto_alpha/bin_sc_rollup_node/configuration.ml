(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

open Protocol.Alpha_context

type t = {
  data_dir : string;
  sc_rollup_address : Sc_rollup.t;
  sc_rollup_node_operator : Signature.Public_key_hash.t;
  rpc_addr : string;
  rpc_port : int;
  minimal_fees : Tez.t;
  minimal_nanotez_per_byte : Q.t;
  minimal_nanotez_per_gas_unit : Q.t;
  force_low_fee : bool;
  fee_cap : Tez.t;
  burn_cap : Tez.t;
}

let default_data_dir =
  Filename.concat (Sys.getenv "HOME") ".tezos-sc-rollup-node"

let relative_filename data_dir = Filename.concat data_dir "config.json"

let filename config = relative_filename config.data_dir

let fee_parameter config : Injection.fee_parameter =
  {
    minimal_fees = config.minimal_fees;
    minimal_nanotez_per_byte = config.minimal_nanotez_per_byte;
    minimal_nanotez_per_gas_unit = config.minimal_nanotez_per_gas_unit;
    force_low_fee = config.force_low_fee;
    fee_cap = config.fee_cap;
    burn_cap = config.burn_cap;
  }

let default_rpc_addr = "127.0.0.1"

let default_rpc_port = 8932

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2794 
   the below default values have been copied from 
   `src/proto_alpha/lib_client/client_proto_args.ml`, but 
   we need to check whether these values are sensible for the rollup 
   node.
*)
let default_minimal_fees =
  match Tez.of_mutez 100L with None -> assert false | Some t -> t

let default_minimal_nanotez_per_gas_unit = Q.of_int 100

let default_minimal_nanotez_per_byte = Q.of_int 1000

let default_force_low_fee = false

let default_fee_cap =
  match Tez.of_string "1.0" with None -> assert false | Some t -> t

let default_burn_cap =
  match Tez.of_string "0" with None -> assert false | Some t -> t

let default_fee_parameter : Injection.fee_parameter =
  {
    minimal_fees = default_minimal_fees;
    minimal_nanotez_per_gas_unit = default_minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte = default_minimal_nanotez_per_byte;
    force_low_fee = default_force_low_fee;
    fee_cap = default_fee_cap;
    burn_cap = default_burn_cap;
  }

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           data_dir;
           sc_rollup_address;
           sc_rollup_node_operator;
           rpc_addr;
           rpc_port;
           minimal_fees;
           minimal_nanotez_per_byte;
           minimal_nanotez_per_gas_unit;
           force_low_fee;
           fee_cap;
           burn_cap;
         } ->
      ( data_dir,
        sc_rollup_address,
        sc_rollup_node_operator,
        rpc_addr,
        rpc_port,
        ( minimal_fees,
          minimal_nanotez_per_byte,
          minimal_nanotez_per_gas_unit,
          force_low_fee,
          fee_cap,
          burn_cap ) ))
    (fun ( data_dir,
           sc_rollup_address,
           sc_rollup_node_operator,
           rpc_addr,
           rpc_port,
           ( minimal_fees,
             minimal_nanotez_per_byte,
             minimal_nanotez_per_gas_unit,
             force_low_fee,
             fee_cap,
             burn_cap ) ) ->
      {
        data_dir;
        sc_rollup_address;
        sc_rollup_node_operator;
        rpc_addr;
        rpc_port;
        minimal_fees;
        minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit;
        force_low_fee;
        fee_cap;
        burn_cap;
      })
    (obj6
       (dft
          "data-dir"
          ~description:"Location of the data dir"
          string
          default_data_dir)
       (req
          "sc-rollup-address"
          ~description:"Smart contract rollup address"
          Protocol.Alpha_context.Sc_rollup.Address.encoding)
       (req
          "sc-rollup-node-operator"
          ~description:
            "Public key hash of the Smart contract rollup node operator"
          Signature.Public_key_hash.encoding)
       (dft "rpc-addr" ~description:"RPC address" string default_rpc_addr)
       (dft "rpc-port" ~description:"RPC port" int16 default_rpc_port)
       (req
          "fee-parameter"
          ~description:"The fee parameter used when injecting operations in L1"
          (obj6
             (dft
                "minimal-fees"
                ~description:"Exclude operations with lower fees"
                Tez.encoding
                default_minimal_fees)
             (dft
                "minimal-nanotez-per-byte"
                ~description:"Exclude operations with lower fees per byte"
                Plugin.Mempool.nanotez_enc
                default_minimal_nanotez_per_byte)
             (dft
                "minimal-nanotez-per-gas-unit"
                ~description:"Exclude operations with lower gas fees"
                Plugin.Mempool.nanotez_enc
                default_minimal_nanotez_per_gas_unit)
             (dft
                "force-low-fee"
                ~description:
                  "Don't check that the fee is lower than the estimated default"
                bool
                default_force_low_fee)
             (dft
                "fee-cap"
                ~description:"The fee cap"
                Tez.encoding
                default_fee_cap)
             (dft
                "burn-cap"
                ~description:"The burn cap"
                Tez.encoding
                default_burn_cap))))

let save config =
  let open Lwt_syntax in
  let json = Data_encoding.Json.construct encoding config in
  let* () = Lwt_utils_unix.create_dir config.data_dir in
  Lwt_utils_unix.Json.write_file (filename config) json

let load ~data_dir =
  let open Lwt_result_syntax in
  let+ json = Lwt_utils_unix.Json.read_file (relative_filename data_dir) in
  Data_encoding.Json.destruct encoding json
