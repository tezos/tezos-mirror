(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Contracts = Tezos_client_alpha_commands.Client_proto_stresstest_contracts

type t = {regular : int; origination : int; contract : (string * int) list}

let encoding =
  let open Data_encoding in
  conv
    (fun {regular; origination; contract} -> (regular, origination, contract))
    (fun (regular, origination, contract) -> {regular; origination; contract})
    (obj3
       (req "regular" int31)
       (req "origination" int31)
       (req "contract" (assoc int31)))

let load path_option =
  match path_option with
  | None ->
      Log.info "Using the default average block description" ;
      Lwt.return {regular = 1; origination = 0; contract = []}
  | Some path -> (
      Log.info "Reading description of the average block from %s" path ;
      Lwt_io.(with_file ~mode:Input path (fun fp -> read fp)) >>= fun text ->
      match Data_encoding.Json.from_string text with
      | Ok json -> Lwt.return (Data_encoding.Json.destruct encoding json)
      | Error msg ->
          Format.kasprintf Stdlib.failwith "failed to parse %s: %s@." path msg)

let check_for_unknown_smart_contracts average_block =
  let check_one (mainnet_address, _) =
    match Contracts.mainnet_address_to_alias mainnet_address with
    | None ->
        Stdlib.failwith ("unknown smart contract address: " ^ mainnet_address)
    | Some _ -> Lwt.return_unit
  in
  List.iter_s check_one average_block.contract
