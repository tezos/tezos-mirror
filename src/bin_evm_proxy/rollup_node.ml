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

open Ethereum_types

module Durable_storage_path = struct
  let accounts = "/eth_accounts"

  let balance = "/balance"

  let account_path (Address s) = accounts ^ "/" ^ s

  let balance_path address = account_path address ^ balance
end

module RPC = struct
  open Tezos_rpc
  open Path

  let smart_rollup_address =
    Service.get_service
      ~description:"Smart rollup address"
      ~query:Query.empty
      ~output:Data_encoding.string
      (open_root / "global" / "smart_rollup_address")

  type state_value_query = {key : string}

  let state_value_query : state_value_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun key -> {key})
    |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
    |> seal

  let durable_state_value =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve value by key from PVM durable storage. PVM state is taken \
         with respect to the specified block level. Value returned in hex \
         format."
      ~query:state_value_query
      ~output:Data_encoding.(option bytes)
      (open_root / "global" / "block" / "head" / "durable" / "wasm_2_0_0"
     / "value")

  let batcher_injection =
    Tezos_rpc.Service.post_service
      ~description:"Inject messages in the batcher's queue"
      ~query:Tezos_rpc.Query.empty
      ~input:
        Data_encoding.(
          def "messages" ~description:"Messages to inject" (list string))
      ~output:
        Data_encoding.(
          def
            "message_hashes"
            ~description:"Hashes of injected L2 messages"
            (list string))
      (open_root / "local" / "batcher" / "injection")

  let call_service ~base =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_service
      Media_type.all_media_types
      ~base

  (** We use [rpc get /global/smart_rollup_address] to check if the rollup-node
      answers. *)
  let is_connected base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base smart_rollup_address () () () in
    match answer with
    | Ok _ -> return ()
    | Error tztrace ->
        failwith
          "Failed to communicate with %a, because %a"
          Uri.pp
          base
          pp_print_trace
          tztrace

  let balance base address =
    let open Lwt_result_syntax in
    let key = Durable_storage_path.balance_path address in
    let+ answer = call_service ~base durable_state_value () {key} () in
    match answer with
    | Some bytes ->
        Bytes.to_string bytes |> Z.of_bits |> Ethereum_types.quantity_of_z
    | None -> Ethereum_types.Qty Z.zero

  let inject_raw_transaction base tx =
    let open Lwt_result_syntax in
    let tx = Hex.of_string tx |> Hex.show in
    (* The injection's service returns a notion of L2 message hash (defined
       by the rollup node) used to track the message's injection in the batcher.
       We do not wish to follow the message's inclusion, and thus, ignore
       the resulted hash. *)
    let* _answer = call_service ~base batcher_injection () () [tx] in
    return_unit
end

module type S = sig
  val assert_connected : unit tzresult Lwt.t

  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  val inject_raw_transaction : string -> unit tzresult Lwt.t
end

module Make (Base : sig
  val base : Uri.t
end) : S = struct
  let assert_connected = RPC.is_connected Base.base

  let balance = RPC.balance Base.base

  let inject_raw_transaction = RPC.inject_raw_transaction Base.base
end
