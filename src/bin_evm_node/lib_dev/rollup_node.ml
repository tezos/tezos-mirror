(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(* Copyright (c) 2023 Trilitech <contact@trili.tech>                         *)
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
open Rollup_node_services

(* The hard limit is 4096 but it needs to add the external message tag. *)
let max_input_size = 4095

let smart_rollup_address_size = 20

let transaction_tag_size = 1

let framing_protocol_tag_size = 1

type transaction =
  | Simple of string
  | NewChunked of (string * int * string)
  | Chunk of string

let encode_transaction ~smart_rollup_address kind =
  let data =
    match kind with
    | Simple data -> "\000" ^ data
    | NewChunked (tx_hash, len, all_chunk_hashes) ->
        let number_of_chunks_bytes = Ethereum_types.u16_to_bytes len in
        "\001" ^ tx_hash ^ number_of_chunks_bytes ^ all_chunk_hashes
    | Chunk data -> "\002" ^ data
  in
  "\000" ^ smart_rollup_address ^ data

let chunk_transaction ~tx_hash ~tx_raw =
  let open Result_syntax in
  let size_per_chunk =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - 2 (* Index as u16 *)
    - (Ethereum_types.transaction_hash_size * 2)
  in
  let* chunks = String.chunk_bytes size_per_chunk (Bytes.of_string tx_raw) in
  let all_chunk_hashes, chunks =
    List.fold_left_i
      (fun i (all_chunk_hashes, chunks) chunk ->
        let chunk_hash = Ethereum_types.hash_raw_tx chunk in
        let all_chunk_hashes = all_chunk_hashes ^ chunk_hash in
        let chunk =
          Chunk (tx_hash ^ Ethereum_types.u16_to_bytes i ^ chunk_hash ^ chunk)
        in
        (all_chunk_hashes, chunk :: chunks))
      ("", [])
      chunks
  in
  let new_chunk_transaction =
    NewChunked (tx_hash, List.length chunks, all_chunk_hashes)
  in
  return (tx_hash, new_chunk_transaction :: chunks)

let make_evm_inbox_transactions tx_raw =
  let open Result_syntax in
  let tx_raw = Ethereum_types.hex_to_bytes tx_raw in
  (* Maximum size describes the maximum size of [tx_raw] to fit
     in a simple transaction. *)
  let maximum_size =
    max_input_size - framing_protocol_tag_size - smart_rollup_address_size
    - transaction_tag_size - Ethereum_types.transaction_hash_size
  in
  let tx_hash = Ethereum_types.hash_raw_tx tx_raw in
  if String.length tx_raw <= maximum_size then
    (* Simple transaction, fits in a single input. *)
    let tx = Simple (tx_hash ^ tx_raw) in
    return (tx_hash, [tx])
  else chunk_transaction ~tx_hash ~tx_raw

let make_encoded_messages ~smart_rollup_address tx_raw =
  let open Result_syntax in
  let* tx_hash, messages = make_evm_inbox_transactions tx_raw in
  let messages =
    List.map
      (fun x ->
        x
        |> encode_transaction ~smart_rollup_address
        |> Hex.of_string |> Hex.show)
      messages
  in
  return (tx_hash, messages)

let smart_rollup_address base =
  let open Lwt_result_syntax in
  let*! answer =
    call_service
      ~base
      ~media_types:[Media_type.octet_stream]
      smart_rollup_address
      ()
      ()
      ()
  in
  match answer with
  | Ok address -> return (Bytes.to_string address)
  | Error tztrace ->
      failwith
        "Failed to communicate with %a, because %a"
        Uri.pp
        base
        pp_print_trace
        tztrace

let inject_raw_transaction base tx =
  let open Lwt_result_syntax in
  (* The injection's service returns a notion of L2 message hash (defined
     by the rollup node) used to track the message's injection in the batcher.
     We do not wish to follow the message's inclusion, and thus, ignore
     the resulted hash. *)
  let* _answer = call_service ~base batcher_injection () () [tx] in
  return_unit

let inject_raw_transaction base ~smart_rollup_address tx_raw =
  let open Lwt_result_syntax in
  let*? tx_hash, messages =
    make_encoded_messages ~smart_rollup_address tx_raw
  in
  let* () = List.iter_es (inject_raw_transaction base) messages in
  return (Ethereum_types.Hash Hex.(of_string tx_hash |> show |> hex_of_string))

let simulate_call base call =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode call in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5900
         for now the status is not used but it should be for error handling *)
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "simulate_call";
      }
  in
  Simulation.call_result r

let estimate_gas base call =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode call in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_gas"];
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/5900
         for now the status is not used but it should be for error handling *)
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "estimate_gas";
      }
  in
  Simulation.gas_estimation r

let is_tx_valid base (Hex tx_raw) =
  let open Lwt_result_syntax in
  let*? messages = Simulation.encode_tx tx_raw in
  let insight_requests =
    [
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_status"];
      Simulation.Encodings.Durable_storage_key ["evm"; "simulation_result"];
    ]
  in
  let* r =
    call_service
      ~base
      simulation
      ()
      ()
      {
        messages;
        reveal_pages = None;
        insight_requests;
        log_kernel_debug_file = Some "tx_validity";
      }
  in
  Simulation.is_tx_valid r

module type S = sig
  val smart_rollup_address : string tzresult Lwt.t

  val balance : Ethereum_types.address -> Ethereum_types.quantity tzresult Lwt.t

  val nonce :
    Ethereum_types.address -> Ethereum_types.quantity option tzresult Lwt.t

  val code : Ethereum_types.address -> Ethereum_types.hex tzresult Lwt.t

  val inject_raw_transaction :
    smart_rollup_address:string -> hex -> hash tzresult Lwt.t

  val current_block :
    full_transaction_object:bool -> Ethereum_types.block tzresult Lwt.t

  val current_block_number : unit -> Ethereum_types.block_height tzresult Lwt.t

  val nth_block :
    full_transaction_object:bool -> Z.t -> Ethereum_types.block tzresult Lwt.t

  val block_by_hash :
    full_transaction_object:bool ->
    Ethereum_types.block_hash ->
    Ethereum_types.block tzresult Lwt.t

  val transaction_receipt :
    Ethereum_types.hash ->
    Ethereum_types.transaction_receipt option tzresult Lwt.t

  val transaction_object :
    Ethereum_types.hash ->
    Ethereum_types.transaction_object option tzresult Lwt.t

  val chain_id : unit -> Ethereum_types.quantity tzresult Lwt.t

  val base_fee_per_gas : unit -> Ethereum_types.quantity tzresult Lwt.t

  val kernel_version : unit -> string tzresult Lwt.t

  val simulate_call : Ethereum_types.call -> Ethereum_types.hash tzresult Lwt.t

  val estimate_gas :
    Ethereum_types.call -> Ethereum_types.quantity tzresult Lwt.t

  val is_tx_valid :
    Ethereum_types.hex -> (Ethereum_types.address, string) result tzresult Lwt.t

  val storage_at :
    Ethereum_types.address ->
    Ethereum_types.quantity ->
    Ethereum_types.hex tzresult Lwt.t
end

module Make (Base : sig
  val base : Uri.t
end) : S = struct
  include Durable_storage.Make (struct
    let read path =
      call_service ~base:Base.base durable_state_value () {key = path} ()
  end)

  let smart_rollup_address = smart_rollup_address Base.base

  let inject_raw_transaction = inject_raw_transaction Base.base

  let simulate_call = simulate_call Base.base

  let estimate_gas = estimate_gas Base.base

  let is_tx_valid = is_tx_valid Base.base
end
