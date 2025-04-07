(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2024 Functori  <contact@functori.com>                       *)
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

type estimate_gas_input_v1 = {
  call : Ethereum_types.call;
  with_da_fees : bool;
      (** If true, the gas returned by the simulation include the DA
          gas units. *)
}

type estimate_gas_input_v2 = {
  call : Ethereum_types.call;
  with_da_fees : bool;
      (** If true, the gas returned by the simulation includes the DA
          gas units. *)
  timestamp : Time.Protocol.t;  (** Timestamp used during simulation. *)
}

type estimate_gas_input =
  | V0 of call
  | V1 of estimate_gas_input_v1
  | V2 of estimate_gas_input_v2

(** [hex_string_to_bytes s] transforms a hex string [s] into a byte string. *)
let hex_string_to_bytes (Hex s) = `Hex s |> Hex.to_bytes_exn

let of_opt of_val = function
  | None -> Rlp.Value Bytes.empty
  | Some v -> of_val v

let of_addr (Address s) = Rlp.Value (hex_string_to_bytes s)

let of_qty (Qty z) = Rlp.Value (Z.to_bits z |> Bytes.of_string)

let of_hash (Hash h) = Rlp.Value (hex_string_to_bytes h)

let rlp_v0 call =
  Rlp.List
    [
      of_opt of_addr call.from;
      of_opt of_addr call.to_;
      of_opt of_qty call.gas;
      of_opt of_qty call.gasPrice;
      of_opt of_qty call.value;
      of_opt of_hash call.data;
    ]

let rlp_v1 ({call; with_da_fees} : estimate_gas_input_v1) =
  Rlp.List
    [
      of_opt of_addr call.from;
      of_opt of_addr call.to_;
      of_opt of_qty call.gas;
      of_opt of_qty call.gasPrice;
      of_opt of_qty call.value;
      of_opt of_hash call.data;
      Ethereum_types.bool_to_rlp_bytes with_da_fees;
    ]

let rlp_v2 ({call; with_da_fees; timestamp} : estimate_gas_input_v2) =
  Rlp.List
    [
      of_opt of_addr call.from;
      of_opt of_addr call.to_;
      of_opt of_qty call.gas;
      of_opt of_qty call.gasPrice;
      of_opt of_qty call.value;
      of_opt of_hash call.data;
      Ethereum_types.bool_to_rlp_bytes with_da_fees;
      Value (Ethereum_types.timestamp_to_bytes timestamp);
    ]

(** Encoding used to forward the call to the kernel, to be used in simulation
    mode only. *)
let rlp_encode input =
  let input =
    (* If the value is not provided, the kernel will put `None` for the transfer
       object. It is not wanted, we want the kernel to put `Some(0)` instead. *)
    match input with
    | V0 call ->
        V0
          {
            call with
            value = Some (Option.value ~default:(Qty Z.zero) call.value);
          }
    | V1 {call; with_da_fees} ->
        V1
          {
            call =
              {
                call with
                value = Some (Option.value ~default:(Qty Z.zero) call.value);
              };
            with_da_fees;
          }
    | V2 {call; with_da_fees; timestamp} ->
        V2
          {
            call =
              {
                call with
                value = Some (Option.value ~default:(Qty Z.zero) call.value);
              };
            with_da_fees;
            timestamp;
          }
  in

  let prefix, rlp =
    match input with
    | V0 call -> (None, rlp_v0 call)
    | V1 input -> (Some '\001', rlp_v1 input)
    | V2 input -> (Some '\002', rlp_v2 input)
  in
  let payload = Rlp.encode rlp in
  match prefix with
  | None -> payload
  | Some prefix -> Bytes.cat (Bytes.make 1 prefix) payload

type simulation_message =
  | Start
  | Simple of string
  | NewChunked of int
  | Chunk of int * string

(* Max input size : 4096B
   - Simulation tag : 1B
   - Chunk tag : 1B
   - Number of chunks : 2B *)
let max_chunk_size = 4092

let split_in_messages call =
  let open Result_syntax in
  let* chunks = String.chunk_bytes max_chunk_size call in
  match chunks with
  | [s] -> return [Start; Simple s]
  | l ->
      let len = List.length l in
      let chunks = List.mapi (fun i c -> Chunk (i, c)) l in
      return (Start :: NewChunked len :: chunks)

(** Tag signaling a simulation message *)
let simulation_tag = "\255"

(** Tag signaling a simulation message containing a full simulation call *)
let simple_tag = "\001"

(** Tag signaling a simulation message starting a serie of chunks *)
let new_chunked_tag = "\002"

(** Tag signaling a simulation message containing a chunk *)
let chunk_tag = "\003"

(** Tag indicating simulation is an evaluation *)
let evaluation_tag = "\000"

(** Tag indicating simulation is a validation

    This tag can no longer be used, but if you plan to add another tag
    please avoid using this one to avoid mistakes.
*)
let _validation_tag = "\001"

(** [add_tag tag bytes] prefixes bytes by the given tag *)
let add_tag tag bytes = tag ^ Bytes.to_string bytes |> String.to_bytes

let encode_message message =
  match message with
  | Start -> simulation_tag
  | Simple s -> simulation_tag ^ simple_tag ^ s
  | NewChunked n ->
      let n_le_str = Ethereum_types.u16_to_bytes n in
      simulation_tag ^ new_chunked_tag ^ n_le_str
  | Chunk (i, c) ->
      let i_le_str = Ethereum_types.u16_to_bytes i in
      simulation_tag ^ chunk_tag ^ i_le_str ^ c

let encode call =
  let open Result_syntax in
  let* messages =
    call |> rlp_encode |> add_tag evaluation_tag |> split_in_messages
  in
  return @@ List.map encode_message messages

type execution_result = {value : hash option; gas_used : quantity option}

type call_result = (execution_result, hash) result

type 'a simulation_result = ('a, string) result

module Encodings = struct
  open Data_encoding

  type eval_result = {
    state_hash : string;
    status : string;
    output : unit;
    inbox_level : unit;
    num_ticks : Z.t;
    insights : bytes list;
        (** The simulation can ask to look at values on the state after
          the simulation. *)
  }

  type insight_request =
    | Pvm_state_key of string list
    | Durable_storage_key of string list

  type simulate_input = {
    messages : string list;
    reveal_pages : string list option;
    insight_requests : insight_request list;
    log_kernel_debug_file : string option;
  }

  let insight_request =
    union
      [
        case
          (Tag 0)
          ~title:"pvm_state"
          ~description:"Path in the PVM state"
          (obj2 (req "kind" (constant "pvm_state")) (req "key" (list string)))
          (function Pvm_state_key key -> Some ((), key) | _ -> None)
          (fun ((), key) -> Pvm_state_key key);
        case
          (Tag 1)
          ~title:"durable_storage"
          ~description:"Path in the PVM durable storage"
          (obj2
             (req "kind" (constant "durable_storage"))
             (req "key" (list string)))
          (function Durable_storage_key key -> Some ((), key) | _ -> None)
          (fun ((), key) -> Durable_storage_key key);
      ]

  let simulate_input =
    conv
      (fun {messages; reveal_pages; insight_requests; log_kernel_debug_file} ->
        (messages, reveal_pages, insight_requests, log_kernel_debug_file))
      (fun (messages, reveal_pages, insight_requests, log_kernel_debug_file) ->
        {messages; reveal_pages; insight_requests; log_kernel_debug_file})
    @@ obj4
         (req
            "messages"
            (list (string' Hex))
            ~description:"Serialized messages for simulation.")
         (opt
            "reveal_pages"
            (list (string' Hex))
            ~description:"Pages (at most 4kB) to be used for revelation ticks")
         (dft
            "insight_requests"
            (list insight_request)
            []
            ~description:"Paths in the PVM to inspect after the simulation")
         (opt
            "log_kernel_debug_file"
            string
            ~description:
              "File in which to emit kernel logs. This file will be created in \
               <data-dir>/simulation_kernel_logs/, where <data-dir> is the \
               data directory of the rollup node.")

  module type RLP_DECODING = sig
    val decode_call_result :
      Rlp.item -> (execution_result, hash) result tzresult

    val simulation_result_from_rlp :
      (Rlp.item -> 'a tzresult) ->
      bytes ->
      (('a, string) result, tztrace) result
  end

  module Rlp_decoding = struct
    module V0 = struct
      let decode_data =
        let open Result_syntax in
        function
        | Rlp.Value v -> return (decode_hash v)
        | Rlp.List _ -> error_with "The simulation returned an ill-encoded data"

      let decode_gas_used =
        let open Result_syntax in
        function
        | Rlp.Value v -> return (decode_number_le v)
        | Rlp.List _ -> error_with "The simulation returned an ill-encoded gas"

      let decode_execution_result =
        let open Result_syntax in
        function
        | Rlp.List [value; gas_used] ->
            let* value = Rlp.decode_option decode_data value in
            let* gas_used = Rlp.decode_option decode_gas_used gas_used in
            return {value; gas_used}
        | _ ->
            error_with
              "The simulation for eth_call/eth_estimateGas returned an \
               ill-encoded format"

      let decode_call_result v =
        let open Result_syntax in
        let decode_revert = function
          | Rlp.Value msg -> return (decode_hash msg)
          | _ -> error_with "The revert message is ill-encoded"
        in
        Rlp.decode_result decode_execution_result decode_revert v

      let simulation_result_from_rlp decode_payload bytes =
        let open Result_syntax in
        let decode_error_msg = function
          | Rlp.Value msg -> return @@ Bytes.to_string msg
          | rlp ->
              error_with
                "The simulation returned an unexpected error message: %a"
                Rlp.pp
                rlp
        in
        let* rlp = Rlp.decode bytes in
        match Rlp.decode_result decode_payload decode_error_msg rlp with
        | Ok v -> Ok v
        | Error e ->
            error_with
              "The simulation returned an unexpected format: %a, with error %a"
              Rlp.pp
              rlp
              pp_print_trace
              e
    end

    module V1 = struct
      include V0

      let simulation_result_from_rlp decode_payload bytes =
        let bytes = Bytes.sub bytes 1 (Bytes.length bytes - 1) in
        simulation_result_from_rlp decode_payload bytes
    end

    let select_rlp_decodings bytes : (module RLP_DECODING) =
      match Bytes.get_uint8 bytes 0 with 1 -> (module V1) | _ -> (module V0)
  end

  let eval_result =
    conv
      (fun {state_hash; status; output; inbox_level; num_ticks; insights} ->
        (state_hash, status, output, inbox_level, num_ticks, insights))
      (fun (state_hash, status, output, inbox_level, num_ticks, insights) ->
        {state_hash; status; output; inbox_level; num_ticks; insights})
    @@ obj6
         (req
            "state_hash"
            string
            ~description:
              "Hash of the state after execution of the PVM on the input \
               messages")
         (req "status" string ~description:"Status of the PVM after evaluation")
         (req
            "output"
            unit
            ~description:"Output produced by evaluation of the messages")
         (req
            "inbox_level"
            unit
            ~description:"Level of the inbox that would contain these messages")
         (req
            "num_ticks"
            z
            ~description:"Ticks taken by the PVM for evaluating the messages")
         (req
            "insights"
            (list bytes)
            ~description:"PVM state values requested after the simulation")
end

let simulation_result bytes =
  let module Encodings = (val Encodings.Rlp_decoding.select_rlp_decodings bytes)
  in
  Encodings.simulation_result_from_rlp Encodings.decode_call_result bytes

let gas_estimation bytes =
  let module Encodings = (val Encodings.Rlp_decoding.select_rlp_decodings bytes)
  in
  Encodings.simulation_result_from_rlp Encodings.decode_call_result bytes
