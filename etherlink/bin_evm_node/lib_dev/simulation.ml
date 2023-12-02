(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** [hex_string_to_bytes s] transforms a hex string [s] into a byte string. *)
let hex_string_to_bytes (Hex s) = `Hex s |> Hex.to_bytes_exn

(** Encoding used to forward the call to the kernel, to be used in simulation
     mode only. *)
let rlp_encode call =
  let of_opt of_val = function
    | None -> Rlp.Value Bytes.empty
    | Some v -> of_val v
  in
  let of_addr (Address s) = Rlp.Value (hex_string_to_bytes s) in
  let of_qty (Qty z) = Rlp.Value (Z.to_bits z |> Bytes.of_string) in
  let of_hash (Hash h) = Rlp.Value (hex_string_to_bytes h) in
  let rlp_form =
    Rlp.List
      [
        of_opt of_addr call.from;
        of_opt of_addr call.to_;
        of_opt of_qty call.gas;
        of_opt of_qty call.gasPrice;
        of_opt of_qty call.value;
        of_opt of_hash call.data;
      ]
  in
  (* we aim to use [String.chunk_bytes] *)
  Rlp.encode rlp_form

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

(** Tag indicating simulation is a validation *)
let validation_tag = "\001"

(** [add_tag tag bytes] prefixes bytes by the given tag *)
let add_tag tag bytes = tag ^ Bytes.to_string bytes |> String.to_bytes

let encode_message = function
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

let encode_tx tx =
  let open Result_syntax in
  let* messages =
    Bytes.of_string tx |> add_tag validation_tag |> split_in_messages
  in
  return @@ List.map encode_message messages

module Encodings = struct
  open Data_encoding

  type eval_result = {
    state_hash : string;
    status : string;
    output : unit;
    inbox_level : unit;
    num_ticks : Z.t;
    insights : bytes option list;
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
            (list (option bytes))
            ~description:"PVM state values requested after the simulation")
end

let call_result bytes =
  let open Lwt_result_syntax in
  match bytes with
  | Some b :: _ ->
      let v = b |> Hex.of_bytes |> Hex.show in
      return (Hash (Hex v))
  | _ -> failwith "Insights of 'call_result' is not Some _ :: _"

let gas_estimation bytes =
  let open Lwt_result_syntax in
  let* simulated_amount =
    match bytes with
    | Some b :: _ -> b |> Bytes.to_string |> Z.of_bits |> return
    | _ -> failwith "Insights of 'gas_estimation' is not Some _ :: _"
  in
  (* See EIP2200 for reference. But the tl;dr is: we cannot do the
     opcode SSTORE if we have less than 2300 gas available, even if we don't
     consume it. The simulated amount then gives an amount of gas insufficient
     to execute the transaction.

     The extra gas units, i.e. 2300, will be refunded.
  *)
  let simulated_amount = Z.(add simulated_amount (of_int 2300)) in
  return (quantity_of_z simulated_amount)

let is_tx_valid bytes =
  let open Lwt_result_syntax in
  match bytes with
  | [Some b; Some payload] ->
      let is_valid =
        b |> Data_encoding.Binary.of_bytes_exn Data_encoding.bool
      in
      if is_valid then
        let address = Ethereum_types.decode_address payload in
        return (Ok address)
      else
        let error_msg = Bytes.to_string payload in
        return (Error error_msg)
  | _ -> failwith "Insights of 'is_tx_valid' is not [Some _, Some _]"
