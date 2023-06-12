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

(** [rope_of_hex_string s] transforms a hex string [s] into a byte string
    represented by a [Rope.t]  *)
let rope_of_hex_string s =
  `Hex s |> Hex.to_bytes_exn |> Bytes.to_string |> Rope.of_string

(** Encoding used to forward the call to the kernel, to be used in simulation
     mode only. *)
let rlp_encode call =
  let of_opt of_val = function
    | None -> Rlp.RlpData Rope.empty
    | Some v -> of_val v
  in
  let of_addr (Address s) = Rlp.RlpData (rope_of_hex_string s) in
  let of_qty (Qty z) = Rlp.RlpData (Rope.of_string @@ Z.to_bits z) in
  let of_hash (Hash h) = Rlp.RlpData (rope_of_hex_string h) in
  let rlp_form =
    Rlp.RlpList
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
  Rlp.encode rlp_form |> Rope.to_string |> Bytes.of_string

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

(** [hex_str_of_binary_string s] translate a binary string into an hax string *)
let hex_str_of_binary_string s = s |> Hex.of_string |> Hex.show

let encode_message = function
  | Start -> hex_str_of_binary_string @@ simulation_tag
  | Simple s -> hex_str_of_binary_string @@ simulation_tag ^ simple_tag ^ s
  | NewChunked n ->
      let n_le_str = Ethereum_types.u16_to_bytes n in
      hex_str_of_binary_string @@ simulation_tag ^ new_chunked_tag ^ n_le_str
  | Chunk (i, c) ->
      let i_le_str = Ethereum_types.u16_to_bytes i in
      hex_str_of_binary_string @@ simulation_tag ^ chunk_tag ^ i_le_str ^ c

let encode call =
  let open Result_syntax in
  let* messages = call |> rlp_encode |> split_in_messages in
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
  }

  let hex_string = conv Bytes.of_string Bytes.to_string bytes

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
      (fun {messages; reveal_pages; insight_requests} ->
        (messages, reveal_pages, insight_requests))
      (fun (messages, reveal_pages, insight_requests) ->
        {messages; reveal_pages; insight_requests})
    @@ obj3
         (req
            "messages"
            (list string)
            ~description:"Serialized messages for simulation.")
         (opt
            "reveal_pages"
            (list hex_string)
            ~description:"Pages (at most 4kB) to be used for revelation ticks")
         (dft
            "insight_requests"
            (list insight_request)
            []
            ~description:"Paths in the PVM to inspect after the simulation")

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

let parse_insights decode (r : Data_encoding.json) =
  let s = Data_encoding.Json.destruct Encodings.eval_result r in
  match s.insights with
  | Some b :: _ -> Lwt.return_ok (decode b)
  | _ ->
      Error_monad.failwith
        "Couldn't parse insights: %s"
        (Data_encoding.Json.to_string r)

let call_result =
  parse_insights (fun b ->
      let v = b |> Hex.of_bytes |> Hex.show in
      Hash v)

let gas_estimation json =
  let open Lwt_result_syntax in
  let decode b = b |> Bytes.to_string |> Z.of_bits in
  let* simulated_amount = parse_insights decode json in
  (* TODO: https://gitlab.com/tezos/tezos/-/issues/5977
     remove this once the gas is accounted correctly *)
  (* minimum gas for any Ethereum transaction *)
  let min_amount = Z.of_int 21000 in
  let amount =
    Z.(
      if simulated_amount < min_amount then simulated_amount + min_amount
      else simulated_amount)
  in
  return @@ quantity_of_z @@ Z.max simulated_amount amount
