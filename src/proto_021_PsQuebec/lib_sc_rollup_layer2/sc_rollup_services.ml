(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

type eval_result = {
  state_hash : Sc_rollup.State_hash.t;
  status : string;
  output : Sc_rollup.output list;
  inbox_level : int32;
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

type commitment_info = {
  commitment : Sc_rollup.Commitment.t;
  commitment_hash : Sc_rollup.Commitment.Hash.t;
  first_published_at_level : Raw_level.t;
  published_at_level : Raw_level.t;
}

module Encodings = struct
  open Data_encoding

  let hex_string = string' Hex

  let eval_result =
    conv
      (fun {state_hash; status; output; inbox_level; num_ticks; insights} ->
        (state_hash, status, output, inbox_level, num_ticks, insights))
      (fun (state_hash, status, output, inbox_level, num_ticks, insights) ->
        {state_hash; status; output; inbox_level; num_ticks; insights})
    @@ obj6
         (req
            "state_hash"
            Sc_rollup.State_hash.encoding
            ~description:
              "Hash of the state after execution of the PVM on the input \
               messages")
         (req "status" string ~description:"Status of the PVM after evaluation")
         (req
            "output"
            (list Sc_rollup.output_encoding)
            ~description:"Output produced by evaluation of the messages")
         (req
            "inbox_level"
            int32
            ~description:"Level of the inbox that would contain these messages")
         (req
            "num_ticks"
            z
            ~description:"Ticks taken by the PVM for evaluating the messages")
         (dft
            "insights"
            (list (option bytes))
            []
            ~description:"PVM state values requested after the simulation")

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
            (list hex_string)
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
         (opt
            "log_kernel_debug_file"
            string
            ~description:
              "File in which to emit kernel logs. This file will be created in \
               <data-dir>/simulation_kernel_logs/, where <data-dir> is the \
               data directory of the rollup node.")

  let commitment_info =
    conv
      (fun {
             commitment;
             commitment_hash;
             first_published_at_level;
             published_at_level;
           }
         ->
        ( commitment,
          commitment_hash,
          first_published_at_level,
          published_at_level ))
      (fun ( commitment,
             commitment_hash,
             first_published_at_level,
             published_at_level )
         ->
        {
          commitment;
          commitment_hash;
          first_published_at_level;
          published_at_level;
        })
    @@ obj4
         (req "commitment" Sc_rollup.Commitment.encoding)
         (req "hash" Sc_rollup.Commitment.Hash.encoding)
         (req "first_published_at_level" Raw_level.encoding)
         (req "published_at_level" Raw_level.encoding)
end

module Query = struct
  type key_query = {key : string}

  let key_query : key_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun key -> {key})
    |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
    |> seal

  let outbox_level_query =
    let open Tezos_rpc.Query in
    query (fun outbox_level ->
        let req name f = function
          | None ->
              raise
                (Invalid (Format.sprintf "Query parameter %s is required" name))
          | Some arg -> f arg
        in
        req "outbox_level" Raw_level.of_int32_exn outbox_level)
    |+ opt_field "outbox_level" Tezos_rpc.Arg.int32 (fun o ->
           Some (Raw_level.to_int32 o))
    |> seal

  let message_index_query =
    let open Tezos_rpc.Query in
    query (fun message_index ->
        let req name f = function
          | None ->
              raise
                (Invalid (Format.sprintf "Query parameter %s is required" name))
          | Some arg -> f arg
        in
        req "index" (fun o -> o) message_index)
    |+ opt_field "index" Tezos_rpc.Arg.uint (fun o -> Some o)
    |> seal

  type simulate_query = {fuel : int64 option}

  let simulate_query : simulate_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun fuel -> {fuel})
    |+ opt_field "fuel" Tezos_rpc.Arg.int64 (fun t -> t.fuel)
    |> seal

  let outbox_query : bool Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun b -> b)
    |+ field "outbox" Tezos_rpc.Arg.bool false (fun b -> b)
    |> seal
end

module Block = struct
  open Tezos_rpc.Path

  type prefix = unit * Rollup_node_services.Arg.block_id

  let path : prefix Tezos_rpc.Path.context = open_root

  let prefix = root / "global" / "block" /: Rollup_node_services.Arg.block_id

  let durable_state_value (pvm_kind : Sc_rollup.Kind.t) =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve value by key from PVM durable storage. PVM state is taken \
         with respect to the specified block level. Value returned in hex \
         format."
      ~query:Query.key_query
      ~output:Data_encoding.(option bytes)
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "value")

  let durable_state_length (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t)
      =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve number of bytes in raw representation of value by key from \
         PVM durable storage. PVM state is taken with respect to the specified \
         block level."
      ~query:Query.key_query
      ~output:Data_encoding.(option int64)
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "length")

  let durable_state_subkeys (pvm_kind : Sc_rollup.Kind.t) =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve subkeys of the specified key from PVM durable storage. PVM \
         state is taken with respect to the specified block level."
      ~query:Query.key_query
      ~output:Data_encoding.(list string)
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "subkeys")

  let durable_state_values (pvm_kind : Sc_rollup.Kind.t) =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve values directly under a given key from PVM durable storage. \
         PVM state is taken with respect to the specified block level."
      ~query:Query.key_query
      ~output:Data_encoding.(list (obj2 (req "key" string) (req "value" bytes)))
      (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "values")

  let status =
    Tezos_rpc.Service.get_service
      ~description:"PVM status at block"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.string
      (path / "status")

  let outbox =
    Tezos_rpc.Service.get_service
      ~description:"Outbox at block for a given outbox level"
      ~query:Query.outbox_level_query
      ~output:Data_encoding.(list Sc_rollup.output_encoding)
      (path / "outbox")

  let simulate =
    Tezos_rpc.Service.post_service
      ~description:"Simulate messages evaluation by the PVM"
      ~query:Tezos_rpc.Query.empty
      ~input:Encodings.simulate_input
      ~output:Encodings.eval_result
      (path / "simulate")

  let level_param =
    let destruct s =
      match Int32.of_string_opt s with
      | None -> Error "Invalid level"
      | Some l -> (
          match Raw_level.of_int32 l with
          | Error _ -> Error "Invalid level"
          | Ok l -> Ok l)
    in
    let construct = Format.asprintf "%a" Raw_level.pp in
    Tezos_rpc.Arg.make ~name:"level" ~construct ~destruct ()

  let outbox_messages =
    Tezos_rpc.Service.get_service
      ~description:"Outbox at block for a given outbox level"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.(list Sc_rollup.output_encoding)
      (path / "outbox" /: level_param / "messages")

  let committed_status =
    Tezos_rpc.Service.get_service
      ~description:
        "Commitment status of the rollup state which will include content of \
         this block"
      ~query:Tezos_rpc.Query.empty
      ~output:Rollup_node_services.Encodings.committed_status
      (path / "committed_status")

  module Helpers = struct
    type nonrec prefix = prefix

    let prefix = prefix

    let path = path / "helpers"

    let outbox_proof_simple =
      Tezos_rpc.Service.get_service
        ~description:
          "Generate serialized output proof for some outbox message at level \
           and index"
        ~query:Query.message_index_query
        ~output:
          Data_encoding.(
            obj2
              (req "commitment" Sc_rollup.Commitment.Hash.encoding)
              (req "proof" Encodings.hex_string))
        (path / "proofs" / "outbox" /: level_param / "messages")
  end
end
