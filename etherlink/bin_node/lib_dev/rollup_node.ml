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

open Rollup_services
open Transaction_format

module MakeBackend (Base : sig
  val base : Uri.t

  val keep_alive : bool

  val timeout : float

  val drop_duplicate_on_injection : bool

  val smart_rollup_address : string

  val finalized : bool

  val ignore_block_param : bool
end) : Services_backend_sig.Backend = struct
  module Reader = struct
    type state = Block_id.t

    let get_state ?block () =
      let open Lwt_result_syntax in
      let block = if Base.ignore_block_param then None else block in
      match block with
      | Some Ethereum_types.Block_parameter.(Block_parameter Finalized) ->
          return Block_id.Finalized
      | Some (Block_parameter Latest) | None ->
          let level : Rollup_services.Block_id.t =
            if Base.finalized then Finalized else Head
          in
          return level
      | Some _ ->
          failwith
            "The EVM node in proxy mode support state requests only on latest \
             or finalized block."

    let read block_id path =
      call_service
        ~keep_alive:Base.keep_alive
        ~base:Base.base
        ~timeout:Base.timeout
        durable_state_value
        ((), block_id)
        {key = path}
        ()

    let subkeys block_id path =
      let open Lwt_result_syntax in
      let* subkeys =
        call_service
          ~keep_alive:Base.keep_alive
          ~base:Base.base
          ~timeout:Base.timeout
          durable_state_subkeys
          ((), block_id)
          {key = path}
          ()
      in
      match subkeys with Some subkeys -> return subkeys | None -> return []
  end

  module TxEncoder = struct
    type transactions = (string * Transaction_object.t) list

    type messages = string list

    let encode_transactions ~smart_rollup_address ~transactions =
      let open Result_syntax in
      let* rev_hashes, messages =
        List.fold_left_e
          (fun (tx_hashes, to_publish) (tx_raw, _) ->
            let* tx_hash, messages =
              make_encoded_messages ~smart_rollup_address tx_raw
            in
            return (tx_hash :: tx_hashes, to_publish @ messages))
          ([], [])
          transactions
      in
      return (List.rev rev_hashes, messages)
  end

  module Publisher = struct
    type messages = TxEncoder.messages

    let publish_messages ~timestamp:_ ~smart_rollup_address:_ ~messages =
      let open Lwt_result_syntax in
      (* The injection's service returns a notion of L2 message ids (defined
         by the rollup node) used to track the message's injection in the batcher.
         We do not wish to follow the message's inclusion, and thus, ignore
         the resulted ids. *)
      let* _answer =
        call_service
          ~keep_alive:Base.keep_alive
          ~base:Base.base
          ~timeout:Base.timeout
          batcher_injection
          ()
          {
            (* to be retro-compatible with rollup node version that does
               not have yet the query in the injection rpc, don't add
               the flag if the `drop_duplicate_on_injection` is false.*)
            drop_duplicate =
              (if Base.drop_duplicate_on_injection then Some true else None);
            order = None;
            (* No uses for it here. *)
          }
          messages
      in
      return_unit
  end

  module SimulatorBackend = struct
    include Reader

    let modify ~key:_ ~value:_ _state =
      failwith "Unsupported primitive for the proxy mode"

    let simulate_and_read ?state_override:_ _state ~input =
      let open Lwt_result_syntax in
      let* json =
        call_service
          ~keep_alive:Base.keep_alive
          ~base:Base.base
          ~timeout:Base.timeout
          simulation
          ()
          ()
          input
      in
      let eval_result =
        Data_encoding.Json.destruct Simulation.Encodings.eval_result json
      in
      match eval_result.insights with
      | [data] -> return data
      | _ -> failwith "Inconsistent simulation results"
  end

  let block_param_to_block_number ~chain_family
      (block_param : Ethereum_types.Block_parameter.extended) =
    let open Lwt_result_syntax in
    let root = Durable_storage_path.root_of_chain_family chain_family in
    let read_from_block_parameter param =
      let* state = Reader.get_state ~block:(Block_parameter param) () in
      let* value =
        Reader.read state (Durable_storage_path.Block.current_number ~root)
      in
      match value with
      | Some value ->
          return (Ethereum_types.Qty (Bytes.to_string value |> Z.of_bits))
      | None -> failwith "Cannot fetch the requested block"
    in
    match block_param with
    | Block_parameter (Number n) -> return n
    | Block_hash {hash; _} -> (
        let* state = Reader.get_state ~block:(Block_parameter Latest) () in
        let* value =
          Reader.read state (Durable_storage_path.Block.by_hash ~root hash)
        in
        match value with
        | Some value ->
            let block = L2_types.block_from_bytes ~chain_family value in
            return (L2_types.block_number block)
        | None ->
            failwith
              "Missing state for block %a"
              Ethereum_types.pp_block_hash
              hash)
    | Block_parameter Latest -> read_from_block_parameter Latest
    | Block_parameter Finalized ->
        (* It is no longer supported because the shared implementaton takes
           the finalized block number using an RPC where the block id is finalized.
           However, when we fetch the content of the block, it's done on latest state,
           not on the finalized one. This needs to be changed if we want to support
           this parameter. *)
        failwith
          "Finalized block parameter is no longer supported on the EVM node in \
           proxy mode"
    | Block_parameter Pending ->
        failwith "Pending block parameter is not supported"
    | Block_parameter Earliest ->
        failwith "Earliest block parameter is not supported"

  module Tracer = struct
    let trace_transaction _ ~block_number:_ ~transaction_hash:_ ~config:_ =
      Lwt_result_syntax.tzfail Tracer_types.Not_supported

    let trace_call _ ~call:_ ~block:_ ~config:_ =
      Lwt_result_syntax.tzfail Tracer_types.Not_supported

    let trace_block _ _ ~block_number:_ ~config:_ =
      Lwt_result_syntax.tzfail Tracer_types.Not_supported
  end

  let smart_rollup_address = Base.smart_rollup_address

  let list_l1_l2_levels ~from_l1_level:_ =
    failwith "L1/L2 levels relationship not available in proxy mode"

  let l2_levels_of_l1_level _ =
    failwith "L1/L2 levels relationship not available in proxy mode"
end

module Make (Base : sig
  val base : Uri.t

  val keep_alive : bool

  val timeout : float

  val drop_duplicate_on_injection : bool

  val smart_rollup_address : string

  val finalized : bool

  val ignore_block_param : bool
end) =
  Services_backend_sig.Make (MakeBackend (Base)) (Evm_execution.No_execution)
