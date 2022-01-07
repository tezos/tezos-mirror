(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Protocol
open Alpha_context

type unsigned_block = {
  unsigned_block_header : Block_header.t;
  operations : Tezos_base.Operation.t list list;
}

type simulation_kind =
  | Filter of Operation_pool.Prioritized.t
  | Apply of {
      ordered_pool : Operation_pool.ordered_pool;
      payload_hash : Block_payload_hash.t;
    }

type simulation_mode = Local of Context.index | Node

let forge_faked_protocol_data ?(payload_hash = Block_payload_hash.zero)
    ~payload_round ~seed_nonce_hash ~liquidity_baking_escape_vote () =
  Block_header.
    {
      contents =
        {
          payload_hash;
          payload_round;
          seed_nonce_hash;
          proof_of_work_nonce = Baking_pow.empty_proof_of_work_nonce;
          liquidity_baking_escape_vote;
        };
      signature = Signature.zero;
    }

let convert_operation (op : packed_operation) : Tezos_base.Operation.t =
  {
    shell = op.shell;
    proto =
      Data_encoding.Binary.to_bytes_exn
        Alpha_context.Operation.protocol_data_encoding
        op.protocol_data;
  }

(* Build the block header : mimics node prevalidation *)
let finalize_block_header shell_header timestamp validation_result
    operations_hash predecessor_block_metadata_hash
    predecessor_ops_metadata_hash =
  let {Tezos_protocol_environment.context; fitness; message; _} =
    validation_result
  in
  let validation_passes = List.length Main.validation_passes in
  (Context_ops.get_test_chain context >>= function
   | Not_running -> return context
   | Running {expiration; _} ->
       if Time.Protocol.(expiration <= timestamp) then
         Context_ops.add_test_chain context Not_running >>= fun context ->
         return context
       else return context
   | Forking _ -> assert false)
  >>=? fun context ->
  (match predecessor_block_metadata_hash with
  | Some predecessor_block_metadata_hash ->
      Context_ops.add_predecessor_block_metadata_hash
        context
        predecessor_block_metadata_hash
  | None -> Lwt.return context)
  >>= fun context ->
  (match predecessor_ops_metadata_hash with
  | Some predecessor_ops_metadata_hash ->
      Context_ops.add_predecessor_ops_metadata_hash
        context
        predecessor_ops_metadata_hash
  | None -> Lwt.return context)
  >>= fun context ->
  let context = Context_ops.hash ~time:timestamp ?message context in
  let header =
    Tezos_base.Block_header.
      {
        shell_header with
        level = Int32.succ shell_header.level;
        validation_passes;
        operations_hash;
        fitness;
        context;
      }
  in
  return header

let retain_live_operations_only ~live_blocks operation_pool =
  Operation_pool.Prioritized.filter
    (fun ({shell; _} : packed_operation) ->
      Block_hash.Set.mem shell.branch live_blocks)
    operation_pool

let forge (cctxt : #Protocol_client_context.full) ~chain_id ~pred_info
    ~timestamp ~liquidity_baking_escape_vote fees_config ~seed_nonce_hash
    ~payload_round simulation_mode simulation_kind constants =
  let predecessor_block = (pred_info : Baking_state.block_info) in
  let hard_gas_limit_per_block = constants.Constants.hard_gas_limit_per_block in
  let chain = `Hash chain_id in
  let filter_via_node ~operation_pool =
    let filtered_operations =
      Operation_selection.filter_operations_without_simulation
        fees_config
        ~hard_gas_limit_per_block
        operation_pool
    in
    let faked_protocol_data =
      forge_faked_protocol_data
        ~payload_round
        ~seed_nonce_hash
        ~liquidity_baking_escape_vote
        ()
    in
    Node_rpc.preapply_block
      cctxt
      ~chain
      ~head:predecessor_block.hash
      ~timestamp
      ~protocol_data:faked_protocol_data
      filtered_operations
    >>=? fun (shell_header, preapply_result) ->
    (* only retain valid operations *)
    let operations =
      List.map (fun l -> List.map snd l.Preapply_result.applied) preapply_result
    in
    let payload_hash =
      let operation_list_hash =
        Stdlib.List.tl operations |> List.flatten
        |> List.map Tezos_base.Operation.hash
        |> Operation_list_hash.compute
      in
      Block_payload.hash
        ~predecessor:shell_header.predecessor
        payload_round
        operation_list_hash
    in
    return (shell_header, operations, payload_hash)
  in
  let filter_with_context ~context_index ~operation_pool =
    let faked_protocol_data =
      forge_faked_protocol_data
        ~payload_round
        ~seed_nonce_hash
        ~liquidity_baking_escape_vote
        ()
    in
    Baking_simulator.begin_construction
      ~timestamp
      ~protocol_data:faked_protocol_data
      context_index
      predecessor_block
      chain_id
    >>=? fun incremental ->
    Operation_selection.filter_operations_with_simulation
      incremental
      fees_config
      ~hard_gas_limit_per_block
      operation_pool
    >>=? fun {
               Operation_selection.operations;
               validation_result;
               operations_hash;
               _;
             } ->
    let _op_pool' =
      Operation_pool.(add_operations empty (List.concat operations))
    in
    protect
      ~on_error:(fun _ -> return_none)
      (fun () ->
        Shell_services.Blocks.metadata_hash
          cctxt
          ~block:(`Hash (predecessor_block.hash, 0))
          ~chain
          ()
        >>=? fun pred_block_metadata_hash ->
        return (Some pred_block_metadata_hash))
    >>=? fun pred_block_metadata_hash ->
    protect
      ~on_error:(fun _ -> return_none)
      (fun () ->
        Shell_services.Blocks.Operation_metadata_hashes.root
          cctxt
          ~block:(`Hash (predecessor_block.hash, 0))
          ~chain
          ()
        >>=? fun pred_op_metadata_hash -> return (Some pred_op_metadata_hash))
    >>=? fun pred_op_metadata_hash ->
    finalize_block_header
      incremental.header
      timestamp
      validation_result
      operations_hash
      pred_block_metadata_hash
      pred_op_metadata_hash
    >>=? fun shell_header ->
    let operations = List.map (List.map convert_operation) operations in
    let payload_hash =
      let operation_list_hash =
        Stdlib.List.tl operations |> List.flatten
        |> List.map Tezos_base.Operation.hash
        |> Operation_list_hash.compute
      in
      Block_payload.hash
        ~predecessor:shell_header.predecessor
        payload_round
        operation_list_hash
    in
    return (shell_header, operations, payload_hash)
  in
  let apply_via_node ~ordered_pool ~payload_hash =
    let operations = Operation_pool.ordered_to_list_list ordered_pool in
    let faked_protocol_data =
      forge_faked_protocol_data
        ~seed_nonce_hash
        ~liquidity_baking_escape_vote
        ~payload_hash
        ~payload_round
        ()
    in
    Node_rpc.preapply_block
      cctxt
      ~chain
      ~head:predecessor_block.hash
      ~timestamp
      ~protocol_data:faked_protocol_data
      operations
    >>=? fun (shell_header, _preapply_result) ->
    let operations = List.map (List.map convert_operation) operations in
    return (shell_header, operations, payload_hash)
  in
  let apply_with_context ~context_index ~ordered_pool ~payload_hash =
    let faked_protocol_data =
      forge_faked_protocol_data
        ~seed_nonce_hash
        ~liquidity_baking_escape_vote
        ~payload_hash
        ~payload_round
        ()
    in
    Shell_services.Chain.chain_id cctxt ~chain () >>=? fun chain_id ->
    Baking_simulator.begin_construction
      ~timestamp
      ~protocol_data:faked_protocol_data
      context_index
      predecessor_block
      chain_id
    >>=? fun incremental ->
    let operations = Operation_pool.ordered_to_list_list ordered_pool in
    (* We must make sure that the given consensus operations are
       pre-checked/pre-filtered before calling this function,
       otherwise, these will fail *)
    List.fold_left_es
      (fun inc op ->
        Baking_simulator.add_operation inc op >>=? fun (inc, _) -> return inc)
      incremental
      (List.flatten operations)
    >>=? fun incremental ->
    let operations_hash =
      Operation_list_list_hash.compute
        (List.map
           (fun sl ->
             Operation_list_hash.compute (List.map Operation.hash_packed sl))
           operations)
    in
    (* We need to compute the final [operations_hash] before
       finalizing the block because it will be used in the cache's nonce. *)
    let incremental =
      {incremental with header = {incremental.header with operations_hash}}
    in
    Baking_simulator.finalize_construction incremental
    >>=? fun (validation_result, _) ->
    protect
      ~on_error:(fun _ -> return_none)
      (fun () ->
        Shell_services.Blocks.metadata_hash
          cctxt
          ~block:(`Hash (predecessor_block.hash, 0))
          ~chain
          ()
        >>=? fun pred_block_metadata_hash ->
        return (Some pred_block_metadata_hash))
    >>=? fun pred_block_metadata_hash ->
    protect
      ~on_error:(fun _ -> return_none)
      (fun () ->
        Shell_services.Blocks.Operation_metadata_hashes.root
          cctxt
          ~block:(`Hash (predecessor_block.hash, 0))
          ~chain
          ()
        >>=? fun pred_op_metadata_hash -> return (Some pred_op_metadata_hash))
    >>=? fun pred_op_metadata_hash ->
    finalize_block_header
      incremental.header
      timestamp
      validation_result
      operations_hash
      pred_block_metadata_hash
      pred_op_metadata_hash
    >>=? fun shell_header ->
    let operations = List.map (List.map convert_operation) operations in
    return (shell_header, operations, payload_hash)
  in
  let simulation_kind =
    match simulation_kind with
    | Filter operation_pool ->
        (* We cannot include operations that are not live with respect
           to our predecessor otherwise the node would reject the block. *)
        let filtered_pool =
          retain_live_operations_only
            ~live_blocks:pred_info.live_blocks
            operation_pool
        in
        Filter filtered_pool
    | Apply _ as x -> x
  in
  (match (simulation_mode, simulation_kind) with
  | (Baking_state.Node, Filter operation_pool) ->
      filter_via_node ~operation_pool
  | (Node, Apply {ordered_pool; payload_hash}) ->
      apply_via_node ~ordered_pool ~payload_hash
  | (Local context_index, Filter operation_pool) ->
      filter_with_context ~context_index ~operation_pool
  | (Local context_index, Apply {ordered_pool; payload_hash}) ->
      apply_with_context ~context_index ~ordered_pool ~payload_hash)
  >>=? fun (shell_header, operations, payload_hash) ->
  Baking_pow.mine
    ~proof_of_work_threshold:constants.proof_of_work_threshold
    shell_header
    (fun proof_of_work_nonce ->
      {
        Block_header.payload_hash;
        payload_round;
        seed_nonce_hash;
        proof_of_work_nonce;
        liquidity_baking_escape_vote;
      })
  >>=? fun contents ->
  let unsigned_block_header =
    {
      Block_header.shell = shell_header;
      protocol_data = {contents; signature = Signature.zero};
    }
  in
  return {unsigned_block_header; operations}
