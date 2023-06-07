(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
open Operation_pool
module Events = Baking_events.Selection

let quota = Main.validation_passes

let consensus_quota = Stdlib.List.nth quota Operation_repr.consensus_pass

let votes_quota = Stdlib.List.nth quota Operation_repr.voting_pass

let anonymous_quota = Stdlib.List.nth quota Operation_repr.anonymous_pass

let managers_quota = Stdlib.List.nth quota Operation_repr.manager_pass

type prioritized_manager = {
  op : Prioritized_operation.t;
  size : int;
  fee : Tez.t;
  gas : Fixed_point_repr.integral_tag Gas.Arith.t;
  weight : Q.t;
  source : public_key_hash;
  counter : Manager_counter.t;
}

module PrioritizedManagerSet = Set.Make (struct
  type t = prioritized_manager

  (* We order the operations by their weights except if they belong
     to the same manager, if they do, we order them by their
     counter. *)
  let compare {source; counter; weight; op; _}
      {source = source'; counter = counter'; weight = weight'; op = op'; _} =
    (* Be careful with the [compare] *)
    let cmp_src = Signature.Public_key_hash.compare source source' in
    if cmp_src = 0 then
      (* we want the smallest counter first *)
      let c = Manager_counter.compare counter counter' in
      if c <> 0 then c
      else
        let c = Prioritized_operation.compare_priority op' op in
        if c <> 0 then c else Q.compare weight' weight
      (* if same counter, biggest weight first *)
    else
      let c = Prioritized_operation.compare_priority op' op in
      if c <> 0 then c
      else
        (* We want the biggest weight first *)
        let c = Q.compare weight' weight in
        if c <> 0 then c else cmp_src
end)

(* Note: This weight is also used by the plugin and the prevalidator to sort
   operations in the pending mempool.
   See {!Tezos_protocol_plugin_alpha.Plugin.Mempool.weight_manager_operation}. *)
let prioritize_manager ~max_size ~hard_gas_limit_per_block ~minimal_fees
    ~minimal_nanotez_per_gas_unit ~minimal_nanotez_per_byte operation =
  let op = Operation_pool.Prioritized_operation.packed operation in
  let {protocol_data = Operation_data {contents; _}; _} = op in
  let open Operation in
  let l = to_list (Contents_list contents) in
  List.fold_left_e
    (fun ((first_source, first_counter, total_fee, total_gas) as acc) ->
       function
      | Contents (Manager_operation {source; counter; fee; gas_limit; _}) ->
          (Environment.wrap_tzresult @@ Tez.(total_fee +? fee))
          >>? fun total_fee ->
          (* There is only one unique source per packed transaction *)
          let first_source = Option.value ~default:source first_source in
          (* We only care about the first counter *)
          let first_counter = Option.value ~default:counter first_counter in
          ok
            ( Some first_source,
              Some first_counter,
              total_fee,
              Gas.Arith.add total_gas gas_limit )
      | _ -> ok acc)
    (None, None, Tez.zero, Gas.Arith.zero)
    l
  |> function
  | Ok (Some source, Some counter, fee, gas) ->
      if Tez.(fee < minimal_fees) then None
      else
        let size = Data_encoding.Binary.length Operation.encoding op in
        let size_f = Q.of_int size in
        let gas_f = Q.of_bigint (Gas.Arith.integral_to_z gas) in
        let fee_f = Q.of_int64 (Tez.to_mutez fee) in
        let size_ratio = Q.(size_f / Q.of_int max_size) in
        let gas_ratio =
          Q.(
            gas_f
            / Q.of_bigint (Gas.Arith.integral_to_z hard_gas_limit_per_block))
        in
        let weight = Q.(fee_f / max size_ratio gas_ratio) in
        let fees_in_nanotez =
          Q.mul (Q.of_int64 (Tez.to_mutez fee)) (Q.of_int 1000)
        in
        let enough_fees_for_gas =
          let minimal_fees_in_nanotez =
            Q.mul
              minimal_nanotez_per_gas_unit
              (Q.of_bigint @@ Gas.Arith.integral_to_z gas)
          in
          Q.compare minimal_fees_in_nanotez fees_in_nanotez <= 0
        in
        let enough_fees_for_size =
          let minimal_fees_in_nanotez =
            Q.mul minimal_nanotez_per_byte (Q.of_int size)
          in
          Q.compare minimal_fees_in_nanotez fees_in_nanotez <= 0
        in
        if enough_fees_for_size && enough_fees_for_gas then
          Some {op = operation; size; weight; fee; gas; source; counter}
        else None
  | _ -> None

let prioritize_managers ~hard_gas_limit_per_block ~minimal_fees
    ~minimal_nanotez_per_gas_unit ~minimal_nanotez_per_byte managers =
  Prioritized_operation_set.fold
    (fun op acc ->
      match
        prioritize_manager
          ~max_size:managers_quota.max_size
          ~hard_gas_limit_per_block
          ~minimal_fees
          ~minimal_nanotez_per_gas_unit
          ~minimal_nanotez_per_byte
          op
      with
      | None -> acc
      | Some w_op -> PrioritizedManagerSet.add w_op acc)
    managers
    PrioritizedManagerSet.empty

(** Simulation *)

type simulation_result = {
  validation_result : Tezos_protocol_environment.validation_result option;
  block_header_metadata : block_header_metadata option;
  operations : packed_operation list list;
  operations_hash : Operation_list_list_hash.t;
}

let validate_operation inc op =
  Baking_simulator.add_operation inc op >>= function
  | Error errs ->
      Events.(emit invalid_operation_filtered) (Operation.hash_packed op, errs)
      >>= fun () -> Lwt.return_none
  | Ok (resulting_state, None) ->
      (* No receipt if force_apply is not set *)
      Lwt.return_some resulting_state
  | Ok (resulting_state, Some receipt) -> (
      (* Check that the metadata are serializable/deserializable *)
      let encoding_result =
        let enc = Protocol.operation_receipt_encoding in
        Option.bind
          (Data_encoding.Binary.to_bytes_opt enc receipt)
          (Data_encoding.Binary.of_bytes_opt enc)
      in
      match encoding_result with
      | None ->
          Events.(emit cannot_serialize_operation_metadata)
            (Operation.hash_packed op)
          >>= fun () -> Lwt.return_none
      | Some _b -> Lwt.return_some resulting_state)

let filter_valid_operations_up_to_quota inc (ops, quota) =
  let {Tezos_protocol_environment.max_size; max_op} = quota in
  let exception Full of (Baking_simulator.incremental * packed_operation list)
  in
  try
    List.fold_left_s
      (fun (inc, curr_size, nb_ops, acc) op ->
        let op_size =
          Data_encoding.Binary.length Alpha_context.Operation.encoding op
        in
        let new_size = curr_size + op_size in
        if new_size > max_size then Lwt.return (inc, curr_size, nb_ops, acc)
        else (
          Option.iter
            (fun max_op -> if max_op = nb_ops + 1 then raise (Full (inc, acc)))
            max_op ;
          validate_operation inc op >>= function
          | None -> Lwt.return (inc, curr_size, nb_ops, acc)
          | Some inc' -> Lwt.return (inc', new_size, nb_ops + 1, op :: acc)))
      (inc, 0, 0, [])
      ops
    >>= fun (inc, _, _, l) -> Lwt.return (inc, List.rev l)
  with Full (inc, l) -> Lwt.return (inc, List.rev l)

let filter_operations_with_simulation initial_inc fees_config
    ~hard_gas_limit_per_block {consensus; votes; anonymous; managers} =
  let {
    Baking_configuration.minimal_fees;
    minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte;
  } =
    fees_config
  in
  filter_valid_operations_up_to_quota
    initial_inc
    (Prioritized_operation_set.operations consensus, consensus_quota)
  >>= fun (inc, consensus) ->
  filter_valid_operations_up_to_quota
    inc
    (Prioritized_operation_set.operations votes, votes_quota)
  >>= fun (inc, votes) ->
  filter_valid_operations_up_to_quota
    inc
    (Prioritized_operation_set.operations anonymous, anonymous_quota)
  >>= fun (inc, anonymous) ->
  (* Sort the managers *)
  let prioritized_managers =
    prioritize_managers
      ~hard_gas_limit_per_block
      ~minimal_fees
      ~minimal_nanotez_per_gas_unit
      ~minimal_nanotez_per_byte
      managers
  in
  filter_valid_operations_up_to_quota
    inc
    ( PrioritizedManagerSet.elements prioritized_managers
      |> List.map (fun {op; _} -> Prioritized_operation.packed op),
      managers_quota )
  >>= fun (inc, managers) ->
  let operations = [consensus; votes; anonymous; managers] in
  let operations_hash =
    Operation_list_list_hash.compute
      (List.map
         (fun sl ->
           Operation_list_hash.compute (List.map Operation.hash_packed sl))
         operations)
  in
  let inc = {inc with header = {inc.header with operations_hash}} in
  Baking_simulator.finalize_construction inc >>=? function
  | Some (validation_result, block_header_metadata) ->
      return
        {
          validation_result = Some validation_result;
          block_header_metadata = Some block_header_metadata;
          operations;
          operations_hash;
        }
  | None ->
      return
        {
          validation_result = None;
          block_header_metadata = None;
          operations;
          operations_hash;
        }

let filter_valid_operations_up_to_quota_without_simulation (ops, quota) =
  let {Tezos_protocol_environment.max_size; max_op} = quota in
  let exception Full of packed_operation list in
  try
    List.fold_left
      (fun (curr_size, nb_ops, acc) op ->
        let op_size =
          Data_encoding.Binary.length Alpha_context.Operation.encoding op
        in
        let new_size = curr_size + op_size in
        if new_size > max_size then (curr_size, nb_ops, acc)
        else (
          Option.iter
            (fun max_op -> if max_op = nb_ops + 1 then raise (Full acc))
            max_op ;
          (new_size, nb_ops + 1, op :: acc)))
      (0, 0, [])
      ops
    |> fun (_, _, l) -> List.rev l
  with Full l -> List.rev l

let filter_operations_without_simulation fees_config ~hard_gas_limit_per_block
    {consensus; votes; anonymous; managers} =
  let consensus =
    filter_valid_operations_up_to_quota_without_simulation
      (Prioritized_operation_set.operations consensus, consensus_quota)
  in
  let votes =
    filter_valid_operations_up_to_quota_without_simulation
      (Prioritized_operation_set.operations votes, votes_quota)
  in
  let anonymous =
    filter_valid_operations_up_to_quota_without_simulation
      (Prioritized_operation_set.operations anonymous, anonymous_quota)
  in
  let {
    Baking_configuration.minimal_fees;
    minimal_nanotez_per_gas_unit;
    minimal_nanotez_per_byte;
  } =
    fees_config
  in
  (* Sort the managers *)
  let prioritized_managers =
    prioritize_managers
      ~hard_gas_limit_per_block
      ~minimal_fees
      ~minimal_nanotez_per_gas_unit
      ~minimal_nanotez_per_byte
      managers
  in
  let managers =
    filter_valid_operations_up_to_quota_without_simulation
      ( PrioritizedManagerSet.elements prioritized_managers
        |> List.map (fun {op; _} -> Prioritized_operation.packed op),
        managers_quota )
  in
  let operations = [consensus; votes; anonymous; managers] in
  operations

let filter_consensus_operations_only inc
    ({consensus; votes; anonymous; managers} as ordered_pool) =
  filter_valid_operations_up_to_quota inc (consensus, consensus_quota)
  >>= fun (incremental, filtered_consensus) ->
  let payload = Operation_pool.payload_of_ordered_pool ordered_pool in
  List.fold_left_es
    (fun inc op ->
      Baking_simulator.add_operation inc op >>=? fun (inc, _) -> return inc)
    incremental
    (List.flatten [votes; anonymous; managers])
  >>=? fun incremental ->
  let filtered_pool =
    Operation_pool.ordered_pool_of_payload
      ~consensus_operations:filtered_consensus
      payload
  in
  return (incremental, filtered_pool)
