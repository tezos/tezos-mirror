(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Chain_services

let get_chain_id store =
  let open Lwt_syntax in
  let main_chain_store = Store.main_chain_store store in
  function
  | `Main -> Lwt.return (Store.Chain.chain_id main_chain_store)
  | `Test ->
      let* testchain = Store.Chain.testchain main_chain_store in
      let testchain = WithExceptions.Option.to_exn ~none:Not_found testchain in
      let testchain_store = Store.Chain.testchain_store testchain in
      Lwt.return (Store.Chain.chain_id testchain_store)
  | `Hash chain_id -> Lwt.return chain_id

let get_chain_id_opt store chain =
  Option.catch_s (fun () -> get_chain_id store chain)

let get_chain_store_exn store chain =
  let open Lwt_syntax in
  let* chain_id = get_chain_id store chain in
  let* chain_store = Store.get_chain_store_opt store chain_id in
  let chain_store = WithExceptions.Option.to_exn ~none:Not_found chain_store in
  Lwt.return chain_store

let predecessors chain_store ignored length head =
  let open Lwt_result_syntax in
  let rec loop acc length block =
    if length <= 0 then return (List.rev acc)
    else
      let* o = Store.Block.read_ancestor_hash chain_store ~distance:1 block in
      match o with
      | None -> return (List.rev acc)
      | Some pred ->
          if Block_hash.Set.mem block ignored then return (List.rev acc)
          else loop (pred :: acc) (length - 1) pred
  in
  let head_hash = Store.Block.hash head in
  loop [head_hash] (length - 1) head_hash

let list_blocks chain_store ?(length = 1) ?min_date blocks =
  let open Lwt_result_syntax in
  let*! requested_blocks =
    match blocks with
    | [] ->
        let*! head = Store.Chain.current_head chain_store in
        Lwt.return [head]
    | blocks ->
        let*! blocks =
          List.filter_map_p (Store.Block.read_block_opt chain_store) blocks
        in
        let blocks =
          match min_date with
          | None -> blocks
          | Some min_date ->
              List.filter
                (fun block ->
                  let timestamp = Store.Block.timestamp block in
                  Time.Protocol.(min_date <= timestamp))
                blocks
        in
        let sorted_blocks =
          List.sort
            (fun b1 b2 ->
              let f1 = Store.Block.fitness b1 in
              let f2 = Store.Block.fitness b2 in
              ~-(Fitness.compare f1 f2))
            blocks
        in
        Lwt.return sorted_blocks
  in
  let* _, blocks =
    List.fold_left_es
      (fun (ignored, acc) block ->
        let* predecessors = predecessors chain_store ignored length block in
        let ignored =
          List.fold_left
            (fun acc v -> Block_hash.Set.add v acc)
            ignored
            predecessors
        in
        return (ignored, predecessors :: acc))
      (Block_hash.Set.empty, [])
      requested_blocks
  in
  return (List.rev blocks)

module Delegators_contribution_implem = struct
  open Chain_services.Delegators_contribution_errors

  let context_and_plugin chain_store block_id =
    let open Lwt_result_syntax in
    let* block =
      let*! block_opt =
        Store.Chain.block_of_identifier_opt chain_store block_id
      in
      match block_opt with
      | Some v -> return v
      | None -> tzfail Cycle_too_far_in_past
    in
    let* (context : Context.t) =
      let*! context_opt = Store.Block.context_opt chain_store block in
      match context_opt with
      | Some v -> return v
      | None -> tzfail Cycle_too_far_in_past
    in
    let context_for_plugin = (Store.Block.shell_header block, context) in
    let*! protocol_hash = Store.Block.protocol_hash_exn chain_store block in
    let* delegators_contribution_plugin =
      match Protocol_plugin.find_delegators_contribution protocol_hash with
      | Some x -> return x
      | None -> tzfail (Protocol_not_supported {protocol_hash})
    in
    return (context_for_plugin, delegators_contribution_plugin)

  let rec delegated_breakdown_at_sampling chain_store ~cycle ~delegate_pkh block
      =
    let open Lwt_result_syntax in
    let* context, (module Delegators_contribution_plugin) =
      context_and_plugin chain_store block
    in
    let* output =
      Delegators_contribution_plugin.delegated_breakdown_at_sampling
        context
        ~cycle
        ~delegate_pkh
    in
    match output with
    | `Ok v -> return v
    | `Retry_at_level level ->
        delegated_breakdown_at_sampling
          chain_store
          ~cycle
          ~delegate_pkh
          (`Level level)
    | `Cycle_too_far_in_future -> tzfail Cycle_too_far_in_future

  let delegators_contribution chain_store cycle delegate_pkh () () =
    let open Lwt_result_syntax in
    let* {
           min_delegated_amount;
           min_delegated_level;
           overstaked;
           total_delegated_including_overdelegated;
           total_delegated_after_limits;
           overdelegated;
         } =
      delegated_breakdown_at_sampling chain_store ~cycle ~delegate_pkh (`Head 0)
    in
    let* context, (module Delegators_contribution_plugin) =
      context_and_plugin chain_store (`Level min_delegated_level)
    in
    let* {
           total_delegated;
           own_delegated;
           delegators_contributions;
           former_delegators_unstake_requests;
         } =
      Delegators_contribution_plugin.min_delegated_breakdown
        context
        ~delegate_pkh
    in
    assert (Int64.equal min_delegated_amount total_delegated) ;
    return
      {
        own_delegated;
        delegators_contributions;
        former_delegators_unstake_requests;
        overstaked;
        total_delegated_including_overdelegated;
        total_delegated_after_limits;
        overdelegated;
      }
end

let register0 dir s f =
  dir :=
    Tezos_rpc.Directory.register
      !dir
      (Tezos_rpc.Service.subst0 s)
      (fun chain p q -> f chain p q)

let register2 dir s f =
  dir :=
    Tezos_rpc.Directory.register
      !dir
      (Tezos_rpc.Service.subst2 s)
      (fun ((chain, x1), x2) p q -> f chain x1 x2 p q)

(* This RPC directory must be instantiated by the node itself. Indeed,
   only the node has access to some particular resources, such as the
   validator or some store internal values computed at runtime, that
   are necessary for some RPCs. *)
let rpc_directory_with_validator dir validator =
  let open Lwt_result_syntax in
  let register1 s f =
    dir :=
      Tezos_rpc.Directory.register
        !dir
        (Tezos_rpc.Service.subst1 s)
        (fun (chain, a) p q -> f chain a p q)
  in
  register0 dir S.is_bootstrapped (fun chain_store () () ->
      match Validator.get validator (Store.Chain.chain_id chain_store) with
      | Error _ -> Lwt.fail Not_found
      | Ok chain_validator ->
          return
            Chain_validator.
              (is_bootstrapped chain_validator, sync_status chain_validator)) ;
  register0 dir S.force_bootstrapped (fun chain_store () b ->
      match Validator.get validator (Store.Chain.chain_id chain_store) with
      | Error _ -> Lwt.fail Not_found
      | Ok chain_validator ->
          let*! v = Chain_validator.force_bootstrapped chain_validator b in
          return v) ;
  register0 dir S.active_peers_heads (fun chain_store () () ->
      match Validator.get validator (Store.Chain.chain_id chain_store) with
      | Error _ -> Lwt.fail Not_found
      | Ok chain_validator ->
          let*! ap = Chain_validator.active_peers_heads chain_validator in
          return ap) ;
  register2
    dir
    S.delegators_contribution
    Delegators_contribution_implem.delegators_contribution ;
  (* invalid_blocks *)
  register0 dir S.Invalid_blocks.list (fun chain_store () () ->
      let convert (hash, {Store_types.level; errors}) = {hash; level; errors} in
      let*! invalid_blocks_map = Store.Block.read_invalid_blocks chain_store in
      let blocks = Block_hash.Map.bindings invalid_blocks_map in
      return (List.map convert blocks)) ;
  register1 S.Invalid_blocks.get (fun chain_store hash () () ->
      let*! o = Store.Block.read_invalid_block_opt chain_store hash in
      match o with
      | None -> Lwt.fail Not_found
      | Some {level; errors} -> return {hash; level; errors}) ;
  register1 S.Invalid_blocks.delete (fun chain_store hash () () ->
      Store.Block.unmark_invalid chain_store hash) ;
  (* protocols *)
  register0 dir S.Protocols.list (fun chain_store () () ->
      let*! protocols = Store.Chain.protocol_levels chain_store in
      Store_types.Protocol_levels.fold
        (fun proto_level
             Store_types.Protocol_levels.{protocol; activation_block; _}
             acc
           -> {protocol; proto_level; activation_block} :: acc)
        protocols
        []
      |> List.sort (fun p1 p2 ->
             Compare.Int.compare p1.proto_level p2.proto_level)
      |> return) ;
  register1 S.Protocols.get (fun chain_store hash () () ->
      let*! protocols = Store.Chain.protocol_levels chain_store in
      let exception Found of protocol_info in
      try
        Store_types.Protocol_levels.iter
          (fun proto_level
               Store_types.Protocol_levels.{protocol; activation_block; _}
             ->
            if Protocol_hash.equal protocol hash then
              raise (Found {protocol; proto_level; activation_block}))
          protocols ;
        raise Not_found
      with Found info -> return info)

(* This RPC directory is agnostic to the node internal
   resources. However, theses RPCs can access a data subset by reading
   the store static values. *)
let rpc_directory_without_validator dir =
  let open Lwt_result_syntax in
  register0 dir S.chain_id (fun chain_store () () ->
      return (Store.Chain.chain_id chain_store)) ;
  register0 dir S.Levels.checkpoint (fun chain_store () () ->
      let*! v = Store.Chain.checkpoint chain_store in
      return v) ;
  register0 dir S.Levels.savepoint (fun chain_store () () ->
      let*! v = Store.Chain.savepoint chain_store in
      return v) ;
  register0 dir S.Levels.caboose (fun chain_store () () ->
      let*! v = Store.Chain.caboose chain_store in
      return v) ;
  (* blocks *)
  register0 dir S.Blocks.list (fun chain q () ->
      list_blocks chain ?length:q#length ?min_date:q#min_date q#heads)

let rpc_directory validator =
  let dir : Store.chain_store Tezos_rpc.Directory.t ref =
    ref Tezos_rpc.Directory.empty
  in
  rpc_directory_without_validator dir ;
  rpc_directory_with_validator dir validator ;
  let register_dynamic_directory2 ?descr s f =
    dir :=
      Tezos_rpc.Directory.register_dynamic_directory
        !dir
        ?descr
        (Tezos_rpc.Path.subst1 s)
        (fun (chain, a) -> f chain a)
  in
  (* blocks *)
  register_dynamic_directory2
    Block_services.path
    Block_directory.build_rpc_directory_with_validator ;
  !dir

(* This RPC directory instantiates only a subset of the chain RPCs as
   it is agnostic to the node internal resources. *)
let rpc_directory_without_validator () =
  let dir : Store.chain_store Tezos_rpc.Directory.t ref =
    ref Tezos_rpc.Directory.empty
  in
  rpc_directory_without_validator dir ;
  let register_dynamic_directory2 ?descr s f =
    dir :=
      Tezos_rpc.Directory.register_dynamic_directory
        !dir
        ?descr
        (Tezos_rpc.Path.subst1 s)
        (fun (chain, a) -> f chain a)
  in
  (* blocks *)
  register_dynamic_directory2
    Block_services.path
    Block_directory.build_rpc_directory_without_validator ;
  !dir

let build_rpc_directory validator =
  let distributed_db = Validator.distributed_db validator in
  let store = Distributed_db.store distributed_db in
  let dir = ref (rpc_directory validator) in
  (* Mempool *)
  let merge d = dir := Tezos_rpc.Directory.merge !dir d in
  merge
    (Tezos_rpc.Directory.map
       (fun chain_store ->
         match Validator.get validator (Store.Chain.chain_id chain_store) with
         | Error _ -> Lwt.fail Not_found
         | Ok chain_validator ->
             Lwt.return (Chain_validator.prevalidator chain_validator))
       Prevalidator.rpc_directory) ;
  Tezos_rpc.Directory.prefix Chain_services.path
  @@ Tezos_rpc.Directory.map
       (fun ((), chain) -> get_chain_store_exn store chain)
       !dir
