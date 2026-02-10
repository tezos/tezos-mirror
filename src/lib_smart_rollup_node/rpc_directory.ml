(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

open Rpc_directory_helpers

(* Add extra services which must live in the rollup node library *)
module Rollup_node_services = struct
  include Rollup_node_services

  module Root = struct
    include Root

    let config =
      Tezos_rpc.Service.get_service
        ~description:"Returns the rollup node configuration"
        ~query:Tezos_rpc.Query.empty
        ~output:Configuration.encoding_no_default
        Tezos_rpc.Path.(root / "config")
  end
end

let get_head_hash_opt node_ctxt =
  let open Lwt_result_syntax in
  let+ res = Node_context.last_processed_head_opt node_ctxt in
  Option.map
    (fun Sc_rollup_block.{header = {block_hash; _}; _} -> block_hash)
    res

let get_head_level_opt node_ctxt =
  let open Lwt_result_syntax in
  let+ res = Node_context.last_processed_head_opt node_ctxt in
  Option.map (fun Sc_rollup_block.{header = {level; _}; _} -> level) res

module Root_directory = Make_directory (struct
  include Rollup_node_services.Root

  type context = Node_context.rw

  type subcontext = Node_context.ro

  let context_of_prefix node_ctxt () =
    Lwt_result.return (Node_context.readonly node_ctxt)
end)

module Global_directory = Make_directory (struct
  include Rollup_node_services.Global

  type context = Node_context.rw

  type subcontext = Node_context.ro

  let context_of_prefix node_ctxt () =
    Lwt_result.return (Node_context.readonly node_ctxt)
end)

module Make_block_directory (X : sig end) = Make_sub_directory (struct
  include Rollup_node_services.Block

  type context = Node_context.rw

  type subcontext = Node_context.ro * Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (Node_context.readonly node_ctxt, block)
end)

(* The block directory needs to be registered in the protocol plugin. *)
module Block_directory = Make_block_directory (struct end)

module Make_block_helpers_directory (X : sig end) = Make_sub_directory (struct
  include Rollup_node_services.Block.Helpers

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type context = Node_context.rw

  (* The context needs to be accessed with write permissions because we need to
     commit on disk to generate the proofs. *)
  type subcontext = Node_context.rw * Block_hash.t

  let context_of_prefix node_ctxt (((), block) : prefix) =
    let open Lwt_result_syntax in
    let+ block = Block_directory_helpers.block_of_prefix node_ctxt block in
    (node_ctxt, block)
end)

(* The block helpers directory needs to be registered in the protocol plugin. *)
module Block_helpers_directory = Make_block_helpers_directory (struct end)

module Local_directory = Make_directory (struct
  include Rollup_node_services.Local

  type context = Node_context.rw

  type subcontext = Node_context.ro

  let context_of_prefix node_ctxt () =
    Lwt_result.return (Node_context.readonly node_ctxt)
end)

module Admin_directory = Make_directory (struct
  include Rollup_node_services.Admin

  type context = Node_context.rw

  type subcontext = Node_context.rw

  let context_of_prefix node_ctxt () = Lwt_result.return node_ctxt
end)

let () =
  Root_directory.register0 Rollup_node_services.Root.ping
  @@ fun _node_ctxt () () -> Lwt_result_syntax.return_unit

let () =
  Root_directory.register0 Rollup_node_services.Root.health
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let degraded = Reference.get node_ctxt.degraded in
  let l1_connection = Layer1.get_status node_ctxt.Node_context.l1_ctxt in
  let* l2_head = Node_context.last_processed_head_opt node_ctxt in
  let l1_head = Layer1.get_latest_head node_ctxt.l1_ctxt in
  let l1_blocks_late =
    match (l1_head, l2_head) with
    | None, _ | _, None -> 0l
    | Some l1, Some l2 -> Int32.sub l1.header.level l2.header.level
  in
  let publisher_worker =
    match Publisher.worker_status () with
    | (`Running | `Crashed _) as st -> [("publisher", st)]
    | `Not_running -> []
  in
  let batcher_worker =
    match Batcher.worker_status () with
    | (`Running | `Crashed _) as st -> [("batcher", st)]
    | `Not_running -> []
  in
  let refutation_coordinator_worker =
    match Refutation_coordinator.worker_status () with
    | (`Running | `Crashed _) as st -> [("refutation_coordinator", st)]
    | `Not_running -> []
  in
  let refutation_player_workers =
    Refutation_player.current_games ()
    |> List.map (fun opponent ->
           ( Format.asprintf
               "refutation_player (against %a)"
               Signature.Public_key_hash.pp
               opponent,
             `Running ))
  in
  let injector_workers =
    Injector.running_worker_tags ()
    |> List.map (fun tags ->
           ( Format.sprintf
               "injector (%s)"
               (String.concat ", " (List.map Operation_kind.to_string tags)),
             `Running ))
  in
  let active_workers =
    publisher_worker @ batcher_worker @ refutation_coordinator_worker
    @ refutation_player_workers @ injector_workers
  in
  let l1_likely_late =
    match l1_head with
    | None -> true
    | Some {header = {timestamp; _}; _} ->
        Time.Protocol.diff Time.System.(to_protocol @@ now ()) timestamp > 60L
  in
  let l1_lateness_threshold =
    if node_ctxt.config.l1_monitor_finalized then 3l else 1l
  in
  let healthy =
    (not degraded) && l1_connection = `Connected
    && l1_blocks_late <= l1_lateness_threshold
    && (not l1_likely_late)
    && List.for_all (fun (_name, st) -> st = `Running) active_workers
  in
  return
    Rollup_node_services.
      {
        healthy;
        degraded;
        l1 =
          {
            connection = l1_connection;
            blocks_late = l1_blocks_late;
            last_seen_head =
              Option.map
                (fun Layer1.{hash; level; header = {timestamp; _}} ->
                  (hash, level, timestamp))
                l1_head;
          };
        active_workers;
      }

let () =
  Root_directory.register0 Rollup_node_services.Root.version
  @@ fun _node_ctxt () () ->
  let open Lwt_result_syntax in
  let version =
    Tezos_version.Version.to_string
      Tezos_version_value.Current_git_info.octez_smart_rollup_node_version
  in
  let store_version = Format.asprintf "%a" Store_version.pp Store.version in
  let context_version = Context.Version.(to_string version) in
  return Rollup_node_services.{version; store_version; context_version}

let () =
  Root_directory.register0 Rollup_node_services.Root.config
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let+ history_mode = Node_context.get_history_mode node_ctxt in
  {node_ctxt.config with history_mode = Some history_mode}

let () =
  Root_directory.register0 Rollup_node_services.Root.ocaml_gc
  @@ fun _node_ctxt () () -> Lwt_result_syntax.return @@ Gc.stat ()

let () =
  Root_directory.register0 Rollup_node_services.Root.memory
  @@ fun _node_ctxt () () ->
  let open Lwt_result_syntax in
  Sys_info.memory_stats () |> lwt_map_error TzTrace.make

let () =
  Global_directory.register0 Rollup_node_services.Global.sc_rollup_address
  @@ fun node_ctxt () () -> Lwt_result.return node_ctxt.config.sc_rollup_address

let () =
  Global_directory.register0 Rollup_node_services.Global.current_tezos_head
  @@ fun node_ctxt () () -> get_head_hash_opt node_ctxt

let () =
  Global_directory.register0 Rollup_node_services.Global.current_tezos_level
  @@ fun node_ctxt () () -> get_head_level_opt node_ctxt

let () =
  Global_directory.register0 Rollup_node_services.Global.last_stored_commitment
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  match head with
  | None -> return_none
  | Some head ->
      let commitment_hash =
        Sc_rollup_block.most_recent_commitment head.header
      in
      let+ commitment =
        Node_context.find_commitment node_ctxt commitment_hash
      in
      Option.map (fun c -> (c, commitment_hash)) commitment

(* Sets up a block watching service. It creates a stream to
   observe block events and asynchronously fetches the next
   block when available *)
let create_block_watcher_service first_block watcher =
  let open Lwt_syntax in
  (* input source block creating a stream to observe the events *)
  let block_stream, stopper = Lwt_watcher.create_stream watcher in
  let shutdown () = Lwt_watcher.shutdown stopper in
  (* generate the next asynchronous event *)
  let next =
    let first_call = ref true in
    fun () ->
      if !first_call then (
        first_call := false ;
        match Result.to_option first_block |> Option.join with
        | Some block -> return_some block
        | None -> Lwt_stream.get block_stream)
      else Lwt_stream.get block_stream
  in
  Tezos_rpc.Answer.return_stream {next; shutdown}

let () =
  Global_directory.gen_register0
    Rollup_node_services.Global.global_block_watcher
  @@ fun node_ctxt () () ->
  let open Lwt_syntax in
  let* head = Node_context.last_processed_head_opt node_ctxt in
  create_block_watcher_service head node_ctxt.global_block_watcher

let () =
  Global_directory.gen_register0
    Rollup_node_services.Global.finalized_block_watcher
  @@ fun node_ctxt () () ->
  let open Lwt_syntax in
  let* finalized = Node_context.get_finalized_head_opt node_ctxt in
  create_block_watcher_service finalized node_ctxt.finalized_block_watcher

let () =
  Block_directory.register0 Rollup_node_services.Block.block
  @@ fun (node_ctxt, block) outbox () ->
  let open Lwt_result_syntax in
  let* get_outbox_messages =
    if not outbox then return_none
    else
      let+ (module Plugin) =
        Protocol_plugins.proto_plugin_for_block node_ctxt block
      in
      Some Plugin.Pvm.get_outbox_messages
  in
  Node_context.get_full_l2_block ?get_outbox_messages node_ctxt block

let () =
  Block_directory.register0 Rollup_node_services.Block.num_messages
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* l2_block = Node_context.get_l2_block node_ctxt block in
  let+ num_messages =
    Node_context.get_num_messages node_ctxt l2_block.header.inbox_witness
  in
  Z.of_int num_messages

let () =
  let open Lwt_result_syntax in
  Block_directory.register0 Rollup_node_services.Block.hash
  @@ fun (_node_ctxt, block) () () -> return block

let () =
  Block_directory.register0 Rollup_node_services.Block.level
  @@ fun (node_ctxt, block) () () -> Node_context.level_of_hash node_ctxt block

let () =
  Block_directory.register0 Rollup_node_services.Block.inbox
  @@ fun (node_ctxt, block) () () ->
  Node_context.get_inbox_by_block_hash node_ctxt block

let () =
  Block_directory.register0 Rollup_node_services.Block.ticks
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let+ l2_block = Node_context.get_l2_block node_ctxt block in
  Z.of_int64 l2_block.num_ticks

let get_state (node_ctxt : _ Node_context.t) block_hash =
  let open Lwt_result_syntax in
  let* ctxt = Node_context.checkout_context node_ctxt block_hash in
  let*! state = Context.PVMState.find ctxt in
  match state with None -> failwith "No state" | Some state -> return state

let () =
  Block_directory.register0 Rollup_node_services.Block.total_ticks
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_block node_ctxt block
  in
  let*! tick = Plugin.Pvm.get_tick node_ctxt.kind state in
  return tick

let () =
  Block_directory.register0 Rollup_node_services.Block.state_hash
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_block node_ctxt block
  in
  let*! hash = Plugin.Pvm.state_hash node_ctxt.kind state in
  return hash

let () =
  Block_directory.register0 Rollup_node_services.Block.state_current_level
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* state = get_state node_ctxt block in
  let* (module Plugin) =
    Protocol_plugins.proto_plugin_for_block node_ctxt block
  in
  let*! current_level = Plugin.Pvm.get_current_level node_ctxt.kind state in
  return current_level

let () =
  Block_directory.register0 Rollup_node_services.Block.state_value
  @@ fun (node_ctxt, block) {key} () ->
  let open Lwt_result_syntax in
  let* ctx = get_state node_ctxt block in
  let path = String.split_on_char '/' key in
  let*! value = Context.PVMState.lookup ctx path in
  match value with
  | None -> failwith "No such key in PVM state"
  | Some value ->
      Format.eprintf "Encoded %S\n@.%!" (Bytes.to_string value) ;
      return value

let () =
  Block_directory.register0 Rollup_node_services.Block.committed_status
  @@ fun (node_ctxt, block) () () ->
  let open Lwt_result_syntax in
  let* block = Node_context.get_l2_block node_ctxt block in
  Publisher.Helpers.committed_status node_ctxt block

let () =
  Local_directory.gen_register0
    Rollup_node_services.Local.synchronized
    (fun node_ctxt () () ->
      let open Lwt_syntax in
      let levels_stream, stopper =
        Lwt_watcher.create_stream node_ctxt.sync.sync_level_input
      in
      let first_call = ref true in
      let synced = ref false in
      let initial_processed_level = ref None in
      let get_percentage processed_tezos_level known_tezos_level =
        let initial =
          match !initial_processed_level with
          | Some l -> l
          | None ->
              initial_processed_level := Some processed_tezos_level ;
              processed_tezos_level
        in
        let total = Int32.sub known_tezos_level initial in
        let done_ = Int32.sub processed_tezos_level initial in
        Int32.to_float done_ *. 100. /. Int32.to_float total
      in
      let next () =
        if !synced then Lwt.return_none
        else
          let levels =
            let+ processed_level =
              if !first_call then (
                first_call := false ;
                Lwt.return_some node_ctxt.sync.processed_level)
              else Lwt_stream.get levels_stream
            in
            let l1_head = Layer1.get_latest_head node_ctxt.l1_ctxt in
            match (processed_level, l1_head) with
            | None, _ | _, None ->
                synced := true ;
                Rollup_node_services.Synchronized
            | Some processed_level, Some l1_head
              when processed_level = l1_head.level ->
                synced := true ;
                Rollup_node_services.Synchronized
            | Some processed_level, Some l1_head ->
                Synchronizing
                  {
                    processed_level;
                    l1_head_level = l1_head.level;
                    percentage_done =
                      get_percentage processed_level l1_head.level;
                  }
          in
          let synchronized =
            let+ () = Node_context.wait_synchronized node_ctxt in
            synced := true ;
            Rollup_node_services.Synchronized
          in
          let+ result = Lwt.pick [levels; synchronized] in
          Some result
      in
      let shutdown () = Lwt_watcher.shutdown stopper in
      Tezos_rpc.Answer.return_stream {next; shutdown})

let () =
  Local_directory.register0 Rollup_node_services.Local.last_published_commitment
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  match Reference.get node_ctxt.lpc with
  | None -> return_none
  | Some commitment ->
      let hash = Octez_smart_rollup.Commitment.hash commitment in
      (* The corresponding level in Store.Commitments.published_at_level is
         available only when the commitment has been published and included
         in a block. *)
      let* published_at_level_info =
        Node_context.commitment_published_at_level node_ctxt hash
      in
      let first_published, published =
        match published_at_level_info with
        | None -> (None, None)
        | Some {first_published_at_level; published_at_level} ->
            (Some first_published_at_level, published_at_level)
      in
      return_some (commitment, hash, first_published, published)

let () =
  Local_directory.register1 Rollup_node_services.Local.commitment
  @@ fun node_ctxt commitment_hash () () ->
  let open Lwt_result_syntax in
  let* commitment = Node_context.find_commitment node_ctxt commitment_hash in
  match commitment with
  | None -> return_none
  | Some commitment ->
      let hash = Octez_smart_rollup.Commitment.hash commitment in
      (* The corresponding level in Store.Commitments.published_at_level is
         available only when the commitment has been published and included
         in a block. *)
      let* published_at_level_info =
        Node_context.commitment_published_at_level node_ctxt hash
      in
      let first_published, published =
        match published_at_level_info with
        | None -> (None, None)
        | Some {first_published_at_level; published_at_level} ->
            (Some first_published_at_level, published_at_level)
      in
      return_some (commitment, hash, first_published, published)

let get_outbox_content (node_ctxt : _ Node_context.t)
    (outbox_level, message_indexes) =
  let open Lwt_result_syntax in
  let* outbox =
    let lcc = (Reference.get node_ctxt.lcc).level in
    let ctxt_block_level = Int32.max lcc outbox_level in
    let* ctxt_block_hash =
      Node_context.hash_of_level_opt node_ctxt ctxt_block_level
    in
    match ctxt_block_hash with
    | None -> return_nil
    | Some ctxt_block_hash ->
        let* ctxt = Node_context.checkout_context node_ctxt ctxt_block_hash in
        let* state = Context.PVMState.get ctxt in
        let*? (module Plugin) =
          Protocol_plugins.proto_plugin_for_protocol
            (Reference.get node_ctxt.current_protocol).hash
        in
        let*! outbox =
          Plugin.Pvm.get_outbox_messages node_ctxt state ~outbox_level
        in
        return outbox
  in
  let messages =
    List.rev_map
      (fun i -> (i, List.assoc ~equal:Int.equal i outbox))
      message_indexes
    |> List.rev
  in
  return (outbox_level, messages)

let () =
  Local_directory.register0 Rollup_node_services.Local.outbox_pending_executable
  @@ fun node_ctxt outbox_level () ->
  let open Lwt_result_syntax in
  let* outbox_messages =
    Node_context.get_executable_pending_outbox_messages ?outbox_level node_ctxt
  in
  List.map_ep (get_outbox_content node_ctxt) outbox_messages

let () =
  Local_directory.register0
    Rollup_node_services.Local.outbox_pending_unexecutable
  @@ fun node_ctxt outbox_level () ->
  let open Lwt_result_syntax in
  let* outbox_messages =
    Node_context.get_unexecutable_pending_outbox_messages
      ?outbox_level
      node_ctxt
  in
  List.map_ep (get_outbox_content node_ctxt) outbox_messages

let () =
  Local_directory.register0 Rollup_node_services.Local.outbox_pending
  @@ fun node_ctxt outbox_level () ->
  let open Lwt_result_syntax in
  let* outbox_messages =
    Node_context.get_pending_outbox_messages ?outbox_level node_ctxt
  in
  List.map_ep
    (fun (outbox_msg, status) ->
      let+ outbos_msg = get_outbox_content node_ctxt outbox_msg in
      (outbos_msg, status))
    outbox_messages

let () =
  Local_directory.register0 Rollup_node_services.Local.gc_info
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let* started_gc_info = Node_context.get_gc_info node_ctxt `Started
  and* successful_gc_info = Node_context.get_gc_info node_ctxt `Successful
  and* last_context_split_level =
    Node_context.get_last_context_split_level node_ctxt
  in
  let first_available_level, last_gc_started_at =
    match started_gc_info with
    | Some gc -> (gc.gc_target, Some gc.gc_triggered_at)
    | None -> (node_ctxt.genesis_info.level, None)
  in
  let last_successful_gc_target =
    match successful_gc_info with
    | Some {gc_target = l; _} -> Some l
    | None -> None
  in
  return
    Rollup_node_services.
      {
        first_available_level;
        last_gc_started_at;
        last_context_split_level;
        last_successful_gc_target;
      }

let () =
  Local_directory.register0 Rollup_node_services.Local.injection
  @@ fun _node_ctxt query messages ->
  Batcher.register_messages
    ?order:query#order
    ~drop_duplicate:query#drop_duplicate
    messages

let () =
  Local_directory.register0 Rollup_node_services.Local.dal_batcher_injection
  @@ fun _node_ctxt () messages ->
  Dal_injection_queue.register_dal_messages ~messages

let () =
  Local_directory.register0 Rollup_node_services.Local.dal_slot_indices
  @@ fun _node_ctxt () slot_indices ->
  Dal_injection_queue.set_dal_slot_indices slot_indices

let () =
  Local_directory.register0 Rollup_node_services.Local.batcher_queue
  @@ fun _node_ctxt () () -> Batcher.get_queue () |> Lwt.return

(** [commitment_level_of_inbox_level node_ctxt inbox_level] returns the level
      of the commitment which should include the inbox of level
      [inbox_level].

      It is computed with the following formula:
      {v
      commitment_level(inbox_level) =
        last_commitment -
         ((last_commitment - inbox_level) / commitment_period
          * commitment_period)
      v}
*)
let commitment_level_of_inbox_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Lwt_result_syntax in
  let last_published_commitment = Reference.get node_ctxt.lpc in
  let+ constants =
    Protocol_plugins.get_constants_of_level node_ctxt inbox_level
  in
  let commitment_period =
    Int32.of_int constants.sc_rollup.commitment_period_in_blocks
  in
  Option.map
    (fun last_published_commitment ->
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/6246
         fix and test last_published_inbox_level in RPC dir. *)
      let last_published = last_published_commitment.Commitment.inbox_level in
      let open Int32 in
      div (sub last_published inbox_level) commitment_period
      |> mul commitment_period |> sub last_published)
    last_published_commitment

let inbox_info_of_level (node_ctxt : _ Node_context.t) inbox_level =
  let open Lwt_result_syntax in
  let+ finalized_level = Node_context.get_finalized_level node_ctxt in
  let finalized = Compare.Int32.(inbox_level <= finalized_level) in
  let lcc = Reference.get node_ctxt.lcc in
  let cemented = Compare.Int32.(inbox_level <= lcc.level) in
  (finalized, cemented)

let injector_operation_status node_ctxt l1_hash =
  let open Lwt_result_syntax in
  match Injector.operation_status l1_hash with
  | None -> return Rollup_node_services.Unknown
  | Some (Pending op) -> return (Rollup_node_services.Pending_injection op)
  | Some (Injected {op; oph; op_index}) ->
      return (Rollup_node_services.Injected {op = op.operation; oph; op_index})
  | Some (Included {op; oph; op_index; l1_block; l1_level}) -> (
      let* finalized, cemented = inbox_info_of_level node_ctxt l1_level in
      let* commitment_level =
        commitment_level_of_inbox_level node_ctxt l1_level
      in
      match commitment_level with
      | None ->
          return
            (Rollup_node_services.Included
               {
                 op = op.operation;
                 oph;
                 op_index;
                 l1_block;
                 l1_level;
                 finalized;
                 cemented;
               })
      | Some commitment_level -> (
          let* block =
            Node_context.find_l2_block_by_level node_ctxt commitment_level
          in
          match block with
          | None ->
              (* Commitment not computed yet for inbox *)
              return
                (Rollup_node_services.Included
                   {
                     op = op.operation;
                     oph;
                     op_index;
                     l1_block;
                     l1_level;
                     finalized;
                     cemented;
                   })
          | Some block -> (
              let commitment_hash =
                WithExceptions.Option.get
                  ~loc:__LOC__
                  block.header.commitment_hash
              in
              (* Commitment computed *)
              let* published_at =
                Node_context.commitment_published_at_level
                  node_ctxt
                  commitment_hash
              in
              match published_at with
              | None | Some {published_at_level = None; _} ->
                  (* Commitment not published yet *)
                  return
                    (Rollup_node_services.Included
                       {
                         op = op.operation;
                         oph;
                         op_index;
                         l1_block;
                         l1_level;
                         finalized;
                         cemented;
                       })
              | Some
                  {
                    first_published_at_level;
                    published_at_level = Some published_at_level;
                  } ->
                  (* Commitment published *)
                  let* commitment =
                    Node_context.get_commitment node_ctxt commitment_hash
                  in
                  return
                    (Rollup_node_services.Committed
                       {
                         op = op.operation;
                         oph;
                         op_index;
                         l1_block;
                         l1_level;
                         finalized;
                         cemented;
                         commitment;
                         commitment_hash;
                         first_published_at_level;
                         published_at_level;
                       }))))

let dal_injected_operations_statuses node_ctxt =
  let open Lwt_result_syntax in
  let*? ids = Dal_injection_queue.get_injection_ids () in
  List.map_es
    (fun l1_hash ->
      let* status = injector_operation_status node_ctxt l1_hash in
      return (l1_hash, status))
    ids

let () =
  Local_directory.register1 Rollup_node_services.Local.batcher_message
  @@ fun node_ctxt hash () () ->
  let open Lwt_result_syntax in
  let*? batch_status = Batcher.message_status hash in
  let* status =
    match batch_status with
    | None -> return (None, Rollup_node_services.Unknown)
    | Some (batch_status, msg) -> (
        let return status = return (Some msg, status) in
        match batch_status with
        | Pending_batch -> return Rollup_node_services.Pending_batch
        | Batched l1_hash ->
            let* res = injector_operation_status node_ctxt l1_hash in
            return res)
  in
  return status

let () =
  Local_directory.register1 Rollup_node_services.Local.injector_operation_status
  @@ fun node_ctxt l1_hash () () -> injector_operation_status node_ctxt l1_hash

let () =
  Local_directory.register0
    Rollup_node_services.Local.dal_injected_operations_statuses
  @@ fun node_ctxt () () -> dal_injected_operations_statuses node_ctxt

let () =
  Local_directory.register1 Rollup_node_services.Local.forget_dal_injection_id
  @@ fun _node_ctxt id () () ->
  Lwt.return @@ Dal_injection_queue.forget_injection_id id

let () =
  Admin_directory.register0 Rollup_node_services.Admin.injector_queues_total
  @@ fun _node_ctxt () () ->
  let open Lwt_result_syntax in
  let totals = Injector.total_queued_operations () in
  return totals

let () =
  Admin_directory.register0 Rollup_node_services.Admin.injector_queues
  @@ fun _node_ctxt tag () ->
  let open Lwt_result_syntax in
  let queues = Injector.get_queues ?tag () in
  let queues =
    List.map
      (fun (tags, ops) ->
        let rops =
          List.rev_map
            (fun Injector.Inj_operation.
                   {operation = op; errors = {count; last_error}; _}
               -> Rollup_node_services.{op; errors = count; last_error})
            ops
        in
        (tags, List.rev rops))
      queues
  in
  return queues

let () =
  Admin_directory.register0 Rollup_node_services.Admin.clear_injector_queues
  @@ fun _node_ctxt query () ->
  Injector.clear_queues
    ~drop_no_order:query#drop_no_order
    ?order_below:query#order
    ?tag:query#operation_tag
    ()

let () =
  Admin_directory.register0 Rollup_node_services.Admin.clear_batcher_queues
  @@ fun _node_ctxt query () ->
  Batcher.clean_queue
    ?order_request:
      (Option.map
         (fun order_below ->
           Batcher_worker_types.
             {drop_no_order = query#drop_no_order; order_below})
         query#order)
    ()

let () =
  Admin_directory.register0 Rollup_node_services.Admin.cancel_gc
  @@ fun node_ctxt () () ->
  let open Lwt_result_syntax in
  let*! canceled = Node_context.cancel_gc node_ctxt in
  return canceled

let add_describe dir =
  Tezos_rpc.Directory.register_describe_directory_service
    dir
    Tezos_rpc.Service.description_service

let top_directory (node_ctxt : _ Node_context.t) =
  List.fold_left
    (fun dir f -> Tezos_rpc.Directory.merge dir (f node_ctxt))
    Tezos_rpc.Directory.empty
    [
      Root_directory.build_directory;
      Global_directory.build_directory;
      Local_directory.build_directory;
      Admin_directory.build_directory;
    ]

let build_block_subdirectory = Block_directory.build_sub_directory

let block_prefix =
  Tezos_rpc.Path.(
    open_root / "global" / "block" /: Rollup_node_services.Arg.block_id)

let protocol_directories = Protocol_hash.Table.create 3

let build_protocol_directory node_ctxt proto =
  let plugin =
    match Protocol_plugins.proto_plugin_for_protocol proto with
    | Error e ->
        Format.kasprintf
          Stdlib.failwith
          "Cannot build RPC directory for %a.\n%a"
          Protocol_hash.pp
          proto
          pp_print_trace
          e
    | Ok p -> p
  in
  let (module Plugin) = plugin in
  let block_directory = Plugin.RPC_directory.block_directory node_ctxt in
  let full_static_dir =
    Tezos_rpc.Directory.merge
      (top_directory node_ctxt)
      (Tezos_rpc.Directory.prefix block_prefix block_directory)
    |> add_describe
  in
  Protocol_hash.Table.replace
    protocol_directories
    proto
    (block_directory, full_static_dir) ;
  (block_directory, full_static_dir)

let build_protocol_directories node_ctxt =
  List.iter
    (fun p -> ignore (build_protocol_directory node_ctxt p))
    (Protocol_plugins.registered_protocols ())

let get_proto_dir ?protocol (node_ctxt : _ Node_context.t) =
  let current_protocol = Reference.get node_ctxt.current_protocol in
  let proto = Option.value protocol ~default:current_protocol.hash in
  match Protocol_hash.Table.find protocol_directories proto with
  | None -> error_with "Unknown protocol %a" Protocol_hash.pp proto
  | Some (block_dir, full_dir) -> Ok (block_dir, full_dir, proto)

let generate_openapi dir proto =
  let open Lwt_result_syntax in
  let*! descr =
    Tezos_rpc.Directory.describe_directory ~recurse:true ~arg:() dir
  in
  let json_api =
    Data_encoding.Json.construct
      Tezos_rpc.Encoding.description_answer_encoding
      descr
  in
  let open Tezos_openapi in
  json_api
  |> Json.annotate ~origin:"description"
  |> Api.parse_tree |> Api.parse_services |> Api.flatten
  |> Convert.convert_api
       ~title:"Smart Rollup Node RPCs"
       ~description:
         (Format.asprintf
            "Smart Rollup Node RPC API for protocol %a"
            Protocol_hash.pp
            proto)
       Tezos_version_value.Bin_version.octez_simple_version_string
  |> Openapi.to_json |> return

let () =
  Root_directory.register0 Rollup_node_services.Root.openapi
  @@ fun node_ctxt {protocol} () ->
  let open Lwt_result_syntax in
  let*? _, dir, proto = get_proto_dir ?protocol node_ctxt in
  generate_openapi dir proto

let directory node_ctxt =
  let dir = top_directory node_ctxt in
  build_protocol_directories node_ctxt ;
  let path =
    Tezos_rpc.Path.(
      open_root / "global" / "block" /: Rollup_node_services.Arg.block_id)
  in
  let dir =
    Tezos_rpc.Directory.register_dynamic_directory
      ~descr:"Dynamic protocol specific RPC directory for the rollup node"
      dir
      path
      (fun ((), block_id) ->
        let open Lwt_syntax in
        let+ dir =
          let open Lwt_result_syntax in
          let* level =
            Block_directory_helpers.block_level_of_id node_ctxt block_id
          in
          let* () = Node_context.check_level_available node_ctxt level in
          let* proto = Node_context.protocol_of_level node_ctxt level in
          let*? block_directory, _, _ =
            get_proto_dir ~protocol:proto.protocol node_ctxt
          in
          return block_directory
        in
        match dir with
        | Ok dir -> dir
        | Error e ->
            Format.kasprintf
              Stdlib.failwith
              "Could not load block directory for block %s: %a"
              (Rollup_node_services.Arg.construct_block_id block_id)
              pp_print_trace
              e)
  in
  add_describe dir

let generate_openapi ?protocol cctxt =
  let open Lwt_result_syntax in
  let protocol =
    Option.value_f protocol ~default:Protocol_plugins.last_registered
  in
  let* node_ctxt =
    Node_context_loader.Internal_for_tests.openapi_context cctxt protocol
  in
  let _, dir = build_protocol_directory node_ctxt protocol in
  generate_openapi dir protocol
