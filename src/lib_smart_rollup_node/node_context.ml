(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

type lcc = {commitment : Commitment.Hash.t; level : int32}

type genesis_info = Metadata.genesis_info = {
  level : int32;
  commitment_hash : Commitment.Hash.t;
}

type 'a store = 'a Store.t constraint 'a = [< `Read | `Write > `Read]

module Node_store = struct
  let close (s : 'a store) = Store.close s

  let init = Store.init

  let check_and_set_history_mode (type a) (mode : a Access_mode.t)
      (store : a Store.t) (history_mode : Configuration.history_mode option) =
    let open Lwt_result_syntax in
    let* stored_history_mode =
      match mode with
      | Read_only -> Store.State.History_mode.get store
      | Read_write -> Store.State.History_mode.get store
    in
    let save_when_rw history_mode =
      match mode with
      | Read_only -> return_unit
      | Read_write -> Store.State.History_mode.set store history_mode
    in
    match (stored_history_mode, history_mode) with
    | None, None -> save_when_rw Configuration.default_history_mode
    | Some _, None -> return_unit
    | None, Some history_mode -> save_when_rw history_mode
    | Some Archive, Some Archive | Some Full, Some Full -> return_unit
    | Some Archive, Some Full ->
        (* Data will be cleaned at next GC, just save new mode *)
        let*! () = Event.convert_history_mode Archive Full in
        save_when_rw Full
    | Some Full, Some Archive ->
        failwith "Cannot transform a full rollup node into an archive one."

  let of_store store = store
end

type debug_logger = string -> unit Lwt.t

type current_protocol = {
  hash : Protocol_hash.t;
  proto_level : int;
  constants : Rollup_constants.protocol_constants;
}

type last_whitelist_update = {message_index : int; outbox_level : Int32.t}

type private_info = {
  last_whitelist_update : last_whitelist_update;
  last_outbox_level_searched : int32;
}

type sync_info = {
  on_synchronized : unit Lwt_condition.t;
  mutable processed_level : int32;
  sync_level_input : int32 Lwt_watcher.input;
}

type 'a t = {
  config : Configuration.t;
  cctxt : Client_context.full;
  degraded : bool Reference.rw;
  dal_cctxt : Tezos_dal_node_lib.Dal_node_client.cctxt option;
  data_dir : string;
  l1_ctxt : Layer1.t;
  genesis_info : genesis_info;
  injector_retention_period : int;
  block_finality_time : int;
  kind : Kind.t;
  unsafe_patches : Pvm_patches.t;
  lockfile : Lwt_unix.file_descr;
  store : 'store store;
  context : 'context Context.t;
  lcc : lcc Reference.rw;
  lpc : Commitment.t option Reference.rw;
  private_info : private_info option Reference.rw;
  kernel_debug_logger : debug_logger;
  finaliser : unit -> unit Lwt.t;
  current_protocol : current_protocol Reference.rw;
  global_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
  finalized_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
  sync : sync_info;
}
  constraint 'a = < store : 'store ; context : 'context >

type rw = < store : [`Read | `Write] ; context : [`Read | `Write] > t

type ro = < store : [`Read] ; context : [`Read] > t

type 'a rw_store = < store : [`Read | `Write] ; context : 'a > t

type 'a rw_context = < store : 'a ; context : [`Read | `Write] > t

let get_operator node_ctxt purpose =
  Purpose.find_operator purpose node_ctxt.config.operators

let is_operator node_ctxt pkh =
  Purpose.mem_operator pkh node_ctxt.config.operators

let is_accuser node_ctxt = node_ctxt.config.mode = Accuser

let is_bailout node_ctxt = node_ctxt.config.mode = Bailout

let is_loser node_ctxt = node_ctxt.config.loser_mode <> Loser_mode.no_failures

let can_inject node_ctxt (op_kind : Operation_kind.t) =
  Configuration.can_inject node_ctxt.config.mode op_kind

let check_op_in_whitelist_or_bailout_mode (node_ctxt : _ t) whitelist =
  let operator = get_operator node_ctxt Operating in
  match operator with
  | Some (Single operator) ->
      error_unless
        (is_bailout node_ctxt
        || List.mem ~equal:Signature.Public_key_hash.equal operator whitelist)
        Rollup_node_errors.Operator_not_in_whitelist
  | None -> Result_syntax.return_unit

let get_fee_parameter node_ctxt operation_kind =
  Operation_kind.Map.find operation_kind node_ctxt.config.fee_parameters
  |> Option.value ~default:(Configuration.default_fee_parameter operation_kind)

let global_lockfile_path ~data_dir = Filename.concat data_dir "lock"

let processing_lockfile_path ~data_dir =
  Filename.concat data_dir "processing_lock"

let gc_lockfile_path ~data_dir = Filename.concat data_dir "gc_lock"

let checkout_context node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* context_hash = Store.L2_blocks.find_context node_ctxt.store block_hash in
  let*? context_hash =
    let open Result_syntax in
    match context_hash with
    | None ->
        tzfail (Rollup_node_errors.Cannot_checkout_context (block_hash, None))
    | Some context -> return context
  in
  let*! ctxt = Context.checkout node_ctxt.context context_hash in
  match ctxt with
  | None ->
      tzfail
        (Rollup_node_errors.Cannot_checkout_context
           (block_hash, Some context_hash))
  | Some ctxt -> return ctxt

let dal_supported node_ctxt =
  node_ctxt.dal_cctxt <> None
  && (Reference.get node_ctxt.current_protocol).constants.dal.feature_enable

let readonly (node_ctxt : _ t) : ro =
  {
    node_ctxt with
    context = Context.readonly node_ctxt.context;
    store = Store.readonly node_ctxt.store;
  }

let readonly_store (node_ctxt : _ t) =
  {node_ctxt with store = Store.readonly node_ctxt.store}

let readonly_context (node_ctxt : _ t) =
  {node_ctxt with context = Context.readonly node_ctxt.context}

(** Abstraction over store  *)

module Lwt_result_option_syntax = struct
  let ( let** ) a f =
    let open Lwt_result_syntax in
    let* a in
    match a with None -> return_none | Some a -> f a
end

let get_history_mode {store; _} =
  let open Lwt_result_syntax in
  let+ mode = Store.State.History_mode.get store in
  Option.value mode ~default:Configuration.default_history_mode

let hash_of_level_opt {store; cctxt; _} level =
  let open Lwt_result_syntax in
  let* hash = Store.L2_levels.find store level in
  match hash with
  | Some hash -> return_some hash
  | None ->
      let*! hash =
        Tezos_shell_services.Shell_services.Blocks.hash
          cctxt
          ~chain:cctxt#chain
          ~block:(`Level level)
          ()
      in
      return (Result.to_option hash)

let hash_of_level node_ctxt level =
  let open Lwt_result_syntax in
  let* hash = hash_of_level_opt node_ctxt level in
  match hash with
  | Some h -> return h
  | None -> failwith "Cannot retrieve hash of level %ld" level

let level_of_hash {l1_ctxt; store; _} hash =
  let open Lwt_result_syntax in
  let* level = Store.L2_blocks.find_level store hash in
  match level with
  | Some level -> return level
  | None ->
      let+ {level; _} = Layer1.fetch_tezos_shell_header l1_ctxt hash in
      level

let save_level {store; _} Layer1.{hash; level} =
  Store.L2_levels.store store level hash

let save_l2_block {store; _} (head : Sc_rollup_block.t) =
  Store.L2_blocks.store store head

let notify_processed_tezos_level node_ctxt level =
  node_ctxt.sync.processed_level <- level ;
  Lwt_watcher.notify node_ctxt.sync.sync_level_input level

let set_l2_head node_ctxt (head : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let+ () =
    Store.State.L2_head.set
      node_ctxt.store
      (head.header.block_hash, head.header.level)
  in
  notify_processed_tezos_level node_ctxt head.header.level ;
  Metrics.wrap (fun () -> Metrics.Inbox.set_head_level head.header.level) ;
  Lwt_watcher.notify node_ctxt.global_block_watcher head

let is_processed {store; _} head =
  let open Lwt_result_syntax in
  let+ level = Store.L2_blocks.find_level store head in
  Option.is_some level

let last_processed_head_opt {store; _} = Store.L2_blocks.find_head store

let find_l2_block {store; _} block_hash = Store.L2_blocks.find store block_hash

let get_l2_block node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* block = find_l2_block node_ctxt block_hash in
  match block with
  | None ->
      failwith "Could not retrieve L2 block for %a" Block_hash.pp block_hash
  | Some block -> return block

let get_l2_block_by_level node_ctxt level =
  let open Lwt_result_syntax in
  Error.trace_lwt_result_with "Could not retrieve L2 block at level %ld" level
  @@ let* block_hash = hash_of_level node_ctxt level in
     get_l2_block node_ctxt block_hash

let find_l2_block_by_level node_ctxt level =
  let open Lwt_result_syntax in
  let* block_hash = hash_of_level_opt node_ctxt level in
  match block_hash with
  | None -> return_none
  | Some block_hash -> find_l2_block node_ctxt block_hash

let set_finalized node_ctxt hash level =
  let open Lwt_result_syntax in
  let* () = Store.State.Finalized_level.set node_ctxt.store (hash, level) in
  let+ finalized_block = find_l2_block node_ctxt hash in
  Option.iter
    (Lwt_watcher.notify node_ctxt.finalized_block_watcher)
    finalized_block

let get_finalized_level {store; _} =
  let open Lwt_result_syntax in
  let+ f = Store.State.Finalized_level.get store in
  match f with None -> 0l | Some (_h, l) -> l

let get_finalized_head_opt {store; _} = Store.L2_blocks.find_finalized store

let head_of_block_level (hash, level) = {Layer1.hash; level}

let block_level_of_head Layer1.{hash; level} = (hash, level)

let get_l2_block_predecessor {store; _} hash =
  Store.L2_blocks.find_predecessor store hash

let get_predecessor_opt node_ctxt (hash, level) =
  let open Lwt_result_syntax in
  let* pred = get_l2_block_predecessor node_ctxt hash in
  match pred with
  | Some p -> return_some p
  | None ->
      (* [head] is not already known in the L2 chain *)
      Layer1.get_predecessor_opt node_ctxt.l1_ctxt (hash, level)

let get_predecessor node_ctxt (hash, level) =
  let open Lwt_result_syntax in
  let* pred = get_l2_block_predecessor node_ctxt hash in
  match pred with
  | Some p -> return p
  | None ->
      (* [head] is not already known in the L2 chain *)
      Layer1.get_predecessor node_ctxt.l1_ctxt (hash, level)

let header_of_hash node_ctxt hash =
  let open Lwt_result_syntax in
  let+ header = Layer1.fetch_tezos_shell_header node_ctxt.l1_ctxt hash in
  {Layer1.hash; level = header.level; header}

let header_of_head node_ctxt Layer1.{hash; level = _} =
  header_of_hash node_ctxt hash

let get_tezos_reorg_for_new_head node_ctxt old_head new_head =
  let open Lwt_result_syntax in
  let old_head =
    match old_head with
    | `Level l -> `Level l
    | `Head Layer1.{hash; level} -> `Head (hash, level)
  in
  let+ reorg =
    Layer1.get_tezos_reorg_for_new_head
      node_ctxt.l1_ctxt
      ~get_old_predecessor:(get_predecessor node_ctxt)
      old_head
      (block_level_of_head new_head)
  in
  Reorg.map head_of_block_level reorg

let get_predecessor_opt node_ctxt head =
  let open Lwt_result_syntax in
  let+ res = get_predecessor_opt node_ctxt (block_level_of_head head) in
  Option.map head_of_block_level res

let get_predecessor node_ctxt head =
  let open Lwt_result_syntax in
  let+ res = get_predecessor node_ctxt (block_level_of_head head) in
  head_of_block_level res

let get_predecessor_header node_ctxt (head : Layer1.header) =
  let open Lwt_result_syntax in
  let*? () =
    error_when (head.level <= 0l) (Layer_1.Cannot_find_predecessor head.hash)
  in
  let pred =
    Layer1.{hash = head.header.predecessor; level = Int32.pred head.level}
  in
  header_of_head node_ctxt pred

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4128
   Unit test the function tick_search. *)

(** Returns the block that is right before [tick]. [big_step_blocks] is used to
    first look for a block before the [tick] *)
let tick_search ~big_step_blocks node_ctxt ?min_level head tick =
  let open Lwt_result_syntax in
  if Z.Compare.(head.Sc_rollup_block.initial_tick <= tick) then
    if Z.Compare.(Sc_rollup_block.final_tick head < tick) then
      (* The head block does not contain the tick *)
      return_none
    else
      (* The starting block contains the tick we want, we are done. *)
      return_some head
  else
    let* last_gc_target = Store.State.Last_gc_target.get node_ctxt.store in
    let first_available_level =
      Option.value last_gc_target ~default:node_ctxt.genesis_info.level
    in
    let min_level =
      match min_level with
      | Some min_level -> Int32.max min_level first_available_level
      | None -> first_available_level
    in
    let rec find_big_step (end_block : Sc_rollup_block.t) =
      (* Each iteration of [find_big_step b] looks if the [tick] happened in
         [b - 4096; b[. *)
      let start_level =
        Int32.sub end_block.header.level (Int32.of_int big_step_blocks)
      in
      let start_level =
        (* We stop the coarse search at [min_level] in any case. *)
        Int32.max start_level min_level
      in
      let* start_block = get_l2_block_by_level node_ctxt start_level in
      if Z.Compare.(start_block.initial_tick <= tick) then
        (* The tick happened after [start_block] and we know it happened before
           [end_block]. *)
        return (start_block, end_block)
      else if start_level = min_level then
        (* We've reached the hard lower bound for the search and we know the
           tick happened before. *)
        failwith
          "Tick %a happened before minimal level %ld with tick %a"
          Z.pp_print
          tick
          min_level
          Z.pp_print
          start_block.initial_tick
      else
        (* The tick happened before [start_block], so we restart the coarse
           search with it as the upper bound. *)
        find_big_step start_block
    in
    let block_level Sc_rollup_block.{header = {level; _}; _} =
      Int32.to_int level
    in
    let rec dicho start_block end_block =
      (* Precondition:
             [start_block <> end_block =>
              end_block.initial_tick > tick >= start_block.initial_tick] *)
      let start_level = block_level start_block in
      let end_level = block_level end_block in
      if end_level - start_level <= 1 then
        (* We have found the interval where the tick happened *)
        return_some start_block
      else
        let middle_level = start_level + ((end_level - start_level) / 2) in
        let* block_middle =
          get_l2_block_by_level node_ctxt (Int32.of_int middle_level)
        in
        if Z.Compare.(block_middle.initial_tick <= tick) then
          dicho block_middle end_block
        else dicho start_block block_middle
    in
    (* First linear approximation *)
    let* start_block, end_block = find_big_step head in
    (* Then do dichotomy on interval [start_block; end_block] *)
    dicho start_block end_block

let block_with_tick node_ctxt ?min_level ~max_level tick =
  let open Lwt_result_syntax in
  let open Lwt_result_option_syntax in
  Error.trace_lwt_result_with
    "Could not retrieve block with tick %a"
    Z.pp_print
    tick
  @@ let** head = last_processed_head_opt node_ctxt in
     (* We start by taking big steps of 4096 blocks for the first
        approximation. This means we need at most 20 big steps to start a
        dichotomy on a 4096 blocks interval (at most 12 steps). We could take
        the refutation period as the big_step_blocks to do a dichotomy on the
        full space but we anticipate refutation to happen most of the time close
        to the head. *)
     let** head =
       if head.header.level <= max_level then return_some head
       else find_l2_block_by_level node_ctxt max_level
     in
     tick_search ~big_step_blocks:4096 node_ctxt ?min_level head tick

let find_commitment {store; _} commitment_hash =
  Store.Commitments.find store commitment_hash

let get_commitment node_ctxt commitment_hash =
  let open Lwt_result_syntax in
  let* commitment = find_commitment node_ctxt commitment_hash in
  match commitment with
  | None ->
      failwith
        "Could not retrieve commitment %a"
        Commitment.Hash.pp
        commitment_hash
  | Some i -> return i

let commitment_exists node_ctxt hash =
  let open Lwt_result_syntax in
  let+ c = find_commitment node_ctxt hash in
  Option.is_some c

let save_commitment {store; _} commitment =
  Store.Commitments.store store commitment

let tick_offset_of_commitment_period node_ctxt (commitment : Commitment.t) =
  let open Lwt_result_syntax in
  let+ commitment_block =
    get_l2_block_by_level node_ctxt commitment.inbox_level
  in
  (* Final tick of commitment period *)
  let commitment_final_tick =
    Z.add commitment_block.initial_tick (Z.of_int64 commitment_block.num_ticks)
  in
  (* Final tick of predecessor commitment, i.e. initial tick of commitment
     period *)
  Z.sub commitment_final_tick (Z.of_int64 commitment.number_of_ticks)

let commitment_published_at_level {store; _} commitment =
  Store.Commitments_published_at_levels.get store commitment

let set_commitment_published_at_level {store; _} hash levels =
  Store.Commitments_published_at_levels.register store hash levels

type commitment_source = Anyone | Us

let commitment_was_published node_ctxt ~source commitment_hash =
  let open Lwt_result_syntax in
  let+ info = commitment_published_at_level node_ctxt commitment_hash in
  match source with
  | Anyone -> Option.is_some info
  | Us -> (
      match info with
      | Some {published_at_level = Some _; _} -> true
      | _ -> false)

let set_lcc node_ctxt lcc =
  let open Lwt_result_syntax in
  let lcc_l1 = Reference.get node_ctxt.lcc in
  let* () = Store.State.LCC.set node_ctxt.store (lcc.commitment, lcc.level) in
  Metrics.wrap (fun () -> Metrics.Info.set_lcc_level_local lcc.level) ;
  if lcc.level > lcc_l1.level then (
    Reference.set node_ctxt.lcc lcc ;
    Metrics.wrap (fun () -> Metrics.Info.set_lcc_level_l1 lcc.level)) ;
  let*! () =
    Commitment_event.last_cemented_commitment_updated lcc.commitment lcc.level
  in
  return_unit

let last_seen_lcc {store; genesis_info; _} =
  let open Lwt_result_syntax in
  let+ lcc = Store.State.LCC.get store in
  match lcc with
  | Some (commitment, level) -> {commitment; level}
  | None ->
      {commitment = genesis_info.commitment_hash; level = genesis_info.level}

let register_published_commitment node_ctxt commitment ~first_published_at_level
    ~level ~published_by_us =
  let open Lwt_result_syntax in
  let commitment_hash = Commitment.hash commitment in
  let* prev_publication =
    Store.Commitments_published_at_levels.get node_ctxt.store commitment_hash
  in
  let published_at_level = if published_by_us then Some level else None in
  let* () =
    if Option.is_none prev_publication || published_by_us then
      set_commitment_published_at_level
        node_ctxt
        commitment_hash
        {first_published_at_level; published_at_level}
    else return_unit
  in
  Metrics.wrap (fun () -> Metrics.Info.set_lpc_level_l1 commitment.inbox_level) ;
  when_ published_by_us @@ fun () ->
  Metrics.wrap (fun () ->
      Metrics.Info.set_lpc_level_local commitment.inbox_level) ;
  let* () =
    Store.State.LPC.set node_ctxt.store (commitment_hash, commitment.inbox_level)
  in
  let update_lpc_ref =
    match Reference.get node_ctxt.lpc with
    | None -> true
    | Some {inbox_level; _} -> commitment.inbox_level >= inbox_level
  in
  if update_lpc_ref then Reference.set node_ctxt.lpc (Some commitment) ;
  return_unit

let find_inbox {store; _} inbox_hash = Store.Inboxes.find store inbox_hash

let get_inbox node_ctxt inbox_hash =
  let open Lwt_result_syntax in
  let* inbox = find_inbox node_ctxt inbox_hash in
  match inbox with
  | None ->
      failwith
        "Could not retrieve inbox %a"
        Octez_smart_rollup.Inbox.Hash.pp
        inbox_hash
  | Some i -> return i

let save_inbox {store; _} inbox = Store.Inboxes.store store inbox

let find_inbox_by_block_hash {store; _} block_hash =
  Store.Inboxes.find_by_block_hash store block_hash

let inbox_of_head node_ctxt Layer1.{hash = block_hash; level = block_level} =
  let open Lwt_result_syntax in
  let* possible_inbox = find_inbox_by_block_hash node_ctxt block_hash in
  (* Pre-condition: forall l. (l >= genesis_level) => inbox[l] <> None. *)
  match possible_inbox with
  | None ->
      (* The inbox exists for each tezos block the rollup should care about.
         That is, every block at or after the origination level. We then join
         the bandwagon and build the inbox on top of the protocol's inbox
         at the end of the origination level. *)
      if block_level >= node_ctxt.genesis_info.level then
        (* Invariant broken, the inbox for this level should exist. *)
        failwith
          "The inbox for block hash %a (level = %ld) is missing."
          Block_hash.pp
          block_hash
          block_level
      else
        (* The rollup node should not care about levels before the genesis
           level. *)
        failwith
          "Asking for the inbox before the genesis level (i.e. %ld), out of \
           the scope of the rollup's node"
          block_level
  | Some inbox -> return inbox

let get_inbox_by_block_hash node_ctxt hash =
  let open Lwt_result_syntax in
  let* level = level_of_hash node_ctxt hash in
  inbox_of_head node_ctxt {hash; level}

let get_inbox_by_level node_ctxt level =
  let open Lwt_result_syntax in
  let* hash = hash_of_level node_ctxt level in
  inbox_of_head node_ctxt {hash; level}

let find_messages {store; _} payload_hash =
  Store.Messages.find store payload_hash

let get_messages node_ctxt messages_hash =
  let open Lwt_result_syntax in
  let* res = find_messages node_ctxt messages_hash in
  match res with
  | None ->
      failwith
        "Could not retrieve messages with payloads merkelized hash %a"
        Merkelized_payload_hashes_hash.pp
        messages_hash
  | Some messages -> return messages

let get_num_messages node_ctxt hash =
  let open Lwt_result_syntax in
  let+ messages = get_messages node_ctxt hash in
  List.length messages

let save_messages {store; _} key ~level messages =
  Store.Messages.store store ~level key (messages :> string list)

let set_outbox_message_executed {store; _} ~outbox_level ~index =
  Store.Outbox_messages.set_outbox_message_executed store ~outbox_level ~index

let register_outbox_messages {store; _} ~outbox_level ~indexes =
  let open Lwt_result_syntax in
  if indexes = [] then
    (* Remove any previously added messages for this level (e.g. in case of
       reorg) because there aren't any in this level. *)
    Store.Outbox_messages.delete_outbox_messages store ~outbox_level
  else
    (* Overwrite (or add) messages for this level. *)
    let*? indexes = Bitset.from_list indexes in
    Store.Outbox_messages.register_outbox_messages store ~outbox_level ~indexes

let get_executable_pending_outbox_messages ?outbox_level
    {store; lcc; current_protocol; _} =
  let open Lwt_result_syntax in
  let max_level = (Reference.get lcc).level in
  let constants = (Reference.get current_protocol).constants.sc_rollup in
  let min_level =
    Int32.sub max_level (Int32.of_int constants.max_active_outbox_levels)
    |> Int32.succ
    (* Protocol uses strict inequality, see function [validate_outbox_level] in
       src/proto_alpha/lib_protocol/sc_rollup_operations.ml. *)
  in
  match outbox_level with
  | None -> Store.Outbox_messages.pending store ~min_level ~max_level
  | Some outbox_level when outbox_level > max_level || outbox_level < min_level
    ->
      return_nil
  | Some outbox_level ->
      Store.Outbox_messages.pending
        store
        ~min_level:outbox_level
        ~max_level:outbox_level

let get_unexecutable_pending_outbox_messages ?outbox_level
    ({store; lcc; _} as node_ctxt) =
  let open Lwt_result_syntax in
  let* head = last_processed_head_opt node_ctxt in
  let*? max_level =
    match head with
    | None -> error_with "No L2 head"
    | Some h -> Ok h.header.level
  in
  let min_level = Int32.succ (Reference.get lcc).level in
  match outbox_level with
  | None -> Store.Outbox_messages.pending store ~min_level ~max_level
  | Some outbox_level when outbox_level > max_level || outbox_level < min_level
    ->
      return_nil
  | Some outbox_level ->
      Store.Outbox_messages.pending
        store
        ~min_level:outbox_level
        ~max_level:outbox_level

let get_pending_outbox_messages ?outbox_level
    ({store; lcc; current_protocol; _} as node_ctxt) =
  let open Lwt_result_syntax in
  let* head = last_processed_head_opt node_ctxt in
  let*? max_level =
    match head with
    | None -> error_with "No L2 head"
    | Some h -> Ok h.header.level
  in
  let lcc = (Reference.get lcc).level in
  let constants = (Reference.get current_protocol).constants.sc_rollup in
  (* Messages below, and including, this level are lost because protocol uses
     strict inequality. See function [validate_outbox_level] in
     src/proto_alpha/lib_protocol/sc_rollup_operations.ml. *)
  let lost_level =
    Int32.sub lcc (Int32.of_int constants.max_active_outbox_levels)
  in

  let+ messages =
    match outbox_level with
    | None -> Store.Outbox_messages.pending store ~min_level:0l ~max_level
    | Some outbox_level when outbox_level > max_level -> return_nil
    | Some outbox_level ->
        Store.Outbox_messages.pending
          store
          ~min_level:outbox_level
          ~max_level:outbox_level
  in
  List.rev_map
    (fun ((outbox_level, _) as msg) ->
      let status =
        if outbox_level <= lost_level then `Lost
        else if outbox_level <= lcc then `Executable
        else `Pending
      in
      (msg, status))
    messages
  |> List.rev

let get_full_l2_block ?get_outbox_messages node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* block = Store.L2_blocks.find_full node_ctxt.store block_hash in
  match block with
  | None ->
      failwith "Could not retrieve L2 block for %a" Block_hash.pp block_hash
  | Some block ->
      let* outbox =
        match get_outbox_messages with
        | None -> return_none
        | Some get_outbox_messages -> (
            let* ctxt = checkout_context node_ctxt block_hash in
            let*! pvm_state = Context.PVMState.find ctxt in
            match pvm_state with
            | None -> return_none
            | Some pvm_state ->
                let*! outbox =
                  get_outbox_messages
                    node_ctxt
                    pvm_state
                    ~outbox_level:block.header.level
                in
                return_some outbox)
      in
      return Sc_rollup_block.{block with content = {block.content with outbox}}

type proto_info = {
  proto_level : int;
  first_level_of_protocol : bool;
  protocol : Protocol_hash.t;
}

let protocol_of_level_with_store (store : _ Store.t) level =
  let open Lwt_result_syntax in
  let* p = Store.Protocols.proto_of_level store level in
  match p with
  | None ->
      failwith
        "Cannot infer protocol for level %ld. This block is likely before the \
         origination of the rollup. Make sure that the L1 octez node is \
         properfly synchronized before starting the rollup node."
        level
  | Some {level = First_known _; proto_level; protocol} ->
      return {protocol; proto_level; first_level_of_protocol = false}
  | Some {level = Activation_level l; proto_level; protocol} ->
      return
        {protocol; proto_level; first_level_of_protocol = level = Int32.succ l}

let protocol_of_level node_ctxt level =
  assert (level >= node_ctxt.genesis_info.level) ;
  protocol_of_level_with_store node_ctxt.store level

let last_seen_protocol node_ctxt =
  let open Lwt_result_syntax in
  let+ p = Store.Protocols.last node_ctxt.store in
  match p with None -> None | Some p -> Some p.protocol

let protocol_activation_level node_ctxt protocol_hash =
  let open Lwt_result_syntax in
  let* p = Store.Protocols.find node_ctxt.store protocol_hash in
  match p with
  | Some p -> return p.level
  | None ->
      failwith
        "Could not determine the activation level of a previously unseen \
         protocol %a"
        Protocol_hash.pp
        protocol_hash

let save_protocol_info node_ctxt (block : Layer1.header)
    ~(predecessor : Layer1.header) =
  let open Lwt_result_syntax in
  let* pred_proto =
    Store.Protocols.proto_of_level node_ctxt.store predecessor.level
  in
  match pred_proto with
  | Some {proto_level; _} when proto_level = block.header.proto_level ->
      (* Nominal case, no protocol change. Nothing to do. *)
      return_unit
  | None ->
      (* No protocols information saved in the rollup node yet, initialize with
         information by looking at the current head and its predecessor.
         We need to figure out if a protocol upgrade happened in one of these two blocks.
      *)
      let* {current_protocol; next_protocol} =
        Tezos_shell_services.Shell_services.Blocks.protocols
          node_ctxt.cctxt
          ~block:(`Hash (block.hash, 0))
          ()
      and* {
             current_protocol = pred_current_protocol;
             next_protocol = pred_next_protocol;
           } =
        Tezos_shell_services.Shell_services.Blocks.protocols
          node_ctxt.cctxt
          ~block:(`Hash (predecessor.hash, 0))
          ()
      in
      (* The first point in the protocol list is the one regarding
         [predecessor]. If it is a migration block we register the activation
         level, otherwise we don't go back any further and consider it as the
         first known block of the protocol. *)
      let pred_proto_info =
        Store.Protocols.
          {
            level =
              (if Protocol_hash.(pred_current_protocol = pred_next_protocol)
               then First_known predecessor.level
               else Activation_level predecessor.level);
            proto_level = predecessor.header.proto_level;
            protocol = pred_next_protocol;
          }
      in
      let protocols =
        if Protocol_hash.(current_protocol = next_protocol) then
          (* There is no protocol upgrade in [head], so no new point to add the protocol list. *)
          [pred_proto_info]
        else
          (* [head] is a migration block, add the new protocol with its activation in the list. *)
          let proto_info =
            Store.Protocols.
              {
                level = Activation_level block.level;
                proto_level = block.header.proto_level;
                protocol = next_protocol;
              }
          in
          [proto_info; pred_proto_info]
      in
      List.iter_es (Store.Protocols.store node_ctxt.store) protocols
  | Some _ ->
      (* There is protocol change in what we know of the predecessor and the
         head. *)
      let is_head_migration_block =
        block.header.proto_level <> predecessor.header.proto_level
      in
      let* {next_protocol = protocol; _} =
        Tezos_shell_services.Shell_services.Blocks.protocols
          node_ctxt.cctxt
          ~block:(`Hash (block.hash, 0))
          ()
      in
      let* cur_proto_info = Store.Protocols.find node_ctxt.store protocol in
      let to_save =
        (* Save the protocol information if we are looking at a migration block,
           or if we had protocol information that we missed (e.g. in degraded
           mode). *)
        is_head_migration_block
        ||
        match cur_proto_info with
        | None -> true
        | Some {level = First_known fl | Activation_level fl; _} ->
            block.header.level < fl
      in
      if not to_save then return_unit
      else
        let proto_info =
          let level =
            if is_head_migration_block then
              Store.Protocols.Activation_level block.level
            else First_known block.level
          in
          Store.Protocols.
            {level; proto_level = block.header.proto_level; protocol}
        in
        Store.Protocols.store node_ctxt.store proto_info

let save_protocols_from_l1 {cctxt; store; _} =
  let open Lwt_result_syntax in
  let open Tezos_shell_services.Chain_services in
  let*! protocols = Protocols.list cctxt () in
  match protocols with
  | Error _ ->
      Format.eprintf
        "Warning: did not fetch protocol activation levels from L1 node" ;
      return_unit
  | Ok protocols ->
      List.iter_es
        (fun {protocol; proto_level; activation_block = _block, level} ->
          let proto_info =
            Store.Protocols.
              {protocol; proto_level; level = Activation_level level}
          in
          Store.Protocols.store store proto_info)
        protocols

let get_slot_header {store; _} ~published_in_block_hash slot_index =
  Error.trace_lwt_result_with
    "Could not retrieve slot header for slot index %d published in block %a"
    slot_index
    Block_hash.pp
    published_in_block_hash
  @@
  let open Lwt_result_syntax in
  let* sh =
    Store.Dal_slots_headers.find_slot_header
      store
      published_in_block_hash
      ~slot_index
  in
  match sh with
  | None ->
      failwith
        "No slot header stored for %a"
        Block_hash.pp
        published_in_block_hash
  | Some sh -> return sh

let get_all_slot_headers {store; _} ~published_in_block_hash =
  Store.Dal_slots_headers.list_slot_headers store published_in_block_hash

let get_slot_indexes {store; _} ~published_in_block_hash =
  Store.Dal_slots_headers.list_slot_indexes store published_in_block_hash

let save_slot_header {store; _} ~published_in_block_hash
    (slot_header : Dal.Slot_header.t) =
  Store.Dal_slots_headers.store store published_in_block_hash slot_header

let find_slot_status {store; _} ~confirmed_in_block_hash slot_index =
  (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/7604

     Remove Dal_slots_statuses if MR
     https://gitlab.com/tezos/tezos/-/merge_requests/15640 is merged. *)
  Store.Dal_slots_statuses.find_slot_status
    store
    confirmed_in_block_hash
    ~slot_index

let list_slots_statuses {store; _} ~confirmed_in_block_hash =
  Store.Dal_slots_statuses.list_slot_statuses store confirmed_in_block_hash

let save_slot_status {store; _} current_block_hash slot_index status =
  Store.Dal_slots_statuses.store store current_block_hash slot_index status

type gc_level = {gc_triggered_at : int32; gc_target : int32}

let get_gc_info_aux {store; _} step =
  let open Lwt_result_syntax in
  let* gc_triggered_at =
    match step with
    | `Started -> Store.State.Last_gc_triggered_at.get store
    | `Successful -> Store.State.Last_successful_gc_triggered_at.get store
  in
  let* gc_target =
    match step with
    | `Started -> Store.State.Last_gc_target.get store
    | `Successful -> Store.State.Last_successful_gc_target.get store
  in
  match (gc_triggered_at, gc_target) with
  | None, _ | _, None -> return_none
  | Some gc_triggered_at, Some gc_target ->
      return_some {gc_triggered_at; gc_target}

let get_gc_info node_ctxt step =
  let open Lwt_result_syntax in
  let* gc_levels = get_gc_info_aux node_ctxt step in
  match (gc_levels, step) with
  | Some _, _ -> return gc_levels
  | None, `Successful ->
      (* The successful GC information might be missing during the transition,
         so we fallback to the started info for now. *)
      get_gc_info_aux node_ctxt `Started
  | None, `Started -> return_none

let first_available_level node_ctxt =
  let open Lwt_result_syntax in
  let+ last_gc_target = Store.State.Last_gc_target.get node_ctxt.store in
  Option.value last_gc_target ~default:node_ctxt.genesis_info.level

let get_last_context_split_level node_ctxt =
  Store.State.Last_context_split.get node_ctxt.store

let save_gc_info {store; _} step ~at_level ~gc_level =
  let open Lwt_syntax in
  let* res =
    let open Lwt_result_syntax in
    match step with
    | `Started ->
        let* () = Store.State.Last_gc_target.set store gc_level in
        Store.State.Last_gc_triggered_at.set store at_level
    | `Successful ->
        let* () = Store.State.Last_successful_gc_target.set store gc_level in
        Store.State.Last_successful_gc_triggered_at.set store at_level
  in
  match res with
  | Error _ -> Event.gc_levels_storage_failure ()
  | Ok () -> return_unit

let splitting_period node_ctxt =
  (* Default splitting period is challenge window / 5 or ~ 3 days unless
     challenge window is too small. *)
  let challenge_window =
    (Reference.get node_ctxt.current_protocol).constants.sc_rollup
      .challenge_window_in_blocks
  in
  let default =
    if challenge_window <= 5 then challenge_window
    else max (challenge_window / 5) 1
  in
  Option.value node_ctxt.config.gc_parameters.context_splitting_period ~default

let save_context_split_level node_ctxt level =
  Store.State.Last_context_split.set node_ctxt.store level

let candidate_gc_level node_ctxt =
  let open Lwt_result_syntax in
  let* history_mode = get_history_mode node_ctxt in
  match history_mode with
  | Archive ->
      (* Never call GC in archive mode *)
      return_none
  | Full ->
      (* GC up to LCC in full mode *)
      let+ lcc = last_seen_lcc node_ctxt in
      Some lcc.level

let gc ?(wait_finished = false) ?(force = false) node_ctxt ~(level : int32) =
  let open Lwt_result_syntax in
  (* [gc_level] is the level corresponding to the hash on which GC will be
     called. *)
  let* gc_level = candidate_gc_level node_ctxt in
  let* last_gc_target =
    Store.State.Last_successful_gc_target.get node_ctxt.store
  in
  let last_gc_target =
    Option.value last_gc_target ~default:node_ctxt.genesis_info.level
  in
  let frequency =
    match node_ctxt.config.gc_parameters.frequency_in_blocks with
    | Some f -> f
    | None -> Int32.of_int (splitting_period node_ctxt)
  in
  match gc_level with
  | None -> return_unit
  | Some gc_level
    when (force || Int32.(sub gc_level last_gc_target >= frequency))
         && Context.is_gc_finished node_ctxt.context -> (
      let* hash = hash_of_level node_ctxt gc_level in
      let* context = Store.L2_blocks.find_context node_ctxt.store hash in
      match context with
      | None ->
          failwith
            "GC: Could not retrieve context for L2 block %a at %ld"
            Block_hash.pp
            hash
            gc_level
      | Some context ->
          let start_timestamp = Time.System.now () in
          let* gc_lockfile =
            Lwt_lock_file.lock
              ~when_locked:`Block
              ~filename:(gc_lockfile_path ~data_dir:node_ctxt.data_dir)
          in
          let*! () = Event.calling_gc ~gc_level ~head_level:level in
          let*! () =
            save_gc_info node_ctxt `Started ~at_level:level ~gc_level
          in
          Metrics.wrap (fun () ->
              Metrics.GC.set_oldest_available_level gc_level) ;
          (* Start both node and context gc asynchronously *)
          let*! () = Context.gc node_ctxt.context context in
          let store_gc_promise = Store.gc node_ctxt.store ~level:gc_level in
          let gc_waiter () =
            let open Lwt_syntax in
            Lwt.finalize
              (fun () ->
                let* () = Context.wait_gc_completion node_ctxt.context
                and* (_ : unit tzresult) = store_gc_promise in
                Metrics.wrap (fun () ->
                    let stop_timestamp = Time.System.now () in
                    Metrics.GC.set_process_time
                    @@ Ptime.diff stop_timestamp start_timestamp) ;
                let*! () =
                  save_gc_info node_ctxt `Successful ~at_level:level ~gc_level
                in
                Event.gc_finished ~gc_level ~head_level:level)
              (fun () -> Lwt_lock_file.unlock gc_lockfile)
          in
          if wait_finished then
            let*! () = gc_waiter () in
            return_unit
          else (
            Lwt.dont_wait gc_waiter (fun _exn -> ()) ;
            return_unit))
  | _ -> return_unit

let cancel_gc node_ctxt =
  let open Lwt_syntax in
  let canceled_context_gc = Context.cancel_gc node_ctxt.context in
  return canceled_context_gc

let check_level_available node_ctxt accessed_level =
  let open Lwt_result_syntax in
  (* accessed_level is potentially unavailable if a GC was started after this
     level *)
  let* first_available_level = first_available_level node_ctxt in
  fail_when
    (accessed_level < first_available_level)
    (Rollup_node_errors.Access_below_first_available_level
       {first_available_level; accessed_level})

(** {2 Synchronization tracking} *)

let is_synchronized node_ctxt =
  let l1_head = Layer1.get_latest_head node_ctxt.l1_ctxt in
  match l1_head with
  | None -> true
  | Some l1_head -> node_ctxt.sync.processed_level = l1_head.level

let wait_synchronized node_ctxt =
  if is_synchronized node_ctxt then Lwt.return_unit
  else Lwt_condition.wait node_ctxt.sync.on_synchronized

(** {2 Kernel tracing} *)

type kernel_tracer = {
  mutable start_time : int64;
  mutable scope : Opentelemetry.Scope.t option;
}

let kernel_tracer =
  {start_time = Opentelemetry.Timestamp_ns.now_unix_ns (); scope = None}

let kernel_store_block_re = Re.Str.regexp ".*Storing block \\([0-9]+\\)"

let reset_kernel_tracing scope =
  kernel_tracer.start_time <- Opentelemetry.Timestamp_ns.now_unix_ns () ;
  kernel_tracer.scope <- Some scope

let kernel_tracing config event =
  let open Lwt_syntax in
  if not config.Configuration.etherlink then event
  else fun msg ->
    let* () = event msg in
    let block_number =
      if Re.Str.string_match kernel_store_block_re msg 0 then
        try Re.Str.matched_group 1 msg |> int_of_string_opt with _ -> None
      else None
    in
    match block_number with
    | None -> return_unit
    | Some block_number ->
        let start_time = kernel_tracer.start_time in
        let end_time = Opentelemetry.Timestamp_ns.now_unix_ns () in
        let* () =
          Interpreter_event.eval_etherlink_block
            block_number
            (Int64.sub end_time start_time)
        in
        if not (Opentelemetry.Collector.has_backend ()) then return_unit
        else
          let trace_id, parent =
            match kernel_tracer.scope with
            | None -> (Opentelemetry.Trace_id.create (), None)
            | Some scope -> (scope.trace_id, Some scope.span_id)
          in
          let span_id = Opentelemetry.Span_id.create () in
          let scope = Opentelemetry.Scope.make ~trace_id ~span_id () in
          let span, _ =
            Opentelemetry.Span.create
              ~attrs:[("etherlink.block.number", `Int block_number)]
              ?parent
              ~id:scope.span_id
              ~trace_id:scope.trace_id
              ~start_time
              ~end_time
              "eval_etherlink_block"
          in
          Opentelemetry.Trace.emit ~service_name:"rollup_node" [span] ;
          kernel_tracer.start_time <- end_time ;
          return_unit

let make_kernel_logger ~enable_tracing ?log_kernel_debug_file ~logs_dir
    (config : Configuration.t) event =
  let open Lwt_syntax in
  let on_kernel_log =
    if enable_tracing then kernel_tracing config event else event
  in
  if not config.log_kernel_debug then
    return (on_kernel_log, fun () -> return_unit)
  else
    let path =
      match log_kernel_debug_file with
      | None -> Filename.concat logs_dir "kernel.log"
      | Some path -> path
    in
    let path_dir = Filename.dirname path in
    let* () = Lwt_utils_unix.create_dir path_dir in
    let* fd =
      Lwt_unix.openfile path Lwt_unix.[O_WRONLY; O_CREAT; O_APPEND] 0o0644
    in
    let chan =
      Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.Output fd
    in
    let kernel_debug msg =
      let* () = Lwt_io.write chan msg in
      let* () = Lwt_io.flush chan in
      let* () = on_kernel_log msg in
      return_unit
    in
    return (kernel_debug, fun () -> Lwt_io.close chan)

(**/**)

module Internal_for_tests = struct
  let write_protocols_in_store (store : [> `Write] store) protocols =
    List.iter_es (Store.Protocols.store store) protocols

  let unsafe_get_store ctxt = ctxt.store
end
