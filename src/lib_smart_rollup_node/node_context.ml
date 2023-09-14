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

type lcc = Store.Lcc.lcc = {commitment : Commitment.Hash.t; level : int32}

type genesis_info = Metadata.genesis_info = {
  level : int32;
  commitment_hash : Commitment.Hash.t;
}

type 'a store = 'a Store.t

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

type 'a t = {
  config : Configuration.t;
  cctxt : Client_context.full;
  dal_cctxt : Dal_node_client.cctxt option;
  dac_client : Dac_observer_client.t option;
  data_dir : string;
  l1_ctxt : Layer1.t;
  genesis_info : genesis_info;
  injector_retention_period : int;
  block_finality_time : int;
  kind : Kind.t;
  lockfile : Lwt_unix.file_descr;
  store : 'a store;
  context : 'a Context.index;
  lcc : ('a, lcc) Reference.t;
  lpc : ('a, Commitment.t option) Reference.t;
  private_info : ('a, private_info option) Reference.t;
  kernel_debug_logger : debug_logger;
  finaliser : unit -> unit Lwt.t;
  mutable current_protocol : current_protocol;
  global_block_watcher : Sc_rollup_block.t Lwt_watcher.input;
}

type rw = [`Read | `Write] t

type ro = [`Read] t

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

let lock ~data_dir =
  let lockfile_path = Filename.concat data_dir "lock" in
  let lock_aux ~data_dir =
    let open Lwt_result_syntax in
    let*! () = Event.acquiring_lock () in
    let*! () = Lwt_utils_unix.create_dir data_dir in
    let* lockfile =
      protect @@ fun () ->
      Lwt_unix.openfile
        lockfile_path
        [Unix.O_CREAT; O_RDWR; O_CLOEXEC; O_SYNC]
        0o644
      |> Lwt_result.ok
    in
    let* () =
      protect ~on_error:(fun err ->
          let*! () = Lwt_unix.close lockfile in
          fail err)
      @@ fun () ->
      let*! () = Lwt_unix.lockf lockfile Unix.F_LOCK 0 in
      return_unit
    in
    return lockfile
  in
  trace (Rollup_node_errors.Could_not_acquire_lock lockfile_path)
  @@ lock_aux ~data_dir

let unlock {lockfile; _} =
  Lwt.finalize
    (fun () -> Lwt_unix.lockf lockfile Unix.F_ULOCK 0)
    (fun () -> Lwt_unix.close lockfile)

let processing_lockfile_path ~data_dir =
  Filename.concat data_dir "processing_lock"

let gc_lockfile_path ~data_dir = Filename.concat data_dir "gc_lock"

let make_kernel_logger event ?log_kernel_debug_file logs_dir =
  let open Lwt_syntax in
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
    let* () = event msg in
    return_unit
  in
  return (kernel_debug, fun () -> Lwt_io.close chan)

let check_and_set_history_mode (type a) (mode : a Store_sigs.mode)
    (store : a Store.store) (history_mode : Configuration.history_mode) =
  let open Lwt_result_syntax in
  let* stored_history_mode =
    match mode with
    | Read_only -> Store.History_mode.read store.history_mode
    | Read_write -> Store.History_mode.read store.history_mode
  in
  let save_when_rw () =
    match mode with
    | Read_only -> return_unit
    | Read_write -> Store.History_mode.write store.history_mode history_mode
  in
  match (stored_history_mode, history_mode) with
  | None, _ -> save_when_rw ()
  | Some Archive, Archive | Some Full, Full -> return_unit
  | Some Archive, Full ->
      (* Data will be cleaned at next GC, just save new mode *)
      let*! () = Event.convert_history_mode Archive Full in
      save_when_rw ()
  | Some Full, Archive ->
      failwith "Cannot transform a full rollup node into an archive one."

let update_metadata ({Metadata.rollup_address; _} as metadata) ~data_dir =
  let open Lwt_result_syntax in
  let* disk_metadata = Metadata.Versioned.read_metadata_file ~dir:data_dir in
  match disk_metadata with
  | Some (V1 {rollup_address = saved_address; context_version; _}) ->
      let*? () = Context.Version.check context_version in
      fail_unless Address.(rollup_address = saved_address)
      @@ Rollup_node_errors.Unexpected_rollup {rollup_address; saved_address}
  | Some (V0 {rollup_address = saved_address; context_version}) ->
      let*? () = Context.Version.check context_version in
      let*? () =
        error_unless Address.(rollup_address = saved_address)
        @@ Rollup_node_errors.Unexpected_rollup {rollup_address; saved_address}
      in
      Metadata.write_metadata_file ~dir:data_dir metadata
  | None -> Metadata.write_metadata_file ~dir:data_dir metadata

let init (cctxt : #Client_context.full) ~data_dir ~irmin_cache_size
    ~index_buffer_size ?log_kernel_debug_file ?last_whitelist_update mode
    l1_ctxt genesis_info ~lcc ~lpc kind current_protocol
    Configuration.(
      {
        sc_rollup_address = rollup_address;
        l2_blocks_cache_size;
        dal_node_endpoint;
        _;
      } as configuration) =
  let open Lwt_result_syntax in
  let* lockfile = lock ~data_dir in
  let metadata =
    {
      Metadata.rollup_address;
      context_version = Context.Version.version;
      kind;
      genesis_info;
    }
  in
  let* () = update_metadata metadata ~data_dir in
  let* () =
    Store_migration.maybe_run_migration
      metadata
      ~storage_dir:(Configuration.default_storage_dir data_dir)
      ~index_buffer_size:Configuration.default_index_buffer_size
  in
  let dal_cctxt =
    Option.map Dal_node_client.make_unix_cctxt dal_node_endpoint
  in
  let* store =
    Store.load
      mode
      ~index_buffer_size
      ~l2_blocks_cache_size
      Configuration.(default_storage_dir data_dir)
  in
  let* context =
    Context.load
      ~cache_size:irmin_cache_size
      mode
      (Configuration.default_context_dir data_dir)
  in
  let* () = check_and_set_history_mode mode store configuration.history_mode in
  let*! () = Event.rollup_exists ~addr:rollup_address ~kind in
  let*! () =
    if dal_cctxt = None && current_protocol.constants.dal.feature_enable then
      Event.warn_dal_enabled_no_node ()
    else Lwt.return_unit
  in
  let* dac_client =
    Option.map_es
      (fun observer_endpoint ->
        Dac_observer_client.init
          {
            observer_endpoint;
            reveal_data_dir = Filename.concat data_dir (Kind.to_string kind);
            timeout_seconds = configuration.dac_timeout;
          })
      configuration.dac_observer_endpoint
  in
  let*! kernel_debug_logger, kernel_debug_finaliser =
    let open Lwt_syntax in
    if configuration.log_kernel_debug then
      make_kernel_logger Event.kernel_debug ?log_kernel_debug_file data_dir
    else return (Event.kernel_debug, fun () -> return_unit)
  in
  let global_block_watcher = Lwt_watcher.create_input () in
  let private_info =
    Option.map
      (fun (message_index, outbox_level) ->
        {
          last_whitelist_update =
            {outbox_level; message_index = Z.to_int message_index};
          last_outbox_level_searched = outbox_level;
        })
      last_whitelist_update
  in
  return
    {
      config = configuration;
      cctxt = (cctxt :> Client_context.full);
      dal_cctxt;
      dac_client;
      data_dir;
      l1_ctxt;
      genesis_info;
      lcc = Reference.new_ lcc;
      lpc = Reference.new_ lpc;
      private_info = Reference.new_ private_info;
      kind;
      injector_retention_period = 0;
      block_finality_time = 2;
      lockfile;
      store;
      context;
      kernel_debug_logger;
      finaliser = kernel_debug_finaliser;
      current_protocol;
      global_block_watcher;
    }

let close ({cctxt; store; context; l1_ctxt; finaliser; _} as node_ctxt) =
  let open Lwt_result_syntax in
  let message = cctxt#message in
  let*! () = message "Running finaliser@." in
  let*! () = finaliser () in
  let*! () = message "Shutting down L1@." in
  let*! () = Layer1.shutdown l1_ctxt in
  let*! () = message "Closing context@." in
  let*! () = Context.close context in
  let*! () = message "Closing store@." in
  let* () = Store.close store in
  let*! () = message "Releasing lock@." in
  let*! () = unlock node_ctxt in
  return_unit

let checkout_context node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* l2_header =
    Store.L2_blocks.header node_ctxt.store.l2_blocks block_hash
  in
  let*? context_hash =
    let open Result_syntax in
    match l2_header with
    | None ->
        tzfail (Rollup_node_errors.Cannot_checkout_context (block_hash, None))
    | Some {context; _} -> return context
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
  && node_ctxt.current_protocol.constants.dal.feature_enable

let readonly (node_ctxt : _ t) =
  {
    node_ctxt with
    store = Store.readonly node_ctxt.store;
    context = Context.readonly node_ctxt.context;
    lcc = Reference.readonly node_ctxt.lcc;
    lpc = Reference.readonly node_ctxt.lpc;
    private_info = Reference.readonly node_ctxt.private_info;
  }

type 'a delayed_write = ('a, rw) Delayed_write_monad.t

(** Abstraction over store  *)

module Lwt_result_option_syntax = struct
  let ( let** ) a f =
    let open Lwt_result_syntax in
    let* a in
    match a with None -> return_none | Some a -> f a
end

let hash_of_level_opt {store; cctxt; _} level =
  let open Lwt_result_syntax in
  let* hash = Store.Levels_to_hashes.find store.levels_to_hashes level in
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
  let* l2_header = Store.L2_blocks.header store.l2_blocks hash in
  match l2_header with
  | Some {level; _} -> return level
  | None ->
      let+ {level; _} = Layer1.fetch_tezos_shell_header l1_ctxt hash in
      level

let save_level {store; _} Layer1.{hash; level} =
  Store.Levels_to_hashes.add store.levels_to_hashes level hash

let save_l2_block {store; _} (head : Sc_rollup_block.t) =
  let head_info = {head with header = (); content = ()} in
  Store.L2_blocks.append
    store.l2_blocks
    ~key:head.header.block_hash
    ~header:head.header
    ~value:head_info

let set_l2_head {store; _} (head : Sc_rollup_block.t) =
  Store.L2_head.write store.l2_head head

let is_processed {store; _} head = Store.L2_blocks.mem store.l2_blocks head

let last_processed_head_opt {store; _} = Store.L2_head.read store.l2_head

let mark_finalized_level {store; _} level =
  Store.Last_finalized_level.write store.last_finalized_level level

let get_finalized_level {store; _} =
  let open Lwt_result_syntax in
  let+ level = Store.Last_finalized_level.read store.last_finalized_level in
  Option.value level ~default:0l

let get_l2_block {store; _} block_hash =
  let open Lwt_result_syntax in
  let* block = Store.L2_blocks.read store.l2_blocks block_hash in
  match block with
  | None ->
      failwith "Could not retrieve L2 block for %a" Block_hash.pp block_hash
  | Some (info, header) -> return {info with Sc_rollup_block.header}

let find_l2_block {store; _} block_hash =
  let open Lwt_result_syntax in
  let+ block = Store.L2_blocks.read store.l2_blocks block_hash in
  Option.map (fun (info, header) -> {info with Sc_rollup_block.header}) block

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

let get_finalized_head_opt node_ctxt =
  let open Lwt_result_syntax in
  let* level =
    Store.Last_finalized_level.read node_ctxt.store.last_finalized_level
  in
  match level with
  | None -> return_none
  | Some level -> find_l2_block_by_level node_ctxt level

let head_of_block_level (hash, level) = {Layer1.hash; level}

let block_level_of_head Layer1.{hash; level} = (hash, level)

let get_l2_block_predecessor node_ctxt hash =
  let open Lwt_result_syntax in
  let+ header = Store.L2_blocks.header node_ctxt.store.l2_blocks hash in
  Option.map
    (fun {Sc_rollup_block.predecessor; level; _} ->
      (predecessor, Int32.pred level))
    header

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

let get_predecessor_header_opt node_ctxt head =
  let open Lwt_result_syntax in
  let* res = get_predecessor_opt node_ctxt (Layer1.head_of_header head) in
  Option.map_es (header_of_head node_ctxt) res

let get_predecessor_header node_ctxt head =
  let open Lwt_result_syntax in
  let* res = get_predecessor node_ctxt (Layer1.head_of_header head) in
  header_of_head node_ctxt res

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4128
   Unit test the function tick_search. *)

(** Returns the block that is right before [tick]. [big_step_blocks] is used to
    first look for a block before the [tick] *)
let tick_search ~big_step_blocks node_ctxt head tick =
  let open Lwt_result_syntax in
  if Z.Compare.(head.Sc_rollup_block.initial_tick <= tick) then
    if Z.Compare.(Sc_rollup_block.final_tick head < tick) then
      (* The head block does not contain the tick *)
      return_none
    else
      (* The starting block contains the tick we want, we are done. *)
      return_some head
  else
    let genesis_level = node_ctxt.genesis_info.level in
    let rec find_big_step (end_block : Sc_rollup_block.t) =
      let start_level =
        Int32.sub end_block.header.level (Int32.of_int big_step_blocks)
      in
      let start_level =
        if start_level < genesis_level then genesis_level else start_level
      in
      let* start_block = get_l2_block_by_level node_ctxt start_level in
      if Z.Compare.(start_block.initial_tick <= tick) then
        return (start_block, end_block)
      else find_big_step start_block
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

let block_with_tick ({store; _} as node_ctxt) ~max_level tick =
  let open Lwt_result_syntax in
  let open Lwt_result_option_syntax in
  Error.trace_lwt_result_with
    "Could not retrieve block with tick %a"
    Z.pp_print
    tick
  @@ let** head = Store.L2_head.read store.l2_head in
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
     tick_search ~big_step_blocks:4096 node_ctxt head tick

let find_commitment {store; _} commitment_hash =
  let open Lwt_result_syntax in
  let+ commitment = Store.Commitments.read store.commitments commitment_hash in
  Option.map fst commitment

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

let commitment_exists {store; _} hash =
  Store.Commitments.mem store.commitments hash

let save_commitment {store; _} commitment =
  let open Lwt_result_syntax in
  let hash = Commitment.hash commitment in
  let+ () =
    Store.Commitments.append store.commitments ~key:hash ~value:commitment
  in
  hash

let commitment_published_at_level {store; _} commitment =
  Store.Commitments_published_at_level.find
    store.commitments_published_at_level
    commitment

let set_commitment_published_at_level {store; _} hash =
  Store.Commitments_published_at_level.add
    store.commitments_published_at_level
    hash

type commitment_source = Anyone | Us

let commitment_was_published {store; _} ~source commitment_hash =
  let open Lwt_result_syntax in
  match source with
  | Anyone ->
      Store.Commitments_published_at_level.mem
        store.commitments_published_at_level
        commitment_hash
  | Us -> (
      let+ info =
        Store.Commitments_published_at_level.find
          store.commitments_published_at_level
          commitment_hash
      in
      match info with
      | Some {published_at_level = Some _; _} -> true
      | _ -> false)

let set_lcc node_ctxt lcc =
  let open Lwt_result_syntax in
  let lcc_l1 = Reference.get node_ctxt.lcc in
  let* () = Store.Lcc.write node_ctxt.store.lcc lcc in
  if lcc.level > lcc_l1.level then Reference.set node_ctxt.lcc lcc ;
  let*! () =
    Commitment_event.last_cemented_commitment_updated lcc.commitment lcc.level
  in
  return_unit

let last_seen_lcc {store; genesis_info; _} =
  let open Lwt_result_syntax in
  let+ lcc = Store.Lcc.read store.lcc in
  match lcc with
  | Some lcc -> lcc
  | None ->
      {commitment = genesis_info.commitment_hash; level = genesis_info.level}

let register_published_commitment node_ctxt commitment ~first_published_at_level
    ~level ~published_by_us =
  let open Lwt_result_syntax in
  let commitment_hash = Commitment.hash commitment in
  let* prev_publication =
    Store.Commitments_published_at_level.mem
      node_ctxt.store.commitments_published_at_level
      commitment_hash
  in
  let published_at_level = if published_by_us then Some level else None in
  let* () =
    if (not prev_publication) || published_by_us then
      set_commitment_published_at_level
        node_ctxt
        commitment_hash
        {first_published_at_level; published_at_level}
    else return_unit
  in
  when_ published_by_us @@ fun () ->
  let* () = Store.Lpc.write node_ctxt.store.lpc commitment in
  let update_lpc_ref =
    match Reference.get node_ctxt.lpc with
    | None -> true
    | Some {inbox_level; _} -> commitment.inbox_level >= inbox_level
  in
  if update_lpc_ref then Reference.set node_ctxt.lpc (Some commitment) ;
  return_unit

let find_inbox {store; _} inbox_hash =
  let open Lwt_result_syntax in
  let+ inbox = Store.Inboxes.read store.inboxes inbox_hash in
  Option.map fst inbox

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

let save_inbox {store; _} inbox =
  let open Lwt_result_syntax in
  let hash = Octez_smart_rollup.Inbox.hash inbox in
  let+ () = Store.Inboxes.append store.inboxes ~key:hash ~value:inbox in
  hash

let find_inbox_by_block_hash ({store; _} as node_ctxt) block_hash =
  let open Lwt_result_syntax in
  let* header = Store.L2_blocks.header store.l2_blocks block_hash in
  match header with
  | None -> return_none
  | Some {inbox_hash; _} -> find_inbox node_ctxt inbox_hash

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

let unsafe_find_stored_messages node_ctxt =
  Store.Messages.read node_ctxt.store.messages

let unsafe_get_stored_messages node_ctxt messages_hash =
  let open Lwt_result_syntax in
  let* res = unsafe_find_stored_messages node_ctxt messages_hash in
  match res with
  | None ->
      failwith
        "Could not retrieve messages with payloads merkelized hash %a"
        Merkelized_payload_hashes_hash.pp
        messages_hash
  | Some (messages, _pred) -> return messages

let get_num_messages {store; _} hash =
  let open Lwt_result_syntax in
  let* msg = Store.Messages.read store.messages hash in
  match msg with
  | None ->
      failwith
        "Could not retrieve number of messages for inbox witness %a"
        Merkelized_payload_hashes_hash.pp
        hash
  | Some (messages, _pred_hash) -> return (List.length messages)

let save_messages {store; _} key ~predecessor messages =
  Store.Messages.append
    store.messages
    ~key
    ~header:predecessor
    ~value:(messages :> string list)

let get_full_l2_block node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* block = get_l2_block node_ctxt block_hash in
  let* inbox = get_inbox node_ctxt block.header.inbox_hash
  and* messages =
    unsafe_get_stored_messages node_ctxt block.header.inbox_witness
  and* commitment =
    Option.map_es (get_commitment node_ctxt) block.header.commitment_hash
  in
  return {block with content = {Sc_rollup_block.inbox; messages; commitment}}

type proto_info = {
  proto_level : int;
  first_level_of_protocol : bool;
  protocol : Protocol_hash.t;
}

let protocol_of_level_with_store (store : _ Store.t) level =
  let open Lwt_result_syntax in
  let* protocols = Store.Protocols.read store.protocols in
  let*? protocols =
    match protocols with
    | None | Some [] ->
        error_with "Cannot infer protocol for level %ld: no protocol info" level
    | Some protos -> Ok protos
  in
  let rec find = function
    | [] ->
        error_with "Cannot infer protocol for level %ld: no information" level
    | {Store.Protocols.level = p_level; proto_level; protocol} :: protos -> (
        (* Latest protocols appear first in the list *)
        match p_level with
        | First_known l when level >= l ->
            Ok {protocol; proto_level; first_level_of_protocol = false}
        | Activation_level l when level > l ->
            (* The block at the activation level is of the previous protocol, so
               we are in the protocol that was activated at [l] only when the
               level we query is after [l]. *)
            Ok
              {
                protocol;
                proto_level;
                first_level_of_protocol = level = Int32.succ l;
              }
        | _ -> (find [@tailcall]) protos)
  in
  Lwt.return (find protocols)

let protocol_of_level node_ctxt level =
  assert (level >= node_ctxt.genesis_info.level) ;
  protocol_of_level_with_store node_ctxt.store level

let last_seen_protocol node_ctxt =
  let open Lwt_result_syntax in
  let+ protocols = Store.Protocols.read node_ctxt.store.protocols in
  match protocols with
  | None | Some [] -> None
  | Some (p :: _) -> Some p.protocol

let protocol_activation_level node_ctxt protocol_hash =
  let open Lwt_result_syntax in
  let* protocols = Store.Protocols.read node_ctxt.store.protocols in
  match
    Option.bind
      protocols
      (List.find_map (function Store.Protocols.{protocol; level; _} ->
           if Protocol_hash.(protocol_hash = protocol) then Some level else None))
  with
  | None ->
      failwith
        "Could not determine the activation level of a previously unseen \
         protocol %a"
        Protocol_hash.pp
        protocol_hash
  | Some l -> return l

let save_protocol_info node_ctxt (block : Layer1.header)
    ~(predecessor : Layer1.header) =
  let open Lwt_result_syntax in
  let* protocols = Store.Protocols.read node_ctxt.store.protocols in
  match protocols with
  | Some ({proto_level; _} :: _)
    when proto_level = block.header.proto_level
         && block.header.proto_level = predecessor.header.proto_level ->
      (* Nominal case, no protocol change. Nothing to do. *)
      return_unit
  | None | Some [] ->
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
      Store.Protocols.write node_ctxt.store.protocols protocols
  | Some
      ({proto_level = last_proto_level; _} :: previous_protocols as protocols)
    ->
      (* block.header.proto_level <> last_proto_level or head is a migration
         block, i.e. there is a protocol change w.r.t. last registered one. *)
      let is_head_migration_block =
        block.header.proto_level <> predecessor.header.proto_level
      in
      let* proto_info =
        let+ {next_protocol = protocol; _} =
          Tezos_shell_services.Shell_services.Blocks.protocols
            node_ctxt.cctxt
            ~block:(`Hash (block.hash, 0))
            ()
        in
        let level =
          if is_head_migration_block then
            Store.Protocols.Activation_level block.level
          else First_known block.level
        in
        Store.Protocols.
          {level; proto_level = block.header.proto_level; protocol}
      in
      let protocols =
        if block.header.proto_level > last_proto_level then
          (* Protocol upgrade, add new item to protocol list *)
          proto_info :: protocols
        else if block.header.proto_level < last_proto_level then (
          (* Reorganization in which a protocol migration block was
             backtracked. *)
          match previous_protocols with
          | [] ->
              (* No info further back, store what we know. *)
              [proto_info]
          | previous_proto :: _ ->
              (* make sure that we are in the case where we backtracked the
                 migration block. *)
              assert (
                Protocol_hash.(proto_info.protocol = previous_proto.protocol)) ;
              (* Remove last stored protocol *)
              previous_protocols)
        else
          (* block.header.proto_level = last_proto_level && is_migration_block *)
          (* Reorganization where we are doing a different protocol
             upgrade. Replace last stored protocol. *)
          proto_info :: previous_protocols
      in
      Store.Protocols.write node_ctxt.store.protocols protocols

let get_slot_header {store; _} ~published_in_block_hash slot_index =
  Error.trace_lwt_result_with
    "Could not retrieve slot header for slot index %d published in block %a"
    slot_index
    Block_hash.pp
    published_in_block_hash
  @@ Store.Dal_slots_headers.get
       store.irmin_store
       ~primary_key:published_in_block_hash
       ~secondary_key:slot_index

let get_all_slot_headers {store; _} ~published_in_block_hash =
  Store.Dal_slots_headers.list_values
    store.irmin_store
    ~primary_key:published_in_block_hash

let get_slot_indexes {store; _} ~published_in_block_hash =
  Store.Dal_slots_headers.list_secondary_keys
    store.irmin_store
    ~primary_key:published_in_block_hash

let save_slot_header {store; _} ~published_in_block_hash
    (slot_header : Dal.Slot_header.t) =
  Store.Dal_slots_headers.add
    store.irmin_store
    ~primary_key:published_in_block_hash
    ~secondary_key:slot_header.id.index
    slot_header

let find_slot_status {store; _} ~confirmed_in_block_hash slot_index =
  Store.Dal_slots_statuses.find
    store.irmin_store
    ~primary_key:confirmed_in_block_hash
    ~secondary_key:slot_index

let list_slots_statuses {store; _} ~confirmed_in_block_hash =
  Store.Dal_slots_statuses.list_secondary_keys_with_values
    store.irmin_store
    ~primary_key:confirmed_in_block_hash

let save_slot_status {store; _} current_block_hash slot_index status =
  Store.Dal_slots_statuses.add
    store.irmin_store
    ~primary_key:current_block_hash
    ~secondary_key:slot_index
    status

let find_confirmed_slots_history {store; _} block =
  Store.Dal_confirmed_slots_history.find store.irmin_store block

let save_confirmed_slots_history {store; _} block hist =
  Store.Dal_confirmed_slots_history.add store.irmin_store block hist

let find_confirmed_slots_histories {store; _} block =
  Store.Dal_confirmed_slots_histories.find store.irmin_store block

let save_confirmed_slots_histories {store; _} block hist =
  Store.Dal_confirmed_slots_histories.add store.irmin_store block hist

let get_gc_levels node_ctxt =
  let open Lwt_result_syntax in
  let+ gc_levels = Store.Gc_levels.read node_ctxt.store.gc_levels in
  match gc_levels with
  | Some gc_levels -> gc_levels
  | None ->
      {
        last_gc_level = node_ctxt.genesis_info.level;
        first_available_level = node_ctxt.genesis_info.level;
      }

let save_gc_info node_ctxt ~at_level ~gc_level =
  let open Lwt_syntax in
  (* Note: Setting the `first_available_level` before GC succeeds simplifies the
     code. However, it may be temporarily inaccurate if the GC run spans over
     several levels or if it fails. This is not foreseen to cause any issues,
     but if greater accuracy is needed, the `first_available_level` can be set
     after the GC finishes via a callback. *)
  let* res =
    Store.Gc_levels.write
      node_ctxt.store.gc_levels
      {last_gc_level = at_level; first_available_level = gc_level}
  in
  match res with
  | Error _ -> Event.gc_levels_storage_failure ()
  | Ok () -> return_unit

let get_gc_level node_ctxt =
  let open Lwt_result_syntax in
  match node_ctxt.config.history_mode with
  | Archive ->
      (* Never call GC in archive mode *)
      return_none
  | Full ->
      (* GC up to LCC in full mode *)
      let+ lcc = last_seen_lcc node_ctxt in
      Some lcc.level

let gc node_ctxt ~(level : int32) =
  let open Lwt_result_syntax in
  (* [gc_level] is the level corresponding to the hash on which GC will be
     called. *)
  let* gc_level = get_gc_level node_ctxt in
  let frequency = node_ctxt.config.gc_parameters.frequency_in_blocks in
  let* {last_gc_level; first_available_level} = get_gc_levels node_ctxt in
  match gc_level with
  | None -> return_unit
  | Some gc_level
    when gc_level > first_available_level
         && Int32.(sub level last_gc_level >= frequency)
         && Context.is_gc_finished node_ctxt.context
         && Store.is_gc_finished node_ctxt.store -> (
      let* hash = hash_of_level node_ctxt gc_level in
      let* header = Store.L2_blocks.header node_ctxt.store.l2_blocks hash in
      match header with
      | None ->
          failwith
            "Could not retrieve L2 block header for %a"
            Block_hash.pp
            hash
      | Some {context; _} ->
          let* gc_lockfile =
            Utils.lock (gc_lockfile_path ~data_dir:node_ctxt.data_dir)
          in
          let*! () = Event.calling_gc ~gc_level ~head_level:level in
          let*! () = save_gc_info node_ctxt ~at_level:level ~gc_level in
          (* Start both node and context gc asynchronously *)
          let*! () = Context.gc node_ctxt.context context in
          let* () = Store.gc node_ctxt.store ~level:gc_level in
          let gc_waiter () =
            let open Lwt_syntax in
            let* () = Context.wait_gc_completion node_ctxt.context
            and* () = Store.wait_gc_completion node_ctxt.store in
            let* () = Event.gc_finished ~gc_level ~head_level:level in
            Utils.unlock gc_lockfile
          in
          Lwt.dont_wait gc_waiter (fun _exn -> ()) ;
          return_unit)
  | _ -> return_unit

let check_level_available node_ctxt accessed_level =
  let open Lwt_result_syntax in
  let* {first_available_level; _} = get_gc_levels node_ctxt in
  fail_when
    (accessed_level < first_available_level)
    (Rollup_node_errors.Access_below_first_available_level
       {first_available_level; accessed_level})

module Internal_for_tests = struct
  let create_node_context cctxt (current_protocol : current_protocol) ~data_dir
      kind =
    let open Lwt_result_syntax in
    let rollup_address = Address.zero in
    let mode = Configuration.Observer in
    let*? operators =
      Purpose.make_operator
        ~needed_purposes:(Configuration.purposes_of_mode mode)
        []
    in
    let loser_mode = Loser_mode.no_failures in
    let l1_blocks_cache_size = Configuration.default_l1_blocks_cache_size in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let index_buffer_size = Configuration.default_index_buffer_size in
    let irmin_cache_size = Configuration.default_irmin_cache_size in
    let config =
      Configuration.
        {
          sc_rollup_address = rollup_address;
          boot_sector_file = None;
          operators;
          rpc_addr = Configuration.default_rpc_addr;
          rpc_port = Configuration.default_rpc_port;
          metrics_addr = None;
          reconnection_delay = 5.;
          fee_parameters = Configuration.default_fee_parameters;
          mode;
          loser_mode;
          dal_node_endpoint = None;
          dac_observer_endpoint = None;
          dac_timeout = None;
          batcher = Configuration.default_batcher;
          injector = Configuration.default_injector;
          l1_blocks_cache_size;
          l2_blocks_cache_size;
          index_buffer_size = Some index_buffer_size;
          irmin_cache_size = Some irmin_cache_size;
          prefetch_blocks = None;
          log_kernel_debug = false;
          no_degraded = false;
          gc_parameters = Configuration.default_gc_parameters;
          history_mode = Configuration.default_history_mode;
          cors = Resto_cohttp.Cors.default;
        }
    in
    let* lockfile = lock ~data_dir in
    let* store =
      Store.load
        Read_write
        ~index_buffer_size
        ~l2_blocks_cache_size
        Configuration.(default_storage_dir data_dir)
    in
    let* context =
      Context.load
        Read_write
        (Configuration.default_context_dir data_dir)
        ~cache_size:irmin_cache_size
    in
    let genesis_info = {level = 0l; commitment_hash = Commitment.Hash.zero} in
    let l1_ctxt = Layer1.Internal_for_tests.dummy cctxt in
    let lcc = Reference.new_ {commitment = Commitment.Hash.zero; level = 0l} in
    let lpc = Reference.new_ None in
    let* () =
      Store.Protocols.write
        store.protocols
        [
          Store.Protocols.
            {
              level = Activation_level 0l;
              proto_level = current_protocol.proto_level;
              protocol = current_protocol.hash;
            };
        ]
    in
    let global_block_watcher = Lwt_watcher.create_input () in
    return
      {
        config;
        cctxt = (cctxt :> Client_context.full);
        dal_cctxt = None;
        dac_client = None;
        data_dir;
        l1_ctxt;
        genesis_info;
        lcc;
        lpc;
        private_info = Reference.new_ None;
        kind;
        injector_retention_period = 0;
        block_finality_time = 2;
        current_protocol;
        lockfile;
        store;
        context;
        kernel_debug_logger = Event.kernel_debug;
        finaliser = (fun () -> Lwt.return_unit);
        global_block_watcher;
      }

  let unsafe_get_store node_ctxt = node_ctxt.store

  let openapi_context cctxt protocol =
    let current_protocol =
      {
        hash = protocol;
        proto_level = 0;
        constants =
          Rollup_constants.
            {
              minimal_block_delay = 0L;
              delay_increment_per_round = 0L;
              sc_rollup =
                {
                  challenge_window_in_blocks = 0;
                  commitment_period_in_blocks = 0;
                  reveal_activation_level =
                    Some
                      {
                        blake2B = 0l;
                        metadata = 0l;
                        dal_page = 0l;
                        dal_parameters = 0l;
                      };
                  max_number_of_stored_cemented_commitments = 0;
                };
              dal =
                {
                  feature_enable = false;
                  attestation_lag = 0;
                  number_of_slots = 0;
                  cryptobox_parameters =
                    {
                      redundancy_factor = 0;
                      page_size = 0;
                      slot_size = 0;
                      number_of_shards = 0;
                    };
                };
            };
      }
    in
    Lwt_utils_unix.with_tempdir "smart-rollup-node-openapi" @@ fun data_dir ->
    let open Lwt_result_syntax in
    let* node_ctxt =
      create_node_context cctxt current_protocol ~data_dir Wasm_2_0_0
    in
    let*! () = Context.close node_ctxt.context in
    let* () = Store.close node_ctxt.store in
    return node_ctxt
end
