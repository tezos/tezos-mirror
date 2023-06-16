(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

type lcc = {commitment : Commitment.Hash.t; level : int32}

type genesis_info = {level : int32; commitment_hash : Commitment.Hash.t}

type 'a store = 'a Store.t

type debug_logger = string -> unit Lwt.t

type 'a t = {
  cctxt : Client_context.full;
  dal_cctxt : Dal_node_client.cctxt option;
  dac_client : Dac_observer_client.t option;
  data_dir : string;
  l1_ctxt : Layer1.t;
  rollup_address : Address.t;
  boot_sector_file : string option;
  mode : Configuration.mode;
  operators : Configuration.operators;
  genesis_info : genesis_info;
  injector_retention_period : int;
  block_finality_time : int;
  kind : Kind.t;
  pvm : (module Pvm.S);
  fee_parameters : Configuration.fee_parameters;
  protocol_constants : Rollup_constants.protocol_constants;
  proto_level : int;
  loser_mode : Loser_mode.t;
  lockfile : Lwt_unix.file_descr;
  store : 'a store;
  context : 'a Context.index;
  lcc : ('a, lcc) Reference.t;
  lpc : ('a, Commitment.t option) Reference.t;
  kernel_debug_logger : debug_logger;
  finaliser : unit -> unit Lwt.t;
}

type rw = [`Read | `Write] t

type ro = [`Read] t

let get_operator node_ctxt purpose =
  Configuration.Operator_purpose_map.find purpose node_ctxt.operators

let is_operator node_ctxt pkh =
  Configuration.Operator_purpose_map.exists
    (fun _ operator -> Signature.Public_key_hash.(operator = pkh))
    node_ctxt.operators

let is_accuser {mode; _} = mode = Accuser

let is_loser {loser_mode; _} = loser_mode <> Loser_mode.no_failures

let get_fee_parameter node_ctxt purpose =
  Configuration.Operator_purpose_map.find purpose node_ctxt.fee_parameters
  |> Option.value ~default:(Configuration.default_fee_parameter ~purpose ())

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
  trace (Sc_rollup_node_errors.Could_not_acquire_lock lockfile_path)
  @@ lock_aux ~data_dir

let unlock {lockfile; _} =
  Lwt.finalize
    (fun () -> Lwt_unix.lockf lockfile Unix.F_ULOCK 0)
    (fun () -> Lwt_unix.close lockfile)

let make_kernel_logger ?log_kernel_debug_file data_dir =
  let open Lwt_syntax in
  let path =
    match log_kernel_debug_file with
    | None -> Filename.concat data_dir "kernel.log"
    | Some path -> path
  in
  let+ fd =
    Lwt_unix.openfile path Lwt_unix.[O_WRONLY; O_CREAT; O_APPEND] 0o0644
  in
  Lwt_io.of_fd ~close:(fun () -> Lwt_unix.close fd) ~mode:Lwt_io.Output fd

let pvm_of_kind : Kind.t -> (module Pvm.S) = function
  | Example_arith -> (module Arith_pvm)
  | Wasm_2_0_0 -> (module Wasm_2_0_0_pvm)

let init (cctxt : #Client_context.full) ~data_dir ?log_kernel_debug_file mode
    l1_ctxt (protocol_constants : Rollup_constants.protocol_constants)
    genesis_info ~lcc ~lpc kind ~proto_level
    Configuration.(
      {
        sc_rollup_address = rollup_address;
        sc_rollup_node_operators = operators;
        boot_sector_file;
        mode = operating_mode;
        fee_parameters;
        loser_mode;
        l2_blocks_cache_size;
        dal_node_endpoint;
        _;
      } as configuration) =
  let open Lwt_result_syntax in
  let* lockfile = lock ~data_dir in
  let* () =
    Store_migration.maybe_run_migration
      ~storage_dir:(Configuration.default_storage_dir data_dir)
  in
  let dal_cctxt =
    Option.map Dal_node_client.make_unix_cctxt dal_node_endpoint
  in
  let* store =
    Store.load
      mode
      ~l2_blocks_cache_size
      Configuration.(default_storage_dir data_dir)
  in
  let* context =
    Context.load mode (Configuration.default_context_dir data_dir)
  in
  let* () = Context.Rollup.check_or_set_address mode context rollup_address in
  let*! () = Event.rollup_exists ~addr:rollup_address ~kind in
  let*! () =
    if dal_cctxt = None && protocol_constants.dal.feature_enable then
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
    let kernel_debug = Event.kernel_debug in
    if configuration.log_kernel_debug then
      let+ chan = make_kernel_logger ?log_kernel_debug_file data_dir in
      let kernel_debug msg =
        let* () = Lwt_io.write chan msg in
        let* () = Lwt_io.flush chan in
        let* () = kernel_debug msg in
        return_unit
      in
      (kernel_debug, fun () -> Lwt_io.close chan)
    else return (kernel_debug, fun () -> return_unit)
  in
  return
    {
      cctxt = (cctxt :> Client_context.full);
      dal_cctxt;
      dac_client;
      data_dir;
      l1_ctxt;
      rollup_address;
      boot_sector_file;
      mode = operating_mode;
      operators;
      genesis_info;
      lcc = Reference.new_ lcc;
      lpc = Reference.new_ lpc;
      kind;
      pvm = pvm_of_kind kind;
      injector_retention_period = 0;
      block_finality_time = 2;
      fee_parameters;
      protocol_constants;
      proto_level;
      loser_mode;
      lockfile;
      store;
      context;
      kernel_debug_logger;
      finaliser = kernel_debug_finaliser;
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
    match l2_header with
    | None ->
        error (Sc_rollup_node_errors.Cannot_checkout_context (block_hash, None))
    | Some {context; _} -> ok context
  in
  let*! ctxt = Context.checkout node_ctxt.context context_hash in
  match ctxt with
  | None ->
      tzfail
        (Sc_rollup_node_errors.Cannot_checkout_context
           (block_hash, Some context_hash))
  | Some ctxt -> return ctxt

let dal_supported node_ctxt =
  node_ctxt.dal_cctxt <> None && node_ctxt.protocol_constants.dal.feature_enable

let readonly (node_ctxt : _ t) =
  {
    node_ctxt with
    store = Store.readonly node_ctxt.store;
    context = Context.readonly node_ctxt.context;
    lcc = Reference.readonly node_ctxt.lcc;
    lpc = Reference.readonly node_ctxt.lpc;
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

let save_l2_head {store; _} (head : Sc_rollup_block.t) =
  let open Lwt_result_syntax in
  let head_info = {head with header = (); content = ()} in
  let* () =
    Store.L2_blocks.append
      store.l2_blocks
      ~key:head.header.block_hash
      ~header:head.header
      ~value:head_info
  in
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
  let hash = Sc_rollup_proto_types.Commitment_hash.to_octez hash in
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

type messages_info = {
  is_first_block : bool;
  predecessor : Block_hash.t;
  predecessor_timestamp : Time.Protocol.t;
  messages : string list;
}

let find_messages node_ctxt messages_hash =
  let open Lwt_result_syntax in
  let messages_hash =
    Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez messages_hash
  in
  let* msg = Store.Messages.read node_ctxt.store.messages messages_hash in
  match msg with
  | None -> return_none
  | Some (messages, block_hash) ->
      let* header = header_of_hash node_ctxt block_hash in
      let* pred_header = header_of_hash node_ctxt header.header.predecessor in
      let* grand_parent_header =
        header_of_hash node_ctxt pred_header.header.predecessor
      in
      let is_first_block =
        pred_header.header.proto_level <> grand_parent_header.header.proto_level
      in
      return_some
        {
          is_first_block;
          predecessor = pred_header.hash;
          predecessor_timestamp = pred_header.header.timestamp;
          messages;
        }

let get_messages_aux find pp node_ctxt hash =
  let open Lwt_result_syntax in
  let* res = find node_ctxt hash in
  match res with
  | None ->
      failwith
        "Could not retrieve messages with payloads merkelized hash %a"
        pp
        hash
  | Some res -> return res

let get_messages node_ctxt =
  get_messages_aux find_messages Merkelized_payload_hashes_hash.pp node_ctxt

let get_messages_without_proto_messages node_ctxt =
  get_messages_aux
    (fun node_ctxt messages_hash ->
      let open Lwt_result_syntax in
      let* msg = Store.Messages.read node_ctxt.store.messages messages_hash in
      match msg with
      | None -> return_none
      | Some (messages, _block_hash) -> return_some messages)
    Merkelized_payload_hashes_hash.pp
    node_ctxt

let get_num_messages {store; _} hash =
  let open Lwt_result_syntax in
  let hash =
    Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez hash
  in
  let* msg = Store.Messages.read store.messages hash in
  match msg with
  | None ->
      failwith
        "Could not retrieve number of messages for inbox witness %a"
        Merkelized_payload_hashes_hash.pp
        hash
  | Some (messages, _block_hash) -> return (List.length messages)

let save_messages {store; _} key ~block_hash messages =
  let key = Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez key in
  Store.Messages.append
    store.messages
    ~key
    ~header:block_hash
    ~value:(messages :> string list)

let get_full_l2_block node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* block = get_l2_block node_ctxt block_hash in
  let* inbox = get_inbox node_ctxt block.header.inbox_hash
  and* messages =
    get_messages_without_proto_messages node_ctxt block.header.inbox_witness
  and* commitment =
    Option.map_es (get_commitment node_ctxt) block.header.commitment_hash
  in
  return {block with content = {Sc_rollup_block.inbox; messages; commitment}}

type proto_info = {
  proto_level : int;
  first_level_of_protocol : bool;
  protocol : Protocol_hash.t;
}

let protocol_of_level node_ctxt level =
  let open Lwt_result_syntax in
  assert (level >= node_ctxt.genesis_info.level) ;
  let* protocols = Store.Protocols.read node_ctxt.store.protocols in
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

module Internal_for_tests = struct
  let create_node_context cctxt protocol_constants ~data_dir kind =
    let open Lwt_result_syntax in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let* lockfile = lock ~data_dir in
    let* store =
      Store.load
        Read_write
        ~l2_blocks_cache_size
        Configuration.(default_storage_dir data_dir)
    in
    let* context =
      Context.load Read_write (Configuration.default_context_dir data_dir)
    in
    let genesis_info = {level = 0l; commitment_hash = Commitment.Hash.zero} in
    let l1_ctxt = Layer1.Internal_for_tests.dummy cctxt in
    let lcc = Reference.new_ {commitment = Commitment.Hash.zero; level = 0l} in
    let lpc = Reference.new_ None in
    return
      {
        cctxt = (cctxt :> Client_context.full);
        dal_cctxt = None;
        dac_client = None;
        data_dir;
        l1_ctxt;
        rollup_address = Address.zero;
        boot_sector_file = None;
        mode = Observer;
        operators = Configuration.Operator_purpose_map.empty;
        genesis_info;
        lcc;
        lpc;
        kind;
        pvm = pvm_of_kind kind;
        injector_retention_period = 0;
        block_finality_time = 2;
        fee_parameters = Configuration.default_fee_parameters;
        protocol_constants;
        proto_level = 0;
        loser_mode = Loser_mode.no_failures;
        lockfile;
        store;
        context;
        kernel_debug_logger = Event.kernel_debug;
        finaliser = (fun () -> Lwt.return_unit);
      }
end
