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

open Protocol
open Alpha_context

type lcc = {commitment : Sc_rollup.Commitment.Hash.t; level : Raw_level.t}

type 'a store = 'a Store.t

type debug_logger = string -> unit Lwt.t

type 'a t = {
  cctxt : Protocol_client_context.full;
  dal_cctxt : Dal_node_client.cctxt option;
  dac_client : Dac_observer_client.t option;
  data_dir : string;
  l1_ctxt : Layer1.t;
  rollup_address : Sc_rollup.t;
  boot_sector_file : string option;
  mode : Configuration.mode;
  operators : Configuration.operators;
  genesis_info : Sc_rollup.Commitment.genesis_info;
  injector_retention_period : int;
  block_finality_time : int;
  kind : Sc_rollup.Kind.t;
  fee_parameters : Configuration.fee_parameters;
  protocol_constants : Constants.t;
  loser_mode : Loser_mode.t;
  lockfile : Lwt_unix.file_descr;
  store : 'a store;
  context : 'a Context.index;
  lcc : ('a, lcc) Reference.t;
  lpc : ('a, Sc_rollup.Commitment.t option) Reference.t;
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2901
   The constants are retrieved from the latest tezos block. These constants can
   be different from the ones used at the creation at the rollup because of a
   protocol amendment that modifies some of them. This need to be fixed when the
   rollup nodes will be able to handle the migration of protocol.
*)
let retrieve_constants cctxt =
  Protocol.Constants_services.all cctxt (cctxt#chain, cctxt#block)

let get_last_cemented_commitment (cctxt : Protocol_client_context.full)
    rollup_address =
  let open Lwt_result_syntax in
  let+ commitment, level =
    Plugin.RPC.Sc_rollup.last_cemented_commitment_hash_with_level
      cctxt
      (cctxt#chain, `Head 0)
      rollup_address
  in
  {commitment; level}

let get_last_published_commitment (cctxt : Protocol_client_context.full)
    rollup_address operator =
  let open Lwt_result_syntax in
  let*! res =
    Plugin.RPC.Sc_rollup.staked_on_commitment
      cctxt
      (cctxt#chain, `Head 0)
      rollup_address
      operator
  in
  match res with
  | Error trace
    when TzTrace.fold
           (fun exists -> function
             | Environment.Ecoproto_error Sc_rollup_errors.Sc_rollup_not_staked
               ->
                 true
             | _ -> exists)
           false
           trace ->
      return_none
  | Error trace -> fail trace
  | Ok None -> return_none
  | Ok (Some (_staked_hash, staked_commitment)) -> return_some staked_commitment

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

let check_fee_parameters Configuration.{fee_parameters; _} =
  let check_value purpose name compare to_string mempool_default value =
    if compare mempool_default value > 0 then
      error_with
        "Bad configuration fee_parameter.%s for %s. It must be at least %s for \
         operations of the injector to be propagated."
        name
        (Configuration.string_of_purpose purpose)
        (to_string mempool_default)
    else Ok ()
  in
  let check purpose
      {
        Injector_sigs.minimal_fees;
        minimal_nanotez_per_byte;
        minimal_nanotez_per_gas_unit;
        force_low_fee = _;
        fee_cap = _;
        burn_cap = _;
      } =
    let open Result_syntax in
    let+ () =
      check_value
        purpose
        "minimal_fees"
        Int64.compare
        Int64.to_string
        (Tez.to_mutez Plugin.Mempool.default_minimal_fees)
        minimal_fees.mutez
    and+ () =
      check_value
        purpose
        "minimal_nanotez_per_byte"
        Q.compare
        Q.to_string
        Plugin.Mempool.default_minimal_nanotez_per_byte
        minimal_nanotez_per_byte
    and+ () =
      check_value
        purpose
        "minimal_nanotez_per_gas_unit"
        Q.compare
        Q.to_string
        Plugin.Mempool.default_minimal_nanotez_per_gas_unit
        minimal_nanotez_per_gas_unit
    in
    ()
  in
  Configuration.Operator_purpose_map.iter_e check fee_parameters

let protocol_max_batch_size =
  let empty_message_op : _ Operation.t =
    let open Protocol in
    let open Alpha_context in
    let open Operation in
    {
      shell = {branch = Block_hash.zero};
      protocol_data =
        {
          signature = Some Signature.zero;
          contents =
            Single
              (Manager_operation
                 {
                   source = Signature.Public_key_hash.zero;
                   fee = Tez.of_mutez_exn Int64.max_int;
                   counter = Manager_counter.Internal_for_tests.of_int max_int;
                   gas_limit =
                     Gas.Arith.integral_of_int_exn ((max_int - 1) / 1000);
                   storage_limit = Z.of_int max_int;
                   operation = Sc_rollup_add_messages {messages = [""]};
                 });
        };
    }
  in
  Protocol.Constants_repr.max_operation_data_length
  - Data_encoding.Binary.length
      Operation.encoding_with_legacy_attestation_name
      (Operation.pack empty_message_op)

let check_batcher_config Configuration.{batcher = {max_batch_size; _}; _} =
  match max_batch_size with
  | Some m when m > protocol_max_batch_size ->
      error_with
        "batcher.max_batch_size must be smaller than %d"
        protocol_max_batch_size
  | _ -> Ok ()

let check_config config =
  let open Result_syntax in
  let+ () = check_fee_parameters config and+ () = check_batcher_config config in
  ()

let init (cctxt : Protocol_client_context.full) ~data_dir ?log_kernel_debug_file
    mode
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
        reconnection_delay;
        _;
      } as configuration) =
  let open Lwt_result_syntax in
  let*? () = check_config configuration in
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
  let* l1_ctxt =
    Layer1.start ~name:"sc_rollup_node" ~reconnection_delay cctxt
  in
  let publisher = Configuration.Operator_purpose_map.find Publish operators in
  let* protocol_constants = retrieve_constants cctxt
  and* lcc = get_last_cemented_commitment cctxt rollup_address
  and* lpc =
    Option.filter_map_es
      (get_last_published_commitment cctxt rollup_address)
      publisher
  and* kind =
    RPC.Sc_rollup.kind cctxt (cctxt#chain, cctxt#block) rollup_address ()
  and* genesis_info =
    RPC.Sc_rollup.genesis_info cctxt (cctxt#chain, cctxt#block) rollup_address
  in
  let*! () = Event.rollup_exists ~addr:rollup_address ~kind in
  let*! () =
    if dal_cctxt = None && protocol_constants.parametric.dal.feature_enable then
      Event.warn_dal_enabled_no_node ()
    else Lwt.return_unit
  in
  let* dac_client =
    Option.map_es
      (fun observer_endpoint ->
        Dac_observer_client.init
          {
            observer_endpoint;
            reveal_data_dir =
              Filename.concat data_dir (Sc_rollup.Kind.to_string kind);
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
      cctxt;
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
      injector_retention_period = 0;
      block_finality_time = 2;
      fee_parameters;
      protocol_constants;
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

let metadata node_ctxt =
  let address = node_ctxt.rollup_address in
  let origination_level = node_ctxt.genesis_info.Sc_rollup.Commitment.level in
  Sc_rollup.Metadata.{address; origination_level}

let dal_supported node_ctxt =
  node_ctxt.dal_cctxt <> None
  && node_ctxt.protocol_constants.parametric.dal.feature_enable

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

let level_of_hash {cctxt; store; _} hash =
  let open Lwt_result_syntax in
  let* l2_header = Store.L2_blocks.header store.l2_blocks hash in
  match l2_header with
  | Some {level; _} -> return (Raw_level.to_int32 level)
  | None ->
      let+ {level; _} = Layer1.fetch_tezos_shell_header cctxt hash in
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
      (predecessor, Int32.pred (Raw_level.to_int32 level)))
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

let header_of_head node_ctxt Layer1.{hash; level} =
  let open Lwt_result_syntax in
  let+ header = Layer1.fetch_tezos_shell_header node_ctxt.cctxt hash in
  {Layer1.hash; level; header}

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
  if Sc_rollup.Tick.(head.Sc_rollup_block.initial_tick <= tick) then
    if Sc_rollup.Tick.(Sc_rollup_block.final_tick head < tick) then
      (* The head block does not contain the tick *)
      return_none
    else
      (* The starting block contains the tick we want, we are done. *)
      return_some head
  else
    let genesis_level = Raw_level.to_int32 node_ctxt.genesis_info.level in
    let rec find_big_step (end_block : Sc_rollup_block.t) =
      let start_level =
        Int32.sub
          (Raw_level.to_int32 end_block.header.level)
          (Int32.of_int big_step_blocks)
      in
      let start_level =
        if start_level < genesis_level then genesis_level else start_level
      in
      let* start_block = get_l2_block_by_level node_ctxt start_level in
      if Sc_rollup.Tick.(start_block.initial_tick <= tick) then
        return (start_block, end_block)
      else find_big_step start_block
    in
    let block_level Sc_rollup_block.{header = {level; _}; _} =
      Raw_level.to_int32 level |> Int32.to_int
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
        if Sc_rollup.Tick.(block_middle.initial_tick <= tick) then
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
    Sc_rollup.Tick.pp
    tick
  @@ let** head = Store.L2_head.read store.l2_head in
     (* We start by taking big steps of 4096 blocks for the first
        approximation. This means we need at most 20 big steps to start a
        dichotomy on a 4096 blocks interval (at most 12 steps). We could take
        the refutation period as the big_step_blocks to do a dichotomy on the
        full space but we anticipate refutation to happen most of the time close
        to the head. *)
     let** head =
       if Raw_level.(head.header.level <= max_level) then return_some head
       else find_l2_block_by_level node_ctxt (Raw_level.to_int32 max_level)
     in
     tick_search ~big_step_blocks:4096 node_ctxt head tick

let get_commitment {store; _} commitment_hash =
  let open Lwt_result_syntax in
  let* commitment = Store.Commitments.find store.commitments commitment_hash in
  match commitment with
  | None ->
      failwith
        "Could not retrieve commitment %a"
        Sc_rollup.Commitment.Hash.pp
        commitment_hash
  | Some c -> return c

let find_commitment {store; _} hash =
  Store.Commitments.find store.commitments hash

let commitment_exists {store; _} hash =
  Store.Commitments.mem store.commitments hash

let save_commitment {store; _} commitment =
  let open Lwt_result_syntax in
  let hash = Sc_rollup.Commitment.hash_uncarbonated commitment in
  let+ () = Store.Commitments.add store.commitments hash commitment in
  hash

let commitment_published_at_level {store; _} commitment =
  Store.Commitments_published_at_level.find
    store.commitments_published_at_level
    commitment

let set_commitment_published_at_level {store; _} =
  Store.Commitments_published_at_level.add store.commitments_published_at_level

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

let get_inbox {store; _} inbox_hash =
  let open Lwt_result_syntax in
  let* inbox = Store.Inboxes.read store.inboxes inbox_hash in
  match inbox with
  | None ->
      failwith "Could not retrieve inbox %a" Sc_rollup.Inbox.Hash.pp inbox_hash
  | Some (i, ()) -> return i

let find_inbox {store; _} hash =
  let open Lwt_result_syntax in
  let+ inbox = Store.Inboxes.read store.inboxes hash in
  Option.map fst inbox

let save_inbox {store; _} inbox =
  let open Lwt_result_syntax in
  let hash = Sc_rollup.Inbox.hash inbox in
  let+ () = Store.Inboxes.append store.inboxes ~key:hash ~value:inbox in
  hash

let find_inbox_by_block_hash ({store; _} as node_ctxt) block_hash =
  let open Lwt_result_syntax in
  let* header = Store.L2_blocks.header store.l2_blocks block_hash in
  match header with
  | None -> return_none
  | Some {inbox_hash; _} -> find_inbox node_ctxt inbox_hash

let genesis_inbox node_ctxt =
  let genesis_level = Raw_level.to_int32 node_ctxt.genesis_info.level in
  Plugin.RPC.Sc_rollup.inbox
    node_ctxt.cctxt
    (node_ctxt.cctxt#chain, `Level genesis_level)

let inbox_of_head node_ctxt Layer1.{hash = block_hash; level = block_level} =
  let open Lwt_result_syntax in
  let* possible_inbox = find_inbox_by_block_hash node_ctxt block_hash in
  (* Pre-condition: forall l. (l > genesis_level) => inbox[l] <> None. *)
  match possible_inbox with
  | None ->
      (* The inbox exists for each tezos block the rollup should care about.
         That is, every block after the origination level. We then join
         the bandwagon and build the inbox on top of the protocol's inbox
         at the end of the origination level. *)
      let genesis_level = Raw_level.to_int32 node_ctxt.genesis_info.level in
      if block_level = genesis_level then genesis_inbox node_ctxt
      else if block_level > genesis_level then
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
  predecessor_timestamp : Timestamp.t;
  messages : Sc_rollup.Inbox_message.t list;
}

let get_messages {store; _} messages_hash =
  let open Lwt_result_syntax in
  let* msg = Store.Messages.read store.messages messages_hash in
  match msg with
  | None ->
      failwith
        "Could not retrieve messages with payloads merkelized hash %a"
        Sc_rollup.Inbox_merkelized_payload_hashes.Hash.pp
        messages_hash
  | Some
      ( messages,
        (is_first_block, predecessor, predecessor_timestamp, _num_messages) ) ->
      return {is_first_block; predecessor; predecessor_timestamp; messages}

let find_messages {store; _} hash =
  let open Lwt_result_syntax in
  let+ msgs = Store.Messages.read store.messages hash in
  Option.map
    (fun ( messages,
           (is_first_block, predecessor, predecessor_timestamp, _num_messages)
         ) -> {is_first_block; predecessor; predecessor_timestamp; messages})
    msgs

let get_num_messages {store; _} hash =
  let open Lwt_result_syntax in
  let* header = Store.Messages.header store.messages hash in
  match header with
  | None ->
      failwith
        "Could not retrieve number of messages for inbox witness %a"
        Sc_rollup.Inbox_merkelized_payload_hashes.Hash.pp
        hash
  | Some (_first_block, _predecessor, _predecessor_timestamp, num_messages) ->
      return num_messages

let save_messages {store; _} key
    {is_first_block; predecessor; predecessor_timestamp; messages} =
  Store.Messages.append
    store.messages
    ~key
    ~header:
      (is_first_block, predecessor, predecessor_timestamp, List.length messages)
    ~value:messages

let get_full_l2_block node_ctxt block_hash =
  let open Lwt_result_syntax in
  let* block = get_l2_block node_ctxt block_hash in
  let* inbox = get_inbox node_ctxt block.header.inbox_hash
  and* {messages; _} = get_messages node_ctxt block.header.inbox_witness
  and* commitment =
    Option.map_es (get_commitment node_ctxt) block.header.commitment_hash
  in
  return {block with content = {Sc_rollup_block.inbox; messages; commitment}}

let get_slot_header {store; _} ~published_in_block_hash slot_index =
  Error.trace_lwt_result_with
    "Could not retrieve slot header for slot index %a published in block %a"
    Dal.Slot_index.pp
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
    (slot_header : Dal.Slot.Header.t) =
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

let find_confirmed_slots_history {store; _} =
  Store.Dal_confirmed_slots_history.find store.irmin_store

let save_confirmed_slots_history {store; _} =
  Store.Dal_confirmed_slots_history.add store.irmin_store

let find_confirmed_slots_histories {store; _} =
  Store.Dal_confirmed_slots_histories.find store.irmin_store

let save_confirmed_slots_histories {store; _} =
  Store.Dal_confirmed_slots_histories.add store.irmin_store

module Internal_for_tests = struct
  let create_node_context cctxt
      ?(constants = Default_parameters.constants_mainnet) ~data_dir kind =
    let open Lwt_result_syntax in
    let l2_blocks_cache_size = Configuration.default_l2_blocks_cache_size in
    let protocol_constants =
      constants
      |> Data_encoding.Binary.to_bytes_exn Constants.Parametric.encoding
      |> Data_encoding.Binary.of_bytes_exn Constants_parametric_repr.encoding
      |> Constants_repr.all_of_parametric
      |> Data_encoding.Binary.to_bytes_exn Constants_repr.encoding
      |> Data_encoding.Binary.of_bytes_exn Constants.encoding
    in
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
    let genesis_info =
      Sc_rollup.Commitment.{level = Raw_level.root; commitment_hash = Hash.zero}
    in
    let l1_ctxt = Layer1.Internal_for_tests.dummy cctxt in
    let lcc =
      Reference.new_
        {commitment = Sc_rollup.Commitment.Hash.zero; level = Raw_level.root}
    in
    let lpc = Reference.new_ None in
    return
      {
        cctxt;
        dal_cctxt = None;
        dac_client = None;
        data_dir;
        l1_ctxt;
        rollup_address = Sc_rollup.Address.zero;
        boot_sector_file = None;
        mode = Observer;
        operators = Configuration.Operator_purpose_map.empty;
        genesis_info;
        lcc;
        lpc;
        kind;
        injector_retention_period = 0;
        block_finality_time = 2;
        fee_parameters = Configuration.default_fee_parameters;
        protocol_constants;
        loser_mode = Loser_mode.no_failures;
        lockfile;
        store;
        context;
        kernel_debug_logger = Event.kernel_debug;
        finaliser = (fun () -> Lwt.return_unit);
      }
end
