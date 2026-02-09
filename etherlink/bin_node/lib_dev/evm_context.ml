(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

exception
  Divergence of {
    level : Ethereum_types.quantity;
    computed_block_hash : Ethereum_types.block_hash;
    expected_block_hash : Ethereum_types.block_hash;
  }

exception Unsync

open Evm_context_types

type init_status = Loaded | Created

type snapshot_source = Url_legacy of string

type head = {
  current_block_hash : Ethereum_types.block_hash;
  finalized_number : Ethereum_types.quantity;
  next_blueprint_number : Ethereum_types.quantity;
  evm_state : Evm_state.t;
  pending_upgrade : Evm_events.Upgrade.t option;
  pending_sequencer_upgrade : Evm_events.Sequencer_upgrade.t option;
}

type parameters = {
  configuration : Configuration.t;
  kernel_path : Pvm_types.kernel option;
  smart_rollup_address : string option;
  store_perm : Sqlite.perm;
  signer : Signer.map option;
  snapshot_source : snapshot_source option;
  tx_container : Services_backend_sig.ex_tx_container;
}

type future_block_info = {
  timestamp : Time.Protocol.t;
  next_tx_index : int32;
  applied_sequencer_upgrade : bool;
  da_fee_per_byte : Ethereum_types.quantity;
  base_fee_per_gas : Z.t;
}

type future_block_info_state =
  | Disabled
  | Awaiting_next_block_info
  | Executing of future_block_info

type session_state = {
  mutable context : Pvm.Context.rw;
  mutable storage_version : int;
  mutable finalized_number : Ethereum_types.quantity;
  mutable next_blueprint_number : Ethereum_types.quantity;
  mutable current_block_hash : Ethereum_types.block_hash;
  mutable pending_upgrade : Evm_store.pending_kernel_upgrade option;
  mutable pending_sequencer_upgrade :
    Evm_store.pending_sequencer_upgrade option;
  mutable evm_state : Evm_state.t;
  mutable last_split_block : (Ethereum_types.quantity * Time.Protocol.t) option;
      (** Garbage collector session related information. *)
  mutable post_transaction_run_hook : (unit -> unit Lwt.t) option;
      (** A function to be run by the worker at the end of the current
          {!Transaction.run} call. This can be used to delay some computation
          only once the commits in Irmin and SQlite have been confirmed. *)
  mutable future_block_info : future_block_info_state;
      (** Instant confirmation state machine:
          - [Disabled]: IC is not enabled (always for sequencer, or for
            observer when IC is not enabled)
          - [Awaiting_next_block_info]: IC enabled, waiting for valid block info
          - [Executing]: Actively executing transactions for IC *)
}

type t = {
  configuration : Configuration.t;
  index : Pvm.Context.rw_index;
  smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
  store : Evm_store.t;
  session : session_state;
  signer : Signer.map option;
  tx_container : Services_backend_sig.ex_tx_container;
  execution_pool : Lwt_domain.pool;
}

let is_sequencer t = Option.is_some t.signer

let pvm_config ctxt =
  Pvm.Kernel.config
    ~preimage_directory:(Configuration.preimages_path ctxt.configuration)
    ?preimage_endpoint:ctxt.configuration.kernel_execution.preimages_endpoint
    ~kernel_debug:true
    ~destination:ctxt.smart_rollup_address
    ~trace_host_funs:ctxt.configuration.opentelemetry.trace_host_functions
    ()

type error += Cannot_apply_blueprint of {local_state_level : Z.t}

type error +=
  | Incorrect_history_mode of {
      store_history_mode : Configuration.history_mode;
      history_mode : Configuration.history_mode;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_cannot_apply_blueprint"
    ~title:"Cannot apply a blueprint"
    ~description:
      "The EVM node could not apply a blueprint on top of its local EVM state."
    ~pp:(fun ppf local_state_level ->
      Format.fprintf
        ppf
        "The EVM node could not apply a blueprint on top of its local EVM \
         state at level %a."
        Z.pp_print
        local_state_level)
    Data_encoding.(obj1 (req "current_state_level" n))
    (function
      | Cannot_apply_blueprint {local_state_level} -> Some local_state_level
      | _ -> None)
    (fun local_state_level -> Cannot_apply_blueprint {local_state_level})

let () =
  register_error_kind
    `Permanent
    ~id:"evm_node_dev_incorrect_history_mode"
    ~title:"Incorrect history mode"
    ~description:"The EVM has stored data with a different history mode."
    ~pp:(fun ppf (store_history_mode, history_mode) ->
      Format.fprintf
        ppf
        "The EVM node has stored data with history mode %a, it cannot be run \
         with history mode %a without risk. Please use the command switch \
         history mode to change history mode for the node."
        Configuration.pp_history_mode_debug
        store_history_mode
        Configuration.pp_history_mode_debug
        history_mode)
    Data_encoding.(
      obj2
        (req "store_history_mode_level" Configuration.history_mode_encoding)
        (req "history_mode_level" Configuration.history_mode_encoding))
    (function
      | Incorrect_history_mode {store_history_mode; history_mode} ->
          Some (store_history_mode, history_mode)
      | _ -> None)
    (fun (store_history_mode, history_mode) ->
      Incorrect_history_mode {store_history_mode; history_mode})

module Types = struct
  type state = t

  type nonrec parameters = parameters
end

module Name = struct
  type t = unit

  let encoding = Data_encoding.unit

  let base = ["evm_node_worker"; "evm_context"]

  let pp _fmt () = ()

  let equal () () = true
end

let head_info, head_info_waker = Lwt.task ()

let init_status, init_status_waker = Lwt.task ()

let execution_config, execution_config_waker = Lwt.task ()

let lock_data_dir ~data_dir = Data_dir.lock ~data_dir

let head_watcher :
    ( Transaction_object.t,
      Transaction_receipt.t )
    Ethereum_types.Subscription.output
    Lwt_watcher.input =
  Lwt_watcher.create_input ()

let receipt_watcher : Transaction_receipt.t Lwt_watcher.input =
  Lwt_watcher.create_input ()

let l1_l2_levels_watcher :
    Ethereum_types.Subscription.l1_l2_levels_output Lwt_watcher.input =
  Lwt_watcher.create_input ()

module State = struct
  let current_blueprint_number ctxt =
    let (Qty next_blueprint_number) = ctxt.session.next_blueprint_number in
    Ethereum_types.Qty (Z.pred next_blueprint_number)

  module Transaction : sig
    (** [run ctxt k] executes [k ctxt' conn] where [ctxt'] is a copy of [ctxt]
        whose [session] field can be safely modified in-place and [conn] is a
        handler to a SQLite connection with a pre-started transaction.

        If [k] returns an [Error] or raises an exception, the changes made to
        [ctxt'] by [k] are not applied to [ctxt] (i.e., it is left unchanged),
        and the SQLite transaction is aborted. *)
    val run : t -> (t -> Sqlite.conn -> 'a tzresult Lwt.t) -> 'a tzresult Lwt.t

    val initialize_head_info : t -> unit
  end = struct
    let with_store_transaction ctxt k =
      Evm_store.use ctxt.store @@ fun conn ->
      Evm_store.with_transaction conn @@ fun conn -> k conn

    (* [dup session] creates a copy of [session] that can be safely modified
       without altering the initial value. *)
    let dup
        {
          context;
          storage_version;
          finalized_number;
          next_blueprint_number;
          current_block_hash;
          pending_upgrade;
          pending_sequencer_upgrade;
          evm_state;
          last_split_block;
          post_transaction_run_hook;
          future_block_info;
        } =
      {
        context;
        storage_version;
        finalized_number;
        next_blueprint_number;
        current_block_hash;
        pending_upgrade;
        pending_sequencer_upgrade;
        evm_state;
        last_split_block;
        post_transaction_run_hook;
        future_block_info;
      }

    (* [apply session session'] modifies [session] in-place to match the content of [session']. *)
    let apply session
        {
          context;
          storage_version;
          finalized_number;
          next_blueprint_number;
          current_block_hash;
          pending_upgrade;
          pending_sequencer_upgrade;
          evm_state;
          last_split_block;
          post_transaction_run_hook;
          future_block_info;
        } =
      session.context <- context ;
      session.storage_version <- storage_version ;
      session.finalized_number <- finalized_number ;
      session.next_blueprint_number <- next_blueprint_number ;
      session.current_block_hash <- current_block_hash ;
      session.pending_upgrade <- pending_upgrade ;
      session.pending_sequencer_upgrade <- pending_sequencer_upgrade ;
      session.evm_state <- evm_state ;
      session.last_split_block <- last_split_block ;
      session.post_transaction_run_hook <- post_transaction_run_hook ;
      session.future_block_info <- future_block_info

    let session_to_head_info session =
      {
        evm_state = session.evm_state;
        finalized_number = session.finalized_number;
        next_blueprint_number = session.next_blueprint_number;
        current_block_hash = session.current_block_hash;
        pending_upgrade =
          Option.map
            (fun pending_upgrade -> pending_upgrade.Evm_store.kernel_upgrade)
            session.pending_upgrade;
        pending_sequencer_upgrade =
          Option.map
            (fun pending_upgrade -> pending_upgrade.Evm_store.sequencer_upgrade)
            session.pending_sequencer_upgrade;
      }

    let run ctxt (k : t -> Sqlite.conn -> 'a tzresult Lwt.t) : 'a tzresult Lwt.t
        =
      let open Lwt_result_syntax in
      let ctxt' = {ctxt with session = dup ctxt.session} in
      let* res =
        with_store_transaction ctxt @@ fun conn ->
        let*! res = k ctxt' conn in
        match res with
        | Ok res ->
            apply ctxt.session ctxt'.session ;
            let*! head_info in
            head_info := session_to_head_info ctxt.session ;
            let (Qty level) = current_blueprint_number ctxt in
            let (Qty finalized_level) = ctxt.session.finalized_number in
            Metrics.set_confirmed_level ~level:finalized_level ;
            Metrics.set_level ~level ;
            return res
        | Error err -> fail err
      in
      let*! () =
        Option.iter_s
          (fun hook -> hook ())
          ctxt.session.post_transaction_run_hook
      in
      ctxt.session.post_transaction_run_hook <- None ;
      return res

    let initialize_head_info ctxt =
      let first_head = ref (session_to_head_info ctxt.session) in
      Lwt.wakeup head_info_waker first_head
  end

  let execute_in_place ctxt ?wasm_entrypoint ?execution_timestamp
      ~native_execution ~data_dir ~config inbox =
    let open Lwt_result_syntax in
    let* evm_state =
      Evm_state.execute
        ~pool:ctxt.execution_pool
        ?wasm_entrypoint
        ?execution_timestamp
        ~native_execution
        ~data_dir
        ~config
        ctxt.session.evm_state
        inbox
    in
    ctxt.session.evm_state <- evm_state ;
    return_unit

  let modify_in_place ctxt ~key ~value =
    let open Lwt_syntax in
    let* evm_state = Evm_state.modify ~key ~value ctxt.session.evm_state in
    ctxt.session.evm_state <- evm_state ;
    return_unit

  let delete_in_place ctxt ~kind key =
    let open Lwt_syntax in
    let* evm_state = Evm_state.delete ~kind ctxt.session.evm_state key in
    ctxt.session.evm_state <- evm_state ;
    return_unit

  let load ~l2_chains ~data_dir ~store_perm:perm index =
    let open Lwt_result_syntax in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let (Ex_chain_family chain_family) =
      Configuration.retrieve_chain_family ~l2_chains
    in
    let* store = Evm_store.init ~chain_family ~data_dir ~perm () in
    Evm_store.use store @@ fun conn ->
    let* latest = Evm_store.Context_hashes.find_latest conn in
    match latest with
    | Some (Qty latest_blueprint_number, checkpoint) ->
        let*! context = Pvm.Context.checkout_exn index checkpoint in
        let*! evm_state = Pvm.State.get context in
        let+ current_block_hash =
          Evm_state.current_block_hash ~chain_family evm_state
        in
        ( store,
          context,
          Ethereum_types.Qty Z.(succ latest_blueprint_number),
          current_block_hash,
          Loaded )
    | None ->
        let context = Pvm.Context.empty index in
        let genesis_parent_hash = L2_types.genesis_parent_hash ~chain_family in
        return
          ( store,
            context,
            Ethereum_types.Qty Z.zero,
            genesis_parent_hash,
            Created )

  let commit store (context : Pvm.Context.rw) evm_state number =
    let open Lwt_result_syntax in
    let*! context = Pvm.State.set context evm_state in
    let*! checkpoint = Pvm.Context.commit context in
    let* () = Evm_store.Context_hashes.store store number checkpoint in
    return context

  let gc ctxt conn gc_level head_level history_mode =
    let open Lwt_result_syntax in
    let*! () = Evm_context_events.gc_started ~gc_level ~head_level in
    let start_timestamp = Time.System.now () in
    let* hash = Evm_store.Context_hashes.find conn gc_level in
    let*? hash =
      Option.to_result
        ~none:
          [
            error_of_fmt
              "Context hash of GC candidate %a is missing from store"
              Ethereum_types.pp_quantity
              gc_level;
          ]
        hash
    in
    let* () = Evm_store.reset_before conn ~l2_level:gc_level ~history_mode in
    let*! () = Pvm.Context.gc ctxt.index hash in
    Metrics.start_pruning () ;
    let gc_waiter () =
      let open Lwt_syntax in
      let* () = Pvm.Context.wait_gc_completion ctxt.index in
      let stop_timestamp = Time.System.now () in
      Metrics.stop_pruning () ;
      Evm_context_events.gc_finished
        ~gc_level
        ~head_level
        (Ptime.diff stop_timestamp start_timestamp)
    in

    let data_dir = ctxt.configuration.data_dir in
    match ctxt.configuration.experimental_features.periodic_snapshot_path with
    | Some path ->
        let*! () =
          Lwt.catch gc_waiter (fun exn ->
              Metrics.stop_pruning () ;
              Evm_context_events.gc_waiter_failed exn ;
              Lwt.reraise exn)
        in
        let snapshot_file =
          if Filename.is_relative path then Filename.concat data_dir path
          else path
        in
        let* _dest =
          Snapshots_legacy.export
            ~snapshot_file
            ~compression:On_the_fly
            ~data_dir
            ()
        in
        return_unit
    | None ->
        Lwt.dont_wait gc_waiter (fun exn ->
            Metrics.stop_pruning () ;
            Evm_context_events.gc_waiter_failed exn) ;
        return_unit

  let maybe_split_context ctxt conn timestamp level =
    let open Lwt_result_syntax in
    let* history_mode = Evm_store.Metadata.get_history_mode conn in
    match history_mode with
    | Archive -> return_none
    | Rolling gc_param | Full gc_param ->
        let split =
          match ctxt.session.last_split_block with
          | None -> true
          | Some (_level, last_split_timestamp) ->
              let timestamp = Time.Protocol.to_seconds timestamp in
              let last_split_timestamp =
                Time.Protocol.to_seconds last_split_timestamp
              in
              Compare.Int64.(
                timestamp
                >= Int64.(
                     add
                       last_split_timestamp
                       (of_int gc_param.split_frequency_in_seconds)))
        in
        if split then (
          Pvm.Context.split ctxt.index ;
          let* () = Evm_store.Irmin_chunks.insert conn level timestamp in
          let*! () = Evm_context_events.gc_split level timestamp in
          let* chunk_needing_gc =
            Evm_store.Irmin_chunks.nth conn gc_param.number_of_chunks
          in
          match chunk_needing_gc with
          | Some (gc_level, _gc_timestamp) ->
              let* () = gc ctxt conn gc_level level history_mode in
              return_some (level, timestamp)
          | None -> return_none)
        else return_none

  let commit_next_head (ctxt : t) conn timestamp evm_state =
    let open Lwt_result_syntax in
    let* context =
      commit
        conn
        ctxt.session.context
        evm_state
        ctxt.session.next_blueprint_number
    in
    let* split_info =
      maybe_split_context ctxt conn timestamp ctxt.session.next_blueprint_number
    in
    return (context, split_info)

  let replace_current_commit (ctxt : t) conn =
    let open Lwt_result_syntax in
    let (Qty next) = ctxt.session.next_blueprint_number in
    let* context =
      commit
        conn
        ctxt.session.context
        ctxt.session.evm_state
        (Qty Z.(pred next))
    in
    ctxt.session.context <- context ;
    return_unit

  let read_from_state evm_state path =
    let open Lwt_result_syntax in
    let*! res = Evm_state.inspect evm_state path in
    return res

  let on_new_delayed_transaction ctxt ~delayed_transaction =
    let open Lwt_result_syntax in
    let*! data_dir, config = execution_config in
    let native_execution =
      ctxt.configuration.kernel_execution.native_execution_policy
      = Configuration.Always
    in
    if
      Storage_version.populate_delayed_inbox_disabled
        ~storage_version:ctxt.session.storage_version
    then
      execute_in_place
        ctxt
        ~native_execution
        ~data_dir
        ~config
        (`Inbox
           [
             "\254" ^ Bytes.to_string
             @@ Rlp.encode
                  (Evm_events.Delayed_transaction.to_rlp delayed_transaction);
           ])
    else
      let*! () =
        modify_in_place
          ctxt
          ~key:"/__delayed_input"
          ~value:
            (Bytes.to_string
            @@ Rlp.encode
                 (Evm_events.Delayed_transaction.to_rlp delayed_transaction))
      in
      execute_in_place
        ctxt
        ~wasm_entrypoint:"populate_delayed_inbox"
        ~native_execution
        ~data_dir
        ~config
        `Skip_stage_one

  let background_preemptive_download config upgrade_event =
    let open Lwt_syntax in
    let open Evm_events.Upgrade in
    let rec downloader root_hash preimages_endpoint preimages =
      Lwt.catch
        (fun () ->
          (* Download the kernel. *)
          Misc.unwrap_error_monad @@ fun () ->
          Kernel_download.download ~preimages ~preimages_endpoint ~root_hash ())
        (fun exn ->
          (*  Error handling. *)
          let* () =
            Events.predownload_kernel_failed
              root_hash
              [Error_monad.error_of_exn exn]
          in
          let* () = Lwt_unix.sleep 60.0 in
          downloader root_hash preimages_endpoint preimages)
    in
    match config.Configuration.kernel_execution.preimages_endpoint with
    | None -> ()
    | Some preimages_endpoint ->
        let (Hash (Hex root_hash)) = upgrade_event.hash in
        let root_hash = `Hex root_hash in
        Lwt.async (fun () ->
            downloader
              root_hash
              preimages_endpoint
              (Configuration.preimages_path config))

  let reset_to_level ctxt conn l2_level checkpoint =
    let open Lwt_result_syntax in
    let*! () = Evm_context_events.reset_at_level l2_level in
    (* Find the [l2_level] evm_state. *)
    let*! context = Pvm.Context.checkout_exn ctxt.index checkpoint in
    let*! evm_state = Pvm.State.get context in
    let* storage_version = Evm_state.storage_version evm_state in
    (* Clear the TX queue if needed, to preserve its invariants about nonces always increasing. *)
    let* () =
      let (Ex_tx_container tx_container) = ctxt.tx_container in
      let (module Tx_container) =
        Services_backend_sig.tx_container_module tx_container
      in
      Tx_container.clear ()
    in
    (* Clear the store. *)
    let* () = Evm_store.reset_after conn ~l2_level in
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    let* pending_sequencer_upgrade =
      Evm_store.Sequencer_upgrades.find_latest_pending conn
    in
    (* Update mutable session values. *)
    let next_blueprint_number = Ethereum_types.Qty.next l2_level in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let (Ex_chain_family chain_family) =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in
    let* current_block_hash =
      Evm_state.current_block_hash ~chain_family evm_state
    in
    ctxt.session.storage_version <- storage_version ;
    ctxt.session.next_blueprint_number <- next_blueprint_number ;
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.current_block_hash <- current_block_hash ;
    ctxt.session.context <- context ;
    ctxt.session.pending_upgrade <- pending_upgrade ;
    ctxt.session.pending_sequencer_upgrade <- pending_sequencer_upgrade ;
    return_unit

  let reset_to_finalized_level exit_error ctxt conn =
    let open Lwt_result_syntax in
    let* safe_checkpoint = Evm_store.Context_hashes.find_finalized conn in
    match safe_checkpoint with
    | Some (finalized_number, checkpoint) ->
        reset_to_level ctxt conn finalized_number checkpoint
    | None ->
        let*! () =
          Evm_context_events.reset_impossible_missing_finalized_state ()
        in
        tzfail exit_error

  let blueprint_applied_event ctxt conn
      ({number = Qty number; hash = expected_block_hash} :
        Evm_events.Blueprint_applied.t) =
    let open Lwt_result_syntax in
    (* We use [max] to not rely on the order of the EVM events (because
       it is possible to see several blueprints applied during on L1
       level). *)
    let (Qty current_finalized) = ctxt.session.finalized_number in
    let latest_finalized_level = Z.max current_finalized number in
    ctxt.session.finalized_number <- Qty latest_finalized_level ;
    let* block_hash_opt =
      Evm_store.Blocks.find_hash_of_number conn (Qty number)
    in
    match block_hash_opt with
    | Some found_block_hash ->
        if found_block_hash = expected_block_hash then
          let*! () =
            Evm_events_follower_events.upstream_blueprint_applied
              (number, expected_block_hash)
          in
          return_unit
        else
          let*! () =
            Evm_events_follower_events.diverged
              (number, expected_block_hash, found_block_hash)
          in
          let exit_error =
            Node_error.Diverged
              {
                level = number;
                expected_block_hash;
                found_block_hash = Some found_block_hash;
                must_exit = true;
              }
          in
          if is_sequencer ctxt then
            (* Sequencer must fail in case of divergence. *)
            tzfail exit_error
          else
            (* Observers needs to reset at finalized level. *)
            let* () = reset_to_finalized_level exit_error ctxt conn in
            return_unit
    | None when is_sequencer ctxt ->
        let*! () =
          Evm_events_follower_events.missing_blueprint
            (number, expected_block_hash)
        in
        tzfail
          (Node_error.Diverged
             {
               level = number;
               expected_block_hash;
               found_block_hash = None;
               must_exit = true;
             })
    | None ->
        let*! () = Evm_events_follower_events.rollup_node_ahead (Qty number) in
        Evm_store.Pending_confirmations.insert
          conn
          (Qty number)
          expected_block_hash

  let check_pending_upgrade ctxt timestamp =
    match ctxt.session.pending_upgrade with
    | None -> None
    | Some upgrade ->
        if Time.Protocol.(upgrade.kernel_upgrade.timestamp <= timestamp) then
          Some upgrade
        else None

  let check_pending_sequencer_upgrade ctxt timestamp =
    match ctxt.session.pending_sequencer_upgrade with
    | None -> None
    | Some sequencer_upgrade ->
        if
          Time.Protocol.(
            sequencer_upgrade.sequencer_upgrade.timestamp <= timestamp)
        then Some sequencer_upgrade
        else None

  let check_upgrade ctxt evm_state =
    let open Lwt_result_syntax in
    function
    | Some
        Evm_store.{kernel_upgrade = Evm_events.Upgrade.{hash = root_hash; _}; _}
      ->
        let*! bytes =
          Evm_state.inspect evm_state Durable_storage_path.kernel_root_hash
        in
        let new_hash_candidate =
          Option.map
            (fun bytes ->
              let (`Hex hex) = Hex.of_bytes bytes in
              Ethereum_types.hash_of_string hex)
            bytes
        in

        let*! () =
          match new_hash_candidate with
          | Some current_root_hash when root_hash = current_root_hash ->
              Events.applied_upgrade
                root_hash
                ctxt.session.next_blueprint_number
          | _ ->
              Events.failed_upgrade root_hash ctxt.session.next_blueprint_number
        in

        (* Even if the upgrade failed, we consider it has been applied. *)
        return_true
    | None -> return_false

  let check_sequencer_upgrade ctxt
      Evm_events.Sequencer_upgrade.{sequencer = new_sequencer; _} =
    let open Lwt_result_syntax in
    let*! bytes =
      Evm_state.inspect
        ctxt.session.evm_state
        Durable_storage_path.sequencer_key
    in
    let*? current_sequencer =
      Option.map_e
        (fun b -> Signature.Public_key.of_b58check (String.of_bytes b))
        bytes
    in
    match current_sequencer with
    | Some current_sequencer when new_sequencer = current_sequencer ->
        let*! () =
          Events.applied_sequencer_upgrade
            new_sequencer
            ctxt.session.next_blueprint_number
        in
        return_true
    | _ ->
        let*! () =
          Events.failed_sequencer_upgrade
            ~new_sequencer
            ~found_sequencer:current_sequencer
            ctxt.session.next_blueprint_number
        in
        return_false

  let execution_gas ~base_fee_per_gas ~da_fee_per_byte receipt object_ =
    let da_fees =
      Fees.gas_used_for_da_fees
        ~da_fee_per_byte
        ~base_fee_per_gas
        Ethereum_types.(object_.input |> hex_to_real_bytes)
    in
    let (Qty gas_used) = receipt.Transaction_receipt.gasUsed in
    Z.sub gas_used da_fees

  let store_block_unsafe ?tez_block ~base_fee_per_gas ~da_fee_per_byte conn
      evm_state block =
    let open Lwt_result_syntax in
    (* Store the block itself. *)
    let* () = Evm_store.Blocks.store ?tez_block conn block in
    (* Store all transactions from the block. *)
    match block.transactions with
    | Ethereum_types.TxHash hashes ->
        let* storage_version =
          Durable_storage.storage_version (read_from_state evm_state)
        in
        if not (Storage_version.legacy_storage_compatible ~storage_version) then
          let* receipts =
            Etherlink_durable_storage.current_transactions_receipts
              block.hash
              storage_version
              (read_from_state evm_state)
          in
          let* transaction_objects =
            Etherlink_durable_storage.current_transactions_objects
              ~block_hash:block.hash
              storage_version
              (read_from_state evm_state)
          in
          let mismatch_error =
            error_of_fmt
              "Inconsistent durable storage: %d receipts and %d transaction \
               objects for block %a"
              (List.length receipts)
              (List.length transaction_objects)
              Ethereum_types.pp_block_hash
              block.hash
          in
          let* receipts_and_objects =
            List.fold_left2_es
              ~when_different_lengths:(TzTrace.make mismatch_error)
              (fun acc receipt object_ -> return ((object_, receipt) :: acc))
              []
              receipts
              transaction_objects
          in
          let store_info_and_acc_gas cumulative_execution_gas (object_, receipt)
              =
            let info = Transaction_info.of_receipt_and_object receipt object_ in
            let* () = Evm_store.Transactions.store conn info in
            return
              ( Z.add
                  cumulative_execution_gas
                  (execution_gas
                     ~base_fee_per_gas
                     ~da_fee_per_byte
                     receipt
                     object_),
                receipt )
          in
          let gas_and_receipts =
            List.fold_left_map_es
              store_info_and_acc_gas
              Z.zero
              receipts_and_objects
          in
          gas_and_receipts
        else
          List.fold_left_map_es
            (fun cumulative_execution_gas hash ->
              let* receipt =
                let* receipt_opt =
                  Etherlink_durable_storage.transaction_receipt
                    (read_from_state evm_state)
                    ~block_hash:block.hash
                    hash
                in
                match receipt_opt with
                | Some receipt -> return receipt
                | None ->
                    failwith
                      "Receipt missing for %a"
                      Ethereum_types.pp_hash
                      hash
              in
              let* object_ =
                let* object_opt =
                  Etherlink_durable_storage.transaction_object
                    (read_from_state evm_state)
                    hash
                in
                match object_opt with
                | Some object_ -> return object_
                | None ->
                    failwith "Object missing for %a" Ethereum_types.pp_hash hash
              in
              let info =
                Transaction_info.of_receipt_and_object receipt object_
              in
              let* () = Evm_store.Transactions.store conn info in
              return
                ( Z.add
                    cumulative_execution_gas
                    (execution_gas
                       ~base_fee_per_gas
                       ~da_fee_per_byte
                       receipt
                       object_),
                  receipt ))
            Z.zero
            hashes
    | TxFull _ ->
        (* Block must be read without the transactions objects. Even though
           we could be resilient and store the objects, it doesn't make sense
           at the moment in the code to deal this case. The node does not need
           to read the full block on block processing. If for some reasons
           the code reads the full block, simply store the transactions objects
           and fetch the receipts here. *)
        assert false

  let store_tez_block_unsafe conn block =
    let open Lwt_result_syntax in
    (* Store the block itself. *)
    let* () = Evm_store.Blocks.tez_store conn block in
    return_nil

  (** [store_finalized_levels ... ~l1_level ~start_l2_level
      ~end_l2_level] will:
      - store the mapping between the [l1_level] and the
        [current_blueprint_number] into [L1_l2_levels_relationships]
        table. This is needed even for nodes not tracking a rollup
        node because the [Evm_store.Context_hashes.find_finalized]
        returns the minimum between the current blueprint and the last
        L2 finalized level known,

      - store the mapping between the [l1_level] and its corresponding
        L2 level range [start_l2_level; end_l2_level] into
        [L1_l2_finalized_levels.store],

      - updates [Metrics.l1_level] metrics,

      - broadcast the mapping to the [l1_l2_levels_watcher] stream
        subscribers. *)
  let store_finalized_levels ctxt conn ~l1_level ~start_l2_level ~end_l2_level =
    let open Lwt_result_syntax in
    let* () =
      Evm_store.L1_l2_levels_relationships.store
        conn
        ~l1_level
        ~latest_l2_level:(current_blueprint_number ctxt)
    in
    let* () =
      Evm_store.L1_l2_finalized_levels.store
        conn
        ~l1_level
        ~start_l2_level
        ~end_l2_level
    in
    Metrics.set_l1_level ~level:l1_level ;
    Broadcast.notify_finalized_levels ~l1_level ~start_l2_level ~end_l2_level ;
    Lwt_watcher.notify
      l1_l2_levels_watcher
      {l1_level; start_l2_level; end_l2_level} ;
    return_unit

  let handle_sequencer_upgrade ctxt conn ~timestamp ~data_dir ~config =
    let open Lwt_result_syntax in
    match check_pending_sequencer_upgrade ctxt timestamp with
    | Some {sequencer_upgrade; _} ->
        (* The sequencer upgrade is based on the l1 timestamp
           found in the inbox. To activate it it's enough to
           trigger a kernel run with a correct timestamp (>=
           activation_timestamp) and an empty inbox. The sequencer
           upgrade is checked in the kernel just after finishing
           to parse the inbox, there is no need for a blueprint to
           be applied.

           Also as the application of the sequencer upgrade is
           done after parsing the blueprint and it clears the
           storage. If we try to do a run with a blueprint :

           - For the previous sequencer, the blueprint will be
           validated but removed when the sequencer upgrade is
           applied.

           - for the new sequencer, the blueprint will not be
           validated.

           Because of all that it's necessary to do a run with no
           specified blueprint. *)
        let* () =
          execute_in_place
            ctxt
            ~execution_timestamp:timestamp
            ~native_execution:
              (ctxt.configuration.kernel_execution.native_execution_policy
             = Configuration.Always)
            ~data_dir
            ~config
            (`Inbox [])
        in
        let* applied_sequencer_upgrade =
          check_sequencer_upgrade ctxt sequencer_upgrade
        in
        let* () =
          when_ applied_sequencer_upgrade @@ fun () ->
          Evm_store.Sequencer_upgrades.record_apply
            conn
            ctxt.session.next_blueprint_number
        in
        return applied_sequencer_upgrade
    | None -> return false

  let set_next_block_info ctxt conn (timestamp : Time.Protocol.t)
      (number : Ethereum_types.quantity) =
    let open Lwt_result_syntax in
    match ctxt.session.future_block_info with
    | Disabled -> return_unit
    | Executing _ ->
        tzfail
          (Node_error.Set_next_block_info_while_executing
             {
               new_level = number;
               current_level = ctxt.session.next_blueprint_number;
             })
    | Awaiting_next_block_info ->
        if Ethereum_types.Qty.(number = ctxt.session.next_blueprint_number) then (
          Octez_telemetry.Trace.add_attrs (fun () ->
              [Telemetry.Attributes.Block.number number]) ;
          let*! data_dir, config = execution_config in
          let* applied_sequencer_upgrade =
            handle_sequencer_upgrade ctxt conn ~timestamp ~data_dir ~config
          in
          let* da_fee_per_byte =
            Etherlink_durable_storage.da_fee_per_byte
              (read_from_state ctxt.session.evm_state)
          in
          let* (Ethereum_types.Qty base_fee_per_gas) =
            Etherlink_durable_storage.base_fee_per_gas
              (read_from_state ctxt.session.evm_state)
          in
          ctxt.session.future_block_info <-
            Executing
              {
                timestamp;
                next_tx_index = 0l;
                applied_sequencer_upgrade;
                da_fee_per_byte;
                base_fee_per_gas;
              } ;
          return_unit)
        else (
          ctxt.session.future_block_info <- Awaiting_next_block_info ;
          return_unit)

  let compute_execution_gas ~tx ~da_fee_per_byte ~base_fee_per_gas ~gas_used =
    match tx with
    | Broadcast.Common (Evm raw_tx) -> (
        match Transaction_object.decode raw_tx with
        | Ok transaction_object ->
            let input_hex = Transaction_object.input transaction_object in
            let da_fees =
              Fees.gas_used_for_da_fees
                ~da_fee_per_byte
                ~base_fee_per_gas
                (Ethereum_types.hex_to_real_bytes input_hex)
            in
            Z.sub gas_used da_fees
        | Error _ -> gas_used)
    | Common (Michelson _) | Delayed _ ->
        (* TODO: Update computation for Michelson case *)
        gas_used

  let execute_single_transaction ctxt (tx : Broadcast.transaction) hash =
    let open Lwt_result_syntax in
    match ctxt.session.future_block_info with
    | Executing
        ({timestamp; next_tx_index; da_fee_per_byte; base_fee_per_gas; _} as
         future_block_info) ->
        let*! data_dir, config = execution_config in
        let* receipt, evm_state =
          Evm_state.execute_single_transaction
            ~pool:ctxt.execution_pool
            ~native_execution:
              (ctxt.configuration.kernel_execution.native_execution_policy
             = Always)
            ~data_dir
            ~config
            ctxt.session.evm_state
            {
              timestamp;
              number = ctxt.session.next_blueprint_number;
              transactions_count = next_tx_index;
            }
            hash
            tx
        in
        Octez_telemetry.Trace.add_attrs (fun () ->
            let (Ethereum_types.Qty gas_used) = receipt.gasUsed in
            let execution_gas =
              compute_execution_gas
                ~tx
                ~da_fee_per_byte
                ~base_fee_per_gas
                ~gas_used
            in
            [Telemetry.Attributes.Transaction.execution_gas execution_gas]) ;
        ctxt.session.evm_state <- evm_state ;
        ctxt.session.future_block_info <-
          Executing
            {future_block_info with next_tx_index = Int32.succ next_tx_index} ;
        let*! () = Events.single_tx_execution_done hash in
        return_some receipt
    | Disabled -> return_none
    | Awaiting_next_block_info ->
        tzfail
          (Node_error.Execute_single_transaction_no_block_info
             {transaction_hash = hash})

  let commit_application_result ~ctxt ~conn ~timestamp ~time_processed ~payload
      ~block ~chain_family evm_state =
    let open Lwt_result_syntax in
    let block_number = L2_types.block_number block in
    let block_hash = L2_types.block_hash block in

    (* Check if observer diverged *)
    let* () =
      if is_sequencer ctxt then return_unit
      else
        let* finalized_hash =
          Evm_store.Pending_confirmations.find_with_level conn block_number
        in
        match finalized_hash with
        | None -> return_unit
        | Some expected_block_hash ->
            if expected_block_hash = block_hash then
              Evm_store.Pending_confirmations.delete_with_level
                conn
                block_number
            else
              let Ethereum_types.(Qty number) = block_number in

              let*! () =
                Evm_events_follower_events.diverged
                  (number, expected_block_hash, block_hash)
              in
              (* If the observer cannot reset to finalized level it must exit. *)
              let exit_error =
                Node_error.Diverged
                  {
                    level = number;
                    expected_block_hash;
                    found_block_hash = Some block_hash;
                    must_exit = true;
                  }
              in
              let* () = reset_to_finalized_level exit_error ctxt conn in
              (* If the observer managed to reset to finalized level it must not exit. *)
              tzfail
                (Node_error.Diverged
                   {
                     level = number;
                     expected_block_hash;
                     found_block_hash = Some block_hash;
                     must_exit = false;
                   })
    in

    (* Set metrics for the block *)
    let () =
      match block with
      | Eth block ->
          let number_of_transactions =
            match block.transactions with
            | TxHash l -> List.length l
            | TxFull l -> List.length l
          in
          Metrics.set_block ~time_processed ~transactions:number_of_transactions ;
          Option.iter
            (fun baseFeePerGas ->
              baseFeePerGas |> Ethereum_types.Qty.to_z |> Metrics.set_gas_price)
            block.baseFeePerGas
      | Tez _ ->
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
          ()
    in
    let* payload in
    let* () =
      Evm_store.Blueprints.store
        conn
        {number = block_number; timestamp; payload}
    in

    (* TezosX: if the Tezos runtime is active, retrieve the Tezos block
       produced alongside the primary EVM block so we can store both in a
       single INSERT. *)
    let* tezosx_tez_block =
      let* runtimes =
        Durable_storage.list_runtimes (read_from_state evm_state)
      in
      if List.mem ~equal:( = ) Tezosx.Tezos runtimes then
        let* tez_block =
          Evm_state.retrieve_block_at_root
            ~chain_family:Michelson
            ~root:Durable_storage_path.tezosx_tezos_blocks_root
            evm_state
        in
        return
          (match tez_block with Some (Tez block) -> Some block | _ -> None)
      else return_none
    in

    (* Store block and share receipts *)
    let* evm_state, receipts, execution_gas =
      let* execution_gas, receipts =
        match block with
        | Eth block ->
            let* da_fee_per_byte =
              Etherlink_durable_storage.da_fee_per_byte
                (read_from_state evm_state)
            in
            let* (Qty base_fee_per_gas) =
              Etherlink_durable_storage.base_fee_per_gas
                (read_from_state evm_state)
            in
            store_block_unsafe
              ?tez_block:tezosx_tez_block
              ~da_fee_per_byte
              ~base_fee_per_gas
              conn
              evm_state
              block
        | Tez block ->
            let+ receipts = store_tez_block_unsafe conn block in
            (* TODO: support extracting the execution gas *)
            (Z.zero, receipts)
      in
      let*! evm_state =
        Evm_state.clear_block_storage chain_family block evm_state
      in
      return (evm_state, receipts, execution_gas)
    in

    List.iter (Lwt_watcher.notify receipt_watcher) receipts ;

    Octez_telemetry.Trace.add_attrs (fun () ->
        Telemetry.Attributes.
          [
            Block.number ctxt.session.next_blueprint_number;
            Block.transaction_count (List.length receipts);
            Block.execution_gas execution_gas;
          ]) ;

    (* Look for potential kernel upgrades *)
    let upgrade_candidate = check_pending_upgrade ctxt timestamp in
    let* applied_kernel_upgrade =
      check_upgrade ctxt evm_state upgrade_candidate
    in
    let* () =
      when_ applied_kernel_upgrade @@ fun () ->
      Evm_store.Kernel_upgrades.record_apply
        conn
        ctxt.session.next_blueprint_number
    in

    let* context, split_info = commit_next_head ctxt conn timestamp evm_state in

    let* evm_state =
      if ctxt.session.storage_version >= 43 then return evm_state
      else Lwt_result.ok (Evm_state.clear_events evm_state)
    in

    return
      (evm_state, context, applied_kernel_upgrade, split_info, execution_gas)

  let reset_future_block_info_if_enabled ctxt =
    match ctxt.session.future_block_info with
    | Disabled -> ()
    | Awaiting_next_block_info | Executing _ ->
        ctxt.session.future_block_info <- Awaiting_next_block_info ;
        ()

  (* Helper to find Divergence or Unsync in error trace.
    The exception emitted by `apply_blueprint_store_unsafe` is caught by
    `Sqlite.with_transaction` and converted to `Error`, so we need to pattern match
    on the error result rather than using `Lwt.catch` to find retry cases. *)
  let find_divergence_or_unsync trace =
    List.find_map
      (function
        | Exn
            (Divergence
               {
                 level = Qty level;
                 computed_block_hash = block_hash;
                 expected_block_hash;
               }) ->
            Some (`Divergence (level, block_hash, expected_block_hash))
        | Exn Unsync -> Some `Unsync
        | _ -> None)
      trace

  (** [apply_blueprint_store_unsafe ctxt conn timestamp chunks payload
      delayed_transactions] applies the blueprint [chunks] on the head of
      [ctxt], and commit the resulting state and the blueprint signed [payload]
      (when the promise is fulfilled) to Irmin and the node’s store.

      If instant confirmations are enabled, the block is instead assembled using the
      previously validated transactions. Resulting block hash is then matched against
      the one provided by the sequencer. If they differ, reset the current block state
      and apply the blueprint chunks through the standard procedure.

      This function expects its connection to the store [conn] to be wrapped in
      a SQL transaction. *)
  let apply_blueprint_store_unsafe ctxt conn timestamp chunks payload
      delayed_transactions sequencer_block_hash =
    let open Lwt_result_syntax in
    Evm_store.assert_in_transaction conn ;

    let*! data_dir, config = execution_config in
    let (Qty next) = ctxt.session.next_blueprint_number in
    let* () =
      List.iter_es
        (Evm_store.Delayed_transactions.store conn (Qty next))
        delayed_transactions
    in

    let time_processed = ref Ptime.Span.zero in

    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let (Ex_chain_family chain_family) =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in

    let* try_apply, applied_sequencer_upgrade =
      Misc.with_timing
        (fun time -> Lwt.return (time_processed := time))
        (fun () ->
          match ctxt.session.future_block_info with
          | Executing {timestamp; applied_sequencer_upgrade; _} ->
              let+ result =
                Evm_state.assemble_block
                  ~pool:ctxt.execution_pool
                  ~native_execution:
                    (ctxt.configuration.kernel_execution.native_execution_policy
                   = Always)
                  ~chain_family
                  ~data_dir
                  ~config
                  ~timestamp
                  ~number:ctxt.session.next_blueprint_number
                  ctxt.session.evm_state
              in
              (result, applied_sequencer_upgrade)
          | Disabled | Awaiting_next_block_info ->
              let* applied_sequencer_upgrade =
                handle_sequencer_upgrade ctxt conn ~timestamp ~data_dir ~config
              in
              let+ result =
                Evm_state.apply_unsigned_chunks
                  ~pool:ctxt.execution_pool
                  ~native_execution_policy:
                    ctxt.configuration.kernel_execution.native_execution_policy
                  ~wasm_pvm_fallback:(not @@ List.is_empty delayed_transactions)
                  ~data_dir
                  ~chain_family
                  ~config
                  ctxt.session.evm_state
                  chunks
              in
              (result, applied_sequencer_upgrade))
    in

    match try_apply with
    | Apply_success {evm_state; block} -> (
        let block_hash = L2_types.block_hash block in
        match (ctxt.session.future_block_info, sequencer_block_hash) with
        (* When IC is executing and sequencer hash is present:
            Node could execute txns incrementally and it received the block hash
            for verification *)
        | Executing _, Some sequencer_block_hash
          when sequencer_block_hash <> block_hash ->
            let*! () = Events.assemble_block_diverged next in
            Lwt.fail
              (Divergence
                 {
                   level = L2_types.block_number block;
                   expected_block_hash = sequencer_block_hash;
                   computed_block_hash = block_hash;
                 })
        (* When observer, future info is present but no sequencer hash was received:
            Should not happen but if it does we re-apply the full blueprint
            as we have no way of checking the assemble validity *)
        | Executing _, None ->
            let*! () = Events.seq_block_hash_missing next in
            Lwt.fail Unsync
        (* Any other case is standard procedure *)
        | _ ->
            reset_future_block_info_if_enabled ctxt ;
            let* ( evm_state,
                   context,
                   applied_kernel_upgrade,
                   split_info,
                   execution_gas ) =
              commit_application_result
                ~ctxt
                ~conn
                ~timestamp
                ~time_processed:!time_processed
                ~payload
                ~block
                ~chain_family
                evm_state
            in
            return
              ( evm_state,
                context,
                block,
                applied_kernel_upgrade,
                applied_sequencer_upgrade,
                split_info,
                execution_gas ))
    | Apply_failure (* Did not produce a block *) ->
        let*! () =
          if is_sequencer ctxt then
            Blueprint_events.invalid_blueprint_produced next
          else Blueprint_events.invalid_blueprint_applied next
        in
        tzfail (Cannot_apply_blueprint {local_state_level = Z.pred next})

  let on_new_head ?split_info ctxt ~applied_kernel_upgrade
      ~applied_sequencer_upgrade evm_state context block blueprint_with_events =
    let open Lwt_syntax in
    let block_hash = L2_types.block_hash block in
    let (Qty level) = ctxt.session.next_blueprint_number in
    ctxt.session.evm_state <- evm_state ;
    ctxt.session.context <- context ;
    ctxt.session.next_blueprint_number <- Qty (Z.succ level) ;
    ctxt.session.current_block_hash <- block_hash ;
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7865 *)
    (match block with
    | Eth block ->
        Lwt_watcher.notify
          head_watcher
          (Ethereum_types.Subscription.NewHeads block)
    | Tez _ ->
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/7866 *)
        ()) ;
    Option.iter
      (fun (split_level, split_timestamp) ->
        ctxt.session.last_split_block <- Some (split_level, split_timestamp))
      split_info ;
    if applied_sequencer_upgrade then
      ctxt.session.pending_sequencer_upgrade <- None ;
    let* () =
      if applied_kernel_upgrade then (
        let* storage_version = Evm_state.storage_version evm_state in
        ctxt.session.pending_upgrade <- None ;
        Result.iter
          (* Etherlink kernel always set a storage version, the error case
             should never happen. *)
          (fun storage_version ->
            ctxt.session.storage_version <- storage_version)
          storage_version ;
        return_unit)
      else return_unit
    in
    ctxt.session.post_transaction_run_hook <-
      Some
        (fun () ->
          let open Lwt_syntax in
          Broadcast.notify_block_hash block_hash ;
          Broadcast.notify_blueprint blueprint_with_events ;
          (* We ignore failure. A failure means the prevalidator is not yet
             started, meaning the call is useless for now. *)
          let* _result = Prevalidator.refresh_state () in
          return_unit) ;
    return_unit

  type error +=
    | Invalid_rollup_node of {
        smart_rollup_address : Tezos_crypto.Hashed.Smart_rollup_address.t;
        rollup_node_smart_rollup_address :
          Tezos_crypto.Hashed.Smart_rollup_address.t;
      }

  let prepare_local_flushed_blueprint ctxt parent_hash
      Evm_events.Flushed_blueprint.
        {transactions; timestamp; level = flushed_level} =
    let open Lwt_result_syntax in
    let sign ~signer chunks =
      let+ blueprint_chunks = Sequencer_blueprint.sign ~signer ~chunks in
      Sequencer_blueprint.create_inbox_payload
        ~smart_rollup_address:
          (Tezos_crypto.Hashed.Smart_rollup_address.to_string
             ctxt.smart_rollup_address)
        ~chunks:blueprint_chunks
    in
    let hashes =
      List.map (fun tx -> tx.Evm_events.Delayed_transaction.hash) transactions
    in
    let* signer =
      match ctxt.signer with
      | None ->
          failwith "Only sequencer is capable to handle the flushed blueprint"
      | Some signer -> return signer
    in
    let chunks =
      Sequencer_blueprint.make_blueprint_chunks
        ~number:flushed_level
        {
          parent_hash;
          delayed_transactions = hashes;
          transactions = [];
          timestamp;
        }
    in

    let* sequencer =
      Durable_storage.sequencer (read_from_state ctxt.session.evm_state)
    in
    let*? signer = Signer.get_signer signer sequencer in
    return (chunks, sign ~signer chunks)

  let clear_head_delayed_inbox ctxt =
    let open Lwt_result_syntax in
    let*! cleaned_evm_state =
      Evm_state.clear_delayed_inbox ctxt.session.evm_state
    in
    ctxt.session.evm_state <- cleaned_evm_state ;
    return_unit

  let rec apply_blueprint ?(events = []) ?expected_block_hash ctxt conn
      timestamp chunks payload delayed_transactions :
      'a L2_types.block tzresult Lwt.t =
    let open Lwt_result_syntax in
    let+ current_block, _execution_gas =
      Misc.with_timing_f_e (fun (block, execution_gas) ->
          Blueprint_events.blueprint_applied block execution_gas)
      @@ fun () ->
      let* ( evm_state,
             context,
             current_block,
             applied_kernel_upgrade,
             applied_sequencer_upgrade,
             split_info,
             execution_gas ) =
        let* () = apply_evm_events conn ctxt events in
        apply_blueprint_store_unsafe
          ctxt
          conn
          timestamp
          chunks
          payload
          delayed_transactions
          expected_block_hash
      in
      let kernel_upgrade =
        match ctxt.session.pending_upgrade with
        | Some {injected_before; kernel_upgrade}
          when injected_before = ctxt.session.next_blueprint_number ->
            Some kernel_upgrade
        | _ -> None
      in

      let sequencer_upgrade =
        match ctxt.session.pending_sequencer_upgrade with
        | Some {injected_before; sequencer_upgrade}
          when injected_before = ctxt.session.next_blueprint_number ->
            Some sequencer_upgrade
        | _ -> None
      in

      let* payload in

      let* current_block =
        match current_block with
        | Eth block ->
            let*? current_block =
              Transaction_object.reconstruct_block payload block
            in
            return (L2_types.Eth current_block)
        | Tez block -> return (L2_types.Tez block)
      in

      let*! () =
        on_new_head
          ?split_info
          ctxt
          ~applied_kernel_upgrade
          ~applied_sequencer_upgrade
          evm_state
          context
          current_block
          {
            delayed_transactions;
            kernel_upgrade;
            sequencer_upgrade;
            blueprint =
              {number = ctxt.session.next_blueprint_number; timestamp; payload};
          }
      in
      return (current_block, execution_gas)
    in
    current_block

  and apply_evm_event_unsafe ctxt conn event =
    let open Lwt_result_syntax in
    let*! () = Evm_events_follower_events.new_event event in
    match event with
    | Evm_events.Upgrade_event upgrade ->
        ctxt.session.pending_upgrade <-
          Some
            {
              kernel_upgrade = upgrade;
              injected_before = ctxt.session.next_blueprint_number;
            } ;
        background_preemptive_download ctxt.configuration upgrade ;
        let payload = Evm_events.Upgrade.to_bytes upgrade |> String.of_bytes in
        let*! () =
          modify_in_place
            ctxt
            ~key:Durable_storage_path.kernel_upgrade
            ~value:payload
        in
        let* () =
          Evm_store.Kernel_upgrades.store
            conn
            ctxt.session.next_blueprint_number
            upgrade
        in
        let*! () = Events.pending_upgrade upgrade in
        return_unit
    | Sequencer_upgrade_event sequencer_upgrade ->
        ctxt.session.pending_sequencer_upgrade <-
          Some
            {
              sequencer_upgrade;
              injected_before = ctxt.session.next_blueprint_number;
            } ;
        let payload =
          Evm_events.Sequencer_upgrade.to_bytes sequencer_upgrade
          |> String.of_bytes
        in
        let*! () =
          modify_in_place
            ctxt
            ~key:Durable_storage_path.sequencer_upgrade
            ~value:payload
        in
        let* () =
          Evm_store.Sequencer_upgrades.store
            conn
            ctxt.session.next_blueprint_number
            sequencer_upgrade
        in
        let*! () = Events.pending_sequencer_upgrade sequencer_upgrade in
        return_unit
    | Blueprint_applied event -> blueprint_applied_event ctxt conn event
    | New_delayed_transaction delayed_transaction ->
        on_new_delayed_transaction ctxt ~delayed_transaction
    | Flush_delayed_inbox flushed_blueprint ->
        let*! () =
          Evm_events_follower_events.flush_delayed_inbox
            ~timestamp:flushed_blueprint.timestamp
            flushed_blueprint.level
        in
        flush_delayed_inbox ctxt conn flushed_blueprint

  and apply_evm_events ?finalized_level conn (ctxt : t) events =
    let open Lwt_result_syntax in
    let* needs_process =
      match finalized_level with
      | None ->
          (* We are not taking events from a rollup block, we don't need to check
             if it's the last known l1 block successor. *)
          return_true
      | Some rollup_block_level -> (
          let* last_known_l1_block =
            let* level = Evm_store.L1_l2_levels_relationships.find conn in
            match level with
            | Some {l1_level; _} -> return_some l1_level
            | None -> return_none
          in
          match last_known_l1_block with
          | None -> return_true
          | Some last_known_l1_block ->
              let level_expected = Int32.succ last_known_l1_block in
              if Int32.equal rollup_block_level level_expected then return_true
              else if Compare.Int32.(rollup_block_level < level_expected) then
                let*! () =
                  Evm_context_events.unexpected_l1_block
                    ~expected_level:level_expected
                    ~provided_level:rollup_block_level
                in
                return false
              else
                tzfail
                  (Node_error.Out_of_sync
                     {level_received = rollup_block_level; level_expected}))
    in
    if needs_process then
      let start_finalized_number = ctxt.session.finalized_number in
      let* () =
        match events with
        | [] ->
            (* Avoid an uncessary {!replace_current_commit} if the list is
               empty. *)
            return_unit
        | events ->
            let* () = List.iter_es (apply_evm_event_unsafe ctxt conn) events in
            replace_current_commit ctxt conn
      in
      let (Qty latest_finalized_number) = ctxt.session.finalized_number in
      Option.iter_es
        (fun l1_level ->
          let* () =
            store_finalized_levels
              ctxt
              conn
              ~l1_level
              ~start_l2_level:start_finalized_number
              ~end_l2_level:(Ethereum_types.Qty latest_finalized_number)
          in
          let*! () =
            Evm_context_events.processed_l1_level
              (l1_level, latest_finalized_number)
          in
          return_unit)
        finalized_level
    else return_unit

  and flush_delayed_inbox ctxt conn
      (Evm_events.Flushed_blueprint.
         {transactions = delayed_transactions; timestamp; level = flushed_level}
       as flushed_blueprint) =
    let open Lwt_result_syntax in
    let before_flushed_level =
      Ethereum_types.Qty.pred
        flushed_blueprint.Evm_events.Flushed_blueprint.level
    in
    (* save upgrades before reset *)
    let* lost_upgrade =
      Evm_store.Kernel_upgrades.find_latest_injected_after
        conn
        before_flushed_level
    in
    let* lost_sequencer_upgrade =
      Evm_store.Sequencer_upgrades.find_latest_injected_after
        conn
        before_flushed_level
    in
    (* The kernel has produced a block for level [flushed_level]. The first thing
       to do is go back to an EVM state before this flushed blueprint. *)
    let* () =
      let* checkpoint =
        Evm_store.Context_hashes.find conn before_flushed_level
      in
      match checkpoint with
      | None ->
          let*! () = Evm_context_events.missing_state before_flushed_level in
          tzfail (Node_error.Cannot_handle_flushed_blueprint flushed_level)
      | Some checkpoint ->
          reset_to_level ctxt conn before_flushed_level checkpoint
    in
    (* Clean unreliable delayed inbox *)
    let* () = clear_head_delayed_inbox ctxt in
    (* Prepare an event list to be reapplied on current head *)
    let events =
      Evm_events.of_parts
        ~delayed_transactions
        ~kernel_upgrade:lost_upgrade
        ~sequencer_upgrade:lost_sequencer_upgrade
    in
    (* TODO: We should iterate when multichain https://gitlab.com/tezos/tezos/-/issues/7859 *)
    let (Ex_chain_family chain_family) =
      Configuration.retrieve_chain_family
        ~l2_chains:ctxt.configuration.experimental_features.l2_chains
    in
    (* Prepare a blueprint payload signed by the sequencer to execute locally. *)
    let* parent_hash =
      Evm_state.current_block_hash ~chain_family ctxt.session.evm_state
    in
    let* chunks, payload =
      prepare_local_flushed_blueprint ctxt parent_hash flushed_blueprint
    in
    (* Apply the blueprint. *)
    let+ _block =
      apply_blueprint
        ~events
        ctxt
        conn
        timestamp
        chunks
        payload
        delayed_transactions
    in
    ()

  let () =
    register_error_kind
      `Permanent
      ~id:"evm_node_dev_invalid_rollup_node"
      ~title:"Invalid rollup node"
      ~description:"The rollup node has a different smart rollup address."
      ~pp:(fun ppf (smart_rollup_address, rollup_node_smart_rollup_address) ->
        Format.fprintf
          ppf
          "The EVM node follows the smart rollup address %a but the rollup \
           node has the address %a, please change the rollup node endpoint."
          Tezos_crypto.Hashed.Smart_rollup_address.pp
          smart_rollup_address
          Tezos_crypto.Hashed.Smart_rollup_address.pp
          rollup_node_smart_rollup_address)
      Data_encoding.(
        obj2
          (req
             "smart_rollup_address"
             Tezos_crypto.Hashed.Smart_rollup_address.encoding)
          (req
             "rollup_node_smart_rollup_address"
             Tezos_crypto.Hashed.Smart_rollup_address.encoding))
      (function
        | Invalid_rollup_node
            {smart_rollup_address; rollup_node_smart_rollup_address} ->
            Some (smart_rollup_address, rollup_node_smart_rollup_address)
        | _ -> None)
      (fun (smart_rollup_address, rollup_node_smart_rollup_address) ->
        Invalid_rollup_node
          {smart_rollup_address; rollup_node_smart_rollup_address})

  let check_smart_rollup_address ~store_smart_rollup_address
      ~smart_rollup_address =
    let open Lwt_result_syntax in
    match (store_smart_rollup_address, smart_rollup_address) with
    | Some store_smart_rollup_address, Some smart_rollup_address ->
        let* () =
          fail_unless
            (Tezos_crypto.Hashed.Smart_rollup_address.equal
               smart_rollup_address
               store_smart_rollup_address)
            (Invalid_rollup_node
               {
                 smart_rollup_address = store_smart_rollup_address;
                 rollup_node_smart_rollup_address = smart_rollup_address;
               })
        in
        return smart_rollup_address
    | None, Some smart_rollup_address -> return smart_rollup_address
    | Some store_smart_rollup_address, None -> return store_smart_rollup_address
    | None, None ->
        (* Ok. This is dirty. In normal condition it does not make sense that
           the sr1 is missing from the local storage and not provided by
           the caller.

           However, it can happen if the node is ran in sandbox mode
           on a non existing context. Returning the zero address here is
           so much convenient for a case that's really not supposed to happen,
           ever. An error that would be detected relatively fast as well.
        *)
        return Tezos_crypto.Hashed.Smart_rollup_address.zero

  let history_mode_switch_status h1 h2 =
    let open Configuration in
    match (h1, h2) with
    | Archive, Archive -> `No_switch
    | (Rolling gc, Rolling gc' | Full gc, Full gc')
      when gc.number_of_chunks = gc'.number_of_chunks ->
        `No_switch
    (* Keep this match explicit *)
    | Rolling _, Full _ | Rolling _, Archive | Full _, Archive -> `Cannot_switch
    | Rolling _, Rolling _
    | Full _, Full _
    | Full _, Rolling _
    | Archive, Rolling _
    | Archive, Full _ ->
        `Needs_switch

  let check_history_mode ?(switch = false) ~store_history_mode ~history_mode ()
      =
    let open Lwt_result_syntax in
    match (store_history_mode, history_mode) with
    | Some h, None -> return h
    | None, None -> return Configuration.default_history_mode
    | None, Some h -> return h
    | Some store_history_mode, Some history_mode -> (
        match history_mode_switch_status store_history_mode history_mode with
        | `No_switch ->
            if switch then
              Format.printf
                "History mode is already %a, not switching@."
                Configuration.pp_history_mode_debug
                store_history_mode ;
            return store_history_mode
        | `Needs_switch when switch ->
            let*! () =
              Evm_context_events.switching_history_mode
                ~from:store_history_mode
                ~to_:history_mode
            in
            return history_mode
        | _ ->
            tzfail (Incorrect_history_mode {store_history_mode; history_mode}))

  let check_metadata ~store_metadata ~smart_rollup_address ~history_mode () =
    let open Lwt_result_syntax in
    let* smart_rollup_address =
      check_smart_rollup_address
        ~store_smart_rollup_address:
          (Option.map
             (fun (metadata : Evm_store.metadata) ->
               metadata.smart_rollup_address)
             store_metadata)
        ~smart_rollup_address
    in
    let* history_mode =
      check_history_mode
        ~store_history_mode:
          (Option.map
             (fun (metadata : Evm_store.metadata) -> metadata.history_mode)
             store_metadata)
        ~history_mode
        ()
    in
    return ({smart_rollup_address; history_mode} : Evm_store.metadata)

  let irmin_load ?snapshot_source configuration =
    let open Lwt_result_syntax in
    let open Configuration in
    let data_dir = configuration.data_dir in
    let*! store_already_exists =
      Lwt_utils_unix.dir_exists (Evm_state.irmin_store_path ~data_dir)
    in
    let* () =
      match snapshot_source with
      | Some (Url_legacy snapshot_url) when not store_already_exists -> (
          let* history_mode =
            Snapshots_legacy.import_from
              ~force:true
              ?history_mode:configuration.history_mode
              ~data_dir
              ~snapshot_file:snapshot_url
              ()
          in
          match history_mode with
          | Some h ->
              let* store =
                Evm_store.init
                  ~chain_family:L2_types.EVM
                  ~data_dir
                  ~perm:Read_write
                  ()
              in
              let* () =
                Evm_store.use store @@ fun conn ->
                Evm_store.Metadata.store_history_mode conn h
              in
              let*! () = Evm_store.close store in
              return_unit
          | None -> return_unit)
      | _ -> return_unit
    in
    Pvm.Context.load
      (module Pvm.Irmin_context)
      ~cache_size:100_000
      ~async_domain:true
      Read_write
      (Evm_state.irmin_store_path ~data_dir)

  let on_disk_kernel = function Pvm_types.On_disk _ -> true | _ -> false

  let init ~(configuration : Configuration.t) ?kernel_path ?smart_rollup_address
      ~store_perm ?signer ?snapshot_source
      ~(tx_container : _ Services_backend_sig.tx_container) () =
    let open Lwt_result_syntax in
    let pool =
      (* All interactions of the Evm_context worker with the kernel are purely
         sequential. As a consequence, a pool of 1 domain is enough. *)
      Lwt_domain.setup_pool 1
    in
    let*! () =
      Lwt_utils_unix.create_dir
        (Evm_state.kernel_logs_directory ~data_dir:configuration.data_dir)
    in
    let preimages_dir = Configuration.preimages_path configuration in
    let*! () = Lwt_utils_unix.create_dir preimages_dir in
    let* index = irmin_load ?snapshot_source configuration in
    let* store, context, next_blueprint_number, current_block_hash, init_status
        =
      load
        ~l2_chains:configuration.experimental_features.l2_chains
        ~data_dir:configuration.data_dir
        ~store_perm
        index
    in
    Evm_store.use store @@ fun conn ->
    let* () =
      let* is_empty = Evm_store.Pending_confirmations.is_empty conn in
      when_
        (Option.is_some signer && not is_empty)
        (fun () ->
          failwith "Store has pending confirmation, state is not final")
    in
    let* pending_upgrade = Evm_store.Kernel_upgrades.find_latest_pending conn in
    let* pending_sequencer_upgrade =
      Evm_store.Sequencer_upgrades.find_latest_pending conn
    in
    let* latest = Evm_store.L1_l2_finalized_levels.last conn in
    let finalized_number, l1_level =
      match latest with
      | None -> (Ethereum_types.Qty Z.zero, None)
      | Some (l1_level, {end_l2_level; _}) -> (end_l2_level, Some l1_level)
    in
    let* {smart_rollup_address; history_mode} =
      let smart_rollup_address =
        Option.map
          Tezos_crypto.Hashed.Smart_rollup_address.of_string_exn
          smart_rollup_address
      in
      let* store_metadata = Evm_store.Metadata.find conn in
      let* metadata =
        check_metadata
          ~store_metadata
          ~smart_rollup_address
          ~history_mode:configuration.history_mode
          ()
      in
      let* () =
        when_ (Option.is_none store_metadata) (fun () ->
            Evm_store.Metadata.store conn metadata)
      in
      return metadata
    in
    let*! () =
      match
        ( history_mode,
          configuration.experimental_features.periodic_snapshot_path )
      with
      | Archive, Some _ -> Events.ignored_periodic_snapshot ()
      | _ -> Lwt.return_unit
    in
    let*! () = Evm_context_events.start_history_mode history_mode in

    let* evm_state, context =
      match kernel_path with
      | Some kernel ->
          if init_status = Loaded then
            let* () =
              when_ (on_disk_kernel kernel) @@ fun () ->
              Lwt_result.ok (Events.ignored_kernel_arg ())
            in
            let*! evm_state = Pvm.State.get context in
            return (evm_state, context)
          else
            let* evm_state = Evm_state.init ~kernel in
            (* By executing the kernel with a minimal inbox, we preemptively
               setup Etherlink (in case an installer is used). *)
            let config =
              Pvm.Kernel.config
                ~preimage_directory:(Configuration.preimages_path configuration)
                ?preimage_endpoint:
                  configuration.kernel_execution.preimages_endpoint
                ~kernel_debug:true
                ~trace_host_funs:
                  configuration.opentelemetry.trace_host_functions
                ~destination:smart_rollup_address
                ()
            in
            (* We write the expected sequencer in the state ahead of time.
               This is necessary to support the sandbox mode, which is
               typically not set up with an installer and therefore would
               execute this first call in proxy mode (leading to an unwanted
               block creation).

               For production setup, the installer will rewrite this value. *)
            let* evm_state =
              match signer with
              | Some signer ->
                  let*? pk, _ = Signer.first_lexicographic_signer signer in
                  Lwt_result.ok
                  @@ Evm_state.modify
                       ~key:Durable_storage_path.sequencer_key
                       ~value:(Signature.Public_key.to_b58check pk)
                       evm_state
              | None -> return evm_state
            in
            let* evm_state =
              Evm_state.execute
                ~pool
                ~data_dir:configuration.data_dir
                ~config
                ~native_execution:false
                evm_state
                (`Inbox [])
            in
            let (Qty next) = next_blueprint_number in
            let* context = commit conn context evm_state (Qty Z.(pred next)) in
            return (evm_state, context)
      | None ->
          if init_status = Loaded then
            let*! evm_state = Pvm.State.get context in
            return (evm_state, context)
          else
            failwith
              "Cannot compute the initial EVM state without the path to the \
               initial kernel"
    in

    let* storage_version = Evm_state.storage_version evm_state in

    let* last_split_block =
      match history_mode with
      | Rolling _ | Full _ -> Evm_store.Irmin_chunks.latest conn
      | Archive -> return_none
    in

    let future_block_info =
      if
        configuration.experimental_features.preconfirmation_stream_enabled
        && Option.is_none signer
      then Awaiting_next_block_info
      else Disabled
    in

    let ctxt =
      {
        configuration;
        index;
        smart_rollup_address;
        session =
          {
            context;
            storage_version;
            finalized_number;
            next_blueprint_number;
            current_block_hash;
            pending_upgrade;
            pending_sequencer_upgrade;
            evm_state;
            last_split_block;
            post_transaction_run_hook = None;
            future_block_info;
          };
        store;
        signer;
        tx_container = Ex_tx_container tx_container;
        execution_pool = pool;
      }
    in

    let* () =
      let* ro_store =
        Evm_store.init
          ~chain_family:L2_types.EVM
          ~data_dir:configuration.data_dir
          ~perm:(Read_only {pool_size = 1})
          ()
      in
      let* evm_node_endpoint =
        if is_sequencer ctxt then return_none
        else
          match configuration.observer with
          | Some observer -> return_some observer.evm_node_endpoint
          | None -> return_none
      in

      Block_storage_setup.enable
        ~keep_alive:configuration.keep_alive
        ~timeout:configuration.rpc_timeout
        ?evm_node_endpoint
        ro_store ;
      return_unit
    in

    let*! () =
      Option.iter_s
        (fun upgrade -> Events.pending_upgrade upgrade.Evm_store.kernel_upgrade)
        pending_upgrade
    in

    return (ctxt, (init_status, smart_rollup_address, l1_level))

  let reset ~data_dir ~l2_level =
    let open Lwt_result_syntax in
    let* store =
      Evm_store.init ~chain_family:L2_types.EVM ~data_dir ~perm:Read_write ()
    in
    Evm_store.use store @@ fun conn ->
    Evm_store.with_transaction conn @@ fun store ->
    Evm_store.reset_after store ~l2_level

  let wasm_pvm_version (ctxt : t) =
    let open Lwt_result_syntax in
    let*! version = Evm_state.wasm_pvm_version ctxt.session.evm_state in
    return version

  let canonical_block_number ctxt = function
    | None -> None
    | Some block_number ->
        if Ethereum_types.Qty.(current_blueprint_number ctxt = block_number)
        then None
        else Some block_number

  let perform_commit = commit

  let patch_state (ctxt : t) conn ?block_number ~commit ~key patch () =
    let open Lwt_result_syntax in
    let block_number = canonical_block_number ctxt block_number in
    let* () =
      match block_number with
      | None -> return_unit
      | Some block_number -> (
          let* hash = Evm_store.Context_hashes.find conn block_number in
          match hash with
          | Some hash ->
              let*! context = Pvm.Context.checkout_exn ctxt.index hash in
              let*! evm_state = Pvm.State.get context in
              ctxt.session.evm_state <- evm_state ;
              return_unit
          | None ->
              failwith
                "Missing context for block number %a"
                Ethereum_types.pp_quantity
                block_number)
    in
    let*! previous_value = Evm_state.inspect ctxt.session.evm_state key in
    let new_value = patch (Option.map Bytes.to_string previous_value) in
    let*! () =
      match new_value with
      | Some value -> modify_in_place ctxt ~key ~value
      | None when Option.is_some previous_value ->
          delete_in_place ctxt ~kind:Value key
      | None -> Lwt.return_unit
    in
    let* (Qty number) =
      match block_number with
      | None ->
          if commit then
            let* () = replace_current_commit ctxt conn in
            return (current_blueprint_number ctxt)
          else return (current_blueprint_number ctxt)
      | Some block_number ->
          if commit then (
            let* context =
              perform_commit
                conn
                ctxt.session.context
                ctxt.session.evm_state
                block_number
            in
            ctxt.session.context <- context ;
            return block_number)
          else return block_number
    in
    let*! () = Events.patched_state key (Qty Z.(succ number)) in
    return_unit

  let observer_apply_reorg ctxt conn blueprint_with_events pred_number =
    let open Lwt_result_syntax in
    (* Checkout the state before the blueprint. *)
    let* checkpoint = Evm_store.Context_hashes.find conn pred_number in
    match checkpoint with
    | None ->
        let*! () =
          Evm_context_events.observer_reorg_cannot_find_state pred_number
        in
        return_none
    | Some checkpoint ->
        let* () = reset_to_level ctxt conn pred_number checkpoint in
        (* Apply the blueprint. *)
        let events =
          Blueprint_types.events_of_blueprint_with_events blueprint_with_events
        in
        let* sequencer =
          Durable_storage.sequencer (read_from_state ctxt.session.evm_state)
        in
        let*? chunks =
          Sequencer_blueprint.chunks_of_external_messages
            blueprint_with_events.blueprint.payload
        in
        let*? chunks = Sequencer_blueprint.check_signatures sequencer chunks in
        let* _block =
          apply_blueprint
            ~events
            ctxt
            conn
            blueprint_with_events.blueprint.timestamp
            chunks
            (return blueprint_with_events.blueprint.payload)
            blueprint_with_events.delayed_transactions
        in

        (* Ask the blueprint follower to restart at blueprint's successor. *)
        return_some ctxt.session.next_blueprint_number

  let rec potential_observer_reorg ~evm_node_endpoint conn ctxt
      blueprint_with_events =
    let open Lwt_result_syntax in
    (*
       1. Check if this blueprint was finalized on L1.
       2. Check if the blueprint is different than the one
          we already received.
       3. Check if the sequencer signed this new blueprint.
       4. Find the divergence point.
       5. Apply the reorganized blueprint.
       6. Restart blueprint follower after this level.
    *)
    let blueprint_number =
      blueprint_with_events.Blueprint_types.blueprint.number
    in
    let*! () = Evm_context_events.observer_potential_reorg blueprint_number in
    (* We check in the local storage if we already saw a blueprint
       for this number, and whether it's the same or not.

       If it's not the same blueprint, it might be the sign of a
       reorganization.
    *)
    let* is_it_the_same_blueprint =
      let+ blueprint_in_store =
        Evm_store.Blueprints.find_with_events conn blueprint_number
      in
      match blueprint_in_store with
      | None ->
          (* The blueprint has been removed from the store, we cannot check it. *)
          false
      | Some blueprint_in_store ->
          Blueprint_types.with_events_equal
            blueprint_in_store
            blueprint_with_events
    in
    if is_it_the_same_blueprint then
      (* The endpoint simply republished an old blueprint. *)
      let*! () =
        Evm_context_events.observer_reorg_old_blueprint blueprint_number
      in
      return_none
    else
      (* As we have a different blueprint we want to check if it was signed
         by the sequencer, otherwise it is meaningless.

         We also decode the blueprint to retrieve the parent hash to see
         if we agree with it.
      *)
      let* sequencer =
        Durable_storage.sequencer (read_from_state ctxt.session.evm_state)
      in
      let*? blueprint_parent_hash =
        Sequencer_blueprint.kernel_blueprint_parent_hash_of_payload
          sequencer
          blueprint_with_events.blueprint.payload
      in
      match blueprint_parent_hash with
      | Some blueprint_parent_hash -> (
          (* If the blueprint parent hash is not equal to whatever block hash
             we have for its predecessor, that means this is not the first
             block of the reorganization and we traverse backward the blueprints
             until we find the blueprint.
          *)
          let (Qty pred_number) = Ethereum_types.Qty.pred blueprint_number in
          let* local_parent_block_hash =
            Evm_store.Blocks.find_hash_of_number conn (Qty pred_number)
          in
          match local_parent_block_hash with
          | None ->
              let*! () =
                Evm_context_events.observer_reorg_cannot_find_divergence
                  blueprint_number
              in
              return_none
          | Some local_parent_block_hash ->
              if local_parent_block_hash <> blueprint_parent_hash then
                (* Reorganization started on an older block, we
                   traverse the chain backward. *)
                let* blueprint_pred =
                  Evm_services.get_blueprint_with_events
                    ~keep_alive:true
                    ~timeout:ctxt.configuration.rpc_timeout
                    ~evm_node_endpoint
                    (Qty pred_number)
                in
                potential_observer_reorg
                  ~evm_node_endpoint
                  conn
                  ctxt
                  blueprint_pred
              else
                (* We agree with the blueprint predecessor but not the blueprint,
                   this is the divergence point. *)
                observer_apply_reorg
                  ctxt
                  conn
                  blueprint_with_events
                  (Qty pred_number))
      | None ->
          let*! () =
            Evm_context_events.observer_reorg_cannot_decode_blueprint
              blueprint_number
          in
          return_none
end

module Worker = Octez_telemetry.Worker.MakeSingle (Name) (Request) (Types)

type worker = Worker.infinite Worker.queue Worker.t

module Handlers = struct
  open Request

  type self = worker

  type launch_error = tztrace

  let on_launch _self ()
      {
        configuration : Configuration.t;
        kernel_path;
        smart_rollup_address : string option;
        store_perm;
        signer;
        snapshot_source;
        tx_container = Ex_tx_container tx_container;
      } =
    let open Lwt_result_syntax in
    let* ctxt, status =
      State.init
        ~configuration
        ?kernel_path
        ?smart_rollup_address
        ~store_perm
        ?signer
        ?snapshot_source
        ~tx_container
        ()
    in
    Lwt.wakeup execution_config_waker
    @@ (ctxt.configuration.data_dir, pvm_config ctxt) ;
    Lwt.wakeup init_status_waker status ;
    State.Transaction.initialize_head_info ctxt ;
    return ctxt

  let on_request : type r request_error.
      self -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
   fun self request ->
    let open Lwt_result_syntax in
    match request with
    | Apply_evm_events {finalized_level; events} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.apply_evm_events ?finalized_level conn ctxt events
    | Apply_blueprint
        {
          events;
          timestamp;
          chunks;
          payload;
          delayed_transactions;
          expected_block_hash;
        } ->
        protect @@ fun () ->
        (* As defined in [blueprint_storage.rs] *)
        let maximum_number_of_chunks = 128 in
        let* () =
          (* As assessed in [check_unsigned_blueprint_chunk] in [parsing.rs] *)
          when_
            (maximum_number_of_chunks
            < List.length (chunks :> Sequencer_blueprint.unsigned_chunk list))
          @@ fun () ->
          failwith
            "Blueprint exceeds the maximum number of chunks allowed by the \
             kernel"
        in
        let ctxt = Worker.state self in
        let fail_on_divergence =
          match ctxt.configuration.observer with
          | Some config -> config.fail_on_divergence
          | None -> true
        in
        let apply_blueprint ?expected_block_hash ctxt conn =
          State.apply_blueprint
            ?events
            ?expected_block_hash
            ctxt
            conn
            timestamp
            chunks
            payload
            delayed_transactions
        in
        let retry_apply_blueprint ctxt conn =
          let*! evm_state = Pvm.State.get ctxt.session.context in
          ctxt.session.evm_state <- evm_state ;
          State.reset_future_block_info_if_enabled ctxt ;
          apply_blueprint ?expected_block_hash:None ctxt conn
        in
        let*! result =
          State.Transaction.run ctxt @@ fun ctxt conn ->
          apply_blueprint ?expected_block_hash ctxt conn
        in
        let* block =
          match result with
          | Ok block -> return block
          | Error trace -> (
              match State.find_divergence_or_unsync trace with
              | Some (`Divergence (level, block_hash, expected_block_hash)) ->
                  if not fail_on_divergence then
                    State.Transaction.run ctxt retry_apply_blueprint
                  else
                    tzfail
                      (Node_error.Diverged
                         {
                           level;
                           expected_block_hash;
                           found_block_hash = Some block_hash;
                           must_exit = true;
                         })
              | Some `Unsync -> State.Transaction.run ctxt retry_apply_blueprint
              | None -> Lwt.return (Error trace))
        in
        let tx_hashes =
          match block with
          | Eth block -> (
              match block.transactions with
              | TxHash tx_hashes -> List.to_seq tx_hashes
              | TxFull tx_objects ->
                  List.to_seq tx_objects |> Seq.map Transaction_object.hash)
          | Tez _ -> Seq.empty
        in
        return tx_hashes
    | Last_known_L1_level -> (
        protect @@ fun () ->
        let ctxt = Worker.state self in
        Evm_store.use ctxt.store @@ fun conn ->
        let+ level = Evm_store.L1_l2_levels_relationships.find conn in
        match level with Some {l1_level; _} -> Some l1_level | None -> None)
    | Patch_state {commit; key; patch; block_number} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.patch_state ?block_number ctxt conn ~commit ~key patch ()
    | Wasm_pvm_version ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.wasm_pvm_version ctxt
    | Potential_observer_reorg {evm_node_endpoint; blueprint_with_events} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.potential_observer_reorg
          ~evm_node_endpoint
          conn
          ctxt
          blueprint_with_events
    | Finalized_levels {l1_level; start_l2_level; end_l2_level} ->
        protect @@ fun () ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        ctxt.session.finalized_number <- end_l2_level ;
        State.store_finalized_levels
          ctxt
          conn
          ~l1_level
          ~start_l2_level
          ~end_l2_level
    | Next_block_info {timestamp; number} ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt conn ->
        State.set_next_block_info ctxt conn timestamp number
    | Execute_single_transaction {tx; hash} ->
        let ctxt = Worker.state self in
        State.Transaction.run ctxt @@ fun ctxt _conn ->
        State.execute_single_transaction ctxt tx hash

  let on_completion (type a err) _self (_r : (a, err) Request.t) (_res : a) _st
      =
    Lwt_syntax.return_unit

  let on_no_request _self = Lwt.return_unit

  let on_close _self = Lwt.return_unit

  module Eq = struct
    type (_, _) eq = Eq : ('a, 'a) eq

    let request : type a b. (a, b) Request.t -> (b, tztrace) eq = function
      | Apply_evm_events _ -> Eq
      | Apply_blueprint _ -> Eq
      | Last_known_L1_level -> Eq
      | Patch_state _ -> Eq
      | Wasm_pvm_version -> Eq
      | Potential_observer_reorg _ -> Eq
      | Finalized_levels _ -> Eq
      | Next_block_info _ -> Eq
      | Execute_single_transaction _ -> Eq
  end

  let on_error (type a b) _self _st (req : (a, b) Request.t) (errs : b) :
      [`Continue | `Shutdown] tzresult Lwt.t =
    let open Lwt_result_syntax in
    match (req, errs) with
    | Apply_evm_events _, [Node_error.Diverged {must_exit = true; _}]
    | Apply_blueprint _, [Node_error.Diverged {must_exit = true; _}] ->
        Lwt_exit.exit_and_raise Node_error.exit_code_when_diverge
    | ( Apply_evm_events _,
        [Node_error.Out_of_sync {level_expected; level_received}] ) ->
        let*! () =
          Evm_events_follower_events.out_of_sync
            ~expected:level_expected
            ~received:level_received
        in
        Lwt_exit.exit_and_raise Node_error.exit_code_when_out_of_sync
    (* Make it explicit that these error don't make the worker crash *)
    | Next_block_info _, [Node_error.Set_next_block_info_while_executing _]
    | ( Execute_single_transaction _,
        [Node_error.Execute_single_transaction_no_block_info _] )
    | _ ->
        let Eq = Eq.request req in
        let request_view = Request.view req in
        let*! () = Evm_context_events.worker_request_failed request_view errs in
        return `Continue
end

let table = Worker.create_table Queue

let worker_promise, worker_waker = Lwt.task ()

type error += No_worker

let check_status_and_return worker =
  match Worker.status worker with
  | Closed (_, _, errs) -> (
      match errs with
      | Some errs -> Error errs
      | None -> error_with "Worker failed with no error")
  | _ -> Ok worker

let worker =
  lazy
    (match Lwt.state worker_promise with
    | Lwt.Return worker -> check_status_and_return worker
    | Lwt.Fail e -> Error (TzTrace.make @@ error_of_exn e)
    | Lwt.Sleep -> Error (TzTrace.make No_worker))

let bind_worker f =
  let open Lwt_result_syntax in
  let res = Lazy.force worker in
  match res with
  | Error [No_worker] ->
      (* There is no worker, nothing to do *)
      return_unit
  | Error errs -> fail errs
  | Ok w -> f w

let worker_add_request ~request =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! (_pushed : bool) = Worker.Queue.push_request w request in
  return_unit

let return_ : (_, _ Worker.message_error) result -> _ =
  let open Lwt_result_syntax in
  function
  | Ok res -> return res
  | Error (Closed (Some trace)) -> Lwt.return (Error trace)
  | Error (Closed None) ->
      failwith
        "Cannot interact with the EVM context worker because it is closed"
  | Error (Request_error err) -> Lwt.return (Error err)
  | Error (Any exn) -> fail_with_exn exn

let smart_rollup_address () =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  return (Worker.state w).smart_rollup_address

let worker_wait_for_request req =
  let open Lwt_result_syntax in
  let*? w = Lazy.force worker in
  let*! res = Worker.Queue.push_request_and_wait w req in
  return_ res

let start ~(configuration : Configuration.t) ?kernel_path ?smart_rollup_address
    ~store_perm ?signer ?snapshot_source
    ~(tx_container : _ Services_backend_sig.tx_container) () =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir:configuration.data_dir in
  let* worker =
    Worker.launch
      table
      ()
      {
        configuration;
        kernel_path;
        smart_rollup_address;
        store_perm;
        signer;
        snapshot_source;
        tx_container = Ex_tx_container tx_container;
      }
      (module Handlers)
  in
  let*! () = Blueprint_events.publisher_is_ready () in
  Lwt.wakeup worker_waker worker ;
  let*! init_status, sr1, l1_level = init_status in
  let*! head_info in
  let (Qty finalized_number) = !head_info.finalized_number in
  let (Qty next_blueprint_number) = !head_info.next_blueprint_number in
  Metrics.set_level ~level:(Z.pred next_blueprint_number) ;
  Metrics.set_confirmed_level ~level:finalized_number ;
  Option.iter (fun l1_level -> Metrics.set_l1_level ~level:l1_level) l1_level ;
  let*! () = Evm_context_events.ready () in
  return (init_status, sr1)

let rollup_node_metadata ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let* metadata =
    Metadata.Versioned.read_metadata_file ~dir:rollup_node_data_dir
  in
  match metadata with
  | None -> failwith "missing metadata in the rollup node data dir"
  | Some (V0 _) -> failwith "metadata has version 0, we need at least version 1"
  | Some (V1 {rollup_address; genesis_info = {level; _}; _}) ->
      return (Address.to_string rollup_address, level)

let init_context_from_rollup_node ~data_dir ~rollup_node_data_dir =
  let open Lwt_result_syntax in
  let open Octez_smart_rollup_node_store in
  let* rollup_node_store =
    Store.init Read_only ~data_dir:rollup_node_data_dir
  in
  let* final_l2_block = Store.L2_blocks.find_finalized rollup_node_store in
  let* final_l2_block =
    match final_l2_block with
    | Some b -> return b
    | None -> failwith "Rollup node has no finalized l2 block"
  in
  let checkpoint = final_l2_block.header.context in
  let rollup_node_context_dir =
    Filename.Infix.(rollup_node_data_dir // "context")
  in
  let evm_context_dir = Evm_state.irmin_store_path ~data_dir in
  let* () =
    Format.eprintf "Acquiring rollup node locks@." ;
    Lwt_lock_file.with_lock
      ~when_locked:`Block
      ~filename:(Filename.concat rollup_node_data_dir "gc_lock")
    @@ fun () ->
    Lwt_lock_file.with_lock
      ~when_locked:`Block
      ~filename:(Filename.concat rollup_node_data_dir "processing_lock")
    @@ fun () ->
    let message =
      Format.sprintf
        "Exporting context for %ld in %s"
        final_l2_block.header.level
        evm_context_dir
    in
    Tezos_stdlib_unix.Utils.copy_dir
      ~progress:(message, Terminal.Color.(rgb 255 153 51))
      rollup_node_context_dir
      evm_context_dir ;
    return_unit
  in
  let* evm_node_index =
    Pvm.Context.load
      (module Pvm.Irmin_context)
      ~cache_size:100_000
      ~async_domain:true
      Read_write
      evm_context_dir
  in
  let*! evm_node_context = Pvm.Context.checkout_exn evm_node_index checkpoint in
  let*! evm_state = Pvm.State.get evm_node_context in
  return (evm_node_context, evm_state, final_l2_block.header.level)

let init_store_from_rollup_node ~chain_family ~data_dir ~evm_state
    ~irmin_context =
  let open Lwt_result_syntax in
  let root = Durable_storage_path.root_of_chain_family chain_family in
  (* Tell the kernel that it is executed by an EVM node *)
  let*! evm_state = Evm_state.flag_local_exec evm_state in
  (* We remove the delayed inbox from the EVM state. Its contents will be
     retrieved by the sequencer by inspecting the evm events. *)
  let*! evm_state = Evm_state.clear_delayed_inbox evm_state in

  (* For changes made to [evm_state] to take effect, we commit the result *)
  let*! evm_node_context = Pvm.State.set irmin_context evm_state in
  let*! checkpoint = Pvm.Context.commit evm_node_context in

  (* Assert we can read the current blueprint number *)
  let* current_blueprint_number =
    let*! current_blueprint_number_opt =
      Evm_state.inspect
        evm_state
        (Durable_storage_path.Block.current_number ~root)
    in
    match current_blueprint_number_opt with
    | Some bytes -> return (Bytes.to_string bytes |> Z.of_bits)
    | None -> failwith "The blueprint number was not found"
  in

  (* Read the current block *)
  let* block =
    Etherlink_durable_storage.current_block
      (Evm_state.read evm_state)
      ~full_transaction_object:true
  in
  (* Init the store *)
  let* store =
    Evm_store.init ~chain_family:L2_types.EVM ~data_dir ~perm:Read_write ()
  in
  let* () =
    Evm_store.(
      use store @@ fun conn ->
      Context_hashes.store conn (Qty current_blueprint_number) checkpoint)
  in
  Evm_store.(use store @@ fun conn -> Blocks.store conn block)

let reset = State.reset

let get_evm_events_from_rollup_node_state ~omit_delayed_tx_events evm_state =
  let open Lwt_result_syntax in
  let* kernel_upgrade =
    let*! kernel_upgrade_payload =
      Evm_state.inspect evm_state Durable_storage_path.kernel_upgrade
    in
    Option.bind kernel_upgrade_payload Evm_events.Upgrade.of_bytes
    |> Option.map (fun e -> Evm_events.Upgrade_event e)
    |> return
  in

  let* sequencer_upgrade =
    let*! sequencer_upgrade_payload =
      Evm_state.inspect evm_state Durable_storage_path.sequencer_upgrade
    in
    Option.bind sequencer_upgrade_payload Evm_events.Sequencer_upgrade.of_bytes
    |> Option.map (fun e -> Evm_events.Sequencer_upgrade_event e)
    |> return
  in

  let* new_delayed_transactions =
    if omit_delayed_tx_events then return []
    else
      let*! hashes = Evm_state.delayed_inbox_hashes evm_state in
      let* events =
        List.map_es
          (fun hash ->
            let* item = Evm_state.get_delayed_inbox_item evm_state hash in
            return (Evm_events.New_delayed_transaction item))
          hashes
      in
      return events
  in

  return
  @@ Option.to_list kernel_upgrade
  @ Option.to_list sequencer_upgrade
  @ new_delayed_transactions

let apply_evm_events ?finalized_level events =
  worker_add_request ~request:(Apply_evm_events {finalized_level; events})

let apply_evm_events' ?finalized_level events =
  worker_wait_for_request (Apply_evm_events {finalized_level; events})

let init_from_rollup_node ~(configuration : Configuration.t)
    ~omit_delayed_tx_events ~rollup_node_data_dir ~tx_container () =
  let open Lwt_result_syntax in
  let* () = lock_data_dir ~data_dir:configuration.data_dir in
  let* irmin_context, evm_state, finalized_level =
    init_context_from_rollup_node
      ~data_dir:configuration.data_dir
      ~rollup_node_data_dir
  in
  let* evm_events =
    get_evm_events_from_rollup_node_state ~omit_delayed_tx_events evm_state
  in
  let (Ex_chain_family chain_family) =
    Configuration.retrieve_chain_family
      ~l2_chains:configuration.Configuration.experimental_features.l2_chains
  in
  let* () =
    init_store_from_rollup_node
      ~chain_family
      ~data_dir:configuration.data_dir
      ~evm_state
      ~irmin_context
  in
  let* smart_rollup_address, _genesis_level =
    rollup_node_metadata ~rollup_node_data_dir
  in
  let* _loaded =
    start
      ~configuration
      ~smart_rollup_address
      ~store_perm:Read_write
      ~tx_container
      ()
  in
  worker_wait_for_request
    (Apply_evm_events
       {finalized_level = Some finalized_level; events = evm_events})

let head_info () =
  let open Lwt_syntax in
  let+ head_info in
  !head_info

let apply_blueprint ?events ?expected_block_hash timestamp payload
    delayed_transactions =
  let open Lwt_result_syntax in
  let*! head = head_info () in
  let* sequencer =
    (* To guess the identity of the sequencer public key to use, we need to
       cover the edge case were a bleuprint contains the sequencer upgrade that
       needs to be triggered to get the correct sequencer. *)
    let incoming_sequencer_upgrade =
      List.find_map
        (function
          | Evm_events.Sequencer_upgrade_event upgrade -> Some upgrade
          | _ -> None)
        (Option.value ~default:[] events)
    in
    match
      Option.either incoming_sequencer_upgrade head.pending_sequencer_upgrade
    with
    | Some {sequencer; timestamp = upgrade_timestamp; _}
      when Time.Protocol.(timestamp >= upgrade_timestamp) ->
        return sequencer
    | _ -> Durable_storage.sequencer (State.read_from_state head.evm_state)
  in
  let*? chunks = Sequencer_blueprint.chunks_of_external_messages payload in
  let*? chunks = Sequencer_blueprint.check_signatures sequencer chunks in
  worker_wait_for_request
    (Apply_blueprint
       {
         events;
         timestamp;
         chunks;
         payload = return payload;
         delayed_transactions;
         expected_block_hash;
       })

let apply_chunks ~signer timestamp chunks delayed_transactions =
  let open Lwt_result_syntax in
  let*! head = head_info () in
  let* expected_sequencer =
    match head.pending_sequencer_upgrade with
    | Some {sequencer; timestamp = upgrade_timestamp; _}
      when Time.Protocol.(timestamp >= upgrade_timestamp) ->
        (* If we are this is the first block after the sequencer upgrade, the sequencer key in the state will still be the previous one. before applying the chunks, the sequencer key will change for the new one.
In order to still be able to sign the chunks, we need to use the next sequencer key instead of the one in the state. *)
        return sequencer
    | _ -> Durable_storage.sequencer (State.read_from_state head.evm_state)
  in
  let*? signer = Signer.get_signer signer expected_sequencer in
  let blueprint_chunks = Sequencer_blueprint.sign ~signer ~chunks in
  let payload =
    let* blueprint_chunks in
    let+ smart_rollup_address = smart_rollup_address () in
    Sequencer_blueprint.create_inbox_payload
      ~smart_rollup_address:
        (Tezos_crypto.Hashed.Smart_rollup_address.to_string
           smart_rollup_address)
      ~chunks:blueprint_chunks
  in
  let* sequencer = Signer.public_key signer in
  if Signature.Public_key.(sequencer = expected_sequencer) then
    let* confirmed_txs =
      worker_wait_for_request
        (Apply_blueprint
           {
             events = None;
             timestamp;
             chunks;
             payload;
             delayed_transactions;
             expected_block_hash = None;
           })
    and* blueprint_chunks
    and* payload in
    return (blueprint_chunks, payload, confirmed_txs)
  else
    failwith
      "Cannot apply a blueprint produced by sequencer %a"
      Signature.Public_key.pp
      sequencer

let apply_finalized_levels ~l1_level ~start_l2_level ~end_l2_level =
  worker_wait_for_request
    (Finalized_levels {l1_level; start_l2_level; end_l2_level})

let next_blueprint_number () =
  let open Lwt_syntax in
  let+ head_info = head_info () in
  head_info.next_blueprint_number

let last_known_l1_level () = worker_wait_for_request Last_known_L1_level

let patch_kernel ?block_number kernel =
  let open Lwt_result_syntax in
  let* kernel, is_binary = Pvm.Kernel.read_kernel kernel in
  let* version = worker_wait_for_request Wasm_pvm_version in
  let* () =
    Pvm.Kernel.check_kernel ~binary:is_binary ~name:"boot.wasm" version kernel
  in
  let* () =
    worker_wait_for_request
      (Patch_state
         {
           commit = true;
           key = "/kernel/boot.wasm";
           patch = Fun.const (Some kernel);
           block_number;
         })
  in
  return_unit

let patch_sequencer_key ?block_number pk =
  worker_wait_for_request
    (Patch_state
       {
         commit = false;
         key = Durable_storage_path.sequencer_key;
         patch = Fun.const (Some (Signature.Public_key.to_b58check pk));
         block_number;
       })

let provision_balance ?block_number address value =
  let new_info u =
    String.of_bytes
      (Tezosx.Tezos_runtime.encode_account_info
         {
           balance = Tezos_types.Tez.of_mutez_exn u;
           nonce = 0L;
           public_key = None;
         })
  in
  let patch_requests =
    let open Request in
    let (Ethereum_types.Qty u) = value in
    match address with
    | Tezosx.Ethereum_address address ->
        [
          Patch_state
            {
              commit = false;
              key = Durable_storage_path.Accounts.balance address;
              patch =
                (let open Ethereum_types in
                 function
                 | Some old_balance ->
                     let (Qty v) =
                       decode_number_le (Bytes.unsafe_of_string old_balance)
                     in
                     Some (String.of_bytes (encode_u256_le (Qty Z.(u + v))))
                 | None -> Some (String.of_bytes (encode_u256_le value)));
              block_number;
            };
        ]
    | Tezos_address address ->
        (* Mutez precision is 10^-6 while eth is 10^-18. *)
        let u = Z.(to_int64 @@ (u / pow (of_int 10) 12)) in
        [
          Patch_state
            {
              commit = false;
              key = Tezosx.Durable_storage_path.Accounts.Tezos.info address;
              patch =
                (function
                | Some old_info -> (
                    match
                      Tezosx.Tezos_runtime.decode_account_info
                        (Bytes.unsafe_of_string old_info)
                    with
                    | Ok old_info ->
                        let balance =
                          Tezos_types.Tez.(
                            of_mutez_exn
                            @@ Int64.add (to_mutez old_info.balance) u)
                        in
                        let info = {old_info with balance} in
                        Some
                          (String.of_bytes
                             (Tezosx.Tezos_runtime.encode_account_info info))
                    | Error _ -> Some (new_info u))
                | None -> Some (new_info u));
              block_number;
            };
          Patch_state
            {
              commit = false;
              key =
                Tezosx.Durable_storage_path.Accounts.Tezos.ethereum_alias
                  address;
              patch =
                (fun _ ->
                  Some
                    (Bytes.to_string
                       (Tezosx.Foreign_address.encode (`Tezos address))));
              block_number;
            };
        ]
  in
  List.iter_es worker_wait_for_request patch_requests

let patch_state ?block_number ~key ~value () =
  worker_wait_for_request
    (Patch_state
       {commit = true; key; patch = Fun.const (Some value); block_number})

let potential_observer_reorg evm_node_endpoint blueprint_with_events =
  worker_wait_for_request
    (Potential_observer_reorg {evm_node_endpoint; blueprint_with_events})

let next_block_info timestamp number =
  worker_wait_for_request (Next_block_info {timestamp; number})

let execute_single_transaction tx hash =
  worker_wait_for_request (Execute_single_transaction {tx; hash})

let shutdown () =
  let open Lwt_result_syntax in
  bind_worker @@ fun w ->
  let*! () = Evm_context_events.shutdown () in
  let*! () = Worker.shutdown w in
  return_unit

let check_history_mode = State.check_history_mode
