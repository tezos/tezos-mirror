(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

include Internal_event.Legacy_logging.Make_semantic(struct
    let name = "shell.snapshots"
  end)

let (//) = Filename.concat
let context_dir data_dir = data_dir // "context"
let store_dir data_dir = data_dir // "store"

type error += Wrong_snapshot_export of History_mode.t * History_mode.t
type error += Wrong_block_export of
    Block_hash.t * [ `Pruned | `Too_few_predecessors | `Cannot_be_found ]
type error += Inconsistent_imported_block of Block_hash.t * Block_hash.t
type error += Snapshot_import_failure of string
type error += Wrong_protocol_hash of Protocol_hash.t
type error += Inconsistent_operation_hashes of
    (Operation_list_list_hash.t * Operation_list_list_hash.t)

let () = begin
  let open Data_encoding in

  register_error_kind
    `Permanent
    ~id:"WrongSnapshotExport"
    ~title:"Wrong snapshot export"
    ~description:"Snapshot exports is not compatible with the current configuration."
    ~pp:begin fun ppf (src,dst) ->
      Format.fprintf ppf
        "Cannot export a %a snapshot from a %a node."
        History_mode.pp dst History_mode.pp src
    end
    (obj2
       (req "src" History_mode.encoding)
       (req "dst" History_mode.encoding))
    (function Wrong_snapshot_export (src,dst) -> Some (src, dst) | _ -> None)
    (fun (src, dst) -> Wrong_snapshot_export (src, dst)) ;

  let pp_wrong_block_export_error ppf kind =
    let str =
      match kind with
      | `Pruned -> "is pruned"
      | `Too_few_predecessors -> "has not enough predecessors"
      | `Cannot_be_found ->  "cannot be found" in
    Format.fprintf ppf "%s" str in
  let error_kind_encoding =
    string_enum
      [ "pruned", `Pruned ;
        "too_few_predecessors", `Too_few_predecessors ;
        "cannot_be_found", `Cannot_be_found ] in
  register_error_kind
    `Permanent
    ~id:"WrongBlockExport"
    ~title:"Wrong block export"
    ~description:"The block to export in the snapshot is not valid."
    ~pp:(fun ppf (bh,kind) ->
        Format.fprintf ppf
          "Fails to export snapshot as the block with block hash %a %a."
          Block_hash.pp bh pp_wrong_block_export_error kind)
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "kind" error_kind_encoding))
    (function Wrong_block_export (bh, kind) -> Some (bh, kind) | _ -> None)
    (fun (bh, kind) -> Wrong_block_export (bh, kind)) ;

  register_error_kind
    `Permanent
    ~id:"InconsistentImportedBlock"
    ~title:"Inconsistent imported block"
    ~description:"The imported block is not the expected one."
    ~pp:begin fun ppf (got,exp) ->
      Format.fprintf ppf
        "The block contained in the file is %a instead of %a."
        Block_hash.pp got Block_hash.pp exp
    end
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_hash_expected" Block_hash.encoding))
    (function Inconsistent_imported_block (got, exp) -> Some (got, exp) | _ -> None)
    (fun (got, exp) -> Inconsistent_imported_block (got, exp)) ;

  register_error_kind
    `Permanent
    ~id:"SnapshotImportFailure"
    ~title:"Snapshot import failure"
    ~description:"The imported snapshot is malformed."
    ~pp:begin fun ppf msg ->
      Format.fprintf ppf
        "The data contained in the snapshot is not valid. The import mechanism \
         failed to validate the file: %s."
        msg
    end
    (obj1 (req "message" string))
    (function Snapshot_import_failure str -> Some str | _ -> None)
    (fun str -> Snapshot_import_failure str) ;

  register_error_kind
    `Permanent
    ~id:"WrongProtocolHash"
    ~title:"Wrong protocol hash"
    ~description:"Wrong protocol hash"
    ~pp:(fun ppf p ->
        Format.fprintf ppf
          "Wrong protocol hash (%a) found in snapshot. Snapshot is corrupted."
          Protocol_hash.pp p)
    (obj1 (req "protocol_hash" Protocol_hash.encoding))
    (function Wrong_protocol_hash p -> Some p | _ -> None)
    (fun p -> Wrong_protocol_hash p) ;

  register_error_kind
    `Permanent
    ~id:"InconsistentOperationHashes"
    ~title:"Inconsistent operation hashes"
    ~description:"The operations given do not match their hashes."
    ~pp:(fun ppf (oph, oph') ->
        Format.fprintf ppf
          "Inconsistent operation hashes. Expected: %a, got: %a."
          Operation_list_list_hash.pp oph Operation_list_list_hash.pp oph')
    (obj2
       (req "expected_operation_hashes" Operation_list_list_hash.encoding)
       (req "received_operation_hashes" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operation_hashes (oph, oph') -> Some (oph, oph')
      | _ -> None)
    (fun (oph, oph') -> Inconsistent_operation_hashes (oph, oph')) ;
end

let compute_export_limit
    block_store chain_data_store
    block_header export_rolling =
  let block_hash = Block_header.hash block_header in
  Store.Block.Contents.read_opt
    (block_store, block_hash) >>= begin function
    | Some contents -> return contents
    | None -> fail (Wrong_block_export (block_hash, `Pruned))
  end >>=? fun { max_operations_ttl ; _ } ->
  if not export_rolling then
    Store.Chain_data.Caboose.read chain_data_store >>=? fun (caboose_level, _) ->
    return (max 1l caboose_level)
  else
    let limit = Int32.(sub
                         block_header.Block_header.shell.level
                         (of_int max_operations_ttl)) in
    (* fails when the limit exceeds the genesis or the genesis is
       included in the export limit *)
    fail_when
      (limit <= 0l)
      (Wrong_block_export (block_hash, `Too_few_predecessors)) >>=? fun () ->
    return limit

(** When called with a block, returns its predecessor if it exists and
    its protocol_data if the block is a transition block (i.e. protocol
    level changing block) or when there is no more predecessor. *)
let pruned_block_iterator index block_store limit header =
  if header.Block_header.shell.level <= limit then
    Context.get_protocol_data_from_header index header >>= fun protocol_data ->
    return (None, Some protocol_data)
  else
    let pred_hash = header.Block_header.shell.predecessor in
    State.Block.Header.read (block_store, pred_hash) >>=? fun pred_header ->
    Store.Block.Operations.bindings (block_store, pred_hash) >>= fun pred_operations ->
    Store.Block.Operation_hashes.bindings (block_store, pred_hash) >>= fun pred_operation_hashes ->
    let pruned_block = {
      Context.Pruned_block.block_header = pred_header ;
      operations = pred_operations ;
      operation_hashes = pred_operation_hashes ;
    } in
    let header_proto_level = header.Block_header.shell.proto_level in
    let pred_header_proto_level = pred_header.Block_header.shell.proto_level in
    if header_proto_level <> pred_header_proto_level then
      Context.get_protocol_data_from_header index header >>= fun proto_data ->
      return (Some pruned_block, Some proto_data)
    else
      return (Some pruned_block, None)

let filename_tag =
  Tag.def "filename" Format.pp_print_string

let block_level_tag =
  Tag.def "block_level" Format.pp_print_int

let export ?(export_rolling=false) ~context_index ~store ~genesis filename block  =
  let chain_id = Chain_id.of_block_hash genesis in
  let chain_store = Store.Chain.get store chain_id in
  let chain_data_store = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  begin Store.Configuration.History_mode.read_opt store >>= function
    | Some (Archive | Full) | None -> return_unit
    | Some (Rolling as history_mode) ->
        if export_rolling then return_unit else
          fail (Wrong_snapshot_export (history_mode, History_mode.Full))
  end >>=? fun () ->
  begin match block with
    | Some block_hash -> Lwt.return (Block_hash.of_b58check block_hash)
    | None ->
        Store.Chain_data.Checkpoint.read_opt (chain_data_store) >|=
        Option.unopt_assert ~loc:__POS__  >>= fun last_checkpoint ->
        if last_checkpoint.shell.level = 0l then
          fail (Wrong_block_export (genesis, `Too_few_predecessors))
        else
          let last_checkpoint_hash = Block_header.hash last_checkpoint in
          lwt_log_notice Tag.DSL.(fun f ->
              f "There is no block hash specified with the `--block` option. Using %a (last checkpoint)"
              -%a Block_hash.Logging.tag last_checkpoint_hash
            ) >>= fun () ->
          return last_checkpoint_hash
  end >>=? fun checkpoint_block_hash ->
  begin State.Block.Header.read_opt
      (block_store, checkpoint_block_hash) >>= function
    | None ->
        fail (Wrong_block_export (checkpoint_block_hash, `Cannot_be_found))
    | Some block_header ->
        let export_mode = if export_rolling then History_mode.Rolling else Full in
        lwt_log_notice Tag.DSL.(fun f ->
            f "Exporting a snapshot in mode %a, targeting block hash %a at level %a"
            -%a History_mode.tag export_mode
            -%a Block_hash.Logging.tag checkpoint_block_hash
            -%a block_level_tag (Int32.to_int block_header.shell.level)
          ) >>= fun () ->
        (* Get block precessor's block header *)
        Store.Block.Predecessors.read
          (block_store, checkpoint_block_hash) 0 >>=? fun pred_block_hash ->
        State.Block.Header.read
          (block_store, pred_block_hash) >>=? fun pred_block_header ->
        (* Get operation list *)
        let validations_passes = block_header.shell.validation_passes in
        map_s
          (fun i -> Store.Block.Operations.read (block_store, checkpoint_block_hash) i)
          (0 -- (validations_passes - 1)) >>=? fun operations ->
        compute_export_limit
          block_store chain_data_store block_header export_rolling >>=? fun export_limit ->
        let iterator = pruned_block_iterator context_index block_store export_limit in
        let block_data = { Context.Block_data.block_header ; operations } in
        return (pred_block_header, block_data, export_mode, iterator)
  end >>=? fun data_to_dump ->
  lwt_log_notice (fun f -> f "Now loading data") >>= fun () ->
  Context.dump_contexts context_index data_to_dump ~filename >>=? fun () ->
  lwt_log_notice Tag.DSL.(fun f ->
      f "@[Successful export: %a@]"
      -% a filename_tag filename
    ) >>= fun () ->
  return_unit

let check_operations_consistency block_header operations operation_hashes =
  (* Compute operations hashes and compare *)
  List.iter2
    (fun (_, op) (_, oph) ->
       let expected_op_hash = List.map Operation.hash op in
       List.iter2 (fun expected found ->
           assert (Operation_hash.equal expected found) (* paul:here *)
         ) expected_op_hash oph ;
    )
    operations operation_hashes ;
  (* Check header hashes based on merkel tree *)
  let hashes = List.map (fun (_,opl) ->
      List.map Operation.hash opl)
      (List.rev operations) in
  let computed_hash =
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute hashes) in
  let are_oph_equal = Operation_list_list_hash.equal
      computed_hash
      block_header.Block_header.shell.operations_hash in
  fail_unless are_oph_equal
    (Inconsistent_operation_hashes
       (computed_hash, block_header.Block_header.shell.operations_hash))

let compute_predecessors ~genesis_hash oldest_level block_hashes i =
  let rec step s d acc =
    if oldest_level = 1l && i - d = -1 then
      List.rev ((s, genesis_hash) :: acc)
    else if i - d < 0 then
      List.rev acc
    else
      step (s + 1) (d * 2) ((s, block_hashes.(i - d)) :: acc) in
  step 0 1 []

let check_context_hash_consistency block_validation_result block_header =
  fail_unless
    (Context_hash.equal
       block_validation_result.Tezos_validation.Block_validation.context_hash
       block_header.Block_header.shell.context)
    (Snapshot_import_failure "resulting context hash does not match")

let set_history_mode store history_mode =
  match history_mode with
  | History_mode.Full | History_mode.Rolling ->
      lwt_log_notice Tag.DSL.(fun f ->
          f "Setting history-mode to %a"
          -%a History_mode.tag history_mode
        ) >>= fun () ->
      Store.Configuration.History_mode.store store history_mode >>= fun () -> return_unit
  | History_mode.Archive ->
      fail (Snapshot_import_failure "cannot import an archive context")

let store_new_head
    chain_state chain_data
    ~genesis block_header
    operations block_validation_result =
  let { Tezos_validation.Block_validation.
        validation_result ;
        block_metadata ;
        ops_metadata ;
        forking_testchain ;
        context_hash } = block_validation_result in
  let validation_store = {
    State.Block.context_hash ;
    message = validation_result.message ;
    max_operations_ttl = validation_result.max_operations_ttl ;
    last_allowed_fork_level = validation_result.last_allowed_fork_level ;
  } in
  State.Block.store
    chain_state
    block_header block_metadata
    operations ops_metadata
    ~forking_testchain
    validation_store >>=? fun new_head ->
  begin match new_head with
    | None ->
        (* Should not happen as the data-dir must be empty *)
        fail (Snapshot_import_failure "a chain head is already present in the store")
    | Some new_head ->
        (* New head is set*)
        Store.Chain_data.Known_heads.remove chain_data genesis >>= fun () ->
        Store.Chain_data.Known_heads.store chain_data (State.Block.hash new_head) >>= fun () ->
        Store.Chain_data.Current_head.store chain_data (State.Block.hash new_head) >>= fun () ->
        return_unit end

let update_checkpoint chain_state checkpoint_header =
  let block_hash = Block_header.hash checkpoint_header in
  (* Imported block is set as the current checkpoint/save_point … *)
  let new_checkpoint = (checkpoint_header.Block_header.shell.level, block_hash) in
  State.Chain.set_checkpoint chain_state checkpoint_header >>= fun () ->
  Lwt.return new_checkpoint

let update_savepoint chain_state new_savepoint =
  State.update_chain_data chain_state begin fun store data ->
    let new_data = { data with save_point = new_savepoint } in
    Store.Chain_data.Save_point.store store new_savepoint >>= fun () ->
    Lwt.return (Some new_data, ())
  end

let update_caboose chain_data ~genesis block_header oldest_header max_op_ttl =
  let oldest_level = oldest_header.Block_header.shell.level in
  let caboose_level =
    if oldest_level = 1l then 0l else oldest_level in
  let caboose_hash =
    if oldest_level = 1l then genesis else Block_header.hash oldest_header in
  let minimal_caboose_level =
    Int32.(sub
             block_header.Block_header.shell.level
             (of_int max_op_ttl)) in
  fail_unless
    Compare.Int32.(caboose_level <= minimal_caboose_level)
    (Snapshot_import_failure
       (Format.sprintf "caboose level (%ld) is not valid" caboose_level)) >>=? fun () ->
  Store.Chain_data.Caboose.store chain_data (caboose_level, caboose_hash) >>= fun () ->
  return_unit

let import_protocol_data index store block_hash_arr level_oldest_block (level, protocol_data) =
  (* Retrieve the original context hash of the block. *)
  let delta = Int32.(to_int (sub level level_oldest_block)) in
  let pruned_block_hash = block_hash_arr.(delta) in
  let block_store = Store.Block.get store in
  begin State.Block.Header.read_opt (block_store, pruned_block_hash) >>= function
    | None -> assert false
    | Some block_header -> Lwt.return block_header
  end >>= fun block_header ->
  let expected_context_hash = block_header.Block_header.shell.context in
  (* Retrieve the input info. *)
  let info = protocol_data.Context.Protocol_data.info in
  let test_chain = protocol_data.test_chain_status in
  let data_hash = protocol_data.data_key in
  let parents = protocol_data.parents in
  let protocol_hash = protocol_data.protocol_hash in
  (* Validate the context hash consistency, and so the protocol data. *)
  Context.validate_context_hash_consistency_and_commit
    ~author:info.author
    ~timestamp:info.timestamp
    ~message:info.message
    ~data_hash
    ~parents
    ~expected_context_hash
    ~test_chain
    ~protocol_hash
    ~index >>= function
  | true ->
      let protocol_level = block_header.shell.proto_level in
      let block_level = block_header.shell.level in
      Store.Chain.Protocol_info.store store protocol_level (protocol_hash, block_level) >>= fun () ->
      return_unit
  | false -> fail (Wrong_protocol_hash protocol_hash)

let import_protocol_data_list index store block_hash_arr level_oldest_block protocol_data =
  let rec aux = function
    | [] -> return_unit
    | (level, protocol_data) :: xs ->
        import_protocol_data
          index store
          block_hash_arr level_oldest_block
          (level, protocol_data) >>=? fun () ->
        aux xs in
  aux protocol_data

let verify_predecessors header_opt pred_hash = match header_opt with
  | None -> return_unit
  | Some header ->
      fail_unless (header.Block_header.shell.level >= 2l &&
                   Block_hash.equal header.shell.predecessor pred_hash)
        (Snapshot_import_failure "inconsistent predecessors")

let verify_oldest_header oldest_header genesis_hash =
  let oldest_level = oldest_header.Block_header.shell.level in
  fail_unless (oldest_level >= 1l ||
               (Compare.Int32.(oldest_level = 1l) &&
                Block_hash.equal oldest_header.Block_header.shell.predecessor genesis_hash))
    (Snapshot_import_failure "inconsistent oldest level")

let block_validation
    succ_header_opt header_hash
    { Context.Pruned_block.block_header ; operations ; operation_hashes } =
  verify_predecessors succ_header_opt header_hash >>=? fun () ->
  check_operations_consistency block_header operations operation_hashes >>=? fun () ->
  return_unit

let import ~data_dir ~dir_cleaner ~patch_context ~genesis filename block =
  lwt_log_notice Tag.DSL.(fun f ->
      f "Importing data from snapshot file %a" -%a filename_tag filename
    ) >>= fun () ->
  begin match block with
    | None ->
        lwt_log_notice (fun f ->
            f "You may consider using the --block <block_hash> \
               argument to verify that the block imported is the one you expect"
          )
    | Some _ -> Lwt.return_unit
  end >>= fun () ->
  lwt_log_notice (fun f ->
      f "Retrieving and validating data. This can take a while, please bear with us"
    ) >>= fun () ->
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis.State.Chain.block in
  (* FIXME: use config value ? *)
  State.init
    ~context_root ~store_root genesis
    ~patch_context:(patch_context None) >>=? fun (state, chain_state, context_index, _history_mode) ->
  Store.init store_root >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let chain_data = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  let open Context in
  Lwt.try_bind
    (fun () ->
       let k_store_pruned_block
           { Context.Pruned_block.block_header ; operations ; operation_hashes }
           pruned_header_hash =
         Store.Block.Pruned_contents.store
           (block_store, pruned_header_hash) { header = block_header } >>= fun () ->
         Lwt_list.iter_s
           (fun (i, v) -> Store.Block.Operations.store (block_store, pruned_header_hash) i v)
           operations >>= fun () ->
         Lwt_list.iter_s
           (fun (i, v) -> Store.Block.Operation_hashes.store (block_store, pruned_header_hash) i v)
           operation_hashes >>= fun () ->
         return_unit
       in
       (* Restore context and fetch data *)
       restore_contexts
         context_index store ~filename k_store_pruned_block block_validation >>=?
       fun (predecessor_block_header, meta, history_mode, oldest_header_opt,
            rev_block_hashes, protocol_data) ->
       let oldest_header = Option.unopt_assert ~loc:__POS__ oldest_header_opt in
       let block_hashes_arr = Array.of_list rev_block_hashes in

       let write_predecessors_table to_write =
         Raw_store.with_atomic_rw store (fun () ->
             Lwt_list.iter_s (fun (current_hash, predecessors_list) ->
                 Lwt_list.iter_s (fun (l, h) ->
                     Store.Block.Predecessors.store (block_store, current_hash) l h
                   ) predecessors_list >>= fun () ->
                 match predecessors_list with
                 | (0, pred_hash) :: _ ->
                     Store.Chain_data.In_main_branch.store (chain_data, pred_hash) current_hash
                 | [] -> Lwt.return_unit
                 | _ :: _ -> assert false )
               to_write) in

       Lwt_list.fold_left_s (fun (cpt, to_write) current_hash ->
           Tezos_stdlib.Utils.display_progress
             ~refresh_rate:(cpt, 1_000)
             "Computing predecessors table %dK elements%!"
             (cpt / 1_000);
           begin if (cpt + 1) mod 5_000 = 0 then
               write_predecessors_table to_write >>= fun () ->
               Lwt.return_nil
             else
               Lwt.return to_write
           end >>= fun to_write ->
           let predecessors_list =
             compute_predecessors
               ~genesis_hash:genesis.block oldest_header.shell.level block_hashes_arr cpt in
           Lwt.return (cpt + 1, (current_hash, predecessors_list) :: to_write)
         ) (0, []) rev_block_hashes >>= fun (_, to_write) ->
       write_predecessors_table to_write >>= fun () ->
       Tezos_stdlib.Utils.display_progress_end () ;

       (* Process data imported from snapshot *)
       let { Block_data.block_header ; operations } = meta in
       let block_hash = Block_header.hash block_header in
       (* Checks that the block hash imported by the snapshot is the expected one *)
       begin
         match block with
         | Some str ->
             let bh = Block_hash.of_b58check_exn str in
             fail_unless
               (Block_hash.equal bh block_hash)
               (Inconsistent_imported_block (bh, block_hash))
         | None ->
             return_unit
       end >>=? fun () ->

       lwt_log_notice Tag.DSL.(fun f ->
           f "Setting current head to block %a"
           -% a Block_hash.Logging.tag (Block_header.hash block_header)
         ) >>= fun () ->
       let pred_context_hash = predecessor_block_header.shell.context in

       checkout_exn context_index pred_context_hash >>= fun predecessor_context ->

       (* ... we can now call apply ... *)
       Tezos_validation.Block_validation.apply
         chain_id
         ~max_operations_ttl:(Int32.to_int predecessor_block_header.shell.level)
         ~predecessor_block_header:predecessor_block_header
         ~predecessor_context
         ~block_header
         operations >>=? fun block_validation_result ->

       check_context_hash_consistency
         block_validation_result
         block_header >>=? fun () ->

       verify_oldest_header oldest_header genesis.block >>=? fun () ->

       (* ... we set the history mode regarding the snapshot version hint ... *)
       set_history_mode store history_mode >>=? fun () ->

       (* ... and we import protocol data...*)
       import_protocol_data_list
         context_index chain_store block_hashes_arr
         oldest_header.Block_header.shell.level protocol_data  >>=? fun () ->

       (* Everything is ok. We can store the new head *)
       store_new_head
         chain_state
         chain_data
         ~genesis:genesis.block
         block_header
         operations
         block_validation_result >>=? fun () ->

       (* Update history mode flags *)
       update_checkpoint chain_state block_header >>= fun new_checkpoint ->
       update_savepoint chain_state new_checkpoint >>= fun () ->
       update_caboose
         chain_data
         ~genesis:genesis.block block_header oldest_header
         block_validation_result.validation_result.max_operations_ttl >>=? fun () ->
       Store.close store ;
       State.close state >>= fun () ->
       return_unit)
    (function
      | Ok () ->
          lwt_log_notice Tag.DSL.(fun f ->
              f "@[Successful import from file %a@]"
              -% a filename_tag filename
            ) >>= fun () ->
          return_unit
      | Error errors ->
          dir_cleaner data_dir >>= fun () ->
          Lwt.return (Error errors))
    (fun exn ->
       dir_cleaner data_dir >>= fun () ->
       Lwt.fail exn)
