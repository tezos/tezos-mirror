(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs. <contact@nomadic-labs.com>               *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

type status =
  | Export_unspecified_hash of Block_hash.t
  | Export_info of History_mode.t * Block_hash.t * Int32.t
  | Export_success of string
  | Set_history_mode of History_mode.t
  | Import_info of string
  | Import_unspecified_hash
  | Import_loading
  | Set_head of Block_hash.t
  | Import_success of string
  | Reconstruct_start_default
  | Reconstruct_end_default of Block_hash.t
  | Reconstruct_enum
  | Reconstruct_success

let status_pp ppf = function
  | Export_unspecified_hash h ->
      Format.fprintf
        ppf
        "There is no block hash specified with the `--block` option. Using %a \
         (last checkpoint)"
        Block_hash.pp
        h
  | Export_info (hm, h, l) ->
      Format.fprintf
        ppf
        "Exporting a snapshot in %a mode, targeting block hash %a at level %a"
        History_mode.pp
        hm
        Block_hash.pp
        h
        Format.pp_print_int
        (Int32.to_int l)
  | Export_success filename ->
      Format.fprintf ppf "@[Successful export: %s@]" filename
  | Set_history_mode hm ->
      Format.fprintf ppf "Setting history-mode to %a" History_mode.pp hm
  | Import_info filename ->
      Format.fprintf ppf "Importing data from snapshot file %s" filename
  | Import_unspecified_hash ->
      Format.fprintf
        ppf
        "You may consider using the --block <block_hash> argument to verify \
         that the block imported is the one you expected"
  | Import_loading ->
      Format.fprintf
        ppf
        "Retrieving and validating data. This can take a while, please bear \
         with us"
  | Set_head h ->
      Format.fprintf ppf "Setting current head to block %a" Block_hash.pp h
  | Import_success filename ->
      Format.fprintf ppf "@[Successful import from file %s@]" filename
  | Reconstruct_start_default ->
      Format.fprintf ppf "Starting reconstruct from genesis"
  | Reconstruct_end_default h ->
      Format.fprintf
        ppf
        "Starting reconstruct toward the predecessor of the current head (%a)"
        Block_hash.pp
        h
  | Reconstruct_enum ->
      Format.fprintf ppf "Enumerating all blocks to reconstruct"
  | Reconstruct_success ->
      Format.fprintf ppf "The storage was successfully reconstructed."

module Definition = struct
  let name = "snapshot"

  type t = status Time.System.stamped

  let encoding =
    let open Data_encoding in
    Time.System.stamped_encoding
    @@ union
         [ case
             (Tag 0)
             ~title:"Export_unspecified_hash"
             Block_hash.encoding
             (function Export_unspecified_hash h -> Some h | _ -> None)
             (fun h -> Export_unspecified_hash h);
           case
             (Tag 1)
             ~title:"Export_info"
             (obj3
                (req "history_mode" History_mode.encoding)
                (req "block_hash" Block_hash.encoding)
                (req "level" int32))
             (function Export_info (hm, h, l) -> Some (hm, h, l) | _ -> None)
             (fun (hm, h, l) -> Export_info (hm, h, l));
           case
             (Tag 2)
             ~title:"Export_success"
             string
             (function Export_success s -> Some s | _ -> None)
             (fun s -> Export_success s);
           case
             (Tag 3)
             ~title:"Set_history_mode"
             History_mode.encoding
             (function Set_history_mode hm -> Some hm | _ -> None)
             (fun hm -> Set_history_mode hm);
           case
             (Tag 4)
             ~title:"Import_info"
             string
             (function Import_info s -> Some s | _ -> None)
             (fun s -> Import_info s);
           case
             (Tag 5)
             ~title:"Import_unspecified_hash"
             empty
             (function Import_unspecified_hash -> Some () | _ -> None)
             (fun () -> Import_unspecified_hash);
           case
             (Tag 6)
             ~title:"Import_loading"
             empty
             (function Import_loading -> Some () | _ -> None)
             (fun () -> Import_loading);
           case
             (Tag 7)
             ~title:"Set_head"
             Block_hash.encoding
             (function Set_head h -> Some h | _ -> None)
             (fun h -> Set_head h);
           case
             (Tag 8)
             ~title:"Import_success"
             string
             (function Import_success s -> Some s | _ -> None)
             (fun s -> Import_success s);
           case
             (Tag 9)
             ~title:"Reconstruct_start_default"
             empty
             (function Reconstruct_start_default -> Some () | _ -> None)
             (fun () -> Reconstruct_start_default);
           case
             (Tag 10)
             ~title:"Reconstruct_end_default"
             Block_hash.encoding
             (function Reconstruct_end_default h -> Some h | _ -> None)
             (fun h -> Reconstruct_end_default h);
           case
             (Tag 11)
             ~title:"Reconstruct_enum"
             empty
             (function Reconstruct_enum -> Some () | _ -> None)
             (fun () -> Reconstruct_enum);
           case
             (Tag 12)
             ~title:"Reconstruct_success"
             empty
             (function Reconstruct_success -> Some () | _ -> None)
             (fun () -> Reconstruct_success) ]

  let pp ~short:_ ppf (status : t) =
    Format.fprintf ppf "%a" status_pp status.data

  let doc = "Snapshots status."

  let level (status : t) =
    match status.data with
    | Export_unspecified_hash _
    | Export_info _
    | Export_success _
    | Set_history_mode _
    | Import_info _
    | Import_unspecified_hash
    | Import_loading
    | Set_head _
    | Import_success _
    | Reconstruct_start_default
    | Reconstruct_end_default _
    | Reconstruct_enum
    | Reconstruct_success ->
        Internal_event.Notice
end

module Event_snapshot = Internal_event.Make (Definition)

let lwt_emit (status : status) =
  let time = Systime_os.now () in
  Event_snapshot.emit
    ~section:(Internal_event.Section.make_sanitized [Definition.name])
    (fun () -> Time.System.stamp ~time status)
  >>= function
  | Ok () ->
      Lwt.return_unit
  | Error el ->
      Format.kasprintf
        Lwt.fail_with
        "Snapshot_event.emit: %a"
        pp_print_error
        el

type error += Wrong_snapshot_export of History_mode.t * History_mode.t

type wrong_block_export_kind =
  | Pruned of Block_hash.t
  | Too_few_predecessors of Block_hash.t
  | Unknown_block of string

let pp_wrong_block_export_kind ppf = function
  | Pruned h ->
      Format.fprintf ppf "block %a because it is pruned" Block_hash.pp h
  | Too_few_predecessors h ->
      Format.fprintf
        ppf
        "block %a because it does not have enough predecessors"
        Block_hash.pp
        h
  | Unknown_block str ->
      Format.fprintf ppf "block %s because it cannot be found" str

let wrong_block_export_kind_encoding =
  let open Data_encoding in
  union
    [ case
        (Tag 0)
        ~title:"pruned"
        Block_hash.encoding
        (function Pruned h -> Some h | _ -> None)
        (fun h -> Pruned h);
      case
        (Tag 1)
        ~title:"too_few_predecessors"
        Block_hash.encoding
        (function Too_few_predecessors h -> Some h | _ -> None)
        (fun h -> Too_few_predecessors h);
      case
        (Tag 2)
        ~title:"unknown_hash"
        string
        (function Unknown_block s -> Some s | _ -> None)
        (fun s -> Unknown_block s) ]

type error += Wrong_block_export of wrong_block_export_kind

type error += Inconsistent_imported_block of Block_hash.t * Block_hash.t

type error += Snapshot_import_failure of string

type error += Wrong_protocol_hash of Protocol_hash.t

type error +=
  | Inconsistent_operation_hashes of
      (Operation_list_list_hash.t * Operation_list_list_hash.t)

type error += Inconsistent_operation_hashes_lengths

type error += Cannot_reconstruct of History_mode.t

type error += Invalid_block_specification of string

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"WrongSnapshotExport"
    ~title:"Wrong snapshot export"
    ~description:
      "Snapshot exports is not compatible with the current configuration."
    ~pp:(fun ppf (src, dst) ->
      Format.fprintf
        ppf
        "Cannot export a %a snapshot from a %a node."
        History_mode.pp
        dst
        History_mode.pp
        src)
    (obj2 (req "src" History_mode.encoding) (req "dst" History_mode.encoding))
    (function
      | Wrong_snapshot_export (src, dst) -> Some (src, dst) | _ -> None)
    (fun (src, dst) -> Wrong_snapshot_export (src, dst)) ;
  register_error_kind
    `Permanent
    ~id:"WrongBlockExport"
    ~title:"Wrong block export"
    ~description:"The block to export in the snapshot is not valid."
    ~pp:(fun ppf kind ->
      Format.fprintf
        ppf
        "Fails to export snapshot using the %a."
        pp_wrong_block_export_kind
        kind)
    (obj1 (req "wrong_block_export" wrong_block_export_kind_encoding))
    (function Wrong_block_export kind -> Some kind | _ -> None)
    (fun kind -> Wrong_block_export kind) ;
  register_error_kind
    `Permanent
    ~id:"InconsistentImportedBlock"
    ~title:"Inconsistent imported block"
    ~description:"The imported block is not the expected one."
    ~pp:(fun ppf (got, exp) ->
      Format.fprintf
        ppf
        "The block contained in the file is %a instead of %a."
        Block_hash.pp
        got
        Block_hash.pp
        exp)
    (obj2
       (req "block_hash" Block_hash.encoding)
       (req "block_hash_expected" Block_hash.encoding))
    (function
      | Inconsistent_imported_block (got, exp) -> Some (got, exp) | _ -> None)
    (fun (got, exp) -> Inconsistent_imported_block (got, exp)) ;
  register_error_kind
    `Permanent
    ~id:"SnapshotImportFailure"
    ~title:"Snapshot import failure"
    ~description:"The imported snapshot is malformed."
    ~pp:(fun ppf msg ->
      Format.fprintf
        ppf
        "The data contained in the snapshot is not valid. The import \
         mechanism failed to validate the file: %s."
        msg)
    (obj1 (req "message" string))
    (function Snapshot_import_failure str -> Some str | _ -> None)
    (fun str -> Snapshot_import_failure str) ;
  register_error_kind
    `Permanent
    ~id:"WrongProtocolHash"
    ~title:"Wrong protocol hash"
    ~description:"Wrong protocol hash"
    ~pp:(fun ppf p ->
      Format.fprintf
        ppf
        "Wrong protocol hash (%a) found in snapshot. Snapshot is corrupted."
        Protocol_hash.pp
        p)
    (obj1 (req "protocol_hash" Protocol_hash.encoding))
    (function Wrong_protocol_hash p -> Some p | _ -> None)
    (fun p -> Wrong_protocol_hash p) ;
  register_error_kind
    `Permanent
    ~id:"InconsistentOperationHashes"
    ~title:"Inconsistent operation hashes"
    ~description:"The operations given do not match their hashes."
    ~pp:(fun ppf (oph, oph') ->
      Format.fprintf
        ppf
        "Inconsistent operation hashes. Expected: %a, got: %a."
        Operation_list_list_hash.pp
        oph
        Operation_list_list_hash.pp
        oph')
    (obj2
       (req "expected_operation_hashes" Operation_list_list_hash.encoding)
       (req "received_operation_hashes" Operation_list_list_hash.encoding))
    (function
      | Inconsistent_operation_hashes (oph, oph') ->
          Some (oph, oph')
      | _ ->
          None)
    (fun (oph, oph') -> Inconsistent_operation_hashes (oph, oph')) ;
  register_error_kind
    `Permanent
    ~id:"InconsistentOperationHashesLengths"
    ~title:"Inconsistent operation hashes lengths"
    ~description:"Different number of operations and hashes given."
    ~pp:(fun ppf () ->
      Format.pp_print_string ppf "Inconsistent operation hashes lengths")
    unit
    (function Inconsistent_operation_hashes_lengths -> Some () | _ -> None)
    (fun () -> Inconsistent_operation_hashes_lengths) ;
  register_error_kind
    `Permanent
    ~id:"CannotReconstruct"
    ~title:"Cannot reconstruct"
    ~description:"Cannot reconstruct"
    ~pp:(fun ppf hm ->
      Format.fprintf
        ppf
        "Cannot reconstruct storage from %a mode."
        History_mode.pp
        hm)
    (obj1 (req "history_mode " History_mode.encoding))
    (function Cannot_reconstruct hm -> Some hm | _ -> None)
    (fun hm -> Cannot_reconstruct hm) ;
  register_error_kind
    `Permanent
    ~id:"InvalidBlockSpecification"
    ~title:"Invalid block specification"
    ~description:"Invalid specification of block to import"
    ~pp:(fun ppf str ->
      Format.fprintf
        ppf
        "Cannot check the given block to import based on %s. You must specify \
         a valid block hash."
        str)
    (obj1 (req "str" string))
    (function Invalid_block_specification s -> Some s | _ -> None)
    (fun s -> Invalid_block_specification s)

open Filename.Infix

let context_dir data_dir = data_dir // "context"

let store_dir data_dir = data_dir // "store"

let compute_export_limit block_store chain_data_store block_header
    export_rolling =
  let block_hash = Block_header.hash block_header in
  Store.Block.Contents.read_opt (block_store, block_hash)
  >>= (function
        | Some contents ->
            return contents
        | None ->
            fail (Wrong_block_export (Pruned block_hash)))
  >>=? fun {max_operations_ttl; _} ->
  if not export_rolling then
    Store.Chain_data.Caboose.read chain_data_store
    >>=? fun (caboose_level, _) -> return (max 1l caboose_level)
  else
    let limit =
      Int32.(
        sub block_header.Block_header.shell.level (of_int max_operations_ttl))
    in
    (* fails when the limit exceeds the genesis or the genesis is
       included in the export limit *)
    fail_when
      (limit <= 0l)
      (Wrong_block_export (Too_few_predecessors block_hash))
    >>=? fun () -> return limit

(** When called with a block, returns its predecessor if it exists and
    its protocol_data if the block is a transition block (i.e. protocol
    level changing block) or when there is no more predecessor. *)
let pruned_block_iterator index block_store limit header =
  if header.Block_header.shell.level <= limit then
    Context.get_protocol_data_from_header index header
    >>= fun protocol_data -> return (None, Some protocol_data)
  else
    let pred_hash = header.Block_header.shell.predecessor in
    State.Block.Header.read (block_store, pred_hash)
    >>=? fun pred_header ->
    Store.Block.Operations.bindings (block_store, pred_hash)
    >>= fun pred_operations ->
    Store.Block.Operation_hashes.bindings (block_store, pred_hash)
    >>= fun pred_operation_hashes ->
    let pruned_block =
      {
        Context.Pruned_block.block_header = pred_header;
        operations = pred_operations;
        operation_hashes = pred_operation_hashes;
      }
    in
    let header_proto_level = header.Block_header.shell.proto_level in
    let pred_header_proto_level = pred_header.Block_header.shell.proto_level in
    if header_proto_level <> pred_header_proto_level then
      Context.get_protocol_data_from_header index header
      >>= fun proto_data -> return (Some pruned_block, Some proto_data)
    else return (Some pruned_block, None)

let parse_block_arg = function
  | None ->
      return_none
  | Some str -> (
    match Block_services.parse_block str with
    | Ok v ->
        return_some v
    | Error err ->
        failwith "Invalid value for `--block`: %s" err )

let export ?(export_rolling = false) ~context_root ~store_root ~genesis
    filename ~block =
  State.init ~context_root ~store_root genesis ~readonly:true
  >>=? fun (state, chain_state, context_index, history_mode) ->
  Store.init store_root
  >>=? fun store ->
  let chain_id = Chain_id.of_block_hash genesis.block in
  let chain_store = Store.Chain.get store chain_id in
  let chain_data_store = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  ( match history_mode with
  | Archive | Full ->
      return_unit
  | Rolling as history_mode ->
      if export_rolling then return_unit
      else fail (Wrong_snapshot_export (history_mode, History_mode.Full)) )
  >>=? fun () ->
  parse_block_arg block
  >>=? (function
         | Some block -> (
             Block_directory.get_block chain_state block
             >>= function
             | None ->
                 fail
                   (Wrong_block_export
                      (Unknown_block (Block_services.to_string block)))
             | Some bh ->
                 return (State.Block.hash bh) )
         | None ->
             Store.Chain_data.Checkpoint.read_opt chain_data_store
             >|= WithExceptions.Option.get ~loc:__LOC__
             >>= fun last_checkpoint ->
             if last_checkpoint.shell.level = 0l then
               fail (Wrong_block_export (Too_few_predecessors genesis.block))
             else
               let last_checkpoint_hash = Block_header.hash last_checkpoint in
               lwt_emit (Export_unspecified_hash last_checkpoint_hash)
               >>= fun () -> return last_checkpoint_hash)
  >>=? fun block_hash ->
  State.Block.Header.read_opt (block_store, block_hash)
  >>= (function
        | None ->
            fail
              (Wrong_block_export
                 (Unknown_block (Block_hash.to_b58check block_hash)))
        | Some block_header ->
            let export_mode =
              if export_rolling then History_mode.Rolling else Full
            in
            lwt_emit
              (Export_info (export_mode, block_hash, block_header.shell.level))
            >>= fun () ->
            (* Get block predecessor's block header *)
            Store.Block.Predecessors.read (block_store, block_hash) 0
            >>=? fun pred_block_hash ->
            State.Block.Header.read (block_store, pred_block_hash)
            >>=? fun pred_block_header ->
            (* Get operation list *)
            let validations_passes = block_header.shell.validation_passes in
            List.map_es
              (fun i ->
                Store.Block.Operations.read (block_store, block_hash) i)
              (0 -- (validations_passes - 1))
            >>=? fun operations ->
            Store.Block.Block_metadata_hash.read_opt
              (block_store, pred_block_hash)
            >>= fun predecessor_block_metadata_hash ->
            ( if pred_block_header.shell.validation_passes = 0 then return_none
            else
              Store.Block.Operations_metadata_hashes.known
                (block_store, pred_block_hash)
                0
              >>= function
              | false ->
                  return_none
              | true ->
                  List.map_es
                    (fun i ->
                      Store.Block.Operations_metadata_hashes.read
                        (block_store, pred_block_hash)
                        i)
                    (0 -- (pred_block_header.shell.validation_passes - 1))
                  >|=? Option.some )
            >>=? fun predecessor_ops_metadata_hashes ->
            compute_export_limit
              block_store
              chain_data_store
              block_header
              export_rolling
            >>=? fun export_limit ->
            let iterator =
              pruned_block_iterator context_index block_store export_limit
            in
            let block_data = {Context.Block_data.block_header; operations} in
            return
              ( pred_block_header,
                block_data,
                predecessor_block_metadata_hash,
                predecessor_ops_metadata_hashes,
                export_mode,
                iterator ))
  >>=? fun data_to_dump ->
  Context.dump_contexts context_index data_to_dump ~filename
  >>=? fun () ->
  lwt_emit (Export_success filename)
  >>= fun () ->
  Store.close store ;
  State.close state >>= fun () -> return_unit

let check_operations_consistency block_header operations operation_hashes =
  (* Compute operations hashes and compare *)
  List.iter2_e
    ~when_different_lengths:Inconsistent_operation_hashes_lengths
    (fun (_, op) (_, oph) ->
      let expected_op_hash = List.map Operation.hash op in
      List.iter2
        ~when_different_lengths:Inconsistent_operation_hashes_lengths
        (fun expected found -> assert (Operation_hash.equal expected found))
        expected_op_hash
        oph)
    operations
    operation_hashes
  |> (function Ok _ as ok -> ok | Error err -> error err) (* To make a trace *)
  >>? fun () ->
  (* Check header hashes based on Merkle tree *)
  let hashes =
    List.rev_map (fun (_, opl) -> List.map Operation.hash opl) operations
  in
  let computed_hash =
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute hashes)
  in
  let are_oph_equal =
    Operation_list_list_hash.equal
      computed_hash
      block_header.Block_header.shell.operations_hash
  in
  error_unless
    are_oph_equal
    (Inconsistent_operation_hashes
       (computed_hash, block_header.Block_header.shell.operations_hash))

let check_operations_consistency block_header operations operation_hashes =
  Lwt.return
  @@ check_operations_consistency block_header operations operation_hashes

let compute_predecessors ~genesis_hash oldest_level block_hashes i =
  let rec step s d acc =
    if oldest_level = 1l && i - d = -1 then List.rev ((s, genesis_hash) :: acc)
    else if i - d < 0 then List.rev acc
    else step (s + 1) (d * 2) ((s, block_hashes.(i - d)) :: acc)
  in
  step 0 1 []

let check_context_hash_consistency block_validation_result block_header =
  fail_unless
    (Context_hash.equal
       block_validation_result.Tezos_validation.Block_validation.context_hash
       block_header.Block_header.shell.context)
    (Snapshot_import_failure "resulting context hash does not match")

let set_history_mode store history_mode =
  lwt_emit (Set_history_mode history_mode)
  >>= fun () ->
  Store.Configuration.History_mode.store store history_mode
  >>= fun () -> return_unit

let store_new_head chain_state chain_data ~genesis block_header operations
    block_validation_result =
  let ({ validation_store;
         block_metadata;
         ops_metadata;
         block_metadata_hash;
         ops_metadata_hashes;
         forking_testchain }
        : Tezos_validation.Block_validation.result) =
    block_validation_result
  in
  State.Block.store
    chain_state
    block_header
    block_metadata
    operations
    ops_metadata
    block_metadata_hash
    ops_metadata_hashes
    ~forking_testchain
    validation_store
  >>=? fun new_head ->
  match new_head with
  | None ->
      (* Should not happen as the data-dir must be empty *)
      fail
        (Snapshot_import_failure
           "a chain head is already registered in the store")
  | Some new_head ->
      (* New head is set*)
      Store.Chain_data.Known_heads.remove chain_data genesis
      >>= fun () ->
      Store.Chain_data.Known_heads.store chain_data (State.Block.hash new_head)
      >>= fun () ->
      Store.Chain_data.Current_head.store
        chain_data
        (State.Block.hash new_head)
      >>= fun () -> return_unit

let update_checkpoint chain_state checkpoint_header =
  let block_hash = Block_header.hash checkpoint_header in
  (* Imported block is set as the current checkpoint/save_point … *)
  let new_checkpoint =
    (checkpoint_header.Block_header.shell.level, block_hash)
  in
  State.Chain.set_checkpoint chain_state checkpoint_header
  >>= fun () -> Lwt.return new_checkpoint

let update_savepoint chain_state new_savepoint =
  State.update_chain_data chain_state (fun store data ->
      let new_data = {data with save_point = new_savepoint} in
      Store.Chain_data.Save_point.store store new_savepoint
      >>= fun () -> Lwt.return (Some new_data, ()))

let update_caboose chain_data ~genesis block_header oldest_header max_op_ttl =
  let oldest_level = oldest_header.Block_header.shell.level in
  let caboose_level = if oldest_level = 1l then 0l else oldest_level in
  let caboose_hash =
    if oldest_level = 1l then genesis else Block_header.hash oldest_header
  in
  let minimal_caboose_level =
    Int32.(sub block_header.Block_header.shell.level (of_int max_op_ttl))
  in
  fail_unless
    Compare.Int32.(caboose_level <= minimal_caboose_level)
    (Snapshot_import_failure
       (Format.sprintf "caboose level (%ld) is not valid" caboose_level))
  >>=? fun () ->
  Store.Chain_data.Caboose.store chain_data (caboose_level, caboose_hash)
  >>= fun () -> return_unit

let import_protocol_data index store block_hash_arr level_oldest_block
    new_header (level, protocol_data) =
  (* Retrieve the original context hash of the block. *)
  let delta = Int32.(to_int (sub level level_oldest_block)) in
  ( if delta = Array.length block_hash_arr then Lwt.return new_header
  else
    let pruned_block_hash = block_hash_arr.(delta) in
    let block_store = Store.Block.get store in
    State.Block.Header.read_opt (block_store, pruned_block_hash)
    >>= function
    | None -> assert false | Some block_header -> Lwt.return block_header )
  >>= fun block_header ->
  let expected_context_hash = block_header.Block_header.shell.context in
  (* Retrieve the input info. *)
  let info = protocol_data.Context.Protocol_data.info in
  let test_chain = protocol_data.test_chain_status in
  let data_hash = protocol_data.data_key in
  let parents = protocol_data.parents in
  let protocol_hash = protocol_data.protocol_hash in
  let predecessor_block_metadata_hash =
    protocol_data.predecessor_block_metadata_hash
  in
  let predecessor_ops_metadata_hash =
    protocol_data.predecessor_ops_metadata_hash
  in
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
    ~predecessor_block_metadata_hash
    ~predecessor_ops_metadata_hash
    ~index
  >>= function
  | true ->
      let protocol_level = block_header.shell.proto_level in
      let block_level = block_header.shell.level in
      Store.Chain.Protocol_info.store
        store
        protocol_level
        (protocol_hash, block_level)
      >>= fun () -> return_unit
  | false ->
      fail (Wrong_protocol_hash protocol_hash)

let import_protocol_data_list index store block_hash_arr level_oldest_block
    new_head protocol_data =
  let rec aux = function
    | [] ->
        return_unit
    | (level, protocol_data) :: xs ->
        import_protocol_data
          index
          store
          block_hash_arr
          level_oldest_block
          new_head
          (level, protocol_data)
        >>=? fun () -> aux xs
  in
  aux protocol_data

let verify_predecessors header_opt pred_hash =
  match header_opt with
  | None ->
      return_unit
  | Some header ->
      fail_unless
        ( header.Block_header.shell.level >= 2l
        && Block_hash.equal header.shell.predecessor pred_hash )
        (Snapshot_import_failure "predecessors inconsistency")

let verify_oldest_header oldest_header genesis_hash =
  let oldest_level = oldest_header.Block_header.shell.level in
  fail_unless
    ( oldest_level >= 1l
    || Compare.Int32.(oldest_level = 1l)
       && Block_hash.equal
            oldest_header.Block_header.shell.predecessor
            genesis_hash )
    (Snapshot_import_failure "oldest level inconsistency")

let block_validation succ_header_opt header_hash
    {Context.Pruned_block.block_header; operations; operation_hashes} =
  verify_predecessors succ_header_opt header_hash
  >>=? fun () ->
  check_operations_consistency block_header operations operation_hashes
  >>=? fun () -> return_unit

(* Reconstruct the storage (without checking if the context/store is already populated) *)
let reconstruct_storage store context_index chain_id ~user_activated_upgrades
    ~user_activated_protocol_overrides block_store chain_state chain_store
    (history_list : Block_hash.t list) =
  let history = Array.of_list history_list in
  let limit = Array.length history in
  let rec reconstruct_chunks level =
    Store.with_atomic_rw store (fun () ->
        let rec reconstruct_chunks level =
          Tezos_stdlib_unix.Utils.display_progress (fun m ->
              m "Reconstructing contexts: %i/%i" level limit) ;
          if level = limit then return level
          else
            let block_hash = history.(level) in
            State.Block.Header.read (block_store, block_hash)
            >>=? fun block_header ->
            let validations_passes = block_header.shell.validation_passes in
            List.map_es
              (fun i ->
                Store.Block.Operations.read (block_store, block_hash) i)
              (0 -- (validations_passes - 1))
            >>=? fun operations ->
            let predecessor_block_hash = block_header.shell.predecessor in
            State.Block.Header.read (block_store, predecessor_block_hash)
            >>=? fun predecessor_block_header ->
            Store.Block.Block_metadata_hash.read_opt
              (block_store, predecessor_block_hash)
            >>= fun predecessor_block_metadata_hash ->
            ( if predecessor_block_header.shell.validation_passes = 0 then
              return_none
            else
              Store.Block.Operations_metadata_hashes.known
                (block_store, predecessor_block_hash)
                0
              >>= function
              | false ->
                  return_none
              | true ->
                  List.map_es
                    (fun i ->
                      Store.Block.Operations_metadata_hashes.read
                        (block_store, predecessor_block_hash)
                        i)
                    ( 0
                    -- (predecessor_block_header.shell.validation_passes - 1)
                    )
                  >|=? fun hashes ->
                  Some
                    ( List.map Operation_metadata_list_hash.compute hashes
                    |> Operation_metadata_list_list_hash.compute ) )
            >>=? fun predecessor_ops_metadata_hash ->
            let pred_context_hash = predecessor_block_header.shell.context in
            Context.checkout_exn context_index pred_context_hash
            >>= fun predecessor_context ->
            let max_operations_ttl =
              Int32.to_int predecessor_block_header.shell.level
            in
            let env =
              {
                Block_validation.max_operations_ttl;
                chain_id;
                predecessor_block_header;
                predecessor_block_metadata_hash;
                predecessor_ops_metadata_hash;
                predecessor_context;
                user_activated_upgrades;
                user_activated_protocol_overrides;
              }
            in
            Tezos_validation.Block_validation.apply env block_header operations
            >>=? fun block_validation_result ->
            check_context_hash_consistency
              block_validation_result.validation_store
              block_header
            >>=? fun () ->
            let { Tezos_validation.Block_validation.validation_store;
                  block_metadata;
                  block_metadata_hash;
                  ops_metadata;
                  ops_metadata_hashes;
                  _ } =
              block_validation_result
            in
            let contents =
              {
                header = block_header;
                Store.Block.message = validation_store.message;
                max_operations_ttl = validation_store.max_operations_ttl;
                last_allowed_fork_level =
                  validation_store.last_allowed_fork_level;
                context = validation_store.context_hash;
                metadata = block_metadata;
              }
            in
            let st = (block_store, block_hash) in
            Store.Block.Pruned_contents.remove st
            >>= fun () ->
            Store.Block.Contents.store st contents
            >>= fun () ->
            List.iteri_p
              (fun i ops ->
                Store.Block.Operation_hashes.store
                  st
                  i
                  (List.map Operation.hash ops))
              operations
            >>= fun () ->
            List.iteri_p
              (fun i ops -> Store.Block.Operations.store st i ops)
              operations
            >>= fun () ->
            List.iteri_p
              (fun i ops -> Store.Block.Operations_metadata.store st i ops)
              ops_metadata
            >>= fun () ->
            Option.iter_s
              (Store.Block.Block_metadata_hash.store st)
              block_metadata_hash
            >>= fun () ->
            Option.iter_s
              (Lwt_list.iteri_p (fun i hashes ->
                   Store.Block.Operations_metadata_hashes.store st i hashes))
              ops_metadata_hashes
            >>= fun () -> reconstruct_chunks (level + 1)
        in
        if (level + 1) mod 1000 = 0 then return level
        else reconstruct_chunks level)
    >>=? fun level ->
    if level = limit then return_unit else reconstruct_chunks limit
  in
  set_history_mode store History_mode.Archive
  >>=? fun () ->
  reconstruct_chunks 0
  >>=? fun _cpt ->
  Tezos_stdlib_unix.Utils.display_progress_end () ;
  Store.Chain.Genesis_hash.read chain_store
  >>=? fun genesis_hash ->
  let new_savepoint = (0l, genesis_hash) in
  update_savepoint chain_state new_savepoint
  >>= fun () ->
  let chain_data = Store.Chain_data.get chain_store in
  Store.Chain_data.Caboose.store chain_data (0l, genesis_hash)
  >>= fun () -> return_unit

let reconstruct chain_id ~user_activated_upgrades
    ~user_activated_protocol_overrides store chain_state context_index =
  let chain_store = Store.Chain.get store chain_id in
  let block_store = Store.Block.get chain_store in
  let chain_data_store = Store.Chain_data.get chain_store in
  Store.Configuration.History_mode.read store
  >>=? fun history_mode ->
  fail_unless
    (history_mode = History_mode.Full)
    (Cannot_reconstruct history_mode)
  >>=? fun () ->
  let low_limit = 1l in
  lwt_emit Reconstruct_start_default
  >>= fun () ->
  Store.Chain_data.Save_point.read chain_data_store
  >>=? fun (_, savepoint_hash) ->
  lwt_emit (Reconstruct_end_default savepoint_hash)
  >>= fun () ->
  State.Block.Header.read (block_store, savepoint_hash)
  >>=? fun savepoint_header ->
  let high_limit = savepoint_header.Block_header.shell.predecessor in
  lwt_emit Reconstruct_enum
  >>= fun () ->
  let rec gather_history low_limit block_hash acc =
    Store.Block.Pruned_contents.read_opt (block_store, block_hash)
    >>= function
    | Some {header; _} ->
        if header.shell.level = low_limit then return (block_hash :: acc)
        else
          gather_history low_limit header.shell.predecessor (block_hash :: acc)
    | None ->
        Store.Block.Contents.known (block_store, block_hash)
        >>= fun is_contents_known ->
        if is_contents_known then return acc
        else
          failwith
            "Unexpected missing block in store: %a"
            Block_hash.pp
            block_hash
  in
  gather_history low_limit high_limit []
  >>=? fun hash_history ->
  reconstruct_storage
    store
    context_index
    chain_id
    ~user_activated_upgrades
    ~user_activated_protocol_overrides
    block_store
    chain_state
    chain_store
    hash_history
  >>=? fun () -> lwt_emit Reconstruct_success >>= fun () -> return_unit

let import ?(reconstruct = false) ?patch_context ~data_dir
    ~user_activated_upgrades ~user_activated_protocol_overrides ~dir_cleaner
    ~genesis filename ~block =
  lwt_emit (Import_info filename)
  >>= fun () ->
  ( match block with
  | None ->
      lwt_emit Import_unspecified_hash
  | Some _ ->
      Lwt.return_unit )
  >>= fun () ->
  lwt_emit Import_loading
  >>= fun () ->
  let context_root = context_dir data_dir in
  let store_root = store_dir data_dir in
  let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
  State.init ~context_root ~store_root genesis ?patch_context
  >>=? fun (state, chain_state, context_index, _history_mode) ->
  Store.init store_root
  >>=? fun store ->
  let chain_store = Store.Chain.get store chain_id in
  let chain_data = Store.Chain_data.get chain_store in
  let block_store = Store.Block.get chain_store in
  let open Context in
  Lwt.try_bind
    (fun () ->
      let k_store_pruned_blocks data =
        Store.with_atomic_rw store (fun () ->
            List.iter_s
              (fun (pruned_header_hash, pruned_block) ->
                Store.Block.Pruned_contents.store
                  (block_store, pruned_header_hash)
                  {header = pruned_block.Context.Pruned_block.block_header}
                >>= fun () ->
                List.iter_s
                  (fun (i, v) ->
                    Store.Block.Operations.store
                      (block_store, pruned_header_hash)
                      i
                      v)
                  pruned_block.operations
                >>= fun () ->
                List.iter_s
                  (fun (i, v) ->
                    Store.Block.Operation_hashes.store
                      (block_store, pruned_header_hash)
                      i
                      v)
                  pruned_block.operation_hashes)
              data)
        >>= fun () -> return_unit
      in
      (* Restore context and fetch data *)
      restore_contexts
        context_index
        ~filename
        k_store_pruned_blocks
        block_validation
      >>=? fun ( predecessor_block_header,
                 meta,
                 predecessor_block_metadata_hash,
                 predecessor_ops_metadata_hashes,
                 history_mode,
                 oldest_header_opt,
                 rev_block_hashes,
                 protocol_data ) ->
      let oldest_header =
        WithExceptions.Option.get ~loc:__LOC__ oldest_header_opt
      in
      let block_hashes_arr = Array.of_list rev_block_hashes in
      let write_predecessors_table to_write =
        Store.with_atomic_rw store (fun () ->
            List.iter_s
              (fun (current_hash, predecessors_list) ->
                List.iter_s
                  (fun (l, h) ->
                    Store.Block.Predecessors.store
                      (block_store, current_hash)
                      l
                      h)
                  predecessors_list
                >>= fun () ->
                match predecessors_list with
                | (0, pred_hash) :: _ ->
                    Store.Chain_data.In_main_branch.store
                      (chain_data, pred_hash)
                      current_hash
                | [] ->
                    Lwt.return_unit
                | _ :: _ ->
                    assert false)
              to_write)
      in
      List.fold_left_s
        (fun (cpt, to_write) current_hash ->
          Tezos_stdlib_unix.Utils.display_progress
            ~refresh_rate:(cpt, 1_000)
            (fun f ->
              f "Computing predecessors table %dK elements%!" (cpt / 1_000)) ;
          ( if (cpt + 1) mod 5_000 = 0 then
            write_predecessors_table to_write >>= fun () -> Lwt.return_nil
          else Lwt.return to_write )
          >>= fun to_write ->
          let predecessors_list =
            compute_predecessors
              ~genesis_hash:genesis.block
              oldest_header.shell.level
              block_hashes_arr
              cpt
          in
          Lwt.return (cpt + 1, (current_hash, predecessors_list) :: to_write))
        (0, [])
        rev_block_hashes
      >>= fun (_, to_write) ->
      write_predecessors_table to_write
      >>= fun () ->
      Tezos_stdlib_unix.Utils.display_progress_end () ;
      (* Process data imported from snapshot *)
      let {Block_data.block_header; operations} = meta in
      let block_hash = Block_header.hash block_header in
      (* Checks that the block hash imported by the snapshot is the expected one *)
      parse_block_arg block
      >>=? (function
             | Some str -> (
               match str with
               | `Hash (bh, _) ->
                   fail_unless
                     (Block_hash.equal bh block_hash)
                     (Inconsistent_imported_block (bh, block_hash))
               | _ ->
                   fail
                     (Invalid_block_specification
                        (Block_services.to_string str)) )
             | None ->
                 return_unit)
      >>=? fun () ->
      lwt_emit (Set_head (Block_header.hash block_header))
      >>= fun () ->
      let pred_context_hash = predecessor_block_header.shell.context in
      checkout_exn context_index pred_context_hash
      >>= fun predecessor_context ->
      let max_operations_ttl =
        Int32.to_int predecessor_block_header.shell.level
      in
      let predecessor_ops_metadata_hash =
        Option.map
          (fun hashes ->
            List.map Operation_metadata_list_hash.compute hashes
            |> Operation_metadata_list_list_hash.compute)
          predecessor_ops_metadata_hashes
      in
      Option.iter_s
        (Store.Block.Block_metadata_hash.store
           (block_store, Block_header.hash predecessor_block_header))
        predecessor_block_metadata_hash
      >>= fun () ->
      Option.iter_s
        (Lwt_list.iteri_p (fun i hashes ->
             Store.Block.Operations_metadata_hashes.store
               (block_store, Block_header.hash predecessor_block_header)
               i
               hashes))
        predecessor_ops_metadata_hashes
      >>= fun () ->
      let env =
        {
          Block_validation.max_operations_ttl;
          chain_id;
          predecessor_block_header;
          predecessor_block_metadata_hash;
          predecessor_ops_metadata_hash;
          predecessor_context;
          user_activated_upgrades;
          user_activated_protocol_overrides;
        }
      in
      (* ... we can now call apply ... *)
      Tezos_validation.Block_validation.apply env block_header operations
      >>=? fun block_validation_result ->
      check_context_hash_consistency
        block_validation_result.validation_store
        block_header
      >>=? fun () ->
      verify_oldest_header oldest_header genesis.block
      >>=? fun () ->
      ( if not reconstruct then set_history_mode store history_mode
      else return_unit )
      >>=? fun () ->
      (* ... and we import protocol data...*)
      import_protocol_data_list
        context_index
        chain_store
        block_hashes_arr
        oldest_header.Block_header.shell.level
        block_header
        protocol_data
      >>=? fun () ->
      (* Everything is ok. We can store the new head *)
      store_new_head
        chain_state
        chain_data
        ~genesis:genesis.block
        block_header
        operations
        block_validation_result
      >>=? fun () ->
      (* Update history mode flags *)
      update_checkpoint chain_state block_header
      >>= fun new_checkpoint ->
      update_savepoint chain_state new_checkpoint
      >>= fun () ->
      update_caboose
        chain_data
        ~genesis:genesis.block
        block_header
        oldest_header
        block_validation_result.validation_store.max_operations_ttl
      >>=? fun () ->
      ( match reconstruct with
      | true ->
          if history_mode = History_mode.Full then
            reconstruct_storage
              store
              context_index
              chain_id
              ~user_activated_upgrades
              ~user_activated_protocol_overrides
              block_store
              chain_state
              chain_store
              rev_block_hashes
            >>=? fun () ->
            lwt_emit Reconstruct_success >>= fun () -> return_unit
          else fail (Cannot_reconstruct history_mode)
      | false ->
          return_unit )
      >>=? fun () ->
      Store.close store ;
      State.close state >>= fun () -> return_unit)
    (function
      | Ok () ->
          lwt_emit (Import_success filename) >>= fun () -> return_unit
      | Error errors ->
          dir_cleaner data_dir >>= fun () -> Lwt.return (Error errors))
    (fun exn -> dir_cleaner data_dir >>= fun () -> Lwt.fail exn)
