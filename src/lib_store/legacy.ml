(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

open Filename.Infix

(* Hardcoded networks data *)
module Hardcoded = struct
  type network = {name : Distributed_db_version.Name.t; cycle_length : int}

  let proj (name, cycle_length) =
    {name = Distributed_db_version.Name.of_string name; cycle_length}

  (* Hardcoded cycle length *)
  let supported_networks =
    List.map
      proj
      [
        ("TEZOS_MAINNET", 4096);
        ("TEZOS_EDO2NET_2021-02-11T14:00:00Z", 2048);
        ("TEZOS_FLORENCENOBANET_2021-03-04T20:00:00Z", 2048);
        ("TEZOS_GRANADANET_2021-05-21T15:00:00Z", 8192);
        ("TEZOS", 8);
      ]

  let cycle_length ~chain_name =
    List.find_map
      (fun {name; cycle_length} ->
        if chain_name = name then Some cycle_length else None)
      supported_networks
    |> WithExceptions.Option.get ~loc:__LOC__

  let check_network ~chain_name =
    if not (List.exists (fun {name; _} -> chain_name = name) supported_networks)
    then
      failwith
        "Cannot perform operation for chain_name %a. Only %a are supported."
        Distributed_db_version.Name.pp
        chain_name
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
           (fun ppf {name; _} ->
             Format.fprintf ppf "%a" Distributed_db_version.Name.pp name))
        supported_networks
    else return_unit

  (* Tells if the setting the checkpoint requires to cement blocks *)
  let may_update_checkpoint ~cycle_length nb_blocks = nb_blocks = cycle_length
end

(* Legacy store conversion *)

type error +=
  | Failed_to_convert_protocol of Protocol_hash.t
  | (* TODO: Better way to handle errors ? *) Failed_to_upgrade of string

let () =
  register_error_kind
    `Permanent
    ~id:"legacy.failed_to_convert_protocol"
    ~title:"Failed to convert protocol"
    ~description:"Failed to convert protocol from legacy store."
    ~pp:(fun ppf ->
      Format.fprintf
        ppf
        "Failed to convert protocol %a from legacy store."
        Protocol_hash.pp)
    Data_encoding.(obj1 (req "protocol_hash" Protocol_hash.encoding))
    (function Failed_to_convert_protocol p -> Some p | _ -> None)
    (fun p -> Failed_to_convert_protocol p) ;
  register_error_kind
    `Permanent
    ~id:"legacy.failed_to_upgrade"
    ~title:"Failed to upgrade"
    ~description:"Failed to upgrade the store."
    ~pp:(fun ppf -> Format.fprintf ppf "Failed to upgrade the store: %s.")
    Data_encoding.(obj1 (req "msg" string))
    (function Failed_to_upgrade s -> Some s | _ -> None)
    (fun s -> Failed_to_upgrade s)

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "legacy"; "upgrade"]

  let level = Internal_event.Notice

  let restoring_after_failure =
    Internal_event.Simple.declare_1
      ~level
      ~section
      ~name:"restoring_after_failure"
      ~msg:
        "cleaning directory {directory} because of failure: restoring the \
         former store"
      ~pp1:Format.pp_print_string
      ("directory", Data_encoding.string)

  let advertise_upgrade_mode =
    Internal_event.Simple.declare_2
      ~level
      ~section
      ~name:"advertise_upgrade_mode"
      ~msg:"upgrading storage from '{old_hm}' to '{new_hm}'"
      ~pp1:Format.pp_print_string
      ("old_hm", Data_encoding.string)
      ~pp2:Format.pp_print_string
      ("new_hm", Data_encoding.string)

  let upgrade_completed =
    Internal_event.Simple.declare_1
      ~level
      ~section
      ~name:"upgrade_completed"
      ~msg:
        "upgrade completed - you may now safely remove the former storage \
         located at '{path}'"
      ~pp1:Format.pp_print_string
      ("path", Data_encoding.string)
end

let hash_header legacy_chain_store header =
  let is_genesis = Compare.Int32.(header.Block_header.shell.level = 0l) in
  if is_genesis then Legacy_store.Chain.Genesis_hash.read legacy_chain_store
  else return (Block_header.hash header)

type with_metadata = Required | Unwanted | Optional

(* Build a Store.Block.block_repr from reading the data from a legacy
   store. [with_metadata] determines the presence of metadata based on
   the following rules:
   - Required: output block must contain metadata.
   - Unwanted: output block must not contain metadata. The block can
     be deprieved of its metadata.
   - Optional: output block may contain metadata. The block is return
     as it is. *)
let make_block_repr ~with_metadata legacy_chain_state hash =
  Legacy_state.Block.read legacy_chain_state hash >>=? fun block ->
  Legacy_state.Block.all_operations block >>= fun operations ->
  Legacy_state.Block.metadata_hash block >>= fun block_metadata_hash ->
  Legacy_state.Block.all_operations_metadata_hashes block
  >>= fun operations_metadata_hashes ->
  let contents =
    ({
       header = block.header;
       operations;
       block_metadata_hash;
       operations_metadata_hashes;
     }
      : Block_repr.contents)
  in
  Legacy_state.Block.read_contents_opt block >>= fun stored_metadata ->
  match (stored_metadata, with_metadata) with
  | (None, Required) -> fail (Legacy_state.Block_contents_not_found block.hash)
  | (None, Unwanted) | (None, Optional) ->
      let metadata = None in
      return ({hash; contents; metadata} : Block_repr.t)
  | (Some m, Required) | (Some m, Optional) ->
      Legacy_state.Block.all_operations_metadata block
      >>= fun operations_metadata ->
      let metadata =
        Some
          ({
             message = m.message;
             max_operations_ttl = m.max_operations_ttl;
             last_allowed_fork_level = m.last_allowed_fork_level;
             block_metadata = m.metadata;
             operations_metadata;
           }
            : Block_repr.metadata)
      in
      return ({hash; contents; metadata} : Block_repr.t)
  | (Some _, Unwanted) ->
      let metadata = None in
      return ({hash; contents; metadata} : Block_repr.t)

(* Updates the protocol table. Inserts entry when the proto_level of
   [prev_block] differs from [proto_level] (which is the protocol
   level of the successor of [prev_block]). *)
(* TODO recheck this function *)
let may_update_protocol_table legacy_chain_store chain_store ~prev_block ~block
    =
  let proto_level = Block_repr.proto_level block in
  let block_level = Block_repr.level block in
  if proto_level <> Block_repr.proto_level prev_block then (
    Legacy_store.Chain.Protocol_info.bindings legacy_chain_store
    >>= fun protocol_table ->
    let (proto_hash, transition_level) : Legacy_store.Chain.Protocol_info.value
        =
      List.assoc ~equal:Int.equal proto_level protocol_table
      |> WithExceptions.Option.get ~loc:__LOC__
    in
    assert (Int32.equal transition_level block_level) ;
    Store.Chain.may_update_protocol_level
      chain_store
      ~pred:(Store.Unsafe.block_of_repr prev_block)
      ~protocol_level:proto_level
      (Store.Unsafe.block_of_repr block, proto_hash))
  else return_unit

(* Reads a block at a given level [i] from a known [block_hash]. The
   target level [i] must be a predecessor of [block_hash]. *)
let read_i legacy_chain_state block_hash i =
  Legacy_state.Block.read legacy_chain_state block_hash >>=? fun block ->
  let pred = Int32.(to_int (sub block.header.shell.level i)) in
  Legacy_state.Block.read_predecessor legacy_chain_state ~pred block_hash
  >>= function
  | Some {header; _} -> return header
  | None -> failwith "Failed to find block at level %ld" i

(* Reads, from the legacy lmdb store, the blocks from [block_hash] to
   [limit] and store them in the floating store. The ~with_metadata
   flag provide information on how to store the floating block: with
   or without metadata *)
let import_floating legacy_chain_state legacy_chain_store chain_store
    ~with_metadata end_hash start_level =
  let block_store = Store.Unsafe.get_block_store chain_store in
  read_i legacy_chain_state end_hash start_level >>=? fun start_header ->
  hash_header legacy_chain_store start_header >>=? fun start_hash ->
  make_block_repr ~with_metadata legacy_chain_state start_hash
  >>=? fun start_block ->
  (make_block_repr
     ~with_metadata
     legacy_chain_state
     start_header.shell.predecessor
   >>= function
   | Ok b -> Lwt.return_some b
   | _ -> Lwt.return_none)
  >>= fun pred_block_opt ->
  make_block_repr ~with_metadata legacy_chain_state end_hash
  >>=? fun end_block ->
  let end_limit = Block_repr.level end_block in
  let nb_floating_blocks =
    Int32.(to_int (succ (sub (Block_repr.level end_block) start_level)))
  in
  Animation.display_progress
    ~pp_print_step:(fun fmt i ->
      Format.fprintf
        fmt
        "Converting floating blocks: %d/%d"
        i
        nb_floating_blocks)
    (fun notify ->
      let rec aux ~pred_block block =
        Block_store.store_block block_store block >>=? fun () ->
        let level = Block_repr.level block in
        if level >= end_limit then notify () >>= fun () -> return_unit
        else
          (* At protocol change, update the protocol_table *)
          (match pred_block with
          | None -> return_unit (* FIXME: should be assert false? *)
          | Some prev_block ->
              may_update_protocol_table
                legacy_chain_store
                chain_store
                ~prev_block
                ~block)
          >>=? fun () ->
          read_i legacy_chain_state end_hash (Int32.succ level)
          >>=? fun next_block ->
          hash_header legacy_chain_store next_block >>=? fun next_block_hash ->
          make_block_repr ~with_metadata legacy_chain_state next_block_hash
          >>=? fun next_block_repr ->
          notify () >>= fun () -> aux ~pred_block:(Some block) next_block_repr
      in
      aux ~pred_block:pred_block_opt start_block)

(* Reads, from the legacy lmdb store, the blocks from [start_block] to
   [end_limit] and store them in the cemented block store *)
let import_cemented legacy_chain_state legacy_chain_store chain_store
    cycle_length ~with_metadata ~start_block ~end_limit =
  let nb_cemented_cycles =
    let cycles =
      Int32.(to_int (sub (Block_repr.level start_block) end_limit))
      / cycle_length
    in
    if end_limit = 1l then succ cycles else cycles
  in
  let write_metadata =
    match with_metadata with Required -> true | Unwanted | Optional -> false
  in
  let display_msg = if write_metadata then " (with metadata)" else "" in
  Animation.display_progress
    ~pp_print_step:(fun fmt i ->
      Format.fprintf
        fmt
        "Converting cemented blocks%s: %d/%d"
        display_msg
        i
        nb_cemented_cycles)
    (fun notify ->
      let rec aux ~prev_block (last_checkpoint : Block_header.t) acc hash =
        make_block_repr ~with_metadata legacy_chain_state hash >>=? fun block ->
        (* At protocol change, update the protocol_table. Arguments
           are inverted as the blocks to cement are browse from high
           to low. *)
        may_update_protocol_table
          legacy_chain_store
          chain_store
          ~prev_block:block
          ~block:prev_block
        >>=? fun () ->
        let new_acc = block :: acc in
        if Block_repr.level block <= end_limit then
          (* The low limit of the cemented blocks to import was
             reached. *)
          if end_limit = 1l then
            (* This case corresponds to networks with [0;1] as first
               cycle.*)
            (* Reading genesis which is not pruned in lmdb *)
            make_block_repr
              ~with_metadata:Unwanted
              legacy_chain_state
              (Block_repr.predecessor block)
            >>=? fun genesis ->
            may_update_protocol_table
              legacy_chain_store
              chain_store
              ~prev_block:genesis
              ~block
            >>=? fun () ->
            let block_store = Store.Unsafe.get_block_store chain_store in
            let blocks =
              match List.hd new_acc with
              | Some br when Block_repr.level br = 0l -> new_acc
              | Some _ -> genesis :: new_acc
              | None -> assert false
            in
            Block_store.cement_blocks
              ~check_consistency:false
              block_store
              blocks
              ~write_metadata
            >>=? fun () ->
            notify () >>= fun () -> return_unit
          else (
            assert (List.length acc = 0) ;
            return_unit)
        else if
          Hardcoded.may_update_checkpoint ~cycle_length (List.length new_acc)
        then
          (* The end of an hardcoded cycle was reached. We need to
             cement the chunk.*)
          let block_store = Store.Unsafe.get_block_store chain_store in
          Block_store.cement_blocks
            ~check_consistency:false
            block_store
            new_acc
            ~write_metadata
          >>=? fun () ->
          notify () >>= fun () ->
          aux
            ~prev_block:block
            (Block_repr.header block)
            []
            (Block_repr.predecessor block)
        else
          aux
            ~prev_block:block
            last_checkpoint
            (block :: acc)
            (Block_repr.predecessor block)
      in
      aux
        ~prev_block:start_block
        (Block_repr.header start_block)
        [start_block]
        (Block_repr.predecessor start_block))

let archive_import legacy_chain_state legacy_chain_store chain_store
    cycle_length (checkpoint_header, checkpoint_level) current_head_hash =
  hash_header legacy_chain_store checkpoint_header >>=? fun checkpoint_hash ->
  (if checkpoint_level = 0l then
   (* Only the floating store should be imported. Nothing was pruned
      yet so we import the metadata. *)
   import_floating
     legacy_chain_state
     legacy_chain_store
     chain_store
     ~with_metadata:Required
     current_head_hash
     (Int32.succ checkpoint_level)
  else
    make_block_repr ~with_metadata:Required legacy_chain_state checkpoint_hash
    >>=? fun checkpoint_block ->
    (* First, import cemented blocks [1l;checkpoint] with metadata as
       nothing is pruned in archive mode. *)
    import_cemented
      legacy_chain_state
      legacy_chain_store
      chain_store
      cycle_length
      ~with_metadata:Required
      ~start_block:checkpoint_block
      ~end_limit:1l
    >>=? fun () ->
    (* Then, import floating blocks [checkpoint;head] with metadata as
       nothing is pruned in archive mode. *)
    import_floating
      legacy_chain_state
      legacy_chain_store
      chain_store
      ~with_metadata:Required
      current_head_hash
      (Int32.succ checkpoint_level))
  >>=? fun () ->
  let new_checkpoint = (checkpoint_hash, checkpoint_level) in
  let genesis = Store.Chain.genesis chain_store in
  let new_caboose = (genesis.block, 0l) in
  let new_savepoint = new_caboose in
  return (new_checkpoint, new_savepoint, new_caboose)

(* As the lmdb store is not compatible with a Full 5, it is upgraded as
   a Full 0. It will converge to a Full 5 afterward. *)
let full_import legacy_chain_state legacy_chain_store chain_store cycle_length
    (checkpoint, checkpoint_level) current_head_hash =
  hash_header legacy_chain_store checkpoint >>=? fun checkpoint_hash ->
  (if checkpoint_level = 0l then
   (* Only the floating store should be imported. Nothing was pruned
      yet so we import the metadata. *)
   import_floating
     legacy_chain_state
     legacy_chain_store
     chain_store
     ~with_metadata:Optional
     current_head_hash
     1l
  else
    make_block_repr ~with_metadata:Unwanted legacy_chain_state checkpoint_hash
    >>=? fun checkpoint_block_repr ->
    (* First, import cemented blocks [1l;checkpoint] without metadata
       as those blocks were pruned. *)
    import_cemented
      legacy_chain_state
      legacy_chain_store
      chain_store
      cycle_length
      ~with_metadata:Unwanted
      ~start_block:checkpoint_block_repr
      ~end_limit:1l
    >>=? fun () ->
    (* Then, import floating blocks [checkpoint;head] with metadata as
       those blocks were not pruned yet. *)
    import_floating
      legacy_chain_state
      legacy_chain_store
      chain_store
      ~with_metadata:Optional
      current_head_hash
      checkpoint.shell.level
    >>=? fun () -> return_unit)
  >>=? fun () ->
  (Legacy_state.Chain.save_point legacy_chain_state >|= fun (l, h) -> (h, l))
  >>= fun legacy_savepoint ->
  let new_checkpoint = legacy_savepoint in
  let genesis = Store.Chain.genesis chain_store in
  let new_caboose = (genesis.block, 0l) in
  let new_savepoint = new_checkpoint in
  return (new_checkpoint, new_savepoint, new_caboose)

(* As the lmdb store is not compatible with a Rolling 5, it is upgraded as
   a Rolling 0. It will converge to a Rolling 5 afterward. *)
let rolling_import legacy_chain_state legacy_chain_store chain_store
    (checkpoint_header, checkpoint_level) current_head_hash =
  hash_header legacy_chain_store checkpoint_header >>=? fun checkpoint_hash ->
  Legacy_state.Chain.caboose legacy_chain_state
  >>= fun (legacy_caboose_level, _legacy_caboose_hash) ->
  (if checkpoint_level = 0l then
   (* Only the floating store should be imported. Nothing was pruned
      yet so we import the metadata. *)
   import_floating
     legacy_chain_state
     legacy_chain_store
     chain_store
     ~with_metadata:Optional
     current_head_hash
     1l
  else
    (* Importing floating [ lmdb_caboose ; lmdb_checkpoint [ without
       metadata as those blocks were pruned. *)
    Legacy_state.Block.read legacy_chain_state checkpoint_hash
    >>=? fun checkpoint_block ->
    let checkpoint_header = checkpoint_block.header in
    assert (Block_header.hash checkpoint_header = checkpoint_hash) ;
    let checkpoint_pred_hash = checkpoint_header.shell.predecessor in
    import_floating
      legacy_chain_state
      legacy_chain_store
      chain_store
      ~with_metadata:Unwanted
      checkpoint_pred_hash
      legacy_caboose_level
    >>=? fun () ->
    (* Importing blocks [ checkpoint ; current_head ] in floating with
       metadata as those blocks were not pruned yet. *)
    import_floating
      legacy_chain_state
      legacy_chain_store
      chain_store
      ~with_metadata:Optional
      current_head_hash
      checkpoint_level)
  >>=? fun () ->
  read_i legacy_chain_state current_head_hash legacy_caboose_level
  >>=? fun new_caboose_header ->
  (if checkpoint_level = 0l then
   let genesis = Store.Chain.genesis chain_store in
   return (genesis.block, 0l)
  else
    hash_header legacy_chain_store new_caboose_header >>=? fun caboose_hash ->
    return (caboose_hash, new_caboose_header.shell.level))
  >>=? fun new_caboose ->
  (Legacy_state.Chain.save_point legacy_chain_state >|= fun (l, h) -> (h, l))
  >>= fun legacy_savepoint ->
  let new_checkpoint = legacy_savepoint in
  let new_savepoint = new_checkpoint in
  return (new_checkpoint, new_savepoint, new_caboose)

let import_blocks legacy_chain_state chain_id chain_store cycle_length
    checkpoint history_mode =
  Legacy_state.Chain.store legacy_chain_state >>= fun legacy_store ->
  let legacy_chain_store = Legacy_store.Chain.get legacy_store chain_id in
  let legacy_chain_data = Legacy_store.Chain_data.get legacy_chain_store in
  Legacy_store.Chain_data.Current_head.read legacy_chain_data
  >>=? fun current_head_hash ->
  (match (history_mode : History_mode.t) with
  | Archive ->
      archive_import
        legacy_chain_state
        legacy_chain_store
        chain_store
        cycle_length
        checkpoint
        current_head_hash
  | Full _ ->
      full_import
        legacy_chain_state
        legacy_chain_store
        chain_store
        cycle_length
        checkpoint
        current_head_hash
  | Rolling _ ->
      rolling_import
        legacy_chain_state
        legacy_chain_store
        chain_store
        checkpoint
        current_head_hash)
  >>=? fun (new_checkpoint, new_savepoint, new_caboose) ->
  return (new_checkpoint, new_savepoint, new_caboose)

let store_known_protocols legacy_store store =
  Legacy_store.Protocol.Contents.bindings legacy_store >>= fun proto_list ->
  List.iter_es
    (fun (h, p) ->
      Store.Protocol.store store h p >>= function
      | Some expected_hash ->
          fail_unless
            (Protocol_hash.equal expected_hash h)
            (Failed_to_convert_protocol h)
      | None -> fail (Failed_to_convert_protocol h) >>=? fun () -> return_unit)
    proto_list

let import_protocols history_mode legacy_store legacy_chain_state store
    _chain_store chain_id =
  let legacy_chain_store = Legacy_store.Chain.get legacy_store chain_id in
  let legacy_chain_data = Legacy_store.Chain_data.get legacy_chain_store in
  match (history_mode : History_mode.t) with
  | Archive | Full _ -> store_known_protocols legacy_store store
  | Rolling _ ->
      Legacy_store.Chain_data.Current_head.read legacy_chain_data
      >>=? fun current_head_hash ->
      Legacy_state.Block.read legacy_chain_state current_head_hash
      >>=? fun current_head ->
      Legacy_state.Chain.caboose legacy_chain_state
      >>= fun (_, legacy_caboose_hash) ->
      (* We store the oldest known protocol and we assume that its
         transition_header is the caboose as the actual transition
         block is unknown. *)
      Legacy_state.Block.read legacy_chain_state legacy_caboose_hash
      >>=? fun transition_block ->
      let transition_header = transition_block.header in
      let protocol_level = current_head.header.shell.proto_level in
      Legacy_store.Chain.Protocol_info.read legacy_chain_store protocol_level
      >>=? fun protocol_info ->
      let protocol_hash = fst protocol_info in
      let chain_store = Store.main_chain_store store in
      let is_genesis = transition_header.shell.level = 0l in
      hash_header legacy_chain_store transition_header
      >>=? fun transition_hash ->
      Legacy_state.Block.last_allowed_fork_level current_head >>=? fun lafl ->
      (if lafl > transition_header.shell.level && not is_genesis then
       make_block_repr
         ~with_metadata:Unwanted
         legacy_chain_state
         transition_hash
      else
        make_block_repr
          ~with_metadata:Unwanted
          legacy_chain_state
          transition_hash)
      >>=? fun transition_block ->
      Store.Unsafe.set_protocol_level
        chain_store
        ~protocol_level
        (Store.Unsafe.block_of_repr transition_block, protocol_hash)

let import_invalid_blocks legacy_chain_store =
  Legacy_store.Block.Invalid_block.fold
    legacy_chain_store
    ~init:Block_hash.Map.empty
    ~f:(fun hash ({level; errors} : Legacy_store.Block.invalid_block) map ->
      Lwt.return
        (Block_hash.Map.add
           hash
           ({level; errors} : Store_types.invalid_block)
           map))

let import_forked_chains legacy_chain_store =
  Legacy_store.Forking_block_hash.fold
    legacy_chain_store
    ~init:Chain_id.Map.empty
    ~f:(fun id hash map -> Lwt.return (Chain_id.Map.add id hash map))

let update_stored_data legacy_chain_store legacy_store new_store ~new_checkpoint
    ~new_savepoint ~new_caboose genesis =
  let chain_store = Store.main_chain_store new_store in
  let store_dir = Store.directory new_store in
  let legacy_chain_data = Legacy_store.Chain_data.get legacy_chain_store in
  Legacy_store.Chain_data.Current_head.read legacy_chain_data
  >>=? fun legacy_head ->
  import_invalid_blocks legacy_chain_store >>= fun invalid_blocks ->
  import_forked_chains legacy_store >>= fun forked_chains ->
  Store.Unsafe.restore_from_legacy_upgrade
    store_dir
    ~genesis
    ~invalid_blocks
    ~forked_chains
  >>=? fun () ->
  Store.Block.read_block chain_store legacy_head >>=? fun new_head ->
  Store.Unsafe.set_head chain_store new_head >>=? fun () ->
  Store.Unsafe.set_checkpoint chain_store new_checkpoint >>=? fun () ->
  Store.Unsafe.set_savepoint chain_store new_savepoint >>=? fun () ->
  let block_store = Store.Unsafe.get_block_store chain_store in
  let cemented_store = Block_store.cemented_block_store block_store in
  let cementing_highwatermark =
    Cemented_block_store.get_highest_cemented_level cemented_store
  in
  Store.Unsafe.set_cementing_highwatermark chain_store cementing_highwatermark
  >>=? fun () -> Store.Unsafe.set_caboose chain_store new_caboose

(* Returns the infered checkpoint of the chain or None if the current
   head is set to genesis. *)
let infer_checkpoint legacy_chain_state chain_id =
  (* When upgrading from a full or rolling node, the checkpoint may
     not be set on a "protocol defined checkpoint". We substitute it
     by using, as a checkpoint, the highest block between the
     savepoint and the last allowed fork level of the current
     head. *)
  Legacy_state.Chain.store legacy_chain_state >>= fun legacy_store ->
  let legacy_chain_store = Legacy_store.Chain.get legacy_store chain_id in
  let legacy_chain_data = Legacy_store.Chain_data.get legacy_chain_store in
  Legacy_store.Chain_data.Current_head.read legacy_chain_data
  >>=? fun head_hash ->
  Legacy_state.Block.read legacy_chain_state head_hash >>=? fun head_contents ->
  if head_contents.header.shell.level = 0l then return_none
  else
    Legacy_state.Block.last_allowed_fork_level head_contents >>=? fun lafl ->
    Legacy_store.Chain_data.Save_point.read legacy_chain_data
    >>=? fun (savepoint_level, savepoint_hash) ->
    Legacy_state.Block.read legacy_chain_state savepoint_hash
    >>=? fun savepoint ->
    if Compare.Int32.(lafl > savepoint_level) then
      read_i legacy_chain_state head_hash lafl >>=? fun lafl_header ->
      return_some (lafl_header, lafl)
    else return_some (Legacy_state.Block.header savepoint, savepoint_level)

let upgrade_cleaner data_dir ~upgraded_store =
  Event.(emit restoring_after_failure) data_dir >>= fun () ->
  Lwt_utils_unix.remove_dir upgraded_store >>= fun () -> Lwt.return_unit

let raw_upgrade chain_name ~new_store ~legacy_state history_mode genesis =
  let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
  Legacy_state.Chain.get legacy_state chain_id >>=? fun legacy_chain_state ->
  Legacy_state.Chain.store legacy_chain_state >>= fun legacy_store ->
  let legacy_chain_store = Legacy_store.Chain.get legacy_store chain_id in
  let cycle_length = Hardcoded.cycle_length ~chain_name in
  (infer_checkpoint legacy_chain_state chain_id >>=? function
   | None ->
       Legacy_state.Block.read legacy_chain_state genesis.block
       >>=? fun genesis_block ->
       return (genesis_block.header, genesis_block.header.shell.level)
   | Some checkpoint -> return checkpoint)
  >>=? fun checkpoint ->
  let new_chain_store = Store.main_chain_store new_store in
  import_protocols
    history_mode
    legacy_store
    legacy_chain_state
    new_store
    new_chain_store
    chain_id
  >>=? fun () ->
  import_blocks
    legacy_chain_state
    chain_id
    new_chain_store
    cycle_length
    checkpoint
    history_mode
  >>=? fun (new_checkpoint, new_savepoint, new_caboose) ->
  update_stored_data
    legacy_chain_store
    legacy_store
    new_store
    ~new_checkpoint
    ~new_savepoint
    ~new_caboose
    genesis

let temporary_former_store_path ~data_dir = data_dir // "lmdb_store_to_remove"

let upgrade_0_0_4 ~data_dir ?patch_context
    ~(chain_name : Distributed_db_version.Name.t) genesis =
  Hardcoded.check_network ~chain_name >>=? fun () ->
  let new_store_tmp = data_dir // "new_store_tmp" in
  Lwt.try_bind
    (fun () ->
      Lwt_unix.file_exists new_store_tmp >>= fun previous_aborted_upgrade ->
      (if previous_aborted_upgrade then Lwt_utils_unix.remove_dir new_store_tmp
      else Lwt.return_unit)
      >>= fun () ->
      let store_to_upgrade = data_dir // "store" in
      let context_root = data_dir // "context" in
      Legacy_state.init
        ~readonly:true
        ~context_root
        ~store_root:store_to_upgrade
        genesis
      >>=? fun (state, _chain_state, _context_index, legacy_history_mode) ->
      let history_mode = History_mode.convert legacy_history_mode in
      Event.(
        emit
          advertise_upgrade_mode
          ( Format.asprintf "%a" History_mode.Legacy.pp legacy_history_mode,
            Format.asprintf "%a" History_mode.pp history_mode ))
      >>= fun () ->
      Store.init
        ?patch_context
        ~store_dir:new_store_tmp
        ~context_dir:context_root
        ~history_mode
        ~allow_testchains:true
        genesis
      >>=? fun store ->
      raw_upgrade
        chain_name
        ~new_store:store
        ~legacy_state:state
        history_mode
        genesis
      >>=? fun () ->
      Legacy_state.close state >>= fun () ->
      Store.close_store store >>= fun () ->
      Lwt_unix.rename store_to_upgrade (temporary_former_store_path ~data_dir)
      >>= fun () ->
      let final_store_path = data_dir // "store" in
      Lwt_unix.rename new_store_tmp final_store_path >>= fun () -> return_unit)
    (function
      | Ok () ->
          Event.(emit upgrade_completed (temporary_former_store_path ~data_dir))
          >>= fun () -> return_unit
      | Error errors ->
          upgrade_cleaner data_dir ~upgraded_store:new_store_tmp >>= fun () ->
          Lwt.return (Error errors))
    (fun exn ->
      upgrade_cleaner data_dir ~upgraded_store:new_store_tmp >>= fun () ->
      fail_with_exn exn)

let upgrade_0_0_5 ~data_dir genesis =
  let floating_stores_to_upgrade = Floating_block_store.[RO; RW; RW_TMP] in
  let store_dir =
    Naming.store_dir ~dir_path:Filename.Infix.(data_dir // "store")
  in
  let chain_id = Chain_id.of_block_hash genesis.Genesis.block in
  let chain_dir = Naming.chain_dir store_dir chain_id in
  (* Remove the potential RO_TMP floating stores *)
  (let path = Naming.dir_path (Naming.floating_blocks_dir chain_dir RO_TMP) in
   Lwt_unix.file_exists path >>= fun exists ->
   if exists then Lwt_utils_unix.remove_dir path else Lwt.return_unit)
  >>= fun () ->
  (* Move the floating stores to upgrade in "_broken" suffixed
     directory *)
  let broken_floating_blocks_dir floating_blocks_dir =
    Naming.dir_path floating_blocks_dir ^ "_broken"
  in
  List.iter_s
    (fun kind ->
      let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
      let path = Naming.dir_path floating_blocks_dir in
      Lwt_unix.file_exists path >>= function
      | false ->
          (* Nothing to do: should only happen with RW_TMP *)
          Lwt.return_unit
      | true ->
          Lwt_unix.rename
            (Naming.dir_path floating_blocks_dir)
            (broken_floating_blocks_dir floating_blocks_dir))
    floating_stores_to_upgrade
  >>= fun () ->
  Stored_data.load (Naming.genesis_block_file chain_dir)
  >>=? fun genesis_block_data ->
  Stored_data.get genesis_block_data >>= fun genesis_block ->
  Block_store.load chain_dir ~genesis_block ~readonly:false
  >>=? fun block_store ->
  (* Set the merge status as Idle: we are overriding the merge *)
  Block_store.write_status block_store Idle >>=? fun () ->
  (* Iter through the blocks and add then into the new floating stores *)
  List.iter_es
    (fun kind ->
      let kind_str =
        (function
          | Floating_block_store.RO -> "RO"
          | RW -> "RW"
          | RW_TMP -> "RW_TMP"
          | _ -> assert false)
          kind
      in
      let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
      Lwt_unix.file_exists (Naming.dir_path floating_blocks_dir) >>= function
      | false ->
          (* Nothing to do: should only happen with RW_TMP *)
          return_unit
      | true ->
          Animation.display_progress
            ~pp_print_step:(fun fmt i ->
              Format.fprintf fmt "upgrading %s floating store %d" kind_str i)
            (fun notify ->
              Lwt_unix.openfile
                Filename.Infix.(
                  broken_floating_blocks_dir floating_blocks_dir // "blocks")
                [Unix.O_CREAT; O_CLOEXEC; Unix.O_RDONLY]
                0o444
              >>= fun fd ->
              Floating_block_store.iter_s_raw_fd
                (fun block ->
                  notify () >>= fun () ->
                  Block_store.store_block block_store block)
                fd
              >>=? fun () ->
              Lwt_unix.close fd >>= fun () -> return_unit))
    floating_stores_to_upgrade
  >>=? fun () ->
  (* Remove the former broken floating stores *)
  List.iter_s
    (fun kind ->
      let floating_blocks_dir = Naming.floating_blocks_dir chain_dir kind in
      Lwt_utils_unix.remove_dir (broken_floating_blocks_dir floating_blocks_dir))
    floating_stores_to_upgrade
  >>= fun () -> return_unit
