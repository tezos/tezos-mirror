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

(** Snapshots for the store

    Snapshots are canonical representations of the store and its
    associated context. Its main purposes it to save and load a
    current state with the minimal necessary amount of
    information. This snapshot may also be shared by third parties to
    facilitate the bootstrap process.

    A snapshot of a block [B] is composed of :
    - The snapshot format version (see [version]), as a file;
    - The metadata of the snapshot (see [metadata]), as a file;
    - Some data to ensure snapshot consistency at import (see
      [block_data]), as a file;
    - A single context containing every key that the block [B-1] needs
      (see below), as a file;
    - The set of (cemented and floating) blocks and their operations
      from the genesis block up to [B] -- it might contain less blocks
      if the snapshot is created from a store using a [Rolling]
      history mode of if it is created as a [Rolling]
      snapshot. Block's metadata are not exported. The cemented blocks
      are exported as a directory containing cycles as files, as well
      as some indexing data. The floating blocks are stored as a
      single file ;
    - The set of necessary Tezos protocols, as a directory containing
      the protocols as single files.

    There are two kinds of snapshot formats that can be exported:
    - [Raw] is a directory containing the aforementioned data as
    independent files;
    - [Tar] is a tar archive containing all the data as a single
    archive file. To achieve better performances while loading the
    snapshot's information (version and metadata), we store first the
    version and then the metadata, to avoid seeking through the whole
    file.

    Importing a snapshot will initialize a fresh store with the data
    contained in the snapshot. As snapshots may be shared between
    users, checks are made to ensure that no malicious data is
    loaded. For instance, we export the context of block [B-1] to make
    sure that the application of the block [B], given its
    predecessor's context, is valid.

    Depending on the history mode, a snapshot might contain less
    blocks. In full, all blocks are present and importing such a
    snapshot will populate the {!Cemented_store} with every cycle up
    to the snapshot's target block. Meanwhile, in [Rolling], only a
    few previous blocks will be exported ([max_op_ttl] from the target
    block), only populating a {!Floating_block_store}. Thus, the
    snapshot size greatly differs depending on the history mode used.

    Snapshots may be created concurrently with a running node. It
    might impact the node for a few seconds to retrieve the necessary
    consistent information to produce the snapshot.
*)

open Store_types

type error +=
  | Incompatible_history_mode of {
      requested : History_mode.t;
      stored : History_mode.t;
    }
  | Invalid_export_block of {
      block : Block_hash.t option;
      reason :
        [ `Pruned
        | `Pruned_pred
        | `Unknown
        | `Unknown_ancestor
        | `Caboose
        | `Genesis
        | `Not_enough_pred ];
    }
  | Invalid_export_path of string
  | Snapshot_file_not_found of string
  | Inconsistent_protocol_hash of {
      expected : Protocol_hash.t;
      got : Protocol_hash.t;
    }
  | Inconsistent_context_hash of {
      expected : Context_hash.t;
      got : Context_hash.t;
    }
  | Inconsistent_context of Context_hash.t
  | Cannot_decode_protocol of Protocol_hash.t
  | Cannot_write_metadata of string
  | Cannot_read of {
      kind :
        [ `Version
        | `Metadata
        | `Block_data
        | `Context
        | `Protocol_table
        | `Protocol
        | `Cemented_cycle ];
      path : string;
    }
  | Inconsistent_floating_store of block_descriptor * block_descriptor
  | Missing_target_block of block_descriptor
  | Cannot_read_floating_store of string
  | Cannot_retrieve_block_interval
  | Invalid_cemented_file of string
  | Missing_cemented_file of string
  | Corrupted_floating_store
  | Invalid_protocol_file of string
  | Target_block_validation_failed of Block_hash.t * string
  | Directory_already_exists of string
  | Empty_floating_store
  | Cannot_create_tmp_export_directory of string
  | Inconsistent_chain_import of {
      expected : Distributed_db_version.Name.t;
      got : Distributed_db_version.Name.t;
    }
  | Inconsistent_imported_block of Block_hash.t * Block_hash.t
  | Wrong_snapshot_file of {filename : string}

(** Current version of snapshots *)
val current_version : int

type snapshot_format = Tar | Raw

val pp_snapshot_format : Format.formatter -> snapshot_format -> unit

val snapshot_format_encoding : snapshot_format Data_encoding.t

type snapshot_header

(** [version snapshot_header] returns the version of a given
    [snapshot_header] as an integer value. *)
val version : snapshot_header -> int

(** Pretty-printer of a snapshot's {!header} *)
val pp_snapshot_header : Format.formatter -> snapshot_header -> unit

(** [read_snapshot_header ~snapshot_path] reads [snapshot_file]'s
   header. *)
val read_snapshot_header :
  snapshot_path:string -> snapshot_header tzresult Lwt.t

(** [export ?snapshot_path snapshot_format ?rolling ~block ~store_dir
   ~context_dir ~chain_name genesis ~on_disk_index] reads from the
   [store_dir] and [context_dir] the current state of the node and
   produces a snapshot, of the given [snapshot_format], in
   [snapshot_file] if it is provided. Otherwise, a snapshot file name
   is automatically generated using the target block as hint. If
   [rolling] is set, only the necessary blocks will be exported. If
   [on_disk] is set, uses an on-disk index to reduce the memory usage of the
   export.*)
val export :
  ?snapshot_path:string ->
  snapshot_format ->
  ?rolling:bool ->
  block:Block_services.block ->
  store_dir:string ->
  context_dir:string ->
  chain_name:Distributed_db_version.Name.t ->
  on_disk:bool ->
  Genesis.t ->
  unit tzresult Lwt.t

(** [import ~snapshot_path ?patch_context ?block ?check_consistency
   ~dst_store_dir ~dst_context_dir chain_name ~user_activated_upgrades
   ~user_activated_protocol_overrides ~ops_metadata_size_limit
   ~in_memory genesis] populates [dst_store_dir] and [dst_context_dir]
   with the data contained in the [snapshot_file]. If
   [check_consistency] is unset, less security checks will be made and
   the import process will be more efficient. If [block] is set, the
   import process will make sure that the block is the correct one we
   load. [patch_context], [user_activated_upgrades] and
   [user_activated_protocol_overrides] are passed to the validator in
   order to validate the target block. [ops_metadata_size_limit]
   determines the maximal size of the metadata to store while
   importing a snapshot. [in_memory] states if the import should be
   all in memory, which is faster but uses more memory. *)
val import :
  snapshot_path:string ->
  ?patch_context:(Context.t -> Context.t tzresult Lwt.t) ->
  ?block:Block_hash.t ->
  ?check_consistency:bool ->
  dst_store_dir:string ->
  dst_context_dir:string ->
  chain_name:Distributed_db_version.Name.t ->
  configured_history_mode:History_mode.t option ->
  user_activated_upgrades:User_activated.upgrades ->
  user_activated_protocol_overrides:User_activated.protocol_overrides ->
  operation_metadata_size_limit:int option ->
  in_memory:bool ->
  Genesis.t ->
  unit tzresult Lwt.t

(** [snapshot_file_kind ~snapshot_file] reads the [snapshot_file] and
    returns its kind. Returns [Invalid] if it is a wrong snapshot
    file. *)
val snapshot_file_kind : snapshot_path:string -> snapshot_format tzresult Lwt.t
