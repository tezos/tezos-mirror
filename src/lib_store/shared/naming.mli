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

open Store_types

(** {1 File convention} *)

(** The 'kind aims to be used to reflect in the type system the
    directory name to ease the readability of the
    code. E.g. [[[`Block]] directory]. *)
type 'kind directory

type 'kind file

type ('kind, 'data) encoded_file = 'data Stored_data.file

val dir_path : 'kind directory -> string

val file_path : 'kind file -> string

val is_json_file : ('kind, 'data) encoded_file -> bool

val make_encoded_file :
  ?json:bool ->
  'kind directory ->
  filename:string ->
  'a Data_encoding.t ->
  ('a -> 'a -> bool) ->
  ('kind, 'a) encoded_file

val encoded_file_path : ('kind, 'data) encoded_file -> string

val file_encoding : ('kind, 'data) encoded_file -> 'data Data_encoding.t

(** {2 Toplevel directory} *)

val store_dir : dir_path:string -> [`Store_dir] directory

(** {2 Protocols directory} *)

val protocol_store_dir :
  [< `Store_dir | `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Protocol_dir] directory

(** Protocol file *)
val protocol_file :
  [`Protocol_dir] directory -> Protocol_hash.t -> [`Protocol] file

(** {2 Chain directory} *)

val chain_dir :
  [< `Store_dir | `Testchains_dir] directory ->
  Chain_id.t ->
  [`Chain_dir] directory

val lock_file : [`Chain_dir] directory -> [`Lockfile] file

val gc_lockfile : [`Chain_dir] directory -> [`Gc_lockfile] file

val reconstruction_lock_file :
  [`Chain_dir] directory -> [`Reconstruction_lockfile] file

val testchains_dir : [`Chain_dir] directory -> [`Testchains_dir] directory

(** {3 Persistent chain data files} *)

val chain_config_file :
  [`Chain_dir] directory -> ([`Chain_config], chain_config) encoded_file

val protocol_levels_file :
  [< `Chain_dir] directory ->
  ( [`Protocol_levels],
    Protocol_levels.protocol_info Protocol_levels.t )
  encoded_file

val legacy_protocol_levels_file :
  [< `Chain_dir] directory ->
  ( [`Protocol_levels],
    Protocol_levels.Legacy.activation_block Protocol_levels.Legacy.t )
  encoded_file

val genesis_block_file :
  [`Chain_dir] directory -> ([`Genesis_block], Block_repr.t) encoded_file

val current_head_file :
  [`Chain_dir] directory -> ([`Current_head], block_descriptor) encoded_file

val cementing_highwatermark_file :
  [`Chain_dir] directory ->
  ([`Cementing_highwatermark], int32 option) encoded_file

val checkpoint_file :
  [`Chain_dir] directory -> ([`Checkpoint], block_descriptor) encoded_file

val target_file :
  [`Chain_dir] directory -> ([`Target], block_descriptor option) encoded_file

val invalid_blocks_file :
  [`Chain_dir] directory ->
  ([`Invalid_blocks], invalid_block Block_hash.Map.t) encoded_file

val forked_chains_file :
  [`Chain_dir] directory ->
  ([`Forked_chains], Block_hash.t Chain_id.Map.t) encoded_file

(** {2 Block store}  *)

val savepoint_file :
  [`Chain_dir] directory -> ([`Savepoint], block_descriptor) encoded_file

val caboose_file :
  [`Chain_dir] directory -> ([`Caboose], block_descriptor) encoded_file

type block_store_status = Idle | Merging

val block_store_status_file :
  [`Chain_dir] directory -> ([`Status], block_store_status) encoded_file

val cemented_blocks_dir :
  [< `Chain_dir | `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Cemented_blocks_dir] directory

val cemented_blocks_level_index_dir :
  [`Cemented_blocks_dir] directory -> [`Cemented_level_index_dir] directory

val cemented_blocks_level_lock_file :
  [`Cemented_level_index_dir] directory -> [`Cemented_level_lock_file] file

val cemented_blocks_hash_index_dir :
  [`Cemented_blocks_dir] directory -> [`Cemented_hash_index_dir] directory

val cemented_blocks_hash_lock_file :
  [`Cemented_hash_index_dir] directory -> [`Cemented_hash_lock_file] file

val cemented_blocks_file :
  [`Cemented_blocks_dir] directory ->
  start_level:int32 ->
  end_level:int32 ->
  [`Cemented_blocks_file] file

val cemented_blocks_metadata_dir :
  [`Cemented_blocks_dir] directory -> [`Cemented_metadata_dir] directory

val cemented_blocks_metadata_file :
  [`Cemented_metadata_dir] directory ->
  [`Cemented_blocks_file] file ->
  [`Cemented_blocks_metadata] file

val cemented_blocks_tmp_metadata_file :
  [`Cemented_metadata_dir] directory ->
  [`Cemented_blocks_file] file ->
  [`Cemented_blocks_metadata] file

(** The type of floating store's kind. *)
type floating_kind = RO | RW | RW_TMP | RO_TMP | Restore of floating_kind

val floating_blocks_dir :
  [`Chain_dir] directory -> floating_kind -> [`Floating_dir] directory

val floating_blocks_index_dir :
  [`Floating_dir] directory -> [`Floating_index_dir] directory

val floating_blocks_file : [`Floating_dir] directory -> [`Floating_blocks] file

(** {2 Snapshots} *)

val snapshot_dir : ?snapshot_path:string -> unit -> [`Snapshot_dir] directory

val snapshot_file :
  snapshot_filename:string -> [`Snapshot_dir] directory -> [`Snapshot_file] file

val snapshot_tmp_dir :
  [`Snapshot_dir] directory -> [`Snapshot_tmp_dir] directory

val snapshot_tmp_tar_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir] directory -> [`Snapshot_tar_file] file

val snapshot_block_data_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Snapshot_block_data] file

val snapshot_context_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Snapshot_context] file

val snapshot_floating_blocks_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Snapshot_floating_blocks] file

val snapshot_metadata_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Snapshot_metadata] file

val snapshot_version_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  [`Snapshot_version] file

val snapshot_protocol_levels_file :
  [< `Snapshot_dir | `Snapshot_tmp_dir | `Tar_archive] directory ->
  ( [`Snapshot_protocol_levels],
    Protocol_levels.protocol_info Protocol_levels.t )
  encoded_file

val snapshot_tar_root : [`Tar_archive] directory
