(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Compression strategy for snapshot archives. *)
type compression =
  | No  (** Produce uncompressed archive. Takes more space. *)
  | On_the_fly
      (** Compress archive on the fly. The rollup node will use less disk space
          to produce the snapshot but will lock the rollup node (if running) for
          a longer time. *)
  | After
      (** Compress archive after snapshot creation. Uses more disk space
          temporarily than {!On_the_fly} but does not lock the rollup node for
          very long. *)

module Header : sig
  (** Versioning of snapshot format. Only one version for now. *)
  type version = V0

  (** Snapshot metadata for version 0. This information is written as a header of
    the archive snapshot file. *)
  type t = {
    version : version;
    history_mode : Configuration.history_mode;
    address : Address.t;
    head_level : int32;
    last_commitment : Commitment.Hash.t;
  }

  (** Fixed size metadata encoding. *)
  val encoding : t Data_encoding.t
end

(** [export ?rollup_node_endpoint cctxt ~no_checks ~compression ~data_dir ~dest
    ~filename] creates a tar gzipped archive with name [filename] (or a
    generated name) in [dest] (or the current directory) containing a snapshot
    of the data of the rollup node with data directory [data_dir]. The path of
    the snapshot archive is returned. If [no_checks] is [true], the integrity of
    the snapshot is not checked at the end. This function will first try to
    cancel any GC on the target node if [rollup_node_endpoint] is specified to
    communicate with it. *)
val export :
  ?rollup_node_endpoint:Uri.t ->
  #Client_context.full ->
  no_checks:bool ->
  compression:compression ->
  data_dir:string ->
  dest:string option ->
  filename:string option ->
  string tzresult Lwt.t

(** [export_compact cctxt ~no_checks ~compression ~data_dir ~dest ~filename]
    creates a tar gzipped archive with name [filename] (or a generated name) in
    [dest] (or the current directory) containing a snapshot of the data of the
    rollup node with data directory [data_dir]. The difference with {!export} is
    that the snapshot contains a single commit for the context (which must be
    reconstructed on import) but is significantly smaller. If [no_checks] is
    [true], we don't check if commitments are published on L1 (integrity is not
    checked because it requires rebuilding the context). *)
val export_compact :
  #Client_context.full ->
  no_checks:bool ->
  compression:compression ->
  data_dir:string ->
  dest:string option ->
  filename:string option ->
  string tzresult Lwt.t

(** [import ~apply_unsafe_patches ~no_checks ~force cctxt ~data_dir
    ~snapshot_file] imports the snapshot at path [snapshot_file] into
    the data directory [data_dir]. If [no_checks] is [true], the
    integrity of the imported data is not checked at the end. Import
    will fail if [data_dir] is already populated unless [force] is set
    to [true]. if [apply_unsafe_patches] is [true] and there is
    [unsafe_pvm_patches] in the configuration they will be applied. *)
val import :
  apply_unsafe_patches:bool ->
  no_checks:bool ->
  force:bool ->
  #Client_context.full ->
  data_dir:string ->
  snapshot_file:string ->
  unit tzresult Lwt.t

(** [info ~snapshot_file] returns information that can be used to inspect the
    snapshot file. *)
val info :
  snapshot_file:string ->
  (Header.t * [`Compressed | `Uncompressed]) tzresult Lwt.t

(** [with_modify_data_dir cctxt ~data_dir ~apply_unsafe_patches ?skip_condition
    f] applies [f] in a read-write context that is created from [data-dir] (and
    a potential existing configuration). The node context given to [f] does not
    follow the L1 chain and [f] is only supposed to modify the data of the
    rollup node. It is used internally by this module to reconstruct contexts
    from a snapshot but can also be use by the {!Repair} module to apply fixes
    off-line. *)
val with_modify_data_dir :
  #Client_context.full ->
  data_dir:string ->
  apply_unsafe_patches:bool ->
  ?skip_condition:
    (Store.rw -> Context.rw -> head:Sc_rollup_block.t -> bool tzresult Lwt.t) ->
  (Node_context.rw -> head:Sc_rollup_block.t -> unit tzresult Lwt.t) ->
  unit tzresult Lwt.t
