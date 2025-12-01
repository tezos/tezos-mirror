(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

val err_implementation_mismatch : expected:string -> got:string -> 'a

type t = Tezos_protocol_environment.Context.t

type block_cache = Tezos_protocol_environment.Context.block_cache

(** Values of type [index] are used to [checkout] contexts specified by their hash. *)
type index

(** [do_not_use__is_duo index] returns [true] if the index is in duo mode

    This function should not be used in "normal" code as it is used when the duo mode
    is enabled. The duo mode is a debug mode used to compare Irmin and Brassaia.

    Making any assumption about the internals of context_ops is a unforgivable mistake *)
val do_not_use__is_duo : unit -> bool

(** Environment variable allowing to chose the
    context provider (Irmin/Brassaia/Duo) *)
val backend_variable : string

(** [context_dir ?envvar root] returns the context's directory concatenated to [root]
    depending on the backend that was chosen with TEZOS_CONTEXT_BACKEND

    Example:
    - [context_dir "/tmp/"] returns ["/tmp/context"] if Irmin or Duo was chosen
    - [context_dir "/tmp/"] returns ["/tmp/brassaia_context"] if Brassaia was chosen

    This function should always be used when doing operations
    on the context directory. *)
val context_dir : string -> string

(** [do_not_use__brassaia_dir root] will return "<root>/brassaia_context"

    This function should not be used in "normal" code as it is used when importing
    to copy the imported "context" to "brassaia_context" in duo mode *)
val do_not_use__brassaia_dir : string -> string

(** [init] uses an environment variable ('TEZOS_CONTEXT_BACKEND') to select the
    `Disk backend between `Shell and `Brassaia. [data_dir] stands for the root
    directory in which the context directory is expected to be find. *)
val init :
  kind:[`Disk | `Memory] ->
  ?patch_context:(t -> (t, tztrace) result Lwt.t) ->
  ?readonly:bool ->
  ?index_log_size:int ->
  data_dir:string ->
  unit ->
  index tzresult Lwt.t

val index : t -> index

(** [mem t k] is an Lwt promise that resolves to [true] iff [k] is bound
      to a value in [t]. *)
val mem : t -> Context.key -> bool Lwt.t

(** [mem_tree t k] is like {!mem} but for trees. *)
val mem_tree : t -> Context.key -> bool Lwt.t

(** [find t k] is an Lwt promise that resolves to [Some v] if [k] is
      bound to the value [v] in [t] and [None] otherwise. *)
val find : t -> Context.key -> Context.value option Lwt.t

(** [add t k v] is an Lwt promise that resolves to [c] such that:

    - [k] is bound to [v] in [c];
    - and [c] is similar to [t] otherwise.

    If [k] was already bound in [t] to a value that is physically equal
    to [v], the result of the function is a promise that resolves to
    [t]. Otherwise, the previous binding of [k] in [t] disappears. *)
val add : t -> Context.key -> Context.value -> t Lwt.t

(** [fold ?depth t root ~order ~init ~f] recursively folds over the trees
      and values of [t]. The [f] callbacks are called with a key relative
      to [root]. [f] is never called with an empty key for values; i.e.,
      folding over a value is a no-op.

      The depth is 0-indexed. If [depth] is set (by default it is not), then [f]
      is only called when the conditions described by the parameter is true:

      - [Eq d] folds over nodes and values of depth exactly [d].
      - [Lt d] folds over nodes and values of depth strictly less than [d].
      - [Le d] folds over nodes and values of depth less than or equal to [d].
      - [Gt d] folds over nodes and values of depth strictly more than [d].
      - [Ge d] folds over nodes and values of depth more than or equal to [d].

      If [order] is [`Sorted] (the default), the elements are traversed in
      lexicographic order of their keys. For large nodes, it is memory-consuming,
      use [`Undefined] for a more memory efficient [fold]. *)
val fold_value :
  ?depth:Tezos_context_sigs__Context.depth ->
  t ->
  Context.key ->
  order:[`Sorted | `Undefined] ->
  init:'a ->
  f:(Context.key -> (unit -> Context.value option Lwt.t) -> 'a -> 'a Lwt.t) ->
  'a Lwt.t

val add_protocol : t -> Protocol_hash.t -> t Lwt.t

val get_protocol : t -> Protocol_hash.t Lwt.t

val add_predecessor_block_metadata_hash : t -> Block_metadata_hash.t -> t Lwt.t

val add_predecessor_ops_metadata_hash :
  t -> Operation_metadata_list_list_hash.t -> t Lwt.t

val hash : time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t

val get_test_chain : t -> Test_chain_status.t Lwt.t

val add_test_chain : t -> Test_chain_status.t -> t Lwt.t

val fork_test_chain :
  t -> protocol:Protocol_hash.t -> expiration:Time.Protocol.t -> t Lwt.t

val commit :
  time:Time.Protocol.t -> ?message:string -> t -> Context_hash.t Lwt.t

(** [gc index commit_hash] removes from disk all the data older than
      the [commit_hash]. Operations needing to checkout commits
      greater or equal to [commit_hash] will continue to work. Calling
      [checkout h'] on GC-ed commits will return [None].

      From the irmin point of view, a successful gc call on a
      [commit_hash] will result in a new prefix file containing that
      [commit_hash] as a root commit. This prefix file is considered
      as standalone as all the data referenced by that commit is
      contained in that file. *)
val gc : index -> Context_hash.t -> unit Lwt.t

(** [wait_gc_completion index] will return a blocking thread if an
      Irmin GC is currently ongoing.

      {b Warning}: this currently only applies to GC started in the
      Irmin instance referenced as [index]; other opened instances
      will always return instantly. *)
val wait_gc_completion : index -> unit Lwt.t

(** [is_gc_allowed index] returns whether or not it is possible to
     run a GC on the given context tree. If false is returned, it
     means that the context was run, at least once, with the indexing
     strategy mode "always", which is not suitable for GC.*)
val is_gc_allowed : index -> bool

(** [split index] creates a new suffix file, also called "chunk",
      into the irmin's file hierarchy.

      To be optimal, the split function is expected to be called
      directly after committing, to the context, a commit (of hash
      [context_hash]) that will be a future candidate of a GC
      target. Thus, the commit [commit_hash] is the last commit stored
      on a given chunk. The GC called on that [commit_hash] will be
      able to extract that [commit_hash] into a new prefix file, and
      then, drop the whole chunk.

      If the last commit of a chunk appears not to be the candidate of
      a future GC, it may result in keeping chunks containing
      partially needed data. This is not an issue, but it should be
      avoided to prevent storing unnecessary data and thus, to
      minimize the disk footprint. *)
val split : index -> unit Lwt.t

(** Sync the context with disk. Only useful for read-only instances.
      Does not fail when the context is not in read-only mode. *)
val sync : index -> unit Lwt.t

val commit_test_chain_genesis : t -> Block_header.t -> Block_header.t Lwt.t

val compute_testchain_genesis : t -> Block_hash.t -> Block_hash.t

val merkle_tree :
  t ->
  Tezos_context_sigs__Context.Proof_types.merkle_leaf_kind ->
  Context.key ->
  Tezos_context_sigs__Context.Proof_types.merkle_tree Lwt.t

val merkle_tree_v2 :
  t ->
  Tezos_context_sigs__Context.Proof_types.merkle_leaf_kind ->
  Context.key ->
  Context.Proof.tree Context.Proof.t Lwt.t

val commit_genesis :
  index ->
  chain_id:Chain_id.t ->
  time:Time.Protocol.t ->
  protocol:Protocol_hash.t ->
  Context_hash.t tzresult Lwt.t

val checkout : index -> Context_hash.t -> t option Lwt.t

val checkout_exn : index -> Context_hash.t -> t Lwt.t

val exists : index -> Context_hash.t -> bool Lwt.t

val close : index -> unit Lwt.t

val compute_testchain_chain_id : t -> Block_hash.t -> Chain_id.t

val export_snapshot : index -> Context_hash.t -> path:string -> unit Lwt.t

val integrity_check :
  ?ppf:Format.formatter ->
  root:string ->
  auto_repair:bool ->
  always:bool ->
  heads:string list option ->
  index ->
  unit Lwt.t

val is_tezedge : t -> bool

module Upgrade : sig
  val v_3_3_upgrade : data_dir:string -> unit tzresult Lwt.t
end
