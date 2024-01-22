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

(** Store abstraction over the disk storage. *)

(**
   {2 Description}

   This component handles the on-disk storage of static objects such
   as blocks, operations, block's metadata, protocols and chain data.
   The store also handle the chain's current state: current head,
   invalid blocks, active testchains, ...

   {2 Concurrency}

   This module is designed to handle concurrent accesses to the
   store. Data-races and deadlocks might still happen through the use
   of potential dangerous calls that are documented as such. Both a
   mutex and a lockfile are present to handle concurrent accesses at
   process levels.

   {2 Context handling}

   The store manages the context and thus handles the context
   initialization through {!Lib_context.Context.init}. This is done,
   among others reasons, for a consistency checking when storing a
   block. The store never commits a context on-disk.

   {2 History mode handling}

   This store handles the three different
   {!Tezos_base.History_mode.t}:

   - Archive: maintains every block that is part of the chain
     including their metadata.

   - Full <offset>: maintains every block that is part of the chain
     but prune the metadata for blocks that are below the following
     threshold level: [last_preserved_block_level] of the current head
     - [offset] cycles.

   - Rolling <offset>: maintains rolling windows which contain recent
     blocks that are part of the chain, along with their metadata. It
     prunes everything that is below the following threshold level:
     [last_preserved_block_level] of the current head - [offset]
     cycles.

   {2 Protocol store}

   The store has a global protocol store which may be shared by
   different chain stores.

   {2 Chain stores and merging}

   The store has a toplevel chain store which is called the
   [main_chain_store] and which can be accessed through the store's
   interface. It is either created if the associated directory is not
   present or loaded if it was previously created. This
   [main_chain_store] is able to recursively spawn testchains which
   are also chain stores. Those testchains are also recursively able
   to spawn their own testchains.

   Each chain store possesses its own block store and therefore its
   own historic. It also maintains a state throughout its run which is
   composed of:

   - [current_head]: the current head of the chain.

   - [target]: the optional block for which the chain must pass. The
     store will not allow to store blocks past this target's block if
     they are not its successors.

   - [checkpoint]: the block that represents the no fork point level:
     blocks that are below this block are discarded.

   - [savepoint]: the block that represents the lowest block with
     metadata in the chain.

   - [caboose]: the block that represents the lowest block known in the
     chain (with or without metadata).

    More details and invariants on these points are provided in the
   {!Chain} module description below.

    When a block is promoted as head of the chain (through
   {!Chain.set_head}) the following happens:

   - A check is made if this head is consistent (i.e. if it's not
     below the checkpoint);

   - If the [last_preserved_block_level] of the head is different from
     the previous head's one, then we can establish that a cycle has
     been completed and we can start cementing this cycle by
     "triggering a merge".

   A merge phase consists of establishing the interval of blocks to
   cement, which is trivially [last_preserved_block_level(new_head)]
   to [last_preserved_block_level(prev_head)], but also, for Full and
   Rolling history modes, keep some extra blocks so that we make sure
   to keep blocks above
   max_operation_ttl(last_preserved_block_level(checkpoint)). This is
   done to make sure that we can export snapshots at the checkpoint
   level later on. This merging operation is asynchronous, the changes
   will be committed on disk only when the merge succeeds. Before
   that, we only retain the changes in RAM so we may keep storing new
   blocks without blocking. If the process crashes while a merge is
   happening, the state is reloaded before the merging point. More
   details are given in {!Chain.set_head}.

   {2 Files hierarchy}

   The store directory is organized as follows:

   - /<protocol_dir>/ the directory containing stored protocols

   - /<protocol_dir>/<protocol_hash_b58>* files containing the encoded
     protocol.

   - /<chain_id_b58>/ the [chain_store_dir] directory containing the
     main chain store.

   [chain_store_dir] is a symbol for all chain stores directory
   hierarchy.

   - [chain_store_dir]/<lock> the lockfile.

   - [chain_store_dir]/<config.json> the chain store's configuration
     as a JSON file.

   - [chain_store_dir]/<block_store> contains every file mentioned in
     {!Block_store}'s format.

   - [chain_store_dir]/<stored_data>* files containing encoded simple
     data structures such as: genesis block, checkpoint, savepoint,
     caboose, protocol levels, forked chains, invalid blocks, etc.

   - [chain_store_dir]/testchains/<testchain_id_b58>/ contains the
     [chains_store_dir]'s test chain, based on a similar hierarchy.

*)

open Store_types

(** The abstract type to manipulate the global store. *)
type t

(** The type alias for the global store. *)
type store = t

(** The abstract type for a chain store. Equivalent to {!Chain.t}. *)
type chain_store

(** {3 Initialization} *)

(** [init ?patch_context ?commit_genesis ?history_mode
    ?block_cache_limit ~store_dir ~context_dir ~allow_testchains
    genesis] initializes the store and a main chain store. If
    [store_dir] (resp. [context_dir]) does not exist, a fresh store
    (resp. context) is created. Otherwise, it loads the store
    (resp. context) from reading the adequate directory. If
    [allow_testchains] is passed, the store will be able to fork
    chains and instantiate testchain's sub chain stores, for all
    chains contained in the store. The chain store created is based on
    the [genesis] provided. Its chain identifier will be computed
    using the {!Chain_id.of_block_hash} function.

    @param patch_context the handle called when initializing the
    context. It usually is passed when creating a sandboxed chain.

    @param commit_genesis overrides the default initial genesis commit
    when the node is created. This is used for when the context must be
    in readonly (e.g. started by an external validator) and we want to
    prevent writing in the context. {b Warning} passing this argument
    will initialize the context in readonly.
      Default: {!Context.commit_genesis} is called with [genesis]

    @param history_mode the history mode used throughout the store. If
    a directory already exists and the given [history_mode] is
    different, the initialization will fail.
      Default: {!History_mode.default} (which should correspond to
    full with 5 extra preserved cycles.)

    @param block_cache_limit allows to override the size of the block
    cache to use. The minimal value is 1.

    @param readonly a flag that, if set to true, prevent writing
    throughout the store {b and} context.
      Default: false
*)
val init :
  ?patch_context:
    (Tezos_protocol_environment.Context.t ->
    Tezos_protocol_environment.Context.t tzresult Lwt.t) ->
  ?commit_genesis:(chain_id:Chain_id.t -> Context_hash.t tzresult Lwt.t) ->
  ?history_mode:History_mode.t ->
  ?readonly:bool ->
  ?block_cache_limit:int ->
  store_dir:string ->
  context_dir:string ->
  allow_testchains:bool ->
  Genesis.t ->
  store tzresult Lwt.t

(** [main_chain_store global_store] returns the main chain store. *)
val main_chain_store : store -> chain_store

(** [close_store global_store] closes the underlying block store and
    context along with every opened file descriptors for every chain
    store and testchain present in [global_store]. If the store is
    already closed, this function is idempotent. *)
val close_store : store -> unit Lwt.t

(** [may_switch_history_mode ?patch_context ~store_dir ~context_dir
    genesis ~new_history_mode] tries switching the store located at
    [store_dir] (if present) to [new_history_mode] when possible. *)
val may_switch_history_mode :
  store_dir:string ->
  context_dir:string ->
  Genesis.t ->
  new_history_mode:History_mode.t ->
  unit tzresult Lwt.t

(** {3 Accessors} *)

(** [directory global_store] returns the path where [global_store] is
    stored. This corresponds to the [store_dir] argument passed to the
    [init] function. *)
val directory : store -> [`Store_dir] Naming.directory

(** [context_index global_store] returns the context's index
    initialized in [global_store]. *)
val context_index : store -> Context_ops.index

(** [allow_testchains global_store] returns true if the store is
    allowed to fork testchains. *)
val allow_testchains : store -> bool

(** [all_chain_stores global_store] returns every initialized chain
    store in [global_store]. The resulting list also comprises the
    initialized testchains. If [allow_testchains] is false, the list
    will only contain a single element. *)
val all_chain_stores : store -> chain_store list Lwt.t

(** [get_chain_store global_store chain_id] returns the initialized
    chain store in [global_store] associated to [chain_id]. *)
val get_chain_store : store -> Chain_id.t -> chain_store tzresult Lwt.t

(** [get_chain_store_opt global_store chain_id] optional version of
    [get_chain_store]. *)
val get_chain_store_opt : store -> Chain_id.t -> chain_store option Lwt.t

val make_pp_store : store -> (Format.formatter -> unit -> unit) Lwt.t

val make_pp_chain_store :
  chain_store -> (Format.formatter -> unit -> unit) Lwt.t

(** The module for handling block-related operations such as storing
    and reading data associated to a single block. *)
module Block : sig
  (** The abstract type for a block. *)
  type t

  (** The type alias for a block. *)
  type block = t

  (** The type for block's metadata. *)
  type metadata = Block_repr.metadata = {
    message : string option;
    max_operations_ttl : int;
    last_preserved_block_level : Int32.t;
    block_metadata : Bytes.t;
    operations_metadata : Block_validation.operation_metadata list list;
  }

  (* FIXME: could be misleading. *)

  (** [equal b1 b2] tests the equality between [b1] and [b2]. {b
      Warning} only block hashes are compared. *)
  val equal : block -> block -> bool

  (** [is_known_valid chain_store bh] tests that the block [bh] is
      known and valid in [chain_store] (i.e. the block is present in
      the block store). *)
  val is_known_valid : chain_store -> Block_hash.t -> bool Lwt.t

  (** [is_known_invalid chain_store bh] tests that the block [bh] is
      invalid in [chain_store] (i.e. the block is present in the
      invalid blocks file). *)
  val is_known_invalid : chain_store -> Block_hash.t -> bool Lwt.t

  (** [is_known_validated chain_store bh] tests that the block [bh]
      is validated in [chain_store] (i.e. the block is present in the
      validated block cache). *)
  val is_known_validated : chain_store -> Block_hash.t -> bool Lwt.t

  (** [is_known chain_store bh] tests that the block [bh] is either
      known valid or known invalid in [chain_store]. *)
  val is_known : chain_store -> Block_hash.t -> bool Lwt.t

  (** [is_genesis chain_store bh] tests that the block [bh] is the
      genesis initialized in [chain_store]. *)
  val is_genesis : chain_store -> Block_hash.t -> bool

  (** [validity chain_store bh] computes the
      {!Block_locator.validity} ([Unknown], [Known_valid] or
      [Known_invalid]) for the block [bh] in [chain_store]. *)
  val validity : chain_store -> Block_hash.t -> Block_locator.validity Lwt.t

  (** [read_block chain_store ?distance bh] tries to read in the
      [chain_store] the block [bh] or the predecessing block at the
      offset [distance] of [bh]. By default, [distance] is 0.*)
  val read_block :
    chain_store -> ?distance:int -> Block_hash.t -> block tzresult Lwt.t

  (** [read_block_opt chain_store ?distance bh] optional version of
      [read_block]. *)
  val read_block_opt :
    chain_store -> ?distance:int -> Block_hash.t -> block option Lwt.t

  (** [read_block_by_level chain_store level] reads in the
      [chain_store] the block at [level]. The block retrieved will be
      the (level(current_head) - level)-th predecessor of the
      [chain_store]'s current head. *)
  val read_block_by_level : chain_store -> int32 -> block tzresult Lwt.t

  (** [read_block_by_level_opt chain_store level] optional version of
      [read_block_by_level]. *)
  val read_block_by_level_opt : chain_store -> int32 -> block option Lwt.t

  (** [read_block_metadata ?distance chain_store bh] reads in the
      [chain_store] the metadata associated to the block [bh] or its
      [distance]-th predecessor if given. Returns [None]
      otherwise. By default, [distance] is 0.

      {b Warning} If the block is already read, prefer the usage of
      [get_block_metadata] which will memoize the result. *)
  val read_block_metadata :
    ?distance:int ->
    chain_store ->
    Block_hash.t ->
    metadata option tzresult Lwt.t

  (** [read_block_metadata_opt ?distance chain_store bh] same as
      [read_block_metadata] but returns [None] in case of errors. *)
  val read_block_metadata_opt :
    ?distance:int -> chain_store -> Block_hash.t -> metadata option Lwt.t

  (** [get_block_metadata chain_store block] reads in the
      [chain_store] the metadata associated to the [block]. Returns
      [None] if the metadata cannot be retrieved. This function also
      memoize the result in the [block] structure so subsequent calls
      will be disk I/O free. *)
  val get_block_metadata : chain_store -> block -> metadata tzresult Lwt.t

  (** [get_block_metadata_opt chain_store block] optional version of
      [get_block_metadata] *)
  val get_block_metadata_opt : chain_store -> block -> metadata option Lwt.t

  (** [read_predecessor chain_store block] reads in the [chain_store]
      the direct predecessor of [block]. Returns [None] if it cannot
      be found. *)
  val read_predecessor : chain_store -> block -> block tzresult Lwt.t

  (** [read_predecessor_opt chain_store block] optional version of
      [read_predecessor]. *)
  val read_predecessor_opt : chain_store -> block -> block option Lwt.t

  (** [read_predecessor_of_hash chain_store bh] reads in [chain_store]
      the predecessor's block of [bh]. *)
  val read_predecessor_of_hash :
    chain_store -> Block_hash.t -> block tzresult Lwt.t

  (** [read_ancestor_hash chain_store ~distance bh] retrieves in the
      [chain_store] the hash of the ancestor of the block [bh] at
      [distance] if it exists. Returns [None] otherwise. *)
  val read_ancestor_hash :
    chain_store ->
    distance:int ->
    Block_hash.t ->
    Block_hash.t option tzresult Lwt.t

  (** [read_ancestor_hash_opt chain_store ~distance bh] same as
      [read_ancestor_hash] but returns [None] on errors. *)
  val read_ancestor_hash_opt :
    chain_store -> distance:int -> Block_hash.t -> Block_hash.t option Lwt.t

  (** [read_ancestor_opt chain_store block] optional version of
      [read_ancestor]. *)
  val read_predecessor_of_hash_opt :
    chain_store -> Block_hash.t -> block option Lwt.t

  (** [read_validated_block chain_store bh] tries to read in the
      [chain_store]'s validated block cache the block [bh].*)
  val read_validated_block : chain_store -> Block_hash.t -> block tzresult Lwt.t

  (** [read_validated_block_opt chain_store bh] optional version of
      [read_validated_block].*)
  val read_validated_block_opt :
    chain_store -> Block_hash.t -> block option Lwt.t

  (** [store_block chain_store ~block_header ~operations
     validation_result] stores in [chain_store] the block with its
     [block_header], [operations] and validation result. Inconsistent
     blocks and validation will result in failures. Returns [None] if
     the block was already stored. If the block is correctly stored,
     the newly created block is returned.

      If the block was successfully stored, then the block is removed
     from the validated block cache. *)
  val store_block :
    chain_store ->
    block_header:Block_header.t ->
    operations:Operation.t list list ->
    Block_validation.result ->
    block option tzresult Lwt.t

  (** [store_validated_block chain_store ~hash ~block_header ~operations]
      stores in [chain_store]'s validated block cache the block with
      its [block_header] and [operations]. *)
  val store_validated_block :
    chain_store ->
    hash:Block_hash.t ->
    block_header:Block_header.t ->
    operations:Operation.t trace trace ->
    unit tzresult Lwt.t

  (** [resulting_context_hash chain_store block] returns the resulting
      context hash of the [block]. This context depends on the
      [block]'s protocol associated semantics, i.e., it can either be
      the one contained in its block header or the stored result of
      its application. *)
  val resulting_context_hash :
    chain_store -> block -> Context_hash.t tzresult Lwt.t

  (** [context_exn chain_store block] checkouts the {b resulting}
      context of the [block] which may differ from its block header's
      one depending on the block's associated protocol semantics. *)
  val context_exn :
    chain_store -> block -> Tezos_protocol_environment.Context.t Lwt.t

  (** [context_opt chain_store block] optional version of
      [context_exn]. *)
  val context_opt :
    chain_store -> block -> Tezos_protocol_environment.Context.t option Lwt.t

  (** [context chain_store block] error monad version of
      [context_exn]. *)
  val context :
    chain_store -> block -> Tezos_protocol_environment.Context.t tzresult Lwt.t

  (** [context_exists chain_store block] tests the existence of the
      [block]'s commit in the context. *)
  val context_exists : chain_store -> block -> bool Lwt.t

  (** [testchain_status chain_store block] returns the test chain
      status stored in context of [block] along with testchain's
      genesis if the testchain is found [Forking] or [Running]. *)
  val testchain_status :
    chain_store ->
    block ->
    (Test_chain_status.t * Block_hash.t option) tzresult Lwt.t

  (** [protocol_hash_exn chain_store block] reads the protocol
      associated to [block] in its associated context. Fails when the
      context is unknown. *)
  val protocol_hash_exn : chain_store -> block -> Protocol_hash.t Lwt.t

  (** [protocol_hash chain_store block] error monad version of
      [protocol_hash_exn]. *)
  val protocol_hash : chain_store -> block -> Protocol_hash.t tzresult Lwt.t

  (** [read_invalid_block_opt chain_store bh] reads in the
      [chain_store] the invalid block [bh] if it exists. *)
  val read_invalid_block_opt :
    chain_store -> Block_hash.t -> invalid_block option Lwt.t

  (** [read_invalid_blocks chain_store] returns the map of all invalid
      blocks of [chain_store]. *)
  val read_invalid_blocks : chain_store -> invalid_block Block_hash.Map.t Lwt.t

  (** [mark_invalid chain_store bh ~level errors] stores the block
      [bh] at [level] with the given [errors]. Fails when trying to
      mark the genesis block as invalid. *)
  val mark_invalid :
    chain_store ->
    Block_hash.t ->
    level:int32 ->
    error list ->
    unit tzresult Lwt.t

  (** [unmark_invalid chain_store bh] unmarks invalid the block [bh]
      in the [chain_store]. *)
  val unmark_invalid : chain_store -> Block_hash.t -> unit tzresult Lwt.t

  (** [descriptor block] returns the pair (hash x level) of [block]. *)
  val descriptor : block -> block_descriptor

  (** {3 Block field accessors} *)

  val hash : block -> Block_hash.t

  val header : block -> Block_header.t

  val operations : block -> Operation.t list list

  val shell_header : block -> Block_header.shell_header

  val level : block -> int32

  val proto_level : block -> int

  val predecessor : block -> Block_hash.t

  val timestamp : block -> Time.Protocol.t

  val validation_passes : block -> int

  val operations_hash : block -> Operation_list_list_hash.t

  val fitness : block -> Fitness.t

  val context_hash : block -> Context_hash.t

  val protocol_data : block -> bytes

  val block_metadata_hash : block -> Block_metadata_hash.t option

  val operations_metadata_hashes :
    block -> Operation_metadata_hash.t list list option

  val operations_metadata_hashes_path :
    block -> int -> Operation_metadata_hash.t list option

  val all_operations_metadata_hash :
    block -> Operation_metadata_list_list_hash.t option

  (** {3 Block metadata field accessors} *)

  val message : metadata -> string option

  val max_operations_ttl : metadata -> int

  val last_preserved_block_level : metadata -> int32

  val block_metadata : metadata -> Bytes.t

  val operations_metadata :
    metadata -> Block_validation.operation_metadata list list

  (** [operations_path block nth] computes the [nth] operations list
      of [block] along with the hash of all operations. *)
  val operations_path :
    block -> int -> Operation.t list * Operation_list_list_hash.path

  (** [operations_hashes_path block nth] computes the [nth] operations
      hash list of [block] along with the hash of all operations. *)
  val operations_hashes_path :
    block -> int -> Operation_hash.t list * Operation_list_list_hash.path

  (** [all_operation_hashes block] computes the hash of all operations
      in [block]. *)
  val all_operation_hashes : block -> Operation_hash.t list list
end

(** The module for handling chain-related operations such as setting
    the head of the chain, updating the chain state, forking
    testchains, ... *)
module Chain : sig
  (** The abstract type alias of a chain store. *)
  type nonrec chain_store = chain_store

  (** The type alias of [chain_store]. *)
  type t = chain_store

  (** The abstract type for testchain. *)
  type testchain

  (** A type alias of a block identifier. *)
  type block_identifier = Block_services.block

  (** [global_store chain_store] returns the global store of
      [chain_store] allowing to retrieve global infos.*)
  val global_store : chain_store -> store

  (** [chain_id chain_store] returns chain id of [chain_store]. *)
  val chain_id : chain_store -> Chain_id.t

  (** [chain_dir chain_store] returns the path of directory of
      [chain_store].*)
  val chain_dir : chain_store -> [`Chain_dir] Naming.directory

  (** [history_mode chain_store] returns the history mode of the
      [chain_store].*)
  val history_mode : chain_store -> History_mode.t

  (** [genesis chain_store] returns the {!Genesis.t} of the
      [chain_store]. *)
  val genesis : chain_store -> Genesis.t

  (** [genesis chain_store] returns the genesis block of the
      [chain_store]. *)
  val genesis_block : chain_store -> Block.t Lwt.t

  (** [current_head chain_store] returns the current head of the
      [chain_store]. *)
  val current_head : chain_store -> Block.t Lwt.t

  (** [expiration chain_store] returns the expiration date of the
      testchain's [chain_store]. *)
  val expiration : chain_store -> Time.Protocol.t option

  (** [checkpoint chain_store] returns the checkpoint associated to
      the [chain_store].

      The checkpoint is a block descriptor
      ({!Store_types.block_descriptor}) pointing to a block that must be
      part of the chain. The checkpoint maintains the set of following
      invariants:

      - The store will only accept blocks that are above its
        checkpoint's level.

      - The checkpoint is updated periodically such that the following
        invariant holds: [checkpoint.level >=
        all_head.last_preserved_block_level]

      The checkpoint will tend to designate the highest block among
      all chain head's [last_preserved_block_level] in a normal
      mode. This is not always true. i.e. after a snapshot import
      where the checkpoint will be set as the imported block and when
      the [target] block is reached, the checkpoint will be set at
      this point. *)
  val checkpoint : chain_store -> block_descriptor Lwt.t

  (** [target chain_store] returns the target block associated to the
      [chain_store] if there is one or [None] if it has already been reached.

      The target is a ({!Store_types.block_descriptor}) pointing to a
      future block that must be reached by the chain. When a block is
      received, if its level is equal to the target's block level then
      its hash must be also be the same. Otherwise, the block is
      considered as invalid and will be discarded.

      The target should only be set manually. Whenever the target
      block is reached, the checkpoint will be updated to this
      block and the target will be set to [None]. *)
  val target : chain_store -> block_descriptor option Lwt.t

  (** [savepoint chain_store] returns the savepoint associated to the
      [chain_store].

      The savepoint is a block descriptor
      ({!Store_types.block_descriptor}) pointing to the lowest level
      block that has its metadata and/or context pruned with the
      following invariant:

      [is_stored(block) \and has_metadata(block) => block.level >=
      savepoint.level]

      For Full and Rolling history modes, the savepoint will be
      periodically updated at each store merge which happens when:

      [pred(head).last_preserved_block_level <
      head.last_preserved_block_level]

      On Archive history mode: [savepoint = genesis]. *)
  val savepoint : chain_store -> block_descriptor Lwt.t

  (** [caboose chain_store] returns the caboose associated to the
      [chain_store].

      The caboose is a block descriptor
      ({!Store_types.block_descriptor}) pointing to the lowest level
      block that we know present in the chain store, if the history
      mode is {b not} Archive, it might not contain its metadata
      depending on the current savepoint:

      [is_stored(block) => block.level >= caboose.level]

      On Archive and Full history mode: [caboose = genesis] *)
  val caboose : chain_store -> block_descriptor Lwt.t

  (** [mempool chain_store] returns the mempool associated to the
      [chain_store]. *)
  val mempool : chain_store -> Mempool.t Lwt.t

  (** [block_of_identifier chain_store identifier] tries to return the block
      of the given [identifier] inside the given [chain_store]. *)
  val block_of_identifier :
    chain_store -> block_identifier -> Block.t tzresult Lwt.t

  (** [block_of_identifier_opt chain_store identifier] optional version of [block_of_identifier]. *)
  val block_of_identifier_opt :
    chain_store -> block_identifier -> Block.t option Lwt.t

  (** [set_mempool chain_store ~head mempool] sets the [mempool] of
      the [chain_store]. Does nothing if [head] is not current_head
      which might happen when a new head concurrently arrives just
      before this operation being called. *)
  val set_mempool :
    chain_store -> head:Block_hash.t -> Mempool.t -> unit tzresult Lwt.t

  (** [live_blocks chain_store] returns the set of previously computed
      live blocks for the current_head's [chain_store]. *)
  val live_blocks :
    chain_store -> (Block_hash.Set.t * Operation_hash.Set.t) Lwt.t

  (** [compute_live_blocks ~block chain_store] computes the set of
      live blocks and live operations relative to [block]. Does nothing
      if [block] is the [chain_store]'s current head as it was
      previously updated in {!set_head}.

      Note: this operation should not be costly in most cases as
      recent blocks and operations are expected to be in a cache. *)
  val compute_live_blocks :
    chain_store ->
    block:Block.t ->
    (Block_hash.Set.t * Operation_hash.Set.t) tzresult Lwt.t

  (** [set_head chain_store block] promotes the [block] as head of the
      [chain_store] and triggers an asynchronous store merge if a
      cycle is ready to be cemented. Triggering a merge will update
      the savepoint, checkpoint and caboose consistently with the
      [chain_store]'s history mode. This function returns the previous
      head. Setting a new head will fail when the block is not fit to
      be promoted as head (i.e. too old or no metadata).

      After a merge:

      - The checkpoint is updated to [lpbl(new_head)] if it was below
        this level or unchanged otherwise;

      - The savepoint will be updated to :
        min(max_op_ttl(lpbl(new_head)), lpbl(new_head) -
        <cycle_length> * <history_mode_offset>) or will remain 0 in
        Archive mode;

      - The caboose will be updated to the same value as the savepoint
        in Rolling mode.

      Note: lpbl(new_head) is the last preserved block level of the
      new head.

      {b Warnings:}

      - We expect blocks to be sequentially promoted as head using
        this function;

      - If a merge is triggered while another is happening, this
        function will block until the first merge is resolved. *)
  val set_head : chain_store -> Block.t -> Block.t tzresult Lwt.t

  (** [is_ancestor chain_store ~head ~ancestor] checks whether the
      [ancestor] is a predecessor or [head] in [chain_store]. *)
  val is_ancestor :
    chain_store ->
    head:block_descriptor ->
    ancestor:block_descriptor ->
    bool Lwt.t

  (** [is_in_chain chain_store block_descr] checks that [block_descr]
      is an ancestor of [chain_store]'s current head. *)
  val is_in_chain : chain_store -> block_descriptor -> bool Lwt.t

  (** [is_acceptable_block chain_store block_descr] checks if
      [block_descr] would be a valid block to be stored in
      [chain_store]. Its predecessor is supposed to be already
      stored. *)
  val is_acceptable_block : chain_store -> block_descriptor -> bool Lwt.t

  (** [compute_locator chain ?max_size head seed] computes a
      locator of the [chain] from [head] to the chain's caboose or until
      the locator contains [max_size] steps.
      [max_size] defaults to 200. *)
  val compute_locator :
    chain_store ->
    ?max_size:int ->
    Block.t ->
    Block_locator.seed ->
    Block_locator.t Lwt.t

  (** [compute_protocol_locator chain ?max_size ~proto_level seed]
      computes a locator for a specific protocol of level [proto_level]
      in the [chain] from the latest block with this protocol to its
      activation block or until the locator contains [max_size] steps.
      [max_size] defaults to 200. *)
  val compute_protocol_locator :
    chain_store ->
    ?max_size:int ->
    proto_level:int ->
    Block_locator.seed ->
    Block_locator.t option Lwt.t

  (** [set_target chain_store target_descr] sets the target for
      [chain_store]. If [target_descr] is already known,
      [set_checkpoint] will be internally called. Fails if the target
      is below the current checkpoint. *)
  val set_target : chain_store -> block_descriptor -> unit tzresult Lwt.t

  (** [testchain chain_store] returns the active testchain
      to the given [chain_store] if it has been instantiated. *)
  val testchain : chain_store -> testchain option Lwt.t

  (** [testchain_forked_block testchain] returns the hash of the forked
     block of its parent chain.*)
  val testchain_forked_block : testchain -> Block_hash.t

  (** [testchain_store testchain] returns the chain store associated
      to this [testchain]. *)
  val testchain_store : testchain -> chain_store

  (** [fork testchain chain_store ~testchain_id ~forked_block
      ~genesis_hash ~genesis_header ~test_protocol ~expiration] forks
      a testchain and activates it for [chain_store]. If a testchain
      with [testchain_id] already existed, it is then loaded from the
      store. If it was already activated, does nothing. It also
      registers this chain in the set of activated testchains.  *)
  val fork_testchain :
    chain_store ->
    testchain_id:Chain_id.t ->
    forked_block:Block.t ->
    genesis_hash:Block_hash.t ->
    genesis_header:Block_header.t ->
    test_protocol:Protocol_hash.t ->
    expiration:Time.Protocol.t ->
    testchain tzresult Lwt.t

  (** [shutdown_testchain chain_store] closes and deactivates the
      testchain running for [chain_store]. Does nothing if no
      testchain is found running for [chain_store]. *)
  val shutdown_testchain : chain_store -> unit tzresult Lwt.t

  (** {2 Chain's protocols} *)

  (** [find_protocol_info chain_store ~protocol_level] returns the
     protocol info associated to the given [protocol_level]. *)
  val find_protocol_info :
    t -> protocol_level:int -> Protocol_levels.protocol_info option Lwt.t

  (** [find_activation_block chain_store ~protocol_level] returns the
      block that activated the protocol of level [protocol_level]. *)
  val find_activation_block :
    chain_store -> protocol_level:int -> block_descriptor option Lwt.t

  (** [find_protocol chain_store ~protocol_level] returns the protocol
      with the level [protocol_level]. *)
  val find_protocol :
    chain_store -> protocol_level:int -> Protocol_hash.t option Lwt.t

  (** [expects_predecessor_context_hash chain_store ~protocol_level]
      returns whether or not a protocol requires the context hash of a
      block to target resulting context of it's predecessor. This
      depends on the environment of each protocol.*)
  val expect_predecessor_context_hash :
    chain_store -> protocol_level:int -> bool tzresult Lwt.t

  (** [all_protocol_levels chain_store] returns all the protocols
      registered in [chain_store]. *)
  val all_protocol_levels :
    chain_store -> Protocol_levels.protocol_info Protocol_levels.t Lwt.t

  (** [may_update_protocol_level chain_store ?pred ?protocol_level
      ~expect_predecessor_context (block, ph)] updates the protocol
      level for the protocol [ph] in [chain_store] with the activation
      [block]. If [pred] is not provided, it reads the [block]'s
      predecessor and check that the [block]'s protocol level is
      increasing compared to its predecessor. If [protocol_level] is
      provided, we use this value instead of the protocol level found
      in [block]. If a previous entry is found, it overwrites it. The
      [expect_predecessor_context] argument specifies which context
      hash semantics should be used. *)
  val may_update_protocol_level :
    chain_store ->
    ?pred:Block.block ->
    ?protocol_level:int ->
    expect_predecessor_context:bool ->
    Block.block * Protocol_hash.t ->
    unit tzresult Lwt.t

  (** [may_update_ancestor_protocol_level chain_store ~head] tries to
      find the activation block of the [head]'s protocol, checks that
      its an ancestor and tries to update it if that's not the
      case. If the registered activation block is not reachable
      (already pruned), this function does nothing. *)
  val may_update_ancestor_protocol_level :
    chain_store -> head:Block.block -> unit tzresult Lwt.t

  (** [watcher chain_store] instantiates a new block watcher for
      [chain_store]. *)
  val watcher : chain_store -> Block.t Lwt_stream.t * Lwt_watcher.stopper

  (** [validated_watcher chain_store] instantiates a new validated block
       watcher for [chain_store]. *)
  val validated_watcher :
    chain_store -> Block.t Lwt_stream.t * Lwt_watcher.stopper

  (** [get_rpc_directory chain_store block] returns the RPC directory
      associated to the [block]. *)
  val get_rpc_directory :
    chain_store ->
    Block.t ->
    (chain_store * Block.t) Tezos_rpc.Directory.t option Lwt.t

  (** [set_rpc_directory chain_store ph next_ph rpc_directory] sets a
      [rpc_directory] for the protocol [ph] and next protocol [next_ph]
      in [chain_store]. *)
  val set_rpc_directory :
    chain_store ->
    protocol_hash:Protocol_hash.t ->
    next_protocol_hash:Protocol_hash.t ->
    (chain_store * Block.t) Tezos_rpc.Directory.t ->
    unit Lwt.t

  (** [register_gc_callback chain_store callback] installs a
      [callback] that may be triggered during a block store merge in
      order to garbage-collect old contexts. *)
  val register_gc_callback :
    chain_store -> (Block_hash.t -> unit tzresult Lwt.t) option -> unit

  (** [register_split_callback chain_store callback] installs a
      [callback] that may be triggered during a [set_head] in order to
      split the context into a new chunk. *)
  val register_split_callback :
    chain_store -> (unit -> unit tzresult Lwt.t) option -> unit
end

(** [global_block_watcher global_store] instantiates a new block
    watcher for every chain store active in [global_store]. *)
val global_block_watcher :
  store -> (Chain.chain_store * Block.block) Lwt_stream.t * Lwt_watcher.stopper

(** The module for handling protocol-related operations. *)
module Protocol : sig
  (** [mem global_store ph] checks the existence of the protocol [ph]
      in [global_store]. *)
  val mem : store -> Protocol_hash.t -> bool

  (** [all global_store ph] returns the set of all stored protocol in
      [global_store]. *)
  val all : store -> Protocol_hash.Set.t

  (** [read global_store ph] reads the protocol [ph] from the
      [global_store]. Returns [None] if it does not exist. *)
  val read : store -> Protocol_hash.t -> Protocol.t option Lwt.t

  (** [store global_store ph protocol] stores the [protocol] under the
      hash [ph] in the [global_store]. Returns [None] if it already
      exists or [Some ph] if it was correctly stored.

      {b Warning} No hash check is made, the caller must be careful
      when storing protocols. *)
  val store :
    store -> Protocol_hash.t -> Protocol.t -> Protocol_hash.t option Lwt.t

  (** [store_raw global_store ph bytes] raw version of [store]. *)
  val store_raw :
    store -> Protocol_hash.t -> bytes -> Protocol_hash.t option Lwt.t

  (** [protocol_watcher global_store] instantiates a new protocol
      watcher in [global_store]. *)
  val protocol_watcher :
    store -> Protocol_hash.t Lwt_stream.t * Lwt_watcher.stopper
end

(** The utility module used to traverse the chain. *)
module Chain_traversal : sig
  (** [path chain_store from_block to_block] retrieves all blocks in
      [chain_store] from [from_block] to [to_block] ([from_block] is
      excluded from the result).

      @raise Invalid_arg if [from_block] is above [to_block]. *)
  val path :
    chain_store ->
    from_block:Block.t ->
    to_block:Block.t ->
    Block.t list option Lwt.t

  (** [common_ancestor chain_store b1 b2] retrieves the common
      ancestor of [b1] and [b2] in [chain_store]. Returns [None] if no
      ancestor can be found. *)
  val common_ancestor :
    chain_store -> Block.t -> Block.t -> Block.t option Lwt.t

  (** [new_blocks ~from_block ~to_block] returns a pair [(ancestor,
      path)], where [ancestor] is the common ancestor of [from_block]
      and [to_block] and where [path] is the chain from [ancestor]
      (excluded) to [to_block] (included).

      @raise assert failure when the two provided blocks do not belong
      to the same [chain]. *)
  val new_blocks :
    chain_store ->
    from_block:Block.t ->
    to_block:Block.t ->
    (Block.t * Block.t list) Lwt.t
end

(** Upgrade a v_2 to v_3 store by rewriting the block store and the
    protocol level's table.

    {b Warning} Not backward-compatible. *)
val v_3_0_upgrade : store_dir:string -> Genesis.t -> unit tzresult Lwt.t

(**/**)

(** Unsafe set of functions intended for internal store manipulation
   (e.g. snapshot, reconstruct, testing). Must not be used outside of
   the [Tezos_store]. *)
module Unsafe : sig
  val repr_of_block : Block.t -> Block_repr.t

  val block_of_repr : Block_repr.t -> Block.t

  val get_block_store : chain_store -> Block_store.block_store

  val load_testchain :
    chain_store -> chain_id:Chain_id.t -> Chain.testchain option tzresult Lwt.t

  (** [set_head chain_store block] sets the block as the current head
      of [chain_store] without checks. *)
  val set_head : chain_store -> Block.t -> unit tzresult Lwt.t

  (** [set_history_mode chain_store history_mode] sets the history mode
      for the [chain_store] without checks. *)
  val set_history_mode : chain_store -> History_mode.t -> unit tzresult Lwt.t

  (** [set_checkpoint chain_store checkpoint] sets the checkpoint for
      the [chain_store] without checks. *)
  val set_checkpoint : chain_store -> block_descriptor -> unit tzresult Lwt.t

  (** [set_cementing_highwatermark chain_store
      cementing_highwatermark] sets the cementing_highwatermark for the
      [chain_store] without checks. *)
  val set_cementing_highwatermark :
    chain_store -> int32 option -> unit tzresult Lwt.t

  (** [set_savepoint chain_store savepoint] sets the savepoint for the
      [chain_store] without checks. *)
  val set_savepoint : chain_store -> block_descriptor -> unit tzresult Lwt.t

  (** [set_caboose chain_store caboose] sets the caboose for the
      [chain_store] without checks. *)
  val set_caboose : chain_store -> block_descriptor -> unit tzresult Lwt.t

  (** [set_protocol_level chain_store protocol_level (block, ph,
      expect_predecessor_context)] updates the protocol level for the
      protocol [ph] in [chain_store] with the activation
      [block] and specifies the [expect_predecessor_context] semantics. *)
  val set_protocol_level :
    chain_store ->
    protocol_level:int ->
    Block.block * Protocol_hash.t * bool ->
    unit tzresult Lwt.t

  (** Snapshots utility functions *)

  (** [open_for_snapshot_export ~store_dir ~context_dir genesis
      ~locked_f] opens the store (resp. context) located in [store_dir]
      (resp. [context_dir]) and gives the [chain_store] whose
      [chain_id] is computed using [genesis] and gives it to
      [locked_f]. [locked_f] starts by taking a lock (using a lockfile) on
      the store to prevent merge from happening during this function.

      {b Warning} [locked_f] must not perform long computations or
      costly I/Os: if the store needs to perform a merge, it will be
      locked while [locked_f] is running. *)
  val open_for_snapshot_export :
    store_dir:string ->
    context_dir:string ->
    Genesis.t ->
    locked_f:(chain_store -> 'a tzresult Lwt.t) ->
    'a tzresult Lwt.t

  (** [restore_from_snapshot ?notify ~store_dir ~context_index
      ~genesis ~genesis_context_hash ~floating_blocks_stream
      ~new_head_with_metadata ~new_head_resulting_context_hash
      ~predecessor_header ~protocol_levels ~history_mode] initialises
      a coherent store in [store_dir] with all the given info
      retrieved from a snapshot. *)
  val restore_from_snapshot :
    ?notify:(unit -> unit Lwt.t) ->
    [`Store_dir] Naming.directory ->
    genesis:Genesis.t ->
    genesis_context_hash:Context_hash.t ->
    floating_blocks_stream:Block_repr.block Lwt_stream.t ->
    new_head_with_metadata:Block_repr.block ->
    new_head_resulting_context_hash:Context_hash.t ->
    predecessor_header:Block_header.t ->
    protocol_levels:Protocol_levels.protocol_info Protocol_levels.t ->
    history_mode:History_mode.t ->
    unit tzresult Lwt.t
end
