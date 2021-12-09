(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* General description of the prevalidator:

   The main role of the prevalidator is the propagation of valid
   operations through the gossip network of Tezos. The baker also uses
   the prevalidator via the [monitor_operations] RPC to filter
   operations that can be included in blocks.

   The prevalidator manages a validation state based upon the current
   head chosen by the validation sub-system. Each time the
   prevalidator receives an operation, it tries to classify it on top
   of the current validation state. If the application of the incoming
   operation succeeds, the validation state is then updated and, the
   operation can be propagated. Otherwise, the handling of the
   operation depends on the classification: [Applied],
   [Branch_delayed], [Branch_refused] or [Refused]. This
   classification is detailed below. Given an operation, its
   classification may change if the head changes. When the validation
   sub-system switches its head, it notifies the prevalidator with the
   new [live_blocks] and [live_operations], triggering also a [flush]
   of the mempool: every operation classified as [Applied] or
   [Branch_delayed] which is anchored (i.e, the [block hash] on which
   the operation is based on when it was created) on a [live block]
   and which is not in the [live operations] (operations which are
   included in [live_blocks]) is set [pending], meaning they are
   waiting to be classified again. Operations classified as
   [Branch_refused] are reclassified only if the old head is not the
   predecessor block of the new head. We use the
   [Chain_validator_worker_state.Event.update] for that purpose (see
   {on_flush}). [Refused] operations are never reclassified. We keep
   track on them to avoid to handle it if it is advertised again in a
   short period of time.

   Plugins may be used as an anti-spam protection mechanism, more
   restrictive than the economic protocol. They are not mandatory and
   come with the shell. By not mandatory, it means that without the
   plugin, the prevalidator still works. However, it may propagate
   outdated operations and the prevalidator can be slower. Indeed,
   plugins add more restrictions on the validation of operations. The
   plugin comes with three functions: [pre_filter], [precheck] and
   [post_filter]. With the exception of locally injected operations,
   pending operations are first pre-filtered.
   The [precheck] is applied before classifying an operation.
   The [post_filter] is applied every time an operation is classified
   as [Applied].

   Error classification:

   The [apply_operation] function from the economic protocol can
   classify an operation as [Refused], [Branch_refused],
   [Branch_delayed], [Outdated] or [Applied].

     - An operation is [Refused] if the protocol rejects this 
   operation with an error classified as [Permanent].

     - An operation is [Outdated] if the operation is too old to be
   applied anymore or if the protocol rejects this operation with an
   error classified as [Outdated]

     - An operation is [Branch_refused] if the operation is anchored
   on a block that has not been validated by the node but could be in
   the future or if the protocol rejects this operation with an error
   classified as [Branch]. This semantics is likely to be weakened to
   also consider [Outdated] operations.

     - An operation is [Branch_delayed] if the initialization of the
   validation state failed (which presumably cannot happen currently)
   or if the protocol rejects this operation with an error classified
   as [Temporary].

     - An operation is [Applied] if it has been successfully
   prechecked, or if the economic protocol succeeded in applying the
   operation on the current validation state. The point of
   prechecking an operation is that it is faster than having the protocol apply
   the operation. Operations are stored in the reverse order of application
   so that adding a new [Applied] operation can be done at the head
   of the list.

   The [classification] data-structure (implemented in
   [Prevalidator_classification]) is used by the [prevalidator] to
   handle operations and their classification given either by the
   plugin or the economic protocol. One important property of this
   data-structure is to answer quickly if an operation is already
   classified or not.

   The interaction between the [Prevalidator_classification] module
   and the [Prevalidator] ensures an invariant that the different
   classifications are {e disjoint}: an operation cannot be in two (or
   more) of these subfields at the same time. The rationale to not
   make this invariant explicit is for performances reasons.

   Operation status:

     Operations are identified uniquely by their hash. Given an
   operation hash, the status can be either: [fetching], [pending],
   [classified], or [banned].

     - An operation is [fetching] if we only know its hash but we did
   not receive yet the corresponding operation.

     - An operation is [pending] if we know its hash and the
   corresponding operation but this operation is not classified on top
   of the current head yet.

     - An operation is [classified] if we know its hash, the
   corresponding operation and was classified according to the
   classification given above. Note that for [Branch_refused]
   operation, the classification may be prior to the last flush.

     - We may also ban an operation locally (through an RPC). A
   [banned] operation is removed from all other fields, and is ignored
   when it is received in any form (its hash, the corresponding
   operation, or a direct injection from the node).

     The prevalidator ensures that an operation cannot be at the same
   time in two of the following fields: [fetching], [pending],
   [in_mempool] (containing the [classified] operations), and
   [banned_operations].

   Propagation of operations:

     An operation is propagated through the [distributed database]
   component (aka [ddb]) which interacts directly with the [p2p]
   network. The prevalidator advertises its mempool (containing only
   operation hashes) through the [ddb]. If a remote peer requests an
   operation, such request will be handled directly by the [ddb]
   without going to the prevalidator. This is why every operation that
   is propagated by the prevalidator should also be in the [ddb]. But
   more important, an operation which we do not want to advertise
   should be removed explicitly from the [ddb] via the
   [Distributed_db.Operation.clear_or_cancel] function.

   It is important that every operation we do not want to propagate
   are cleaned up from the [Distributed_db] explicitely. Operations we
   do not want to propagate are operations classified as [Refused] or
   [Outdated], already included in block, or filtered out by the
   plugin.

   The [mempool] field contains only operations which are in the
   [in_mempool] field and that we accept to propagate. In particular,
   we do not propagate operations classified as [Refused] or
   [Outdated].

     There are two ways to propagate our mempool:

     - Either when we classify operations as applied

     - Or when a peer requests explicitly our mempool

     In the first case, only the newly classified operations are
   propagated. In the second case, current applied operations and
   pending operations are sent to the peer. Every time an operation is
   removed from the [in_mempool] field, this operation should be
   cleaned up in the [Distributed_db.Operation] requester.

   There is an [advertisement_delay] to postpone the next mempool
   advertisement if we advertised our mempool not long ago. Early
   consensus operations will be propagated once the block is
   validated. Every time an operation is [classified], it is recorded
   into the [operation_stream]. Such stream can be used by an external
   service to get the classification of an operation (via the
   [monitor_operations] RPC). This also means an operation can be
   notified several times if it is classified again after a
   [flush].

   Internally, the prevalidator implementation is split between
   the [Requests] module and the [Handlers] module.

   The [Requests] module contains the top-level functions called to implement
   the various requests defined in {!Prevalidator_worker_state}. These
   transitions form the meat of the prevalidator implementation: that is where
   the logic lies. This module is written in an imperative style: most
   functions return [unit] instead of returning an updated value.
   We aim to make this module functional in the near future.

   The [Handlers] module implement the functions needed by the
   {!Worker.T.HANDLERS} API. These functions concern the lifecycle
   of a [Worker], such as what happens when it starts and when it is shutdown.
   Except for initialization, the [Handlers] module is mostly boilerplate. *)

open Prevalidator_worker_state
module Event = Prevalidator_event

type limits = {
  max_refused_operations : int;
  operation_timeout : Time.System.Span.t;
  operations_batch_size : int;
  disable_precheck : bool;
}

let default_limits =
  {
    operation_timeout = Time.System.Span.of_seconds_exn 10.;
    max_refused_operations = 1000;
    operations_batch_size = 50;
    disable_precheck = false;
  }

(* Minimal delay between two mempool advertisements *)
let advertisement_delay = 0.1

type error +=
  | Manager_operation_replaced of {
      old_hash : Operation_hash.t;
      new_hash : Operation_hash.t;
    }

let () =
  register_error_kind
    `Permanent
    ~id:"prevalidator.manager_operation_replaced"
    ~title:"Manager operation replaced"
    ~description:"The manager operation has been replaced"
    ~pp:(fun ppf (old_hash, new_hash) ->
      Format.fprintf
        ppf
        "The manager operation %a has been replaced with %a"
        Operation_hash.pp
        old_hash
        Operation_hash.pp
        new_hash)
    (Data_encoding.obj2
       (Data_encoding.req "old_hash" Operation_hash.encoding)
       (Data_encoding.req "new_hash" Operation_hash.encoding))
    (function
      | Manager_operation_replaced {old_hash; new_hash} ->
          Some (old_hash, new_hash)
      | _ -> None)
    (fun (old_hash, new_hash) ->
      Manager_operation_replaced {old_hash; new_hash})

module Name = struct
  type t = Chain_id.t * Protocol_hash.t

  let encoding = Data_encoding.tup2 Chain_id.encoding Protocol_hash.encoding

  let base = ["prevalidator"]

  let pp fmt (chain_id, proto_hash) =
    Format.fprintf
      fmt
      "%a:%a"
      Chain_id.pp_short
      chain_id
      Protocol_hash.pp_short
      proto_hash

  let equal (c1, p1) (c2, p2) =
    Chain_id.equal c1 c2 && Protocol_hash.equal p1 p2
end

module Dummy_event = struct
  type t = unit

  let pp = Format.pp_print_cut

  let encoding = Data_encoding.unit

  let level () = Internal_event.Debug
end

module Logger =
  Worker_logger.Make (Dummy_event) (Request)
    (struct
      let worker_name = "node_prevalidator"
    end)

module Classification = Prevalidator_classification

(** This module encapsulates pending operations to maintain them in two
    different data structure and avoid coslty repetitive convertions when
    handling batches in [classify_pending_operations]. *)
module Pending_ops = Prevalidator_pending_operations

(** Module encapsulating some types that are used both in production
    and in tests. Having them in a module makes it possible to
    [include] this module in {!Internal_for_tests} below and avoid
    code duplication.

    The raison d'etre of these records of functions is to be able to use
    alternative implementations of all functions in tests.

    The purpose of the {!Tools.tools} record is to abstract away from {!Store.chain_store}.
    Under the hood [Store.chain_store] requires an Irmin store on disk,
    which makes it impractical for fast testing: every test would need
    to create a temporary folder on disk which doesn't scale well.

    The purpose of the {!Tools.worker_tools} record is to abstract away
    from the {!Worker} implementation. This implementation is overkill
    for testing: we don't need asynchronicity and concurrency in our
    pretty basic existing tests. Having this abstraction allows to get
    away with a much simpler state machine model of execution and
    to have simpler test setup. *)
module Tools = struct
  (** Functions provided by {!Distributed_db} and {!Store.chain_store}
      that are used in various places of the mempool. Gathered here so that we can test
      the mempool without requiring a full-fledged [Distributed_db]/[Store.Chain_store]. *)
  type 'prevalidation_t tools = {
    advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
        (** [advertise_current_head mempool head] sends a
            [Current_head (chain_id, head_header, mempool)] message to all known
            active peers for the chain being considered. *)
    chain_tools : Store.Block.t Classification.chain_tools;
        (** Lower-level tools provided by {!Prevalidator_classification} *)
    create :
      predecessor:Store.Block.t ->
      live_operations:Operation_hash.Set.t ->
      timestamp:Time.Protocol.t ->
      unit ->
      'prevalidation_t tzresult Lwt.t;
        (** Creates a new prevalidation context w.r.t. the protocol associated to the
            predecessor block. *)
    fetch :
      ?peer:P2p_peer.Id.t ->
      ?timeout:Time.System.Span.t ->
      Operation_hash.t ->
      Operation.t tzresult Lwt.t;
        (** [fetch ?peer ?timeout oph] returns the value when it is known.
            It can fail with [Requester.Timeout] if [timeout] is provided and the value
            isn't known before the timeout expires. It can fail with [Requester.Cancel] if
            the request is canceled. *)
    read_block : Block_hash.t -> Store.Block.t tzresult Lwt.t;
        (** [read_block bh] tries to read the block [bh] from the chain store. *)
    send_get_current_head : ?peer:P2p_peer_id.t -> unit -> unit;
        (** [send_get_current_head ?peer ()] sends a [Get_Current_head]
            to a given peer, or to all known active peers for the chain considered.
            Expected answer is a [Get_current_head] message *)
    set_mempool : head:Block_hash.t -> Mempool.t -> unit tzresult Lwt.t;
        (** [set_mempool ~head mempool] sets the [mempool] of
            the [chain_store] of the chain considered. Does nothing if [head] differs
            from current_head which might happen when a new head concurrently arrives just
            before this operation is being called. *)
  }

  (** Abstraction over services implemented in production by {!Worker}
      but implemented differently in tests.

      Also see the enclosing module documentation as to why we have this record. *)
  type worker_tools = {
    push_request : unit Prevalidator_worker_state.Request.t -> unit Lwt.t;
        (** Adds a message to the queue. *)
    push_request_now : unit Prevalidator_worker_state.Request.t -> unit;
        (** Adds a message to the queue immediately. *)
  }
end

type 'a parameters = {limits : limits; tools : 'a Tools.tools}

(** The type needed for the implementation of [Make] below, but
 *  which is independent from the protocol. *)
type ('protocol_data, 'a) types_state_shell = {
  classification : 'protocol_data Classification.t;
  parameters : 'a parameters;
  mutable predecessor : Store.Block.t;
  mutable timestamp : Time.System.t;
  mutable live_blocks : Block_hash.Set.t;
  mutable live_operations : Operation_hash.Set.t;
  mutable fetching : Operation_hash.Set.t;
  mutable pending : 'protocol_data Pending_ops.t;
  mutable mempool : Mempool.t;
  mutable advertisement : [`Pending of Mempool.t | `None];
  mutable banned_operations : Operation_hash.Set.t;
  worker : Tools.worker_tools;
}

(** The concrete production instance of {!block_tools} *)
let block_tools : Store.Block.t Classification.block_tools =
  {
    hash = Store.Block.hash;
    operations = Store.Block.operations;
    all_operation_hashes = Store.Block.all_operation_hashes;
  }

(** How to create an instance of {!chain_tools} from a {!Distributed_db.chain_db}. *)
let mk_chain_tools (chain_db : Distributed_db.chain_db) :
    Store.Block.t Classification.chain_tools =
  let new_blocks ~from_block ~to_block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Chain_traversal.new_blocks chain_store ~from_block ~to_block
  in
  let read_predecessor_opt block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Block.read_predecessor_opt chain_store block
  in
  let inject_operation oph op =
    Distributed_db.inject_operation chain_db oph op >|= ignore
  in
  {
    clear_or_cancel = Distributed_db.Operation.clear_or_cancel chain_db;
    inject_operation;
    new_blocks;
    read_predecessor_opt;
  }

(** Module type used both in production and in tests. *)
module type S = sig
  (** Type instantiated by {!Filter.Mempool.state}. *)
  type filter_state

  (** Type instantiated by {!Filter.Mempool.config}. *)
  type filter_config

  (** Similar to the type [operation] from the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type protocol_operation

  (** Type instantiated by {!Prevalidation.t} *)
  type prevalidation_t

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable filter_state : filter_state;
        (** Internal state of the filter in the plugin *)
    mutable validation_state : prevalidation_t tzresult;
    mutable operation_stream :
      (Classification.classification
      * protocol_operation Prevalidation.operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

  (** This function fetches an operation if it is not already handled
      as defined by [already_handled] below. The implementation makes
      sure to fetch an operation at most once, modulo operations
      lost because of bounded buffers becoming full.

      This function is an intruder to this module type. It just happens
      that it is needed both by internals of the implementation of {!S}
      and by the internals of the implementation of {!T}; so it needs
      to be exposed here. *)
  val may_fetch_operation :
    (protocol_operation, prevalidation_t) types_state_shell ->
    P2p_peer_id.t option ->
    Operation_hash.t ->
    unit Lwt.t

  (** The function called after every call to a function of {!API}. *)
  val handle_unprocessed : types_state -> unit Lwt.t

  (** The inner API of the mempool i.e. functions called by the worker
      when an individual request arrives. These functions are the
      most high-level ones that we test. All these [on_*] functions
      correspond to a single event. Possible
      sequences of calls to this API are always of the form:

      on_*; handle_unprocessed; on_*; handle_unprocessed; ... *)
  module Requests : sig
    val on_advertise : _ types_state_shell -> unit

    val on_arrived :
      types_state -> Operation_hash.t -> Operation.t -> unit tzresult Lwt.t

    val on_ban : types_state -> Operation_hash.t -> unit tzresult Lwt.t

    val on_flush :
      handle_branch_refused:bool ->
      types_state ->
      Store.Block.t ->
      Block_hash.Set.t ->
      Operation_hash.Set.t ->
      unit tzresult Lwt.t

    val on_inject :
      types_state -> force:bool -> Operation.t -> unit tzresult Lwt.t

    val on_notify :
      _ types_state_shell -> P2p_peer_id.t -> Mempool.t -> unit Lwt.t
  end
end

(** Module type used exclusively in production. *)
module type T = sig
  include S

  val name : Name.t

  module Types : Worker_intf.TYPES with type state = types_state

  module Worker :
    Worker.T
      with type Event.t = Dummy_event.t
       and type 'a Request.t = 'a Request.t
       and type Request.view = Request.view
       and type Types.state = types_state

  type worker = Worker.infinite Worker.queue Worker.t

  val initialization_errors : unit tzresult Lwt.t

  val worker : worker Lazy.t
end

type t = (module T)

(** A functor for obtaining the testable part of this file (see
    the instantiation of this functor in {!Internal_for_tests} at the
    end of this file). Contrary to the production-only functor {!Make} below,
    this functor doesn't assume a specific chain store implementation,
    which is the crux for having it easily unit-testable. *)
module Make_s
    (Filter : Prevalidator_filters.FILTER)
    (Prevalidation_t : Prevalidation.T
                         with type validation_state =
                               Filter.Proto.validation_state
                          and type protocol_operation = Filter.Proto.operation
                          and type operation_receipt =
                               Filter.Proto.operation_receipt) :
  S
    with type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type protocol_operation = Filter.Proto.operation
     and type prevalidation_t = Prevalidation_t.t = struct
  type filter_state = Filter.Mempool.state

  type filter_config = Filter.Mempool.config

  type protocol_operation = Filter.Proto.operation

  type prevalidation_t = Prevalidation_t.t

  type 'operation_data operation = 'operation_data Prevalidation.operation

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable filter_state : filter_state;
    mutable validation_state : prevalidation_t tzresult;
    mutable operation_stream :
      (Classification.classification
      * protocol_operation Prevalidation.operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

  (* This function is in [Lwt] only for logging. *)
  let already_handled ~origin shell oph =
    if Operation_hash.Set.mem oph shell.banned_operations then
      Event.(emit ban_operation_encountered) (origin, oph) >|= fun () -> true
    else
      Lwt.return
        (Pending_ops.mem oph shell.pending
        || Operation_hash.Set.mem oph shell.fetching
        || Operation_hash.Set.mem oph shell.live_operations
        || Classification.is_in_mempool oph shell.classification <> None
        || Classification.is_known_unparsable oph shell.classification)

  let advertise (shell : ('operation_data, _) types_state_shell) mempool =
    match shell.advertisement with
    | `Pending {Mempool.known_valid; pending} ->
        shell.advertisement <-
          `Pending
            {
              known_valid = known_valid @ mempool.Mempool.known_valid;
              pending = Operation_hash.Set.union pending mempool.pending;
            }
    | `None ->
        shell.advertisement <- `Pending mempool ;
        Lwt.dont_wait
          (fun () ->
            Lwt_unix.sleep advertisement_delay >>= fun () ->
            shell.worker.push_request_now Advertise ;
            Lwt.return_unit)
          (fun exc ->
            Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))

  (* Each classified operation should be notified exactly ONCE for a
     given stream. Operations which cannot be parsed are not notified. *)
  let handle_classification
      ~(notifier :
         Classification.classification ->
         protocol_operation Prevalidation.operation ->
         unit) shell (op, kind) =
    Classification.add kind op shell.classification ;
    notifier kind op

  let mk_notifier operation_stream classification op =
    (* This callback is safe encapsulation-wise, because it depends
       on an "harmless" field of [types_state_shell]: [operation_stream] *)
    Lwt_watcher.notify operation_stream (classification, op)

  let pre_filter shell ~filter_config ~filter_state ~validation_state ~notifier
      (parsed_op : protocol_operation operation) : [`High | `Low | `Drop] Lwt.t
      =
    let validation_state_before =
      Option.map
        Prevalidation_t.validation_state
        (Option.of_result validation_state)
    in
    Filter.Mempool.pre_filter
      ~filter_state
      ?validation_state_before
      filter_config
      parsed_op.protocol
    >|= function
    | (`Branch_delayed _ | `Branch_refused _ | `Refused _ | `Outdated _) as errs
      ->
        handle_classification ~notifier shell (parsed_op, errs) ;
        `Drop
    | `Passed_prefilter priority -> (priority :> [`High | `Low | `Drop])

  let post_filter ~filter_config ~filter_state ~validation_state_before
      ~validation_state_after op receipt =
    Filter.Mempool.post_filter
      filter_config
      ~filter_state
      ~validation_state_before
      ~validation_state_after
      (op, receipt)

  let set_mempool shell mempool =
    shell.mempool <- mempool ;
    shell.parameters.tools.set_mempool
      ~head:(Store.Block.hash shell.predecessor)
      shell.mempool

  let remove_from_advertisement oph = function
    | `Pending mempool -> `Pending (Mempool.remove oph mempool)
    | `None -> `None

  (* This function retrieves an old/replaced operation and reclassifies it as
     [`Outdated]. Note that we don't need to re-flush the mempool, as this
     function is only called in precheck mode.

     The operation is expected to be (a) parsable and (b) in the "prechecked"
     class. So, we softly handle the situations where the operation is
     unparsable or not found in any class in case this invariant is broken
     for some reason.
  *)
  let reclassify_replaced_manager_op old_hash new_hash shell =
    shell.advertisement <-
      remove_from_advertisement old_hash shell.advertisement ;
    match Classification.remove old_hash shell.classification with
    | Some (op, _class) ->
        let err = Manager_operation_replaced {old_hash; new_hash} in
        [(op, `Outdated [err])]
    | None ->
        (* This case should not happen. *)
        shell.parameters.tools.chain_tools.clear_or_cancel old_hash ;
        []

  let precheck ~disable_precheck ~filter_config ~filter_state ~validation_state
      (op : protocol_operation operation) =
    let validation_state = Prevalidation_t.validation_state validation_state in
    if disable_precheck then Lwt.return `Undecided
    else
      Filter.Mempool.precheck
        filter_config
        ~filter_state
        ~validation_state
        op.hash
        op.protocol
      >|= function
      | `Passed_precheck filter_state ->
          (* The [precheck] optimization triggers: no need to call the
              protocol [apply_operation]. *)
          `Passed_precheck filter_state
      | `Passed_precheck_with_replace (old_oph, filter_state) ->
          (* Same as `Passed_precheck, but the operation whose hash is returned
             should be reclassified to Outdated *)
          `Passed_precheck_with_replace (old_oph, filter_state)
      | (`Branch_delayed _ | `Branch_refused _ | `Refused _ | `Outdated _) as
        errs ->
          (* Note that we don't need to distinguish some failure cases
             of [Filter.Mempool.precheck], hence grouping them under `Fail. *)
          `Fail errs
      | `Undecided ->
          (* The caller will need to call the protocol's [apply_operation]
             function. *)
          `Undecided

  (* [classify_operation shell filter_config filter_state validation_state
      mempool op oph] allows to determine the class of a given operation.

     Once it's parsed, the operation is prechecked and/or applied in the current
     filter/validation state to determine if it could be included in a block on
     top of the current head or not. If yes, the operation is accumulated in
     the given [mempool].

     The function returns a tuple
     [(filter_state, validation_state, mempool, to_handle)], where:
     - [filter_state] is the (possibly) updated filter_state,
     - [validation_state] is the (possibly) updated validation_state,
     - [mempool] is the (possibly) updated mempool,
     - [to_handle] contains the given operation and its classification, and all
       operations whose classes are changed/impacted by this classification
       (eg. in case of operation replacement).
  *)
  let classify_operation shell ~filter_config ~filter_state ~validation_state
      mempool op :
      (filter_state
      * prevalidation_t
      * Mempool.t
      * (protocol_operation operation * Classification.classification) trace)
      Lwt.t =
    (precheck
       ~disable_precheck:shell.parameters.limits.disable_precheck
       ~filter_config
       ~filter_state
       ~validation_state
       op
     >>= function
     | `Fail errs ->
         (* Precheck rejected the operation *)
         Lwt.return_error errs
     | `Passed_precheck filter_state ->
         (* Precheck succeeded *)
         Lwt.return_ok ((filter_state, validation_state), [], `Prechecked)
     | `Passed_precheck_with_replace (old_oph, filter_state) ->
         (* Precheck succeeded, but an old operation is replaced *)
         let to_handle = reclassify_replaced_manager_op old_oph op.hash shell in
         Lwt.return_ok ((filter_state, validation_state), to_handle, `Prechecked)
     | `Undecided -> (
         (* Precheck was not able to classify *)
         Prevalidation_t.apply_operation validation_state op
         >>= function
         | Applied (new_validation_state, receipt) -> (
             (* Apply succeeded, call post_filter *)
             post_filter
               ~filter_config
               ~filter_state
               ~validation_state_before:
                 (Prevalidation_t.validation_state validation_state)
               ~validation_state_after:
                 (Prevalidation_t.validation_state new_validation_state)
               op.protocol
               receipt
             >>= function
             | `Passed_postfilter new_filter_state ->
                 (* Post_filter ok, accept operation *)
                 Lwt.return_ok
                   ((new_filter_state, new_validation_state), [], `Applied)
             | `Refused _ as op_class ->
                 (* Post_filter refused the operation *)
                 Lwt.return_error op_class)
         (* Apply rejected the operation *)
         | Branch_delayed e -> Lwt.return_error (`Branch_delayed e)
         | Branch_refused e -> Lwt.return_error (`Branch_refused e)
         | Refused e -> Lwt.return_error (`Refused e)
         | Outdated e -> Lwt.return_error (`Outdated e)))
    >>= function
    | Error op_class ->
        Lwt.return (filter_state, validation_state, mempool, [(op, op_class)])
    | Ok ((f_state, v_state), to_handle, op_class) ->
        let mempool = Mempool.cons_valid op.hash mempool in
        let to_handle = (op, op_class) :: to_handle in
        Lwt.return (f_state, v_state, mempool, to_handle)

  (* Classify pending operations into either: [Refused |
     Branch_delayed | Branch_refused | Applied | Outdated].
     To ensure fairness with other worker requests, classification of
     operations is done by batch of [operation_batch_size] operations.

     This function ensures the following invariants:

     - If an operation is classified, it is not part of the [pending]
     map

     - A classified operation is part of the [in_mempool] set

     - A classified operation is part only of one of the following
     classes: [Branch_refused, Branch_delayed, Refused, Applied]

     Moreover, this function ensures that only each newly classified
     operations are advertised to the remote peers. However, if a peer
     requests our mempool, we advertise all our classified operations and
     all our pending operations. *)
  let classify_pending_operations ~notifier shell filter_config filter_state
      state =
    Pending_ops.fold_es
      (fun _prio
           oph
           op
           (acc_filter_state, acc_validation_state, acc_mempool, limit) ->
        if limit <= 0 then
          (* Using Error as an early-return mechanism *)
          Lwt.return_error (acc_filter_state, acc_validation_state, acc_mempool)
        else (
          shell.pending <- Pending_ops.remove oph shell.pending ;
          classify_operation
            shell
            ~filter_config
            ~filter_state:acc_filter_state
            ~validation_state:acc_validation_state
            acc_mempool
            op
          >|= fun ( new_filter_state,
                    new_validation_state,
                    new_mempool,
                    to_handle ) ->
          List.iter (handle_classification ~notifier shell) to_handle ;
          ok (new_filter_state, new_validation_state, new_mempool, limit - 1)))
      shell.pending
      ( filter_state,
        state,
        Mempool.empty,
        shell.parameters.limits.operations_batch_size )
    >>= function
    | Error (filter_state, state, advertised_mempool) ->
        (* Early return after iteration limit was reached *)
        shell.worker.push_request Request.Leftover >>= fun () ->
        Lwt.return (filter_state, state, advertised_mempool)
    | Ok (filter_state, state, advertised_mempool, _) ->
        Lwt.return (filter_state, state, advertised_mempool)

  let update_advertised_mempool_fields pv_shell delta_mempool =
    if Mempool.is_empty delta_mempool then Lwt.return_unit
    else
      (* We only advertise newly classified operations. *)
      let mempool_to_advertise =
        Mempool.
          {delta_mempool with known_valid = List.rev delta_mempool.known_valid}
      in
      advertise pv_shell mempool_to_advertise ;
      let our_mempool =
        {
          (* Using List.rev_map is ok since the size of pv.shell.classification.applied
             cannot be too big. *)
          (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2065
             This field does not only contain valid operation *)
          Mempool.known_valid =
            List.rev_map
              (fun op -> op.Prevalidation.hash)
              pv_shell.classification.applied_rev
            @ (Operation_hash.Map.to_seq pv_shell.classification.prechecked
              |> Seq.map fst |> List.of_seq);
          pending = Pending_ops.hashes pv_shell.pending;
        }
      in
      set_mempool pv_shell our_mempool >>= fun _res -> Lwt.pause ()

  let handle_unprocessed pv =
    let notifier = mk_notifier pv.operation_stream in
    match pv.validation_state with
    | Error err ->
        (* At the time this comment was written (26/05/21), this is dead
           code since [Proto.begin_construction] cannot fail. *)
        Pending_ops.iter
          (fun _prio _oph op ->
            handle_classification ~notifier pv.shell (op, `Branch_delayed err))
          pv.shell.pending ;
        pv.shell.pending <- Pending_ops.empty ;
        Lwt.return_unit
    | Ok state ->
        if Pending_ops.is_empty pv.shell.pending then Lwt.return_unit
        else
          Event.(emit processing_operations) () >>= fun () ->
          classify_pending_operations
            ~notifier
            pv.shell
            pv.filter_config
            pv.filter_state
            state
          >>= fun (filter_state, validation_state, delta_mempool) ->
          pv.filter_state <- filter_state ;
          pv.validation_state <- Ok validation_state ;
          update_advertised_mempool_fields pv.shell delta_mempool

  (* This function fetches one operation through the
     [distributed_db]. On errors, we emit an event and proceed as
     usual. *)
  let fetch_operation (shell : ('operation_data, _) types_state_shell) ?peer oph
      =
    Event.(emit fetching_operation) oph >|= fun () ->
    shell.parameters.tools.fetch
      ~timeout:shell.parameters.limits.operation_timeout
      ?peer
      oph
    >>= function
    | Ok op ->
        shell.worker.push_request_now (Arrived (oph, op)) ;
        Lwt.return_unit
    | Error (Distributed_db.Operation.Canceled _ :: _) ->
        Event.(emit operation_included) oph
    | Error _ ->
        (* This may happen if the peer timed out for example. *)
        Event.(emit operation_not_fetched) oph

  (* This function fetches an operation if it is not already handled
     by the mempool. To ensure we fetch at most a given operation,
     we record it in the [pv.fetching] field.

     Invariant: This function should be the only one to modify this
     field.

     Invariant: To ensure, there is no leak, we ensure that when the
     promise [p] is terminated, we remove the operation from the
     fetching operations. This is to ensure that if an error
     happened, we can still fetch this operation in the future. *)
  let may_fetch_operation (shell : ('operation_data, _) types_state_shell) peer
      oph =
    let origin =
      match peer with
      | Some peer -> Format.asprintf "notified by %a" P2p_peer.Id.pp peer
      | None -> "leftover from previous run"
    in
    already_handled ~origin shell oph >>= fun already_handled ->
    if not already_handled then
      ignore
        (Lwt.finalize
           (fun () ->
             shell.fetching <- Operation_hash.Set.add oph shell.fetching ;
             fetch_operation shell ?peer oph)
           (fun () ->
             shell.fetching <- Operation_hash.Set.remove oph shell.fetching ;
             Lwt.return_unit)) ;
    Lwt.return_unit

  (** Module containing functions that are the internal transitions
      of the mempool. These functions are called by the {!Worker} when
      an event arrives. *)
  module Requests = struct
    let on_arrived (pv : types_state) oph op =
      already_handled ~origin:"arrived" pv.shell oph >>= fun already_handled ->
      if already_handled then return_unit
      else
        match Prevalidation_t.parse oph op with
        | Error _ ->
            Event.(emit unparsable_operation) oph >|= fun () ->
            Prevalidator_classification.add_unparsable
              oph
              pv.shell.classification ;
            ok ()
        | Ok parsed_op -> (
            pre_filter
              pv.shell
              ~filter_config:pv.filter_config
              ~filter_state:pv.filter_state
              ~validation_state:pv.validation_state
              ~notifier:(mk_notifier pv.operation_stream)
              parsed_op
            >>= function
            | `Drop -> return_unit
            | (`High | `Low) as prio ->
                if
                  not
                    (Block_hash.Set.mem
                       op.Operation.shell.branch
                       pv.shell.live_blocks)
                then (
                  pv.shell.parameters.tools.chain_tools.clear_or_cancel oph ;
                  return_unit)
                else (
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/1723
                     Should this have an influence on the peer's score ? *)
                  pv.shell.pending <-
                    Pending_ops.add parsed_op prio pv.shell.pending ;
                  return_unit))

    let on_inject (pv : types_state) ~force op =
      let oph = Operation.hash op in
      (* Currently, an injection is always done with priority = `High, because:
         - We want to process and propagate the injected operations fast,
         - We don't want to call prefilter to get the priority.
         But, this may change in the future
      *)
      let prio = `High in
      already_handled ~origin:"injected" pv.shell oph >>= fun already_handled ->
      if already_handled then
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1722
           Is this an error? *)
        return_unit
      else
        match Prevalidation_t.parse oph op with
        | Error err ->
            failwith
              "Invalid operation %a: %a."
              Operation_hash.pp
              oph
              Error_monad.pp_print_trace
              err
        | Ok parsed_op -> (
            if force then (
              pv.shell.parameters.tools.chain_tools.inject_operation oph op
              >>= fun () ->
              pv.shell.pending <-
                Pending_ops.add parsed_op prio pv.shell.pending ;
              return_unit)
            else if
              not
                (Block_hash.Set.mem
                   op.Operation.shell.branch
                   pv.shell.live_blocks)
            then
              failwith
                "Operation %a is branched on a block %a which is too old"
                Operation_hash.pp
                oph
                Block_hash.pp
                op.Operation.shell.branch
            else
              pv.validation_state >>?= fun validation_state ->
              let notifier = mk_notifier pv.operation_stream in
              classify_operation
                pv.shell
                ~filter_config:pv.filter_config
                ~filter_state:pv.filter_state
                ~validation_state
                Mempool.empty
                parsed_op
              >>= fun (filter_state, validation_state, delta_mempool, to_handle)
                ->
              let op_status =
                (* to_handle contains the given operation and its classification, and
                   all operations whose classes are changed/impacted by this
                   classification (eg. in case of operation replacement). Here, we
                   retrieve the classification of our operation. *)
                List.find_opt
                  (function
                    | (({hash; _} : protocol_operation operation), _) ->
                        Operation_hash.equal hash oph)
                  to_handle
              in
              match op_status with
              | Some (_h, (`Applied | `Prechecked)) ->
                  (* TODO: https://gitlab.com/tezos/tezos/-/issues/2294
                     In case of `Passed_precheck_with_replace, we may want to only do
                     the injection/replacement if a flag `replace` is set to true
                     in the injection query. *)
                  pv.shell.parameters.tools.chain_tools.inject_operation oph op
                  >>= fun () ->
                  (* Call handle & update_advertised_mempool only if op is accepted *)
                  List.iter (handle_classification ~notifier pv.shell) to_handle ;
                  pv.filter_state <- filter_state ;
                  pv.validation_state <- Ok validation_state ;
                  (* Note that in this case, we may advertise an operation and bypass
                     the prioritirization strategy. *)
                  update_advertised_mempool_fields pv.shell delta_mempool
                  >>= return
              | Some
                  ( _h,
                    ( `Branch_delayed e
                    | `Branch_refused e
                    | `Refused e
                    | `Outdated e ) ) ->
                  Lwt.return
                  @@ error_with
                       "Error while applying operation %a:@ %a"
                       Operation_hash.pp
                       oph
                       pp_print_trace
                       e
              | None ->
                  (* This case should not happen *)
                  failwith
                    "Unexpected error while injecting operation %a. Operation \
                     not found after classifying it."
                    Operation_hash.pp
                    oph)

    let on_notify (shell : ('operation_data, _) types_state_shell) peer mempool
        =
      let may_fetch_operation = may_fetch_operation shell (Some peer) in
      List.iter_s may_fetch_operation mempool.Mempool.known_valid >>= fun () ->
      Seq.iter_s
        may_fetch_operation
        (Operation_hash.Set.to_seq mempool.Mempool.pending)

    let on_flush ~handle_branch_refused pv new_predecessor new_live_blocks
        new_live_operations =
      let old_predecessor = pv.shell.predecessor in
      pv.shell.predecessor <- new_predecessor ;
      pv.shell.live_blocks <- new_live_blocks ;
      pv.shell.live_operations <- new_live_operations ;
      Lwt_watcher.shutdown_input pv.operation_stream ;
      pv.operation_stream <- Lwt_watcher.create_input () ;
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      pv.shell.timestamp <- timestamp_system ;
      let timestamp = Time.System.to_protocol timestamp_system in
      pv.shell.parameters.tools.create
        ~predecessor:new_predecessor
        ~live_operations:new_live_operations
        ~timestamp
        ()
      >>= fun validation_state ->
      pv.validation_state <- validation_state ;
      Filter.Mempool.on_flush
        pv.filter_config
        pv.filter_state
        ?validation_state:
          (Option.map
             Prevalidation_t.validation_state
             (Option.of_result validation_state))
        ~predecessor:(Store.Block.header new_predecessor)
        ()
      >>=? fun filter_state ->
      pv.filter_state <- filter_state ;
      Classification.recycle_operations
        ~from_branch:old_predecessor
        ~to_branch:new_predecessor
        ~live_blocks:new_live_blocks
        ~parse:(fun oph op -> Result.to_option (Prevalidation_t.parse oph op))
        ~classes:pv.shell.classification
        ~pending:(Pending_ops.operations pv.shell.pending)
        ~block_store:block_tools
        ~chain:pv.shell.parameters.tools.chain_tools
        ~handle_branch_refused
      >>= fun new_pending_operations ->
      (* Could be implemented as Operation_hash.Map.filter_s which
         does not exist for the moment. *)
      Operation_hash.Map.fold_s
        (fun _oph op (pending, nb_pending) ->
          pre_filter
            pv.shell
            ~filter_config:pv.filter_config
            ~filter_state:pv.filter_state
            ~validation_state:pv.validation_state
            ~notifier:(mk_notifier pv.operation_stream)
            op
          >|= function
          | `Drop -> (pending, nb_pending)
          | (`High | `Low) as prio ->
              (* Here, an operation injected in this node with `High priority will
                 now get its approriate priority. *)
              (Pending_ops.add op prio pending, nb_pending + 1))
        new_pending_operations
        (Pending_ops.empty, 0)
      >>= fun (new_pending_operations, nb_pending) ->
      Event.(emit operations_to_reclassify) nb_pending >>= fun () ->
      pv.shell.pending <- new_pending_operations ;
      set_mempool pv.shell Mempool.empty

    let on_advertise (shell : ('protocol_data, _) types_state_shell) =
      match shell.advertisement with
      | `None ->
          () (* May happen if nothing to advertise since last advertisement. *)
      | `Pending mempool ->
          shell.advertisement <- `None ;
          (* In this case, mempool is not empty, but let's avoid advertising
             empty mempools in case this invariant is broken. *)
          if not (Mempool.is_empty mempool) then
            shell.parameters.tools.advertise_current_head
              ~mempool
              shell.predecessor

    (* If [flush_if_prechecked] is [true], removing a prechecked
       operation triggers a flush of the mempool. Because flushing may
       be costly this should be done only when the action is triggered
       locally by the user. This allows a better UX if the user bans a
       prechecked operation so that a branch delayed operation becomes
       [applied] again. *)
    let remove ~flush_if_prechecked pv oph =
      pv.shell.parameters.tools.chain_tools.clear_or_cancel oph ;
      pv.shell.advertisement <-
        remove_from_advertisement oph pv.shell.advertisement ;
      pv.shell.banned_operations <-
        Operation_hash.Set.add oph pv.shell.banned_operations ;
      match Classification.remove oph pv.shell.classification with
      | None ->
          pv.shell.pending <- Pending_ops.remove oph pv.shell.pending ;
          pv.shell.fetching <- Operation_hash.Set.remove oph pv.shell.fetching ;
          return_unit
      | Some (_op, classification) -> (
          match (classification, flush_if_prechecked) with
          | (`Prechecked, true) | (`Applied, _) ->
              (* Modifying the list of operations classified as [Applied]
                 might change the classification of all the operations in
                 the mempool. Hence if the removed operation has been
                 applied we flush the mempool to force the
                 reclassification of all the operations except the one
                 removed. *)
              on_flush
                ~handle_branch_refused:false
                pv
                pv.shell.predecessor
                pv.shell.live_blocks
                pv.shell.live_operations
              >|=? fun () ->
              pv.shell.pending <- Pending_ops.remove oph pv.shell.pending
          | (`Branch_delayed _, _)
          | (`Branch_refused _, _)
          | (`Refused _, _)
          | (`Outdated _, _)
          | (`Prechecked, false) ->
              pv.filter_state <-
                Filter.Mempool.remove ~filter_state:pv.filter_state oph ;
              return_unit)

    let on_ban pv oph_to_ban =
      pv.shell.banned_operations <-
        Operation_hash.Set.add oph_to_ban pv.shell.banned_operations ;
      remove ~flush_if_prechecked:true pv oph_to_ban
  end
end

module type ARG = sig
  val limits : limits

  val chain_db : Distributed_db.chain_db

  val chain_id : Chain_id.t
end

(** The functor that is not tested, in other words used only in production.
    This functor's code is not tested (contrary to functor {!Make_s} above),
    because it hardcodes a dependency to [Store.chain_store] in its instantiation
    of type [chain_store]. This is what makes the code of this functor
    not testable for the moment, because [Store.chain_store] has poor
    testing capabilities.

    Note that, because this functor [include]s {!Make_s}, it is a
    strict extension of [Make_s]. *)
module Make
    (Filter : Prevalidator_filters.FILTER)
    (Arg : ARG)
    (Prevalidation_t : Prevalidation.T
                         with type validation_state =
                               Filter.Proto.validation_state
                          and type protocol_operation = Filter.Proto.operation
                          and type operation_receipt =
                               Filter.Proto.operation_receipt
                          and type chain_store = Store.chain_store) :
  T with type prevalidation_t = Prevalidation_t.t = struct
  include Make_s (Filter) (Prevalidation_t)

  let name = (Arg.chain_id, Filter.Proto.hash)

  module Types = struct
    type state = types_state

    type parameters = limits * Distributed_db.chain_db
  end

  module Worker :
    Worker.T
      with type Name.t = Name.t
       and type Event.t = Dummy_event.t
       and type 'a Request.t = 'a Request.t
       and type Request.view = Request.view
       and type Types.state = Types.state
       and type Types.parameters = Types.parameters =
    Worker.Make (Name) (Dummy_event) (Prevalidator_worker_state.Request) (Types)
      (Logger)

  open Types

  type worker = Worker.infinite Worker.queue Worker.t

  (** Mimics [Data_encoding.Json.construct] but accepts argument
      [?include_default_fields] to pass on to [Json_encoding.construct]. *)
  let data_encoding_json_construct ?include_default_fields e v =
    Json_encoding.construct
      ?include_default_fields
      (Data_encoding.Json.convert e)
      v
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1876
     Remove this function upon the next release of [data-encoding]. *)

  (** Returns a json describing the prevalidator's [filter_config].
      The boolean [include_default] ([true] by default) indicates
      whether the json should include the fields which have a value
      equal to their default value. *)
  let get_filter_config_json ?(include_default = true) pv =
    let include_default_fields = if include_default then `Always else `Never in
    data_encoding_json_construct
      ~include_default_fields
      Filter.Mempool.config_encoding
      pv.filter_config

  let build_rpc_directory w =
    lazy
      (let dir : state RPC_directory.t ref = ref RPC_directory.empty in
       let module Proto_services =
         Block_services.Make (Filter.Proto) (Filter.Proto)
       in
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.get_filter RPC_path.open_root)
           (fun pv params () ->
             return
               (get_filter_config_json
                  ~include_default:params#include_default
                  pv)) ;
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.set_filter RPC_path.open_root)
           (fun pv () obj ->
             (try
                let config =
                  Data_encoding.Json.destruct Filter.Mempool.config_encoding obj
                in
                pv.filter_config <- config ;
                Lwt.return_unit
              with _ -> Event.(emit invalid_mempool_filter_configuration) ())
             >>= fun () -> return (get_filter_config_json pv)) ;
       (* Ban an operation (from its given hash): remove it from the
          mempool if present. Add it to the set pv.banned_operations
          to prevent it from being fetched/processed/injected in the
          future.
          Note: If the baker has already received the operation, then
          it's necessary to restart it manually to flush the operation
          from it. *)
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.ban_operation RPC_path.open_root)
           (fun _pv () oph ->
             Worker.Queue.push_request_and_wait w (Request.Ban oph)) ;
       (* Unban an operation (from its given hash): remove it from the
          set pv.banned_operations (nothing happens if it was not banned). *)
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.unban_operation RPC_path.open_root)
           (fun pv () oph ->
             pv.shell.banned_operations <-
               Operation_hash.Set.remove oph pv.shell.banned_operations ;
             return_unit) ;
       (* Unban all operations: clear the set pv.banned_operations. *)
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.unban_all_operations RPC_path.open_root)
           (fun pv () () ->
             pv.shell.banned_operations <- Operation_hash.Set.empty ;
             return_unit) ;
       dir :=
         RPC_directory.gen_register
           !dir
           (Proto_services.S.Mempool.pending_operations RPC_path.open_root)
           (fun pv params () ->
             let map_op_error oph (op, error) acc =
               op.Prevalidation.protocol |> fun res ->
               Operation_hash.Map.add oph (res, error) acc
             in
             let applied =
               List.rev_map
                 (fun op -> (op.Prevalidation.hash, op.Prevalidation.protocol))
                 pv.shell.classification.applied_rev
             in
             let filter f map =
               Operation_hash.Map.fold f map Operation_hash.Map.empty
             in
             let refused =
               filter
                 map_op_error
                 (Classification.map pv.shell.classification.refused)
             in
             let outdated =
               filter
                 map_op_error
                 (Classification.map pv.shell.classification.outdated)
             in
             let branch_refused =
               filter
                 map_op_error
                 (Classification.map pv.shell.classification.branch_refused)
             in
             let branch_delayed =
               filter
                 map_op_error
                 (Classification.map pv.shell.classification.branch_delayed)
             in
             let unprocessed =
               Pending_ops.fold
                 (fun _prio oph op acc ->
                   Operation_hash.Map.add oph op.protocol acc)
                 pv.shell.pending
                 Operation_hash.Map.empty
             in
             (* FIXME https://gitlab.com/tezos/tezos/-/issues/2250

                We merge prechecked operation with applied operation
                so that the encoding of the RPC does not need to be
                changed. Once prechecking will be done by the protocol
                and not the plugin, we will change the encoding to
                reflect that. *)
             let prechecked_with_applied =
               (Operation_hash.Map.bindings pv.shell.classification.prechecked
               |> List.rev_map (fun (oph, op) ->
                      (oph, op.Prevalidation.protocol)))
               @ applied
             in
             let pending_operations =
               {
                 Proto_services.Mempool.applied = prechecked_with_applied;
                 refused;
                 outdated;
                 branch_refused;
                 branch_delayed;
                 unprocessed;
               }
             in
             Proto_services.Mempool.pending_operations_version_dispatcher
               ~version:params#version
               pending_operations) ;
       dir :=
         RPC_directory.register
           !dir
           (Proto_services.S.Mempool.request_operations RPC_path.open_root)
           (fun pv t () ->
             pv.shell.parameters.tools.send_get_current_head ?peer:t#peer_id () ;
             return_unit) ;
       dir :=
         RPC_directory.gen_register
           !dir
           (Proto_services.S.Mempool.monitor_operations RPC_path.open_root)
           (fun pv params () ->
             Lwt_mutex.with_lock pv.lock @@ fun () ->
             let (op_stream, stopper) =
               Lwt_watcher.create_stream pv.operation_stream
             in
             (* Convert ops *)
             let fold_op hash (Prevalidation.{protocol; _}, error) acc =
               (hash, protocol, error) :: acc
             in
             (* First call : retrieve the current set of op from the mempool *)
             let applied =
               if params#applied then
                 List.map
                   (fun op -> (op.Prevalidation.hash, op.protocol, []))
                   pv.shell.classification.applied_rev
               else []
             in
             (* FIXME https://gitlab.com/tezos/tezos/-/issues/2250

                For the moment, applied and prechecked operations are
                handled the same way for the user point of view. *)
             let prechecked =
               if params#applied then
                 Operation_hash.Map.fold
                   (fun hash op acc ->
                     (hash, op.Prevalidation.protocol, []) :: acc)
                   pv.shell.classification.prechecked
                   []
               else []
             in
             let refused =
               if params#refused then
                 Operation_hash.Map.fold
                   fold_op
                   (Classification.map pv.shell.classification.refused)
                   []
               else []
             in
             let branch_refused =
               if params#branch_refused then
                 Operation_hash.Map.fold
                   fold_op
                   (Classification.map pv.shell.classification.branch_refused)
                   []
               else []
             in
             let branch_delayed =
               if params#branch_delayed then
                 Operation_hash.Map.fold
                   fold_op
                   (Classification.map pv.shell.classification.branch_delayed)
                   []
               else []
             in
             let current_mempool =
               List.concat
                 [applied; prechecked; refused; branch_refused; branch_delayed]
               |> List.map (function
                      | (hash, op, []) -> ((hash, op), None)
                      | (hash, op, errors) -> ((hash, op), Some errors))
             in
             let current_mempool = ref (Some current_mempool) in
             let filter_result = function
               | `Prechecked | `Applied -> params#applied
               | `Refused _ -> params#refused
               | `Outdated _ -> params#outdated
               | `Branch_refused _ -> params#branch_refused
               | `Branch_delayed _ -> params#branch_delayed
             in
             let rec next () =
               match !current_mempool with
               | Some mempool ->
                   current_mempool := None ;
                   Lwt.return_some mempool
               | None -> (
                   Lwt_stream.get op_stream >>= function
                   | Some (kind, op) when filter_result kind ->
                       let errors =
                         match kind with
                         | `Prechecked | `Applied -> None
                         | `Branch_delayed errors
                         | `Branch_refused errors
                         | `Refused errors
                         | `Outdated errors ->
                             Some errors
                       in
                       Lwt.return_some
                         [(Prevalidation.(op.hash, op.protocol), errors)]
                   | Some _ -> next ()
                   | None -> Lwt.return_none)
             in
             let shutdown () = Lwt_watcher.shutdown stopper in
             RPC_answer.return_stream {next; shutdown}) ;
       !dir)

  (** Module implementing the events at the {!Worker} level. Contrary
      to {!Requests}, these functions depend on [Worker]. *)
  module Handlers = struct
    type self = worker

    let on_request : type r. worker -> r Request.t -> r tzresult Lwt.t =
     fun w request ->
      let pv = Worker.state w in
      (match request with
      | Request.Flush (hash, event, live_blocks, live_operations) ->
          Requests.on_advertise pv.shell ;
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/1727
             Rebase the advertisement instead. *)
          pv.shell.parameters.tools.read_block hash
          >>=? fun block : r tzresult Lwt.t ->
          let handle_branch_refused =
            Chain_validator_worker_state.Event.(
              match event with
              | Head_increment | Ignored_head -> false
              | Branch_switch -> true)
          in
          Lwt_mutex.with_lock pv.lock @@ fun () ->
          Requests.on_flush
            ~handle_branch_refused
            pv
            block
            live_blocks
            live_operations
      | Request.Notify (peer, mempool) ->
          Requests.on_notify pv.shell peer mempool >>= fun () -> return_unit
      | Request.Leftover ->
          (* unprocessed ops are handled just below *)
          return_unit
      | Request.Inject {op; force} -> Requests.on_inject pv ~force op
      | Request.Arrived (oph, op) -> Requests.on_arrived pv oph op
      | Request.Advertise ->
          Requests.on_advertise pv.shell ;
          return_unit
      | Request.Ban oph -> Requests.on_ban pv oph)
      >>=? fun r ->
      handle_unprocessed pv >>= fun () -> return r

    let on_close w =
      let pv = Worker.state w in
      Operation_hash.Set.iter
        pv.shell.parameters.tools.chain_tools.clear_or_cancel
        pv.shell.fetching ;
      Lwt.return_unit

    let mk_tools (chain_db : Distributed_db.chain_db) :
        prevalidation_t Tools.tools =
      let advertise_current_head ~mempool bh =
        Distributed_db.Advertise.current_head chain_db ~mempool bh
      in
      let chain_tools = mk_chain_tools chain_db in
      let create ~predecessor ~live_operations ~timestamp =
        let chain_store = Distributed_db.chain_store chain_db in
        Prevalidation_t.create
          chain_store
          ?protocol_data:None
          ~predecessor
          ~live_operations
          ~timestamp
      in
      let fetch ?peer ?timeout oph =
        Distributed_db.Operation.fetch chain_db ?timeout ?peer oph ()
      in
      let read_block bh =
        let chain_store = Distributed_db.chain_store chain_db in
        Store.Block.read_block chain_store bh
      in
      let send_get_current_head ?peer () =
        Distributed_db.Request.current_head chain_db ?peer ()
      in
      let set_mempool ~head mempool =
        let chain_store = Distributed_db.chain_store chain_db in
        Store.Chain.set_mempool chain_store ~head mempool
      in
      {
        advertise_current_head;
        chain_tools;
        create;
        fetch;
        read_block;
        send_get_current_head;
        set_mempool;
      }

    let mk_worker_tools w : Tools.worker_tools =
      let push_request r = Worker.Queue.push_request w r in
      let push_request_now r = Worker.Queue.push_request_now w r in
      {push_request; push_request_now}

    let on_launch w _ (limits, chain_db) =
      let chain_store = Distributed_db.chain_store chain_db in
      Store.Chain.current_head chain_store >>= fun predecessor ->
      let predecessor_header = Store.Block.header predecessor in
      Store.Chain.mempool chain_store >>= fun mempool ->
      Store.Chain.live_blocks chain_store
      >>= fun (live_blocks, live_operations) ->
      let timestamp_system = Tezos_stdlib_unix.Systime_os.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      Prevalidation_t.create
        chain_store
        ~predecessor
        ~timestamp
        ~live_operations
        ()
      >>= fun validation_state ->
      let fetching =
        List.fold_left
          (fun s h -> Operation_hash.Set.add h s)
          Operation_hash.Set.empty
          mempool.known_valid
      in
      let classification_parameters =
        Classification.
          {
            map_size_limit = limits.max_refused_operations;
            on_discarded_operation =
              Distributed_db.Operation.clear_or_cancel chain_db;
          }
      in
      let classification = Classification.create classification_parameters in
      let parameters = {limits; tools = mk_tools chain_db} in
      let shell =
        {
          classification;
          parameters;
          predecessor;
          timestamp = timestamp_system;
          live_blocks;
          live_operations;
          mempool = Mempool.empty;
          fetching;
          pending = Pending_ops.empty;
          advertisement = `None;
          banned_operations = Operation_hash.Set.empty;
          worker = mk_worker_tools w;
        }
      in

      Filter.Mempool.init
        Filter.Mempool.default_config
        ?validation_state:
          (Option.map
             Prevalidation_t.validation_state
             (Option.of_result validation_state))
        ~predecessor:predecessor_header
        ()
      >>=? fun filter_state ->
      let pv =
        {
          shell;
          validation_state;
          filter_state;
          operation_stream = Lwt_watcher.create_input ();
          rpc_directory = build_rpc_directory w;
          filter_config =
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/1725
               initialize from config file *)
            Filter.Mempool.default_config;
          lock = Lwt_mutex.create ();
        }
      in
      Seq.iter_s
        (may_fetch_operation pv.shell None)
        (Operation_hash.Set.to_seq fetching)
      >>= fun () -> return pv

    let on_error _w r st errs =
      Event.(emit request_failed) (r, st, errs) >|= fun () ->
      match r with
      | Request.(View (Inject _)) -> Result.return_unit
      | _ -> Error errs

    let on_completion _w r _ st =
      match Request.view r with
      | Request.View (Flush _) | View (Inject _) | View (Ban _) ->
          Event.(emit request_completed_notice) (Request.view r, st)
      | View (Notify _) | View Leftover | View (Arrived _) | View Advertise ->
          Event.(emit request_completed_debug) (Request.view r, st)

    let on_no_request _ = return_unit
  end

  let table = Worker.create_table Queue

  (* NOTE: we register a single worker for each instantiation of this Make
   * functor (and thus a single worker for the single instantiation of Worker).
   * Whilst this is somewhat abusing the intended purpose of worker, it is part
   * of a transition plan to a one-worker-per-peer architecture. *)
  let worker_promise =
    Worker.launch table name (Arg.limits, Arg.chain_db) (module Handlers)

  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1266

     If the interface of worker would not use tzresult we would
     see that this is not necessary since the function
     [Handlers.on_launch] do not actually raise any error. *)
  let initialization_errors = worker_promise >>=? fun _ -> return_unit

  let worker =
    lazy
      (match Lwt.state worker_promise with
      | Lwt.Return (Ok worker) -> worker
      | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep -> assert false)
end

module ChainProto_registry = Map.Make (struct
  type t = Chain_id.t * Protocol_hash.t

  let compare (c1, p1) (c2, p2) =
    let pc = Protocol_hash.compare p1 p2 in
    if pc = 0 then Chain_id.compare c1 c2 else pc
end)

let chain_proto_registry : t ChainProto_registry.t ref =
  ref ChainProto_registry.empty

let create limits (module Filter : Prevalidator_filters.FILTER) chain_db =
  let chain_store = Distributed_db.chain_store chain_db in
  let chain_id = Store.Chain.chain_id chain_store in
  match
    ChainProto_registry.find (chain_id, Filter.Proto.hash) !chain_proto_registry
  with
  | None ->
      let module Prevalidation_t = Prevalidation.Make (Filter.Proto) in
      let module Prevalidator =
        Make
          (Filter)
          (struct
            let limits = limits

            let chain_db = chain_db

            let chain_id = chain_id
          end)
          (Prevalidation_t)
      in
      (* Checking initialization errors before giving a reference to dangerous
       * `worker` value to caller. *)
      Prevalidator.initialization_errors >>=? fun () ->
      chain_proto_registry :=
        ChainProto_registry.add
          Prevalidator.name
          (module Prevalidator : T)
          !chain_proto_registry ;
      return (module Prevalidator : T)
  | Some p -> return p

let shutdown (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  chain_proto_registry :=
    ChainProto_registry.remove Prevalidator.name !chain_proto_registry ;
  Prevalidator.Worker.shutdown w

let flush (t : t) event head live_blocks live_operations =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait
    w
    (Request.Flush (head, event, live_blocks, live_operations))

let notify_operations (t : t) peer mempool =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request w (Request.Notify (peer, mempool))

let inject_operation (t : t) ~force op =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.push_request_and_wait w (Inject {op; force})

let status (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.status w

let running_workers () =
  ChainProto_registry.fold
    (fun (id, proto) t acc -> (id, proto, t) :: acc)
    !chain_proto_registry
    []

let pending_requests (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests w

let current_request (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.current_request w

let information (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.information w

let pipeline_length (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests_length w

let empty_rpc_directory : unit RPC_directory.t =
  RPC_directory.gen_register
    RPC_directory.empty
    (Block_services.Empty.S.Mempool.pending_operations RPC_path.open_root)
    (fun _pv params () ->
      let pending_operations =
        {
          Block_services.Empty.Mempool.applied = [];
          refused = Operation_hash.Map.empty;
          outdated = Operation_hash.Map.empty;
          branch_refused = Operation_hash.Map.empty;
          branch_delayed = Operation_hash.Map.empty;
          unprocessed = Operation_hash.Map.empty;
        }
      in
      Block_services.Empty.Mempool.pending_operations_version_dispatcher
        ~version:params#version
        pending_operations)

let rpc_directory : t option RPC_directory.t =
  RPC_directory.register_dynamic_directory
    RPC_directory.empty
    (Block_services.mempool_path RPC_path.open_root)
    (function
      | None ->
          Lwt.return
            (RPC_directory.map (fun _ -> Lwt.return_unit) empty_rpc_directory)
      | Some t -> (
          let module Prevalidator : T = (val t : T) in
          Prevalidator.initialization_errors >>= function
          | Error _ ->
              Lwt.return
                (RPC_directory.map
                   (fun _ -> Lwt.return_unit)
                   empty_rpc_directory)
          | Ok () ->
              let w = Lazy.force Prevalidator.worker in
              let pv = Prevalidator.Worker.state w in
              let pv_rpc_dir = Lazy.force pv.rpc_directory in
              Lwt.return (RPC_directory.map (fun _ -> Lwt.return pv) pv_rpc_dir)
          ))
