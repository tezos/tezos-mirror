(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

     - An operation is [Refused] if the operation cannot be parsed or
   if the protocol rejects this operation with an error classified as
   [Permanent].

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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1491
   This module should not use [Prevalidation.parse_unsafe] *)

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

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1794
   We should use chain_tools instead of chain_db *)
type parameters = {limits : limits; chain_db : Distributed_db.chain_db}

module Classification = Prevalidator_classification

(** This module encapsulates pending operations to maintain them in two
    different data structure and avoid coslty repetitive convertions when
    handling batches in [classify_pending_operations]. *)
module Pending_ops = Prevalidator_pending_operations

(** The type needed for the implementation of [Make] below, but
 *  which is independent from the protocol. *)
type types_state_shell = {
  classification : Classification.t;
  parameters : parameters;
  mutable predecessor : Store.Block.t;
  mutable timestamp : Time.System.t;
  mutable live_blocks : Block_hash.Set.t;
  mutable live_operations : Operation_hash.Set.t;
  mutable fetching : Operation_hash.Set.t;
  mutable pending : Pending_ops.t;
  mutable mempool : Mempool.t;
  mutable advertisement : [`Pending of Mempool.t | `None];
  mutable banned_operations : Operation_hash.Set.t;
}

(** The concrete production instance of {!block_tools} *)
let block_tools : Store.Block.t Classification.block_tools =
  {
    hash = Store.Block.hash;
    operations = Store.Block.operations;
    all_operation_hashes = Store.Block.all_operation_hashes;
  }

(** How to create an instance of {!chain_tools} from a {!Distributed_db.chain_db}.
    Prefer short-lived values, to avoid hiding mutable state for too long. *)
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

module type T = sig
  (** Type instantiated by {!Filter.Mempool.state}. *)
  type filter_state

  (** Type instantiated by {!Filter.Mempool.config}. *)
  type filter_config

  (** Similar to the same type in the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type operation_data

  (** Type instantiated by {!Prevalidation.t} *)
  type prevalidation_t

  val name : Name.t

  type types_state = {
    shell : types_state_shell;
    mutable filter_state : filter_state;
        (** Internal state of the filter in the plugin *)
    mutable validation_state : prevalidation_t tzresult;
    mutable operation_stream :
      (Classification.classification
      * Operation_hash.t
      * Operation.shell_header
      * operation_data)
      Lwt_watcher.input;
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

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

module type ARG = sig
  val limits : limits

  val chain_db : Distributed_db.chain_db

  val chain_id : Chain_id.t
end

type t = (module T)

module Make
    (Filter : Prevalidator_filters.FILTER)
    (Arg : ARG)
    (Prevalidation_t : Prevalidation.T
                         with type validation_state =
                               Filter.Proto.validation_state
                          and type operation_data = Filter.Proto.operation_data
                          and type operation_receipt =
                               Filter.Proto.operation_receipt
                          and type chain_store = Store.chain_store) :
  T
    with type filter_state = Filter.Mempool.state
     and type filter_config = Filter.Mempool.config
     and type operation_data = Filter.Proto.operation_data
     and type prevalidation_t = Prevalidation_t.t = struct
  type filter_state = Filter.Mempool.state

  type filter_config = Filter.Mempool.config

  type operation_data = Filter.Proto.operation_data

  type prevalidation_t = Prevalidation_t.t

  let name = (Arg.chain_id, Filter.Proto.hash)

  type 'operation_data operation = 'operation_data Prevalidation.operation

  type types_state = {
    shell : types_state_shell;
    mutable filter_state : filter_state;
    mutable validation_state : prevalidation_t tzresult;
    mutable operation_stream :
      (Classification.classification
      * Operation_hash.t
      * Operation.shell_header
      * Filter.Proto.operation_data)
      Lwt_watcher.input;
    mutable rpc_directory : types_state RPC_directory.t lazy_t;
    mutable filter_config : filter_config;
    lock : Lwt_mutex.t;
  }

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

  (* This function is in [Lwt] only for logging. *)
  let already_handled ~origin shell oph =
    if Operation_hash.Set.mem oph shell.banned_operations then
      Event.(emit ban_operation_encountered) (origin, oph) >|= fun () -> true
    else
      Lwt.return
        (Pending_ops.mem oph shell.pending
        || Operation_hash.Set.mem oph shell.fetching
        || Operation_hash.Set.mem oph shell.live_operations
        || Classification.is_in_mempool oph shell.classification)

  let advertise (w : worker) (shell : types_state_shell) mempool =
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
            Worker.Queue.push_request_now w Advertise ;
            Lwt.return_unit)
          (fun exc ->
            Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))

  (* Each classified operation should be notified exactly ONCE for a
     given stream. Operations which cannot be parsed are not notified. *)
  let handle
      ~(notifier :
         Classification.classification ->
         Operation_hash.t ->
         Operation.shell_header ->
         Filter.Proto.operation_data ->
         unit) shell op kind =
    (match op with
    | `Parsed ({hash; raw; _} : Filter.Proto.operation_data operation)
    | `Unparsed (hash, raw) ->
        Classification.add kind hash raw shell.classification) ;
    match op with
    | `Parsed
        ({raw; protocol_data; hash} : Filter.Proto.operation_data operation) ->
        notifier kind hash raw.shell protocol_data
    | _ -> ()

  let mk_notifier operation_stream classification hash shell_header op_data =
    (* This callback is safe encapsulation-wise, because it depends
       on an "harmless" field of [types_state_shell]: [operation_stream] *)
    Lwt_watcher.notify
      operation_stream
      (classification, hash, shell_header, op_data)

  let pre_filter shell ~filter_config ~filter_state ~validation_state ~chain_db
      ~notifier oph raw : [`High | `Low | `Drop] Lwt.t =
    match Prevalidation_t.parse raw with
    | Error _ ->
        Event.(emit unparsable_operation) oph >|= fun () ->
        Distributed_db.Operation.clear_or_cancel chain_db oph ;
        `Drop
    | Ok parsed_op -> (
        let op =
          {
            Filter.Proto.shell = raw.shell;
            protocol_data = parsed_op.protocol_data;
          }
        in
        let validation_state_before =
          Option.map
            Prevalidation_t.validation_state
            (Option.of_result validation_state)
        in
        Filter.Mempool.pre_filter
          ~filter_state
          ?validation_state_before
          filter_config
          op.Filter.Proto.protocol_data
        >|= function
        | (`Branch_delayed _ | `Branch_refused _ | `Refused _ | `Outdated _) as
          errs ->
            handle ~notifier shell (`Parsed parsed_op) errs ;
            `Drop
        | `Passed_prefilter priority -> (priority :> [`High | `Low | `Drop]))

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
    let chain_store = Distributed_db.chain_store shell.parameters.chain_db in
    Store.Chain.set_mempool
      chain_store
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
  let reclassify_replaced_manager_op ~notifier old_hash new_hash shell =
    shell.advertisement <-
      remove_from_advertisement old_hash shell.advertisement ;
    match Classification.remove old_hash shell.classification with
    | Some (op, _class) -> (
        (* In this block, we add [old_hash] to the classification. This
           does not break the "classes are disjoint" invariant, because we
           just removed [old_hash] with [!Classification.remove] above. *)
        match Prevalidation_t.parse op with
        | Error errors ->
            (* This should likely not happen, as we already parsed the op *)
            handle ~notifier shell (`Unparsed (old_hash, op)) (`Refused errors)
        | Ok op ->
            let err = Manager_operation_replaced {old_hash; new_hash} in
            handle ~notifier shell (`Parsed op) (`Outdated [err]))
    | None ->
        (* This case should not happen. *)
        Distributed_db.Operation.clear_or_cancel
          shell.parameters.chain_db
          old_hash

  let precheck ~disable_precheck ~filter_config ~filter_state ~validation_state
      oph (op : Filter.Proto.operation_data operation) =
    let validation_state = Prevalidation_t.validation_state validation_state in
    if disable_precheck then Lwt.return `Undecided
    else
      Filter.Mempool.precheck
        filter_config
        ~filter_state
        ~validation_state
        op.raw.shell
        oph
        op.protocol_data
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

  let classify_operation ~notifier shell ~filter_config ~filter_state
      ~validation_state mempool op oph =
    match Prevalidation_t.parse op with
    | Error errors ->
        handle ~notifier shell (`Unparsed (oph, op)) (`Refused errors) ;
        Lwt.return (filter_state, validation_state, mempool)
    | Ok op -> (
        precheck
          ~disable_precheck:shell.parameters.limits.disable_precheck
          ~filter_config
          ~filter_state
          ~validation_state
          oph
          op
        >>= function
        | `Fail errs ->
            handle ~notifier shell (`Parsed op) errs ;
            Lwt.return (filter_state, validation_state, mempool)
        | `Passed_precheck filter_state ->
            handle ~notifier shell (`Parsed op) `Prechecked ;
            let new_mempool = Mempool.cons_valid op.hash mempool in
            Lwt.return (filter_state, validation_state, new_mempool)
        | `Passed_precheck_with_replace (old_oph, filter_state) ->
            reclassify_replaced_manager_op ~notifier old_oph oph shell ;
            handle ~notifier shell (`Parsed op) `Applied ;
            let new_mempool = Mempool.cons_valid op.hash mempool in
            Lwt.return (filter_state, validation_state, new_mempool)
        | `Undecided -> (
            Prevalidation_t.apply_operation validation_state op >>= function
            | Applied (new_validation_state, receipt) -> (
                post_filter
                  ~filter_config
                  ~filter_state
                  ~validation_state_before:
                    (Prevalidation_t.validation_state validation_state)
                  ~validation_state_after:
                    (Prevalidation_t.validation_state new_validation_state)
                  op.protocol_data
                  receipt
                >>= function
                | `Passed_postfilter new_filter_state ->
                    handle ~notifier shell (`Parsed op) `Applied ;
                    let new_mempool = Mempool.cons_valid op.hash mempool in
                    Lwt.return
                      (new_filter_state, new_validation_state, new_mempool)
                | `Refused _ as classification ->
                    handle ~notifier shell (`Parsed op) classification ;
                    Lwt.return (filter_state, validation_state, mempool))
            | Branch_delayed errors ->
                handle ~notifier shell (`Parsed op) (`Branch_delayed errors) ;
                Lwt.return (filter_state, validation_state, mempool)
            | Branch_refused errors ->
                handle ~notifier shell (`Parsed op) (`Branch_refused errors) ;
                Lwt.return (filter_state, validation_state, mempool)
            | Refused errors ->
                handle ~notifier shell (`Parsed op) (`Refused errors) ;
                Lwt.return (filter_state, validation_state, mempool)
            | Outdated errors ->
                handle ~notifier shell (`Parsed op) (`Outdated errors) ;
                Lwt.return (filter_state, validation_state, mempool)))

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
  let classify_pending_operations ~notifier w shell filter_config filter_state
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
            ~notifier
            shell
            ~filter_config
            ~filter_state:acc_filter_state
            ~validation_state:acc_validation_state
            acc_mempool
            op
            oph
          >|= fun (new_filter_state, new_validation_state, new_mempool) ->
          ok (new_filter_state, new_validation_state, new_mempool, limit - 1)))
      shell.pending
      ( filter_state,
        state,
        Mempool.empty,
        shell.parameters.limits.operations_batch_size )
    >>= function
    | Error (filter_state, state, advertised_mempool) ->
        (* Early return after iteration limit was reached *)
        Worker.Queue.push_request w Request.Leftover >>= fun () ->
        Lwt.return (filter_state, state, advertised_mempool)
    | Ok (filter_state, state, advertised_mempool, _) ->
        Lwt.return (filter_state, state, advertised_mempool)

  let handle_unprocessed w pv =
    let notifier = mk_notifier pv.operation_stream in
    match pv.validation_state with
    | Error err ->
        (* At the time this comment was written (26/05/21), this is dead
           code since [Proto.begin_construction] cannot fail. *)
        Pending_ops.iter
          (fun _prio oph op ->
            handle
              ~notifier
              pv.shell
              (`Unparsed (oph, op))
              (`Branch_delayed err))
          pv.shell.pending ;
        pv.shell.pending <- Pending_ops.empty ;
        Lwt.return_unit
    | Ok state ->
        if Pending_ops.is_empty pv.shell.pending then Lwt.return_unit
        else
          Event.(emit processing_operations) () >>= fun () ->
          classify_pending_operations
            ~notifier
            w
            pv.shell
            pv.filter_config
            pv.filter_state
            state
          >>= fun (filter_state, state, advertised_mempool) ->
          let remaining_pendings = Pending_ops.hashes pv.shell.pending in
          pv.filter_state <- filter_state ;
          pv.validation_state <- Ok state ;
          (* We advertise only newly classified operations. *)
          let mempool_to_advertise =
            {
              advertised_mempool with
              known_valid = List.rev advertised_mempool.known_valid;
            }
          in
          advertise w pv.shell mempool_to_advertise ;
          let our_mempool =
            {
              (* Using List.rev_map is ok since the size of pv.shell.classification.applied
                 cannot be too big. *)
              (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2065
                 This field does not only contain valid operation *)
              Mempool.known_valid =
                List.rev_map fst pv.shell.classification.applied_rev
                @ (Operation_hash.Map.to_seq pv.shell.classification.prechecked
                  |> Seq.map fst |> List.of_seq);
              pending = remaining_pendings;
            }
          in
          set_mempool pv.shell our_mempool >>= fun _res -> Lwt.pause ()

  (* This function fetches one operation through the
     [distributed_db]. On errors, we emit an event and proceed as
     usual. *)
  let fetch_operation w (shell : types_state_shell) ?peer oph =
    Event.(emit fetching_operation) oph >|= fun () ->
    Distributed_db.Operation.fetch
      ~timeout:shell.parameters.limits.operation_timeout
      shell.parameters.chain_db
      ?peer
      oph
      ()
    >>= function
    | Ok op ->
        Worker.Queue.push_request_now w (Arrived (oph, op)) ;
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
  let may_fetch_operation w (shell : types_state_shell) peer oph =
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
             fetch_operation w shell ?peer oph)
           (fun () ->
             shell.fetching <- Operation_hash.Set.remove oph shell.fetching ;
             Lwt.return_unit)) ;
    Lwt.return_unit

  (** Module containing functions that are the internal transitions
      of the mempool. These functions are called by the {!Worker} when
      an event arrives. *)
  module Requests = struct
    let on_arrived (pv : state) oph op =
      already_handled ~origin:"arrived" pv.shell oph >>= fun already_handled ->
      if already_handled then return_unit
      else
        pre_filter
          pv.shell
          ~filter_config:pv.filter_config
          ~filter_state:pv.filter_state
          ~validation_state:pv.validation_state
          ~chain_db:pv.shell.parameters.chain_db
          ~notifier:(mk_notifier pv.operation_stream)
          oph
          op
        >>= function
        | `Drop -> return_unit
        | (`High | `Low) as prio ->
            if
              not
                (Block_hash.Set.mem
                   op.Operation.shell.branch
                   pv.shell.live_blocks)
            then (
              Distributed_db.Operation.clear_or_cancel
                pv.shell.parameters.chain_db
                oph ;
              return_unit)
            else (
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/1723
                 Should this have an influence on the peer's score ? *)
              pv.shell.pending <- Pending_ops.add oph op prio pv.shell.pending ;
              return_unit)

    let on_inject (pv : state) ~force op =
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
      else if force then (
        Distributed_db.inject_operation pv.shell.parameters.chain_db oph op
        >>= fun (_ : bool) ->
        pv.shell.pending <- Pending_ops.add oph op prio pv.shell.pending ;
        return_unit)
      else if
        not (Block_hash.Set.mem op.Operation.shell.branch pv.shell.live_blocks)
      then
        failwith
          "Operation %a is branched on a block %a which is too old"
          Operation_hash.pp
          oph
          Block_hash.pp
          op.Operation.shell.branch
      else
        pv.validation_state >>?= fun validation_state ->
        Prevalidation_t.parse op >>?= fun parsed_op ->
        Prevalidation_t.apply_operation validation_state parsed_op >>= function
        | Applied (_, _result) ->
            Distributed_db.inject_operation pv.shell.parameters.chain_db oph op
            >>= fun (_ : bool) ->
            pv.shell.pending <- Pending_ops.add oph op prio pv.shell.pending ;
            return_unit
        | res ->
            failwith
              "Error while applying operation %a:@ %a"
              Operation_hash.pp
              oph
              Prevalidation_t.pp_result
              res

    let on_notify w (shell : types_state_shell) peer mempool =
      let may_fetch_operation = may_fetch_operation w shell (Some peer) in
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
      let chain_store =
        Distributed_db.chain_store pv.shell.parameters.chain_db
      in
      Prevalidation_t.create
        chain_store
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
        ~classification:pv.shell.classification
        ~pending:(Pending_ops.operations pv.shell.pending)
        ~block_store:block_tools
        ~chain:(mk_chain_tools pv.shell.parameters.chain_db)
        ~handle_branch_refused
      >>= fun new_pending_operations ->
      (* Could be implemented as Operation_hash.Map.filter_s which
         does not exist for the moment. *)
      Operation_hash.Map.fold_s
        (fun oph op (pending, nb_pending) ->
          pre_filter
            pv.shell
            ~filter_config:pv.filter_config
            ~filter_state:pv.filter_state
            ~validation_state:pv.validation_state
            ~chain_db:pv.shell.parameters.chain_db
            ~notifier:(mk_notifier pv.operation_stream)
            oph
            op
          >|= function
          | `Drop -> (pending, nb_pending)
          | (`High | `Low) as prio ->
              (* Here, an operation injected in this node with `High priority will
                 now get its approriate priority. *)
              (Pending_ops.add oph op prio pending, nb_pending + 1))
        new_pending_operations
        (Pending_ops.empty, 0)
      >>= fun (new_pending_operations, nb_pending) ->
      Event.(emit operations_to_reclassify) nb_pending >>= fun () ->
      pv.shell.pending <- new_pending_operations ;
      set_mempool pv.shell Mempool.empty

    let on_advertise (shell : types_state_shell) =
      match shell.advertisement with
      | `None -> () (* should not happen *)
      | `Pending mempool ->
          shell.advertisement <- `None ;
          Distributed_db.Advertise.current_head
            shell.parameters.chain_db
            ~mempool
            shell.predecessor

    (* If [flush_if_prechecked] is [true], removing a prechecked
       operation triggers a flush of the mempool. Because flushing may
       be costly this should be done only when the action is triggered
       locally by the user. This allows a better UX if the user bans a
       prechecked operation so that a branch delayed operation becomes
       [applied] again. *)
    let remove ~flush_if_prechecked pv oph =
      Distributed_db.Operation.clear_or_cancel pv.shell.parameters.chain_db oph ;
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
             let map_op op =
               match Prevalidation_t.parse_unsafe op.Operation.proto with
               | Ok protocol_data ->
                   Some {Filter.Proto.shell = op.shell; protocol_data}
               | Error _ -> None
             in
             let map_op_error oph (op, error) acc =
               match map_op op with
               | None -> acc
               | Some res -> Operation_hash.Map.add oph (res, error) acc
             in
             let applied =
               List.rev_filter_map
                 (fun (hash, op) ->
                   match map_op op with
                   | Some op -> Some (hash, op)
                   | None -> None)
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
                   match map_op op with
                   | Some op -> Operation_hash.Map.add oph op acc
                   | None -> acc)
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
               |> List.rev_filter_map (fun (oph, op) ->
                      Option.map (fun proto_op -> (oph, proto_op)) (map_op op))
               )
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
             Distributed_db.Request.current_head
               pv.shell.parameters.chain_db
               ?peer:t#peer_id
               () ;
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
             let map_op error (hash, op) =
               match Prevalidation_t.parse_unsafe op.Operation.proto with
               | Error _ -> None
               | Ok protocol_data ->
                   Some
                     ( hash,
                       Filter.Proto.{shell = op.shell; protocol_data},
                       error )
             in
             let fold_op hash (op, error) acc =
               match map_op error (hash, op) with
               | Some op -> op :: acc
               | None -> acc
             in
             (* First call : retrieve the current set of op from the mempool *)
             let applied =
               if params#applied then
                 List.filter_map (map_op []) pv.shell.classification.applied_rev
               else []
             in
             (* FIXME https://gitlab.com/tezos/tezos/-/issues/2250

                For the moment, applied and prechecked operations are
                handled the same way for the user point of view. *)
             let prechecked =
               if params#applied then
                 Operation_hash.Map.bindings pv.shell.classification.prechecked
                 |> List.filter_map (map_op [])
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
                   | Some (kind, hash, shell, protocol_data)
                     when filter_result kind ->
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
                         [((hash, {Filter.Proto.shell; protocol_data}), errors)]
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
          let chain_store =
            Distributed_db.chain_store pv.shell.parameters.chain_db
          in
          Store.Block.read_block chain_store hash
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
          Requests.on_notify w pv.shell peer mempool >>= fun () -> return_unit
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
      handle_unprocessed w pv >>= fun () -> return r

    let on_close w =
      let pv = Worker.state w in
      Operation_hash.Set.iter
        (Distributed_db.Operation.clear_or_cancel pv.shell.parameters.chain_db)
        pv.shell.fetching ;
      Lwt.return_unit

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
      let parameters = {limits; chain_db} in
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
        (may_fetch_operation w pv.shell None)
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
