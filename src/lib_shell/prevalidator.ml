(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2025 Nomadic Labs, <contact@nomadic-labs.com>          *)
(*                                                                           *)
(*****************************************************************************)

(** Minimal delay between two mempool advertisements *)
let advertisement_delay = 0.1

module Profiler = (val Profiler.wrap Shell_profiling.mempool_profiler)

(** Argument that will be provided to {!Worker.MakeGroup} to create
    the prevalidator worker. *)
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

open Prevalidator_worker_state

(** A prevalidator instance, tailored to a specific protocol (even if
    it is not visible in this module type). *)
module type T = sig
  type types_state

  val get_rpc_directory :
    types_state -> types_state Tezos_rpc.Directory.t lazy_t

  val name : Name.t

  module Types : Worker_intf.TYPES with type state = types_state

  module Worker :
    Worker.T
      with type ('a, 'b) Request.t = ('a, 'b) Request.t
       and type Request.view = Request.view
       and type Types.state = types_state

  type worker = Worker.infinite Worker.queue Worker.t

  val worker : worker Lazy.t
end

open Shell_operation
module Events = Prevalidator_events
module Classification = Prevalidator_classification

(** This module encapsulates pending operations to maintain them in two
    different data structure and avoid costly repetitive conversions when
    handling batches in [classify_pending_operations]. *)
module Pending_ops = Prevalidator_pending_operations

(** Module encapsulating some types that are used both in production
    and in tests. Having them in a module makes it possible to
    [include] this module in {!Internal_for_tests} below and avoid
    code duplication.

    The raison d'être of these records of functions is to be able to use
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
  type tools = {
    advertise_current_head : mempool:Mempool.t -> Store.Block.t -> unit;
        (** [advertise_current_head mempool head] sends a
            [Current_head (chain_id, head_header, mempool)] message to all known
            active peers for the chain being considered. *)
    chain_tools : Store.Block.t Classification.chain_tools;
        (** Lower-level tools provided by {!Prevalidator_classification} *)
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
    push_request :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> bool Lwt.t;
        (** Adds a message to the queue. *)
    push_request_now :
      (unit, Empty.t) Prevalidator_worker_state.Request.t -> unit;
        (** Adds a message to the queue immediately. *)
  }
end

type 'prevalidation_t parameters = {
  limits : Shell_limits.prevalidator_limits;
  tools : Tools.tools;
  flush :
    head:Store.Block.t ->
    timestamp:Time.Protocol.t ->
    'prevalidation_t ->
    'prevalidation_t tzresult Lwt.t;
      (** Create a new empty prevalidation state, recycling some elements
            of the provided previous prevalidation state. *)
  chain_store : Store.chain_store;
}

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
      (** An operation is in [fetching] while the ddb is actively
          requesting it from peers. It is removed from it when the
          operation arrives or if the request fails (e.g. timeout). *)
  mutable pending : 'protocol_data Pending_ops.t;
  mutable mempool : Mempool.t;
  mutable advertisement : [`Pending of Mempool.t | `None];
  mutable banned_operations : Operation_hash.Set.t;
  worker : Tools.worker_tools;
}

let metrics = Shell_metrics.Mempool.init ["mempool"]

(** The concrete production instance of {!block_tools} *)
let block_tools : Store.Block.t Classification.block_tools =
  {
    bhash = Store.Block.hash;
    operations = Store.Block.operations;
    all_operation_hashes = Store.Block.all_operation_hashes;
  }

(** How to create an instance of {!chain_tools} from a {!Distributed_db.chain_db}. *)
let mk_chain_tools (chain_db : Distributed_db.chain_db) :
    Store.Block.t Classification.chain_tools =
  let open Lwt_syntax in
  let new_blocks ~from_block ~to_block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Chain_traversal.new_blocks chain_store ~from_block ~to_block
  in
  let read_predecessor_opt block =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Block.read_predecessor_opt chain_store block
  in
  let inject_operation oph op =
    let* _ = Distributed_db.inject_operation chain_db oph op in
    Lwt.return_unit
  in
  {
    clear_or_cancel = Distributed_db.Operation.clear_or_cancel chain_db;
    inject_operation;
    new_blocks;
    read_predecessor_opt;
  }

(** Module type used both in production and in tests. *)
module type S = sig
  (** Type instantiated by {!Prevalidation.T.config}. *)
  type config

  (** Similar to the type [operation] from the protocol,
      see {!Tezos_protocol_environment.PROTOCOL} *)
  type protocol_operation

  (** Type instantiated by {!Prevalidation.t} *)
  type prevalidation_t

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable validation_state : prevalidation_t;
        (** Internal prevalidation state. Among others, this contains the
            internal states of the protocol mempool and of the plugin. *)
    mutable operation_stream :
      (Classification.classification * protocol_operation operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state Tezos_rpc.Directory.t lazy_t;
    mutable config : config;
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
    unit

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
      types_state ->
      Operation_hash.t ->
      Operation.t ->
      (unit, Empty.t) result Lwt.t

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

    val on_notify : _ types_state_shell -> P2p_peer_id.t -> Mempool.t -> unit
  end
end

(** A functor for obtaining the testable part of this file (see
    the instantiation of this functor in {!Internal_for_tests} at the
    end of this file). Contrary to the production-only functor {!Make} below,
    this functor doesn't assume a specific chain store implementation,
    which is the crux for having it easily unit-testable. *)
module Make_s
    (Proto : Protocol_plugin.T)
    (Prevalidation_t :
      Prevalidation.T with type protocol_operation = Proto.operation) :
  S
    with type config = Prevalidation_t.config
     and type protocol_operation = Proto.operation
     and type prevalidation_t = Prevalidation_t.t = struct
  type config = Prevalidation_t.config

  type protocol_operation = Proto.operation

  type prevalidation_t = Prevalidation_t.t

  type types_state = {
    shell : (protocol_operation, prevalidation_t) types_state_shell;
    mutable validation_state : prevalidation_t;
    mutable operation_stream :
      (Classification.classification * protocol_operation operation)
      Lwt_watcher.input;
    mutable rpc_directory : types_state Tezos_rpc.Directory.t lazy_t;
    mutable config : config;
    lock : Lwt_mutex.t;
  }

  let already_handled ~origin shell oph =
    (if Operation_hash.Set.mem oph shell.banned_operations then (
       (* In order to avoid data-races (for instance in
          [may_fetch_operation]), this event is triggered
          asynchronously which may lead to misordered events. *)
       ignore
         (Unit.catch_s (fun () ->
              Events.(emit ban_operation_encountered) (origin, oph))) ;
       true)
     else if Classification.is_in_mempool oph shell.classification <> None then
       true [@profiler.mark {verbosity = Debug} ["is_classified"]]
     else if Operation_hash.Set.mem oph shell.live_operations then true
       [@profiler.mark {verbosity = Debug} ["is_live_operation"]]
     else if Pending_ops.mem oph shell.pending then true
       [@profiler.mark {verbosity = Debug} ["is_pending"]]
     else if Classification.is_known_unparsable oph shell.classification then
       true [@profiler.mark {verbosity = Debug} ["is_known_unparsable"]]
     else false [@profiler.mark {verbosity = Debug} ["not already handled"]])
    [@profiler.aggregate_f {verbosity = Debug} "already_handled"]

  let advertise (shell : ('operation_data, _) types_state_shell) mempool =
    let open Lwt_syntax in
    match shell.advertisement with
    | `Pending {Mempool.known_valid; pending} ->
        shell.advertisement <-
          `Pending
            {
              known_valid =
                Operation_hash.Set.union known_valid mempool.Mempool.known_valid;
              pending = Operation_hash.Set.union pending mempool.pending;
            }
    | `None ->
        shell.advertisement <- `Pending mempool ;
        Lwt.dont_wait
          (fun () ->
            let* () = Lwt_unix.sleep advertisement_delay in
            shell.worker.push_request_now Advertise ;
            Lwt.return_unit)
          (fun exc ->
            Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc))

  (* Each classified operation should be notified exactly ONCE for a
     given stream. Operations which cannot be parsed are not notified. *)
  let handle_classification
      ~(notifier :
         Classification.classification -> protocol_operation operation -> unit)
      shell (op, kind) =
    Classification.add kind op shell.classification ;
    notifier kind op

  let mk_notifier operation_stream classification op =
    (* This callback is safe encapsulation-wise, because it depends
       on an "harmless" field of [types_state_shell]: [operation_stream] *)
    Lwt_watcher.notify operation_stream (classification, op)

  type pre_filter_result = Drop | Priority of Pending_ops.priority

  let pre_filter pv ~notifier parsed_op : pre_filter_result =
    let v =
      Prevalidation_t.pre_filter pv.validation_state pv.config parsed_op
    in
    match v with
    | (`Branch_delayed _ | `Branch_refused _ | `Refused _ | `Outdated _) as errs
      ->
        handle_classification ~notifier pv.shell (parsed_op, errs) ;
        Events.(emit__dont_wait__use_with_care operation_classified)
          parsed_op.hash ;
        Drop
    | `Passed_prefilter priority -> Priority priority

  let set_mempool shell mempool =
    shell.mempool <- mempool ;
    shell.parameters.tools.set_mempool
      ~head:(Store.Block.hash shell.predecessor)
      shell.mempool

  let remove_from_advertisement oph = function
    | `Pending mempool -> `Pending (Mempool.remove oph mempool)
    | `None -> `None

  (* This function retrieves an old/replaced operation and reclassifies it as
     [replacement_classification].

     The operation is expected to be (a) parsable and (b) in the "validated"
     class. So, we softly handle the situations where the operation is
     unparsable or not found in any class in case this invariant is broken
     for some reason.
  *)
  let reclassify_replaced_manager_op old_hash shell
      (replacement_classification : [< Classification.error_classification]) =
    shell.advertisement <-
      remove_from_advertisement old_hash shell.advertisement ;
    match Classification.remove old_hash shell.classification with
    | Some (op, _class) ->
        Some (op, (replacement_classification :> Classification.classification))
    | None ->
        (* This case should not happen. *)
        shell.parameters.tools.chain_tools.clear_or_cancel old_hash ;
        None

  let handle_classify_operation_result shell
      (status_and_priority : Pending_ops.status_and_priority) op classification
      replacements :
      (Operation_hash.t * bool) option
      * (protocol_operation operation * Classification.classification) trace =
    let[@warning "-26"] section =
      match status_and_priority.priority with
      | High -> "classify_operation : consensus"
      | Medium -> "classify_operation : voting/anonymous"
      | Low _ -> "classify_operation : manager"
    in
    ((let to_replace =
        List.filter_map
          (fun (replaced_oph, new_classification) ->
            reclassify_replaced_manager_op replaced_oph shell new_classification)
          replacements
      in
      let to_handle = (op, classification) :: to_replace in
      let validated_operation =
        match classification with
        | `Validated ->
            let is_advertisable =
              match
                (status_and_priority.status, status_and_priority.priority)
              with
              | Fresh, _ ->
                  true
                  [@profiler.mark
                    {verbosity = Debug} ["freshly validated operation"]]
              | Reclassified, High ->
                  true
                  [@profiler.mark
                    {verbosity = Debug} ["reclassified high priority operation"]]
              | Reclassified, Medium ->
                  false
                  [@profiler.mark
                    {verbosity = Debug}
                      ["reclassified medium priority operation"]]
              | Reclassified, Low _ ->
                  (* Reclassified operations with medium and low priority are not
                    reclassified *)
                  false
                  [@profiler.mark
                    {verbosity = Debug} ["reclassified low priority operation"]]
            in
            Some (op.hash, is_advertisable)
        | `Branch_refused _ ->
            None
            [@profiler.mark {verbosity = Debug} ["branch_refused operation"]]
        | `Branch_delayed _ ->
            None
            [@profiler.mark {verbosity = Debug} ["branch_delayed operation"]]
        | `Refused _ ->
            None [@profiler.mark {verbosity = Debug} ["refused operation"]]
        | `Outdated _ ->
            None [@profiler.mark {verbosity = Debug} ["outdated operation"]]
      in
      (validated_operation, to_handle))
    [@profiler.aggregate_f {verbosity = Info} section])

  (** Determine the classification of a given operation in the current
      validation state, i.e. whether it could be included in a block on top of
      the current head, and if not, why. If yes, the operation is accumulated in
      the given [mempool].

      The function returns a tuple [(validation_state, validated_operation,
      to_handle)], where:
      - [validation_state] is the (possibly) updated validation_state,
      - [validated_operation] is an (operation * bool) option set to [None] if
        the operation has not been validated. If the operation has been
        validated the function return [Some (operation,is_advertisable)].
        [is_advertisable] is true if the operation must be advertise.
      - [to_handle] contains the given operation and its classification, and all
        operations whose classes are changed/impacted by this classification
        (eg. in case of operation replacement).
  *)
  let legacy_classify_operation shell ~config ~validation_state
      (status_and_priority : Pending_ops.status_and_priority) op :
      (prevalidation_t
      * (Operation_hash.t * bool) option
      * (protocol_operation operation * Classification.classification) list)
      Lwt.t =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7985 *)
    let open Lwt_syntax in
    let* validation_state, op, classification, replacements =
      Prevalidation_t.legacy_add_operation validation_state config op
    in
    let validated_operation, to_handle =
      handle_classify_operation_result
        shell
        status_and_priority
        op
        classification
        replacements
    in
    Lwt.return (validation_state, validated_operation, to_handle)

  (** Determine the classification of a given operation in the current
      validation state, i.e. whether it could be included in a block on top of
      the current head, and if not, why. If yes, the operation is accumulated in
      the given [mempool].

      The function returns a tuple [(validation_state, validated_operation,
      to_handle)], where:
      - [validation_state] is the (possibly) updated validation_state,
      - [validated_operation] is an (operation * bool) option set to [None] if
        the operation has not been validated. If the operation has been
        validated the function return [Some (operation,is_advertisable)].
        [is_advertisable] is true if the operation must be advertise.
      - [to_handle] contains the given operation and its classification, and all
        operations whose classes are changed/impacted by this classification
        (eg. in case of operation replacement).
  *)
  let new_classify_operation shell ~config ~validation_state
      (status_and_priority : Pending_ops.status_and_priority) res :
      prevalidation_t
      * (Operation_hash.t * bool) option
      * (protocol_operation operation * Classification.classification) list =
    let validation_state, op, classification, replacements =
      match res with
      | Ok op -> Prevalidation_t.add_valid_operation validation_state config op
      | Error (op, res) -> (validation_state, op, res, [])
    in
    let validated_operation, to_handle =
      handle_classify_operation_result
        shell
        status_and_priority
        op
        classification
        replacements
    in
    (validation_state, validated_operation, to_handle)

  (* Classify pending operations into either:
     [Refused | Outdated | Branch_delayed | Branch_refused | Validated].
     To ensure fairness with other worker requests, classification of
     operations is done by batch of [operation_batch_size] operations.

     This function ensures the following invariants:

     - If an operation is classified, it is not part of the [pending]
     map

     - See {!type-Prevalidator_classification.t} for additional details
     and invariants on the classifications themselves.

     Moreover, this function ensures that only each newly classified
     operations are advertised to the remote peers. However, if a peer
     requests our mempool, we advertise all our classified operations and
     all our pending operations. *)
  let legacy_classify_pending_operations ~notifier shell config state =
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/7985 *)
    let open Lwt_syntax in
    let* r =
      Pending_ops.fold_es
        (fun status_and_priority
             oph
             op
             ( acc_validation_state,
               advertisable_mempool,
               validated_mempool,
               limit )
           ->
          if limit <= 0 then
            (* Using Error as an early-return mechanism *)
            Lwt.return_error
              (acc_validation_state, advertisable_mempool, validated_mempool)
          else (
            (* Defined as a function to avoid an useless allocation *)
            shell.pending <- Pending_ops.remove oph shell.pending ;
            let* new_validation_state, validated_operation, to_handle =
              legacy_classify_operation
                shell
                ~config
                ~validation_state:acc_validation_state
                status_and_priority
                op
            in
            List.iter (handle_classification ~notifier shell) to_handle ;
            let+ () = Events.(emit operation_classified) oph in
            let advertisable_mempool, validated_mempool =
              match validated_operation with
              | None -> (advertisable_mempool, validated_mempool)
              | Some (oph, true) ->
                  ( Mempool.cons_valid oph advertisable_mempool,
                    Mempool.cons_valid oph validated_mempool )
              | Some (oph, false) ->
                  ( advertisable_mempool,
                    Mempool.cons_valid oph validated_mempool )
            in
            Ok
              ( new_validation_state,
                advertisable_mempool,
                validated_mempool,
                limit - 1 )))
        shell.pending
        ( state,
          Mempool.empty,
          Mempool.empty,
          shell.parameters.limits.operations_batch_size )
    in
    match r with
    | Error (state, advertisable_mempool, validated_mempool) ->
        (* Early return after iteration limit was reached *)
        let* (_was_pushed : bool) =
          shell.worker.push_request Request.Leftover
        in
        Lwt.return (state, advertisable_mempool, validated_mempool)
    | Ok (state, advertisable_mempool, validated_mempool, _) ->
        Lwt.return (state, advertisable_mempool, validated_mempool)

  (** [create_batches state operations batch_size] build a list sub-batches
      containing at most [batch_size] operations each. This step sequentially
      pre-validate operations and return the result of this pre-validation: a
      list of remaining checks to execute. *)
  let create_batches state operations batch_size =
    let open Lwt_syntax in
    Pending_ops.fold_es
      (fun status_and_priority oph op (validated, pending, size) ->
        if size = batch_size then Lwt.return_error (validated, pending)
        else
          let* checks = Prevalidation_t.partial_op_validation state op in
          let size = size + 1 in
          let validated, pending =
            let item = (status_and_priority, oph, op, checks) in
            match checks with
            | Ok (_, []) -> (item :: validated, pending)
            | _ ->
                let pending =
                  if size mod batch_size = 0 then [item] :: pending
                  else
                    match pending with
                    | hd :: tl -> (item :: hd) :: tl
                    | [] -> [item] :: pending
                in
                (validated, pending)
          in
          Lwt.return_ok (validated, pending, size))
      operations
      ([], [], 0)

  (** Execute the pending checks. *)
  let process_partially_validated_op (status_and_priority, oph, op, checks) =
    let validation =
      Result.map_error
        (fun classification ->
          (op, (classification :> Classification.classification)))
        (Prevalidation_t.handle_partially_validated checks)
    in
    (status_and_priority, oph, validation)

  (** [update_after_operation_validation
        shell config notifier
        (validation_state, advertisable_mempool, validated_mempool)
        (status_and_priority, oph, operation_validation)]

      Removes the operation from the shell's pending ops and returns updated
      [validation_state, advertisable_mempool, validated_mempool]. *)
  let update_after_operation_validation shell config notifier
      (validation_state, advertisable_mempool, validated_mempool)
      (status_and_priority, oph, operation_validation) =
    let open Lwt_syntax in
    let* () = Events.(emit operation_classified) oph in
    shell.pending <- Pending_ops.remove oph shell.pending ;
    let state, validated_operation, to_handle =
      new_classify_operation
        shell
        ~config
        ~validation_state
        status_and_priority
        operation_validation
    in
    List.iter (handle_classification ~notifier shell) to_handle ;
    let advertisable_mempool, validated_mempool =
      match validated_operation with
      | None -> (advertisable_mempool, validated_mempool)
      | Some (oph, true) ->
          ( Mempool.cons_valid oph advertisable_mempool,
            Mempool.cons_valid oph validated_mempool )
      | Some (oph, false) ->
          (advertisable_mempool, Mempool.cons_valid oph validated_mempool)
    in
    Lwt.return (state, advertisable_mempool, validated_mempool)

  (** Classify pending operations into either:
      [Refused | Outdated | Branch_delayed | Branch_refused | Validated].
      To ensure fairness with other worker requests, classification of
      operations is done by batch of [operation_batch_size] operations.

      This function ensures the following invariants:

      - If an operation is classified, it is not part of the [pending]
      map

      - See {!type-Prevalidator_classification.t} for additional details
      and invariants on the classifications themselves.

      Moreover, this function ensures that only each newly classified
      operations are advertised to the remote peers. However, if a peer
      requests our mempool, we advertise all our classified operations and
      all our pending operations. *)
  let new_classify_pending_operations ~notifier shell config state =
    let open Lwt_syntax in
    let max_batch_size = shell.parameters.limits.operations_batch_size in
    let sub_batch_size =
      max 1 (max_batch_size / Tezos_bees.Task_worker.number_of_domains)
    in
    let* list_r = create_batches state shell.pending sub_batch_size in
    let validated, pending, leftover =
      (* If we returned with an error, it means that more elements are available
         in the structure. *)
      match list_r with
      | Ok (validated, pending, _n) -> (validated, pending, false)
      | Error (validated, pending) -> (validated, pending, true)
    in
    let* state, advertisable_mempool, validated_mempool =
      Lwt_list.fold_left_s
        (fun acc op ->
          process_partially_validated_op op
          |> update_after_operation_validation shell config notifier acc)
        (state, Mempool.empty, Mempool.empty)
        validated
    in
    let update_after_operation_validation =
      update_after_operation_validation shell config notifier
    in
    let* (state, advertisable_mempool, validated_mempool), leftover =
      match pending with
      | [[op]] ->
          process_partially_validated_op op
          |> update_after_operation_validation
               (state, advertisable_mempool, validated_mempool)
          |> Lwt.map (fun states -> (states, leftover))
      | [batch] ->
          List.map process_partially_validated_op batch
          |> Lwt_list.fold_left_s
               update_after_operation_validation
               (state, advertisable_mempool, validated_mempool)
          |> Lwt.map (fun states -> (states, leftover))
      | _ ->
          (* When there is more than one sub-batch, we delegate pending checks to
             the task_worker *)
          let* results =
            Lwt_eio.run_eio @@ fun () ->
            Tezos_bees.Task_worker.launch_tasks_and_wait
              "process_partially_validated_op"
              (List.map process_partially_validated_op)
              pending
          in
          Lwt_list.fold_left_s
            (fun (states, leftover) -> function
              | Ok res ->
                  Lwt_list.fold_left_s
                    update_after_operation_validation
                    states
                    res
                  |> Lwt.map (fun states -> (states, leftover))
              | Error _ ->
                  (* In case of a worker error, ignore it because concerned batch
                     will be treated in the next classify_pending_operations loop.
                     We simply force leftover to be true. *)
                  Lwt.return (states, true))
            ((state, advertisable_mempool, validated_mempool), leftover)
            results
    in
    let* () =
      (* If the list has been return via Error, it means that we exited
         before handling all of the operations *)
      if leftover then
        Lwt.map ignore (shell.worker.push_request Request.Leftover)
      else Lwt.return_unit
    in
    Lwt.return (state, advertisable_mempool, validated_mempool)

  let classify_pending_operations =
    if Protocol.compare_version Proto.environment_version V15 < 0 then
      (* TODO: https://gitlab.com/tezos/tezos/-/issues/7985 *)
      legacy_classify_pending_operations
    else new_classify_pending_operations

  let update_advertised_mempool_fields pv_shell advertisable_mempool
      validated_mempool =
    let open Lwt_syntax in
    ((if not (Mempool.is_empty advertisable_mempool) then
        (* We only advertise newly classified operations. *)
        advertise
          pv_shell
          advertisable_mempool
        [@profiler.aggregate_f {verbosity = Debug} "advertise mempool"]
        [@profiler.wrap_f
          {driver_ids = [Opentelemetry]}
            (Opentelemetry_profiler.trace "advertise mempool")] ;
      if Mempool.is_empty validated_mempool then Lwt.return_unit
      else
        let our_mempool =
          let known_valid =
            (Operation_hash.Set.union
               validated_mempool.known_valid
               pv_shell.mempool.known_valid
             [@profiler.aggregate_f
               {verbosity = Debug} "union validated hashes"])
          in
          let pending =
            (Pending_ops.hashes
               pv_shell.pending
             [@profiler.aggregate_f {verbosity = Debug} "pending hashes"])
          in
          {Mempool.known_valid; pending}
        in
        let* _res =
          (set_mempool
             pv_shell
             our_mempool
           [@profiler.aggregate_s {verbosity = Debug} "set mempool"])
        in
        Lwt.pause ())
    [@profiler.aggregate_s
      {verbosity = Notice} "update_advertised_mempool_fields"])

  let handle_unprocessed pv =
    let open Lwt_syntax in
    let notifier = mk_notifier pv.operation_stream in
    if Pending_ops.is_empty pv.shell.pending then Lwt.return_unit
    else
      let* () = Events.(emit processing_operations) () in
      let* validation_state, advertisable_mempool, validated_mempool =
        (classify_pending_operations
           ~notifier
           pv.shell
           pv.config
           pv.validation_state
         [@profiler.aggregate_s
           {verbosity = Notice} "classify pending operations"])
      in
      pv.validation_state <- validation_state ;
      update_advertised_mempool_fields
        pv.shell
        advertisable_mempool
        validated_mempool

  (* This function fetches one operation through the
     [distributed_db]. On errors, we emit an event and proceed as
     usual. *)
  let fetch_operation ~notify_arrival
      (shell : ('operation_data, _) types_state_shell) ?peer oph =
    let open Lwt_syntax in
    let* () = Events.(emit fetching_operation) oph in
    let* r =
      protect @@ fun () ->
      shell.parameters.tools.fetch
        ~timeout:shell.parameters.limits.operation_timeout
        ?peer
        oph
    in
    match r with
    | Ok op ->
        if notify_arrival then shell.worker.push_request_now (Arrived (oph, op)) ;
        Lwt.return_unit
    | Error err -> (
        (* Make sure to remove the operation from fetching if the
           retrieval fails. This only needs to be done once. *)
        if notify_arrival then
          shell.fetching <- Operation_hash.Set.remove oph shell.fetching ;
        match err with
        | Distributed_db.Operation.Canceled _ :: _ ->
            Events.(emit operation_included) oph
        | _ ->
            (* This may happen if the peer timed out for example. *)
            Events.(emit operation_not_fetched) oph)

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
    (let origin =
       match peer with Some peer -> Events.Peer peer | None -> Leftover
     in
     let spawn_fetch_operation ~notify_arrival =
       ignore
         (Unit.catch_s (fun () ->
              fetch_operation ~notify_arrival shell ?peer oph))
     in
     if Operation_hash.Set.mem oph shell.fetching then
       (* If the operation is already being fetched, we notify the DDB
          that another peer may also be requested for the resource. In
          any case, the initial fetching thread will still be resolved
          and push an arrived worker request. *)
       spawn_fetch_operation
         ~notify_arrival:false
       [@profiler.mark {verbosity = Debug} ["already fetching"]]
     else if not (already_handled ~origin shell oph) then (
       shell.fetching <- Operation_hash.Set.add oph shell.fetching ;
       spawn_fetch_operation ~notify_arrival:true))
    [@profiler.aggregate_f {verbosity = Info} "may_fetch_operation"]

  (** Module containing functions that are the internal transitions
      of the mempool. These functions are called by the {!Worker} when
      an event arrives. *)
  module Requests = struct
    module Parser = MakeParser (Proto)

    let on_arrived (pv : types_state) oph op : (unit, Empty.t) result Lwt.t =
      let open Lwt_syntax in
      pv.shell.fetching <- Operation_hash.Set.remove oph pv.shell.fetching ;
      if already_handled ~origin:Events.Arrived pv.shell oph then return_ok_unit
      else if
        not (Block_hash.Set.mem op.Operation.shell.branch pv.shell.live_blocks)
      then (
        pv.shell.parameters.tools.chain_tools.clear_or_cancel oph ;
        let* () =
          Events.(emit unknown_operation_branch) (op.Operation.shell.branch, oph)
        in
        return_ok_unit)
      else
        match Parser.parse oph op with
        | Error _ ->
            ()
            [@profiler.overwrite
              Opentelemetry_profiler.add_event "parse_operation failed"] ;
            let* () = Events.(emit unparsable_operation) oph in
            Prevalidator_classification.add_unparsable
              oph
              pv.shell.classification ;
            return_ok_unit
        | Ok parsed_op -> (
            ()
            [@profiler.overwrite
              Opentelemetry_profiler.add_event "parse_operation succeeded"] ;
            let v =
              pre_filter
                pv
                ~notifier:(mk_notifier pv.operation_stream)
                parsed_op
            in
            match v with
            | Drop -> return_ok_unit
            | Priority ((High | Medium | Low _) as priority) ->
                (* TODO: https://gitlab.com/tezos/tezos/-/issues/1723
                     Should this have an influence on the peer's score ? *)
                (* The operation has never been handled by the prevalidator,
                   we add it with a Fresh status in the pending
                   data-strutcure to be handled with higher priority *)
                pv.shell.pending <-
                  Pending_ops.(
                    add parsed_op {status = Fresh; priority} pv.shell.pending) ;
                return_ok_unit)

    let on_inject (pv : types_state) ~force op =
      let open Lwt_result_syntax in
      let oph = Operation.hash op in
      (* Currently, an injection is always done with the highest priority, because:
         - We want to process and propagate the injected operations fast,
         - We don't want to call prefilter to get the priority.
         But, this may change in the future
      *)
      let status_and_priority = Pending_ops.{status = Fresh; priority = High} in
      if already_handled ~origin:Events.Injected pv.shell oph then
        (* FIXME: https://gitlab.com/tezos/tezos/-/issues/1722
           Is this an error? *)
        return_unit
      else
        let parse_op oph op =
          match Parser.parse oph op with
          | Error err ->
              ()
              [@profiler.overwrite
                {driver_ids = [Opentelemetry]}
                  (Opentelemetry_profiler.add_event "parse_operation failed")] ;
              failwith
                "Invalid operation %a: %a"
                Operation_hash.pp
                oph
                Error_monad.pp_print_trace
                err
          | Ok parsed_op ->
              ()
              [@profiler.overwrite
                {driver_ids = [Opentelemetry]}
                  (Opentelemetry_profiler.add_event "parse_operation succeeded")] ;
              return parsed_op
        in
        if force then (
          let* parsed_op = parse_op oph op in
          let*! () =
            pv.shell.parameters.tools.chain_tools.inject_operation oph op
          in
          pv.shell.pending <-
            Pending_ops.add parsed_op status_and_priority pv.shell.pending ;
          let*! () = Events.(emit operation_injected) oph in
          return_unit)
        else if
          not
            (Block_hash.Set.mem op.Operation.shell.branch pv.shell.live_blocks)
        then
          failwith
            "Operation %a is branched on either:\n\
            \ - a block %a which is too old (%d blocks in the past)\n\
            \ - a predecessor block from an alternative branch which is now \
             unknown"
            Operation_hash.pp
            oph
            Block_hash.pp
            op.Operation.shell.branch
            (Block_hash.Set.cardinal pv.shell.live_blocks)
        else
          let* parsed_op = parse_op oph op in
          let notifier = mk_notifier pv.operation_stream in
          let*! validation_state, validated_operation, to_handle =
            legacy_classify_operation
              pv.shell
              ~config:pv.config
              ~validation_state:pv.validation_state
              status_and_priority
              parsed_op
          in
          let op_status =
            (* to_handle contains the given operation and its classification, and
               all operations whose classes are changed/impacted by this
               classification (eg. in case of operation replacement). Here, we
               retrieve the classification of our operation. *)
            List.find_opt
              (function
                | ({hash; _} : protocol_operation operation), _ ->
                    Operation_hash.equal hash oph)
              to_handle
          in
          match op_status with
          | Some (_h, `Validated) ->
              ()
              [@profiler.overwrite
                {driver_ids = [Opentelemetry]}
                  (Opentelemetry_profiler.add_event "operation validated")] ;
              (* TODO: https://gitlab.com/tezos/tezos/-/issues/2294
                 We may want to only do the injection/replacement if a
                 flag `replace` is set to true in the injection query. *)
              let*! () =
                pv.shell.parameters.tools.chain_tools.inject_operation oph op
              in
              (* Call handle & update_advertised_mempool only if op is accepted *)
              List.iter (handle_classification ~notifier pv.shell) to_handle ;
              let*! () = Events.(emit operation_classified) oph in
              pv.validation_state <- validation_state ;
              (* Note that in this case, we may advertise an operation and bypass
                 the prioritirization strategy. *)
              let*! () =
                match validated_operation with
                | None -> Lwt.return_unit
                | Some (oph, is_advertisable) ->
                    update_advertised_mempool_fields
                      pv.shell
                      (if is_advertisable then
                         Mempool.cons_valid oph Mempool.empty
                       else Mempool.empty)
                      (Mempool.cons_valid oph Mempool.empty)
              in
              let*! () = Events.(emit operation_injected) oph in
              return_unit
          | Some
              ( _h,
                ( `Branch_delayed e
                | `Branch_refused e
                | `Refused e
                | `Outdated e ) ) ->
              ()
              [@profiler.overwrite
                {driver_ids = [Opentelemetry]}
                  (Opentelemetry_profiler.add_event "operation rejected")] ;
              Lwt.return
              @@ error_with
                   "Error while validating injected operation %a:@ %a"
                   Operation_hash.pp
                   oph
                   pp_print_trace
                   e
          | None ->
              (* This case should not happen *)
              failwith
                "Unexpected error while injecting operation %a. Operation not \
                 found after classifying it."
                Operation_hash.pp
                oph

    let on_notify (shell : ('operation_data, _) types_state_shell) peer mempool
        =
      let may_fetch_operation = may_fetch_operation shell (Some peer) in
      let () =
        Operation_hash.Set.iter may_fetch_operation mempool.Mempool.known_valid
      in
      Seq.iter
        may_fetch_operation
        (Operation_hash.Set.to_seq mempool.Mempool.pending)

    let on_flush ~handle_branch_refused pv new_predecessor new_live_blocks
        new_live_operations =
      let open Lwt_result_syntax in
      let old_predecessor = pv.shell.predecessor in
      pv.shell.predecessor <- new_predecessor ;
      pv.shell.live_blocks <- new_live_blocks ;
      pv.shell.live_operations <- new_live_operations ;
      Lwt_watcher.shutdown_input pv.operation_stream ;
      pv.operation_stream <- Lwt_watcher.create_input () ;
      let timestamp_system = Tezos_base.Time.System.now () in
      pv.shell.timestamp <- timestamp_system ;
      let timestamp = Time.System.to_protocol timestamp_system in
      let* validation_state =
        (pv.shell.parameters.flush
           ~head:new_predecessor
           ~timestamp
           pv.validation_state
         [@profiler.aggregate_s {verbosity = Info} "flush state"])
      in
      pv.validation_state <- validation_state ;
      let*! new_pending_operations =
        Classification.recycle_operations
          ~from_branch:old_predecessor
          ~to_branch:new_predecessor
          ~live_blocks:new_live_blocks
          ~parse:(fun oph op -> Result.to_option (Parser.parse oph op))
          ~classes:pv.shell.classification
          ~pending:(Pending_ops.operations pv.shell.pending)
          ~block_store:block_tools
          ~chain:pv.shell.parameters.tools.chain_tools
          ~handle_branch_refused
      in
      (* Could be implemented as Operation_hash.Map.filter_s which
         does not exist for the moment. *)
      let*! new_pending_operations, nb_pending =
        Operation_hash.Map.fold_s
          (fun oph op (pending, nb_pending) ->
            ((let v =
                pre_filter pv ~notifier:(mk_notifier pv.operation_stream) op
              in
              match v with
              | Drop -> Lwt.return (pending, nb_pending)
              | Priority ((High | Medium | Low _) as priority) ->
                  (* Here, an operation injected in this node with High priority will
                    now get its appropriate priority. *)
                  let status =
                    (* If the operation has not yet been classified we set its
                      status to Fresh *)
                    if Pending_ops.mem oph pv.shell.pending then
                      Pending_ops.Fresh
                    else Reclassified
                  in
                  Lwt.return
                    ( Pending_ops.add op {status; priority} pending,
                      nb_pending + 1 ))
            [@profiler.aggregate_s {verbosity = Info} "flushed operations"]))
          new_pending_operations
          (Pending_ops.empty, 0)
      in
      let*! () = Events.(emit operations_to_reclassify) nb_pending in
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

    (* If [flush_if_validated] is [true], removing a validated
       operation triggers a flush of the mempool. Because flushing may
       be costly this should be done only when the action is triggered
       locally by the user. This allows a better UX if the user bans a
       [validated] operation with the express goal to allow a
       [branch_delayed] operation to become [validated] again. *)
    let remove ~flush_if_validated pv oph =
      let open Lwt_result_syntax in
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
          match (classification, flush_if_validated) with
          | `Validated, true ->
              let+ () =
                on_flush
                  ~handle_branch_refused:false
                  pv
                  pv.shell.predecessor
                  pv.shell.live_blocks
                  pv.shell.live_operations
              in
              pv.shell.pending <- Pending_ops.remove oph pv.shell.pending
          | `Branch_delayed _, _
          | `Branch_refused _, _
          | `Refused _, _
          | `Outdated _, _
          | `Validated, false ->
              pv.validation_state <-
                Prevalidation_t.remove_operation pv.validation_state oph ;
              return_unit)

    let on_ban pv oph_to_ban =
      let open Lwt_result_syntax in
      pv.shell.banned_operations <-
        Operation_hash.Set.add oph_to_ban pv.shell.banned_operations ;
      let* res = remove ~flush_if_validated:true pv oph_to_ban in
      let*! () = Events.(emit operation_banned) oph_to_ban in
      return res
  end
end

module type ARG = sig
  val limits : Shell_limits.prevalidator_limits

  val chain_db : Distributed_db.chain_db

  val chain_id : Chain_id.t

  val tools : Tools.tools
end

module WorkerGroup = Worker.MakeGroup (Name) (Prevalidator_worker_state.Request)

(** The functor that is not tested, in other words used only in production.
    This functor's code is not tested (contrary to functor {!Make_s} above),
    because it hardcodes a dependency to [Store.chain_store] in its instantiation
    of type [chain_store]. This is what makes the code of this functor
    not testable for the moment, because [Store.chain_store] has poor
    testing capabilities.

    Note that, because this functor [include]s {!Make_s}, it is a
    strict extension of [Make_s]. *)
module Make
    (Proto : Protocol_plugin.T)
    (Arg : ARG)
    (Prevalidation_t :
      Prevalidation.T
        with type protocol_operation = Proto.operation
         and type chain_store = Store.chain_store) : T = struct
  module S = Make_s (Proto) (Prevalidation_t)
  open S

  type types_state = S.types_state

  let get_rpc_directory pv = pv.rpc_directory

  let name = (Arg.chain_id, Proto.hash)

  module Types = struct
    type state = types_state

    type parameters = Shell_limits.prevalidator_limits * Distributed_db.chain_db
  end

  module Worker :
    Worker.T
      with type Name.t = Name.t
       and type ('a, 'b) Request.t = ('a, 'b) Request.t
       and type Request.view = Request.view
       and type Types.state = Types.state
       and type Types.parameters = Types.parameters =
    WorkerGroup.MakeWorker (Types)

  open Types

  type worker = Worker.infinite Worker.queue Worker.t

  (** Return a json describing the prevalidator's [config].
      The boolean [include_default] ([true] by default) indicates
      whether the json should include the fields which have a value
      equal to their default value. *)
  let get_config_json ?(include_default = true) pv =
    let include_default_fields = if include_default then `Always else `Never in
    Data_encoding.Json.construct
      ~include_default_fields
      Prevalidation_t.config_encoding
      pv.config

  let fetch_context pv =
    let open Lwt_result_syntax in
    let*! ctxt_e =
      let* context =
        Prevalidation_t.get_context
          pv.shell.parameters.chain_store
          ~predecessor:pv.shell.predecessor
          ~timestamp:(Time.System.to_protocol pv.shell.timestamp)
      in
      Proto.Plugin.get_context
        context
        ~head:(Store.Block.header pv.shell.predecessor).shell
    in
    match ctxt_e with
    | Error errs ->
        let*! () = Events.(emit pending_operation_context_error) errs in
        Lwt.return_none
    | Ok ctxt -> Lwt.return_some ctxt

  let filter_validation_passes allowed_validation_passes
      (op : protocol_operation) =
    match allowed_validation_passes with
    | [] -> true
    | validation_passes -> (
        match Proto.acceptable_pass op with
        | None -> false
        | Some validation_pass ->
            List.mem ~equal:Compare.Int.equal validation_pass validation_passes)

  let filter_sources ctxt sources (op : protocol_operation) =
    let open Lwt_syntax in
    match (ctxt, sources) with
    | None, _ | _, [] -> return_true
    | Some ctxt, sources ->
        let+ op_sources = Proto.Plugin.sources_from_operation ctxt op in
        List.exists
          (fun source ->
            List.mem ~equal:Signature.Public_key_hash.equal source sources)
          op_sources

  let filter_operation_hashes ophs oph =
    if ophs = [] then true else List.mem ~equal:Operation_hash.equal oph ophs

  let build_rpc_directory w =
    lazy
      (let open Lwt_result_syntax in
       let dir : state Tezos_rpc.Directory.t ref =
         ref Tezos_rpc.Directory.empty
       in
       let module Proto_services = Block_services.Make (Proto) (Proto) in
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.get_filter Tezos_rpc.Path.open_root)
           (fun pv params () ->
             return (get_config_json ~include_default:params#include_default pv)) ;
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.set_filter Tezos_rpc.Path.open_root)
           (fun pv () obj ->
             let open Lwt_syntax in
             let* () =
               try
                 let config =
                   Data_encoding.Json.destruct
                     Prevalidation_t.config_encoding
                     obj
                 in
                 pv.config <- config ;
                 Lwt.return_unit
               with _ -> Events.(emit invalid_mempool_filter_configuration) ()
             in
             (* We return [get_config_json pv] rather than [obj] in
                order to show omitted fields (which have been reset to
                their default values), and also in case [obj] is invalid. *)
             return_ok (get_config_json pv)) ;
       (* Ban an operation (from its given hash): remove it from the
          mempool if present. Add it to the set pv.banned_operations
          to prevent it from being fetched/processed/injected in the
          future.
          Note: If the baker has already received the operation, then
          it's necessary to restart it manually to flush the operation
          from it. *)
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.ban_operation Tezos_rpc.Path.open_root)
           (fun _pv () oph ->
             let open Lwt_result_syntax in
             let*! r = Worker.Queue.push_request_and_wait w (Request.Ban oph) in
             match r with
             | Error (Closed None) -> fail [Worker_types.Terminated]
             | Error (Closed (Some errs)) -> fail errs
             | Error (Request_error err) -> fail err
             | Error (Any exn) -> fail [Exn exn]
             | Ok () -> return_unit) ;
       (* Unban an operation (from its given hash): remove it from the
          set pv.banned_operations (nothing happens if it was not banned). *)
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.unban_operation Tezos_rpc.Path.open_root)
           (fun pv () oph ->
             pv.shell.banned_operations <-
               Operation_hash.Set.remove oph pv.shell.banned_operations ;
             return_unit) ;
       (* Unban all operations: clear the set pv.banned_operations. *)
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.unban_all_operations
              Tezos_rpc.Path.open_root)
           (fun pv () () ->
             pv.shell.banned_operations <- Operation_hash.Set.empty ;
             return_unit) ;
       dir :=
         Tezos_rpc.Directory.gen_register
           !dir
           (Proto_services.S.Mempool.pending_operations
              Tezos_rpc.Path.open_root)
           (fun pv params () ->
             let open Lwt_syntax in
             let sources =
               List.map_e Signature.Public_key_hash.of_b58check params#sources
             in
             let ophs =
               List.map_e Operation_hash.of_b58check params#operation_hash
             in
             match (sources, ophs) with
             | Error errs, _ | _, Error errs -> Tezos_rpc.Answer.fail errs
             | Ok sources, Ok ophs ->
                 let* ctxt =
                   if sources = [] then Lwt.return_none else fetch_context pv
                 in
                 let filter oph protocol res =
                   let* is_in_sources = filter_sources ctxt sources protocol in
                   if
                     is_in_sources
                     && filter_validation_passes
                          params#validation_passes
                          protocol
                     && filter_operation_hashes ophs oph
                   then return @@ Some (oph, res)
                   else return_none
                 in
                 let* validated =
                   if params#validated then
                     let filtered_seq =
                       Lwt_seq.filter_map_s
                         (fun (oph, op) -> filter oph op.protocol op.protocol)
                         (Lwt_seq.of_seq
                            (Classification.Sized_map.to_map
                               pv.shell.classification.validated
                            |> Operation_hash.Map.to_seq))
                     in
                     Lwt_seq.to_list filtered_seq
                   else return_nil
                 in
                 let process_map map =
                   let open Operation_hash in
                   let seq =
                     Lwt_seq.filter_map_s
                       (fun (oph, (op, error)) ->
                         filter oph op.protocol (op.protocol, error))
                       (Lwt_seq.of_seq (Map.to_seq map))
                   in
                   let* l = Lwt_seq.to_list seq in
                   return @@ Map.of_seq @@ List.to_seq l
                 in
                 let* refused =
                   if params#refused then
                     process_map
                       (Classification.map pv.shell.classification.refused)
                   else Lwt.return Operation_hash.Map.empty
                 in
                 let* outdated =
                   if params#outdated then
                     process_map
                       (Classification.map pv.shell.classification.outdated)
                   else Lwt.return Operation_hash.Map.empty
                 in
                 let* branch_refused =
                   if params#branch_refused then
                     process_map
                       (Classification.map
                          pv.shell.classification.branch_refused)
                   else Lwt.return Operation_hash.Map.empty
                 in
                 let* branch_delayed =
                   if params#branch_delayed then
                     process_map
                       (Classification.map
                          pv.shell.classification.branch_delayed)
                   else Lwt.return Operation_hash.Map.empty
                 in
                 let* unprocessed =
                   let open Operation_hash in
                   Map.fold_s
                     (fun oph {protocol; _} acc ->
                       let* is_in_sources =
                         filter_sources ctxt sources protocol
                       in
                       if
                         is_in_sources
                         && filter_validation_passes
                              params#validation_passes
                              protocol
                         && filter_operation_hashes ophs oph
                       then return @@ Map.add oph protocol acc
                       else return acc)
                     (Pending_ops.operations pv.shell.pending)
                     Map.empty
                 in
                 let pending_operations =
                   {
                     Proto_services.Mempool.validated;
                     refused;
                     outdated;
                     branch_refused;
                     branch_delayed;
                     unprocessed;
                   }
                 in
                 Tezos_rpc.Answer.return (params#version, pending_operations)) ;
       dir :=
         Tezos_rpc.Directory.register
           !dir
           (Proto_services.S.Mempool.request_operations
              Tezos_rpc.Path.open_root)
           (fun pv t () ->
             pv.shell.parameters.tools.send_get_current_head ?peer:t#peer_id () ;
             return_unit) ;
       dir :=
         Tezos_rpc.Directory.gen_register
           !dir
           (Proto_services.S.Mempool.monitor_operations
              Tezos_rpc.Path.open_root)
           (fun pv params () ->
             Lwt_mutex.with_lock pv.lock @@ fun () ->
             let open Lwt_syntax in
             let sources =
               List.map_e Signature.Public_key_hash.of_b58check params#sources
             in
             match sources with
             | Error errs -> Tezos_rpc.Answer.fail errs
             | Ok sources ->
                 let op_stream, stopper =
                   Lwt_watcher.create_stream pv.operation_stream
                 in
                 (* First call : retrieve the current set of op from the mempool *)
                 let validated_seq =
                   if params#validated then
                     Classification.Sized_map.to_map
                       pv.shell.classification.validated
                     |> Operation_hash.Map.to_seq
                     |> Seq.map (fun (hash, {protocol; _}) ->
                            ((hash, protocol), None))
                   else Seq.empty
                 in
                 let process_error_map map =
                   let open Operation_hash in
                   map |> Map.to_seq
                   |> Seq.map (fun (hash, (op, error)) ->
                          ((hash, op.protocol), Some error))
                 in
                 let refused_seq =
                   if params#refused then
                     process_error_map
                       (Classification.map pv.shell.classification.refused)
                   else Seq.empty
                 in
                 let branch_refused_seq =
                   if params#branch_refused then
                     process_error_map
                       (Classification.map
                          pv.shell.classification.branch_refused)
                   else Seq.empty
                 in
                 let branch_delayed_seq =
                   if params#branch_delayed then
                     process_error_map
                       (Classification.map
                          pv.shell.classification.branch_delayed)
                   else Seq.empty
                 in
                 let outdated_seq =
                   if params#outdated then
                     process_error_map
                       (Classification.map pv.shell.classification.outdated)
                   else Seq.empty
                 in
                 let* ctxt =
                   if sources = [] then Lwt.return_none else fetch_context pv
                 in
                 let filter ((_, op), _) =
                   let* is_in_sources = filter_sources ctxt sources op in
                   return
                     (filter_validation_passes params#validation_passes op
                     && is_in_sources)
                 in
                 let* current_mempool =
                   Seq.append outdated_seq branch_delayed_seq
                   |> Seq.append branch_refused_seq
                   |> Seq.append refused_seq |> Seq.append validated_seq
                   |> Lwt_seq.of_seq |> Lwt_seq.filter_s filter
                   |> Lwt_seq.to_list
                 in
                 let current_mempool = ref (Some current_mempool) in
                 let filter_result = function
                   | `Validated -> params#validated
                   | `Refused _ -> params#refused
                   | `Outdated _ -> params#outdated
                   | `Branch_refused _ -> params#branch_refused
                   | `Branch_delayed _ -> params#branch_delayed
                 in
                 let rec next () =
                   match !current_mempool with
                   | Some mempool ->
                       current_mempool := None ;
                       Lwt.return_some (params#version, mempool)
                   | None -> (
                       let* o = Lwt_stream.get op_stream in
                       match o with
                       | Some (kind, op) ->
                           let* is_in_sources =
                             filter_sources ctxt sources op.protocol
                           in
                           if
                             filter_validation_passes
                               params#validation_passes
                               op.protocol
                             && is_in_sources && filter_result kind
                           then
                             let errors =
                               match kind with
                               | `Validated -> None
                               | `Branch_delayed errors
                               | `Branch_refused errors
                               | `Refused errors
                               | `Outdated errors ->
                                   Some errors
                             in
                             Lwt.return_some
                               ( params#version,
                                 [((op.hash, op.protocol), errors)] )
                           else next ()
                       | None -> Lwt.return_none)
                 in
                 let shutdown () = Lwt_watcher.shutdown stopper in
                 Tezos_rpc.Answer.return_stream {next; shutdown}) ;
       !dir)

  (** Module implementing the events at the {!Worker} level. Contrary
      to {!Requests}, these functions depend on [Worker]. *)
  module Handlers = struct
    type self = worker

    let on_request : type r request_error.
        worker ->
        (r, request_error) Request.t ->
        (r, request_error) result Lwt.t =
     fun w request ->
      let open Lwt_result_syntax in
      Prometheus.Counter.inc_one metrics.worker_counters.worker_request_count ;
      let pv = Worker.state w in
      let post_processing :
          (r, request_error) result Lwt.t -> (r, request_error) result Lwt.t =
       fun r ->
        let open Lwt_syntax in
        let* () =
          (handle_unprocessed
             pv
           [@profiler.aggregate_s
             {verbosity = Notice; metadata = [("prometheus", "")]}
               "handle_unprocessed"])
        in
        r
      in
      post_processing
      @@
      match request with
      | Request.Flush (hash, event, live_blocks, live_operations) -> (
          () [@profiler.stop] ;
          ()
          [@profiler.record
            {
              verbosity = Notice;
              metadata = [("prometheus", "__ENABLE_CHILDREN_ONLY__")];
            }
              (Format.sprintf "%s" (Block_hash.to_b58check hash))] ;
          Requests.on_advertise pv.shell ;
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/1727
             Rebase the advertisement instead. *)
          let* block = pv.shell.parameters.tools.read_block hash in
          let handle_branch_refused =
            Chain_validator_worker_state.(
              match event with
              | Head_increment | Ignored_head -> false
              | Branch_switch -> true)
          in
          Lwt_mutex.with_lock pv.lock
          @@ fun () : (r, error trace) result Lwt.t ->
          (Requests.on_flush
             ~handle_branch_refused
             pv
             block
             live_blocks
             live_operations
           [@profiler.aggregate_s
             {verbosity = Notice; metadata = [("prometheus", "")]} "on_flush"]))
      | Request.Notify (peer, mempool) ->
          Requests.on_notify
            pv.shell
            peer
            mempool
          [@profiler.aggregate_f
            {verbosity = Notice; metadata = [("prometheus", "")]} "on_notify"] ;
          return_unit
      | Request.Leftover ->
          ()
          [@profiler.mark
            {verbosity = Notice; metadata = [("prometheus", "")]} ["leftover"]] ;
          (* unprocessed ops are handled just below *)
          return_unit
      | Request.Inject {op; force} ->
          Requests.on_inject
            pv
            ~force
            op
          [@profiler.wrap_f
            {driver_ids = [Opentelemetry]}
              (Opentelemetry_profiler.trace_operation
                 (`Operation op)
                 "on_inject")]
          [@profiler.aggregate_s
            {verbosity = Notice; metadata = [("prometheus", "")]} "on_inject"]
      | Request.Arrived (oph, op) ->
          Requests.on_arrived
            pv
            oph
            op
          [@profiler.aggregate_s
            {verbosity = Notice; metadata = [("prometheus", "")]} "on_arrived"]
          [@profiler.wrap_s
            Opentelemetry_profiler.trace_operation (`Hash oph) "on_arrived"]
      | Request.Advertise ->
          Requests.on_advertise
            pv.shell
          [@profiler.aggregate_f
            {verbosity = Notice; metadata = [("prometheus", "")]} "on_advertise"] ;
          return_unit
      | Request.Ban oph ->
          Requests.on_ban
            pv
            oph
          [@profiler.aggregate_s
            {verbosity = Notice; metadata = [("prometheus", "")]} "on_ban"]

    let on_close w =
      let pv = Worker.state w in
      Lwt_watcher.shutdown_input pv.operation_stream ;
      Operation_hash.Set.iter
        pv.shell.parameters.tools.chain_tools.clear_or_cancel
        pv.shell.fetching ;
      Lwt.return_unit

    let mk_worker_tools w : Tools.worker_tools =
      let push_request r = Worker.Queue.push_request w r in
      let push_request_now r = Worker.Queue.push_request_now w r in
      {push_request; push_request_now}

    type launch_error = error trace

    let on_launch w _ (limits, chain_db) : (state, launch_error) result Lwt.t =
      let open Lwt_result_syntax in
      let chain_store = Distributed_db.chain_store chain_db in
      let flush = Prevalidation_t.flush (Distributed_db.chain_store chain_db) in
      let*! head = Store.Chain.current_head chain_store in
      ()
      [@profiler.record
        {
          verbosity = Notice;
          metadata = [("prometheus", "__ENABLE_CHILDREN_ONLY__")];
        }
          (Format.sprintf "%s" (Block_hash.to_b58check (Store.Block.hash head)))] ;

      let*! mempool = Store.Chain.mempool chain_store in
      let*! live_blocks, live_operations =
        Store.Chain.live_blocks chain_store
      in
      let timestamp_system = Tezos_base.Time.System.now () in
      let timestamp = Time.System.to_protocol timestamp_system in
      let* validation_state =
        Prevalidation_t.create chain_store ~head ~timestamp
      in
      let fetching = mempool.known_valid in
      let* () =
        (* We clear the shared mempool datatype to ensure that at protocol
           change, every signature operation is checked at block validation
           time. *)
        Store.Chain.set_mempool
          chain_store
          ~head:(Store.Block.hash head)
          Mempool.empty
      in
      let classification_parameters =
        Classification.
          {
            map_size_limit = limits.Shell_limits.max_refused_operations;
            on_discarded_operation =
              Distributed_db.Operation.clear_or_cancel chain_db;
          }
      in
      let classification = Classification.create classification_parameters in
      let parameters = {limits; tools = Arg.tools; chain_store; flush} in
      let shell =
        {
          classification;
          parameters;
          predecessor = head;
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
      Shell_metrics.Mempool.set_validated_collector (fun () ->
          Prevalidator_classification.Sized_map.cardinal
            shell.classification.validated
          |> float_of_int) ;
      Shell_metrics.Mempool.set_refused_collector (fun () ->
          Prevalidator_classification.cardinal shell.classification.refused
          |> float_of_int) ;
      Shell_metrics.Mempool.set_branch_refused_collector (fun () ->
          Prevalidator_classification.cardinal
            shell.classification.branch_refused
          |> float_of_int) ;
      Shell_metrics.Mempool.set_branch_delayed_collector (fun () ->
          Prevalidator_classification.cardinal
            shell.classification.branch_delayed
          |> float_of_int) ;
      Shell_metrics.Mempool.set_outdated_collector (fun () ->
          Prevalidator_classification.cardinal shell.classification.outdated
          |> float_of_int) ;
      Shell_metrics.Mempool.set_unprocessed_collector (fun () ->
          Prevalidator_pending_operations.cardinal shell.pending |> float_of_int) ;

      let pv =
        {
          shell;
          validation_state;
          operation_stream = Lwt_watcher.create_input ();
          rpc_directory = build_rpc_directory w;
          config =
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/1725
               initialize from config file *)
            Prevalidation_t.default_config;
          lock = Lwt_mutex.create ();
        }
      in
      Seq.iter
        (may_fetch_operation pv.shell None)
        (Operation_hash.Set.to_seq fetching) ;
      return pv

    let on_error (type a b) _w st (request : (a, b) Request.t) (errs : b) :
        [`Continue | `Shutdown] tzresult Lwt.t =
      Prometheus.Counter.inc_one metrics.worker_counters.worker_error_count ;
      let open Lwt_syntax in
      let return_ok_unit = Lwt.return_ok `Continue in
      match request with
      | Request.(Inject _) as r ->
          let* () = Events.(emit request_failed) (Request.view r, st, errs) in
          return_ok_unit
      | Request.Notify _ -> ( match errs with _ -> .)
      | Request.Leftover -> ( match errs with _ -> .)
      | Request.Arrived _ -> ( match errs with _ -> .)
      | Request.Advertise -> ( match errs with _ -> .)
      | Request.Flush _ ->
          let request_view = Request.view request in
          let* () = Events.(emit request_failed) (request_view, st, errs) in
          Lwt.return_error errs
      | Request.Ban _ ->
          let request_view = Request.view request in
          let* () = Events.(emit request_failed) (request_view, st, errs) in
          Lwt.return_error errs

    let on_completion _w r _ st =
      Prometheus.Counter.inc_one metrics.worker_counters.worker_completion_count ;
      match Request.view r with
      | View (Inject _) | View (Ban _) | Request.View (Flush _) ->
          Events.(emit request_completed_info) (Request.view r, st)
      | View (Notify _) | View Leftover | View (Arrived _) | View Advertise ->
          Events.(emit request_completed_debug) (Request.view r, st)

    let on_no_request _ = Lwt.return_unit
  end

  let table = Worker.create_table Queue

  (* NOTE: we register a single worker for each instantiation of this Make
   * functor (and thus a single worker for the single instantiation of Worker).
   * Whilst this is somewhat abusing the intended purpose of worker, it is part
   * of a transition plan to a one-worker-per-peer architecture. *)
  let worker_promise =
    Worker.launch table name (Arg.limits, Arg.chain_db) (module Handlers)

  let worker =
    lazy
      (match Lwt.state worker_promise with
      | Lwt.Return (Ok worker) -> worker
      | Lwt.Return (Error _) | Lwt.Fail _ | Lwt.Sleep -> assert false)
end

let mk_tools chain_db : Tools.tools =
  let advertise_current_head ~mempool bh =
    Distributed_db.Advertise.current_head chain_db ~mempool bh
  in
  let chain_tools = mk_chain_tools chain_db in
  let fetch ?peer ?timeout oph =
    Distributed_db.Operation.fetch chain_db ?timeout ?peer oph ()
  in
  let read_block bh =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Block.read_block chain_store bh
  in
  let send_get_current_head ?peer () =
    match peer with
    | None -> Distributed_db.Request.current_head_from_all chain_db
    | Some peer -> Distributed_db.Request.current_head_from_peer chain_db peer
  in
  let set_mempool ~head mempool =
    let chain_store = Distributed_db.chain_store chain_db in
    Store.Chain.set_mempool chain_store ~head mempool
  in
  {
    advertise_current_head;
    chain_tools;
    fetch;
    read_block;
    send_get_current_head;
    set_mempool;
  }

let make limits chain_db chain_id tools (module Proto : Protocol_plugin.T) =
  let module Prevalidation_t = Prevalidation.Make (Proto) in
  let module Prevalidator =
    Make
      (Proto)
      (struct
        let limits = limits

        let chain_db = chain_db

        let chain_id = chain_id

        let tools = tools
      end)
      (Prevalidation_t)
  in
  (module Prevalidator : T)

module ChainProto_registry = Map.Make (struct
  type t = Chain_id.t * Protocol_hash.t

  let compare (c1, p1) (c2, p2) =
    let pc = Protocol_hash.compare p1 p2 in
    if pc = 0 then Chain_id.compare c1 c2 else pc
end)

(** {2 Public interface} *)

type t = (module T)

let chain_proto_registry : t ChainProto_registry.t ref =
  ref ChainProto_registry.empty

let create limits (module Proto : Protocol_plugin.T) chain_db =
  let open Lwt_result_syntax in
  let chain_store = Distributed_db.chain_store chain_db in
  let chain_id = Store.Chain.chain_id chain_store in
  match
    ChainProto_registry.find (chain_id, Proto.hash) !chain_proto_registry
  with
  | None ->
      let prevalidator =
        make limits chain_db chain_id (mk_tools chain_db) (module Proto)
      in
      let (module Prevalidator : T) = prevalidator in
      chain_proto_registry :=
        ChainProto_registry.add
          Prevalidator.name
          prevalidator
          !chain_proto_registry ;
      return prevalidator
  | Some p -> return p

let shutdown (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  chain_proto_registry :=
    ChainProto_registry.remove Prevalidator.name !chain_proto_registry ;
  Prevalidator.Worker.shutdown w

let flush (t : t) event head live_blocks live_operations =
  let open Lwt_result_syntax in
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let*! r =
    Prevalidator.Worker.Queue.push_request_and_wait
      w
      (Request.Flush (head, event, live_blocks, live_operations))
  in
  match r with
  | Ok r -> Lwt.return_ok r
  | Error (Closed None) -> fail [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> fail errs
  | Error (Any exn) -> fail [Exn exn]
  | Error (Request_error error_trace) -> fail error_trace

let notify_operations (t : t) peer mempool =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let open Lwt_result_syntax in
  let*! (_was_pushed : bool) =
    Prevalidator.Worker.Queue.push_request w (Request.Notify (peer, mempool))
  in
  Lwt.return_unit

let inject_operation (t : t) ~force op =
  let module Prevalidator : T = (val t) in
  let open Lwt_result_syntax in
  let w = Lazy.force Prevalidator.worker in
  let*! r =
    Prevalidator.Worker.Queue.push_request_and_wait w (Inject {op; force})
  in
  match r with
  | Ok r -> Lwt.return_ok r
  | Error (Closed None) -> fail [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> fail errs
  | Error (Any exn) -> fail [Exn exn]
  | Error (Request_error error_trace) -> fail error_trace

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

let empty_rpc_directory : unit Tezos_rpc.Directory.t =
  Tezos_rpc.Directory.gen_register
    Tezos_rpc.Directory.empty
    (Block_services.Empty.S.Mempool.pending_operations Tezos_rpc.Path.open_root)
    (fun _pv params () ->
      let pending_operations =
        {
          Block_services.Empty.Mempool.validated = [];
          refused = Operation_hash.Map.empty;
          outdated = Operation_hash.Map.empty;
          branch_refused = Operation_hash.Map.empty;
          branch_delayed = Operation_hash.Map.empty;
          unprocessed = Operation_hash.Map.empty;
        }
      in
      Tezos_rpc.Answer.return (params#version, pending_operations))

let rpc_directory : t option Tezos_rpc.Directory.t =
  Tezos_rpc.Directory.register_dynamic_directory
    Tezos_rpc.Directory.empty
    (Block_services.mempool_path Tezos_rpc.Path.open_root)
    (function
    | None ->
        Lwt.return
          (Tezos_rpc.Directory.map
             (fun _ -> Lwt.return_unit)
             empty_rpc_directory)
    | Some t ->
        let module Prevalidator : T = (val t : T) in
        let w = Lazy.force Prevalidator.worker in
        let pv = Prevalidator.Worker.state w in
        let pv_rpc_dir = Lazy.force (Prevalidator.get_rpc_directory pv) in
        Lwt.return (Tezos_rpc.Directory.map (fun _ -> Lwt.return pv) pv_rpc_dir))

module Internal_for_tests = struct
  module Tools = Tools

  let mk_chain_tools = mk_chain_tools

  let create tools limits (module Proto : Protocol_plugin.T) chain_db =
    let open Lwt_result_syntax in
    let chain_store = Distributed_db.chain_store chain_db in
    let chain_id = Store.Chain.chain_id chain_store in
    match
      ChainProto_registry.find (chain_id, Proto.hash) !chain_proto_registry
    with
    | None ->
        let prevalidator = make limits chain_db chain_id tools (module Proto) in
        let (module Prevalidator : T) = prevalidator in
        chain_proto_registry :=
          ChainProto_registry.add
            Prevalidator.name
            prevalidator
            !chain_proto_registry ;
        return prevalidator
    | Some p -> return p

  let advertise_mempool (t : t) =
    let module Prevalidator : T = (val t) in
    let w = Lazy.force Prevalidator.worker in
    let open Lwt_result_syntax in
    let*! (_was_pushed : bool) =
      Prevalidator.Worker.Queue.push_request w Request.Advertise
    in
    Lwt.return_unit
end
