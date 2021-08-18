(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

open Chain_validator_worker_state

module Name = struct
  type t = Chain_id.t

  let encoding = Chain_id.encoding

  let base = ["validator"; "chain"]

  let pp = Chain_id.pp_short

  let equal = Chain_id.equal
end

module Request = struct
  include Request

  type _ t =
    | Validated : Store.Block.t -> Event.update t
    | Notify_branch : P2p_peer.Id.t * Block_locator.t -> unit t
    | Notify_head : P2p_peer.Id.t * Block_header.t * Mempool.t -> unit t
    | Disconnection : P2p_peer.Id.t -> unit t

  let view (type a) (req : a t) : view =
    match req with
    | Validated block -> Hash (Store.Block.hash block)
    | Notify_branch (peer_id, _) -> PeerId peer_id
    | Notify_head (peer_id, _, _) -> PeerId peer_id
    | Disconnection peer_id -> PeerId peer_id
end

type synchronisation_limits = {latency : int; threshold : int}

type limits = {
  synchronisation : synchronisation_limits;
  worker_limits : Worker_types.limits;
}

module Types = struct
  include Worker_state

  type parameters = {
    parent : Name.t option;
    (* inherit bootstrap status from parent chain validator *)
    db : Distributed_db.t;
    chain_store : Store.chain_store;
    block_validator : Block_validator.t;
    block_validator_process : Block_validator_process.t;
    global_valid_block_input : Store.Block.t Lwt_watcher.input;
    global_chains_input : (Chain_id.t * bool) Lwt_watcher.input;
    start_prevalidator : bool;
    prevalidator_limits : Prevalidator.limits;
    peer_validator_limits : Peer_validator.limits;
    limits : limits;
  }

  type state = {
    parameters : parameters;
    chain_db : Distributed_db.chain_db;
    (* This state should be updated everytime a block is validated or
       when we receive a message [Current_head] or [Current_branch]
       with a known head. Because the chain validator does not handle
       directly these messages, this is done through callbacks. *)
    synchronisation_state : Synchronisation_heuristic.t;
    mutable current_status : Synchronisation_heuristic.status option;
    (* This should be true if after updating the synchronisatio_state,
       the status was [Synchronized] at least once. When this flag is
       true, the node starts its mempool. *)
    mutable bootstrapped : bool;
    bootstrapped_waiter : unit Lwt.t;
    bootstrapped_wakener : unit Lwt.u;
    valid_block_input : Store.Block.t Lwt_watcher.input;
    new_head_input : Store.Block.t Lwt_watcher.input;
    mutable child : (state * (unit -> unit Lwt.t (* shutdown *))) option;
    mutable prevalidator : Prevalidator.t option;
    active_peers :
      (Peer_validator.t, Error_monad.tztrace) P2p_peer.Error_table.t;
  }

  let view (state : state) _ : view =
    let {bootstrapped; active_peers; _} = state in
    {
      bootstrapped;
      active_peers =
        P2p_peer.Error_table.fold_keys (fun id l -> id :: l) active_peers [];
    }
end

module Logger =
  Worker_logger.Make (Event) (Request)
    (struct
      let worker_name = "node_chain_validator"
    end)

module Worker = Worker.Make (Name) (Event) (Request) (Types) (Logger)
open Types

type t = Worker.infinite Worker.queue Worker.t

let table = Worker.create_table Queue

let shutdown w = Worker.shutdown w

let shutdown_child nv active_chains =
  Option.iter_s
    (fun ({parameters = {chain_store; global_chains_input; _}; _}, shutdown) ->
      let test_chain_id = Store.Chain.chain_id chain_store in
      Lwt_watcher.notify global_chains_input (test_chain_id, false) ;
      Chain_id.Table.remove active_chains test_chain_id ;
      Store.Chain.shutdown_testchain nv.parameters.chain_store >>= function
      | Error _err ->
          (* FIXME *)
          Lwt.return_unit
      | Ok () ->
          shutdown () >>= fun () ->
          nv.child <- None ;
          Lwt.return_unit)
    nv.child

(* Update the synchronisation state and if it is relevant, set the
   bootstrapped flag to true. Assume:

   - [peer_id] is not us.

   - [block] is known as valid. *)
let update_synchronisation_state w
    ((block, peer_id) : Block_header.t * P2p_peer.Id.t) =
  let nv = Worker.state w in
  Synchronisation_heuristic.update
    nv.synchronisation_state
    (block.shell.timestamp, peer_id) ;
  let status = Synchronisation_heuristic.get_status nv.synchronisation_state in
  (match status with
  | Synchronised _ when nv.bootstrapped = false ->
      nv.bootstrapped <- true ;
      Lwt.wakeup_later nv.bootstrapped_wakener () ;
      Worker.record_event w Bootstrapped
  | _ -> ()) ;
  if nv.current_status <> Some status then (
    nv.current_status <- Some status ;
    Worker.record_event w (Sync_status status))

(* The synchronisation state is updated only for blocks known as
   valid. Assume:

   - [peer_id] is not us *)
let check_and_update_synchronisation_state w (block, peer_id) : unit Lwt.t =
  let nv = Worker.state w in
  let hash = Block_header.hash block in
  Store.Block.is_known_valid nv.parameters.chain_store hash
  >>= fun known_valid ->
  if known_valid then (
    update_synchronisation_state w (block, peer_id) ;
    Lwt.return_unit)
  else Lwt.return_unit

(* Called for every validated block. *)
let notify_new_block w block =
  let nv = Worker.state w in
  Option.iter
    (fun id ->
      List.assoc ~equal:Chain_id.equal id (Worker.list table)
      |> Option.iter (fun w ->
             let nv = Worker.state w in
             Lwt_watcher.notify nv.valid_block_input block))
    nv.parameters.parent ;
  Lwt_watcher.notify nv.valid_block_input block ;
  Lwt_watcher.notify nv.parameters.global_valid_block_input block ;
  Worker.Queue.push_request_now w (Validated block)

(* Called for every validated block coming from a remote peer_id. *)
let notify_new_foreign_block w peer_id block =
  notify_new_block w block ;
  update_synchronisation_state w (Store.Block.header block, peer_id)

let with_activated_peer_validator w peer_id f =
  let nv = Worker.state w in
  P2p_peer.Error_table.find_or_make nv.active_peers peer_id (fun () ->
      Peer_validator.create
        ~notify_new_block:(notify_new_foreign_block w peer_id)
        ~notify_termination:(fun _pv ->
          P2p_peer.Error_table.remove nv.active_peers peer_id)
        nv.parameters.peer_validator_limits
        nv.parameters.block_validator
        nv.chain_db
        peer_id)
  >>=? fun pv ->
  match Peer_validator.status pv with
  | Worker_types.Running _ -> f pv
  | Worker_types.Closing (_, _)
  | Worker_types.Closed (_, _, _)
  | Worker_types.Launching _ ->
      return_unit

let may_update_protocol_level chain_store ~prev ~block =
  let prev_proto_level = Store.Block.proto_level prev in
  let new_proto_level = Store.Block.proto_level block in
  if Compare.Int.(prev_proto_level < new_proto_level) then
    Store.Block.context chain_store block >>=? fun context ->
    Context.get_protocol context >>= fun new_protocol ->
    Store.Chain.may_update_protocol_level
      chain_store
      ~protocol_level:new_proto_level
      (block, new_protocol)
  else return_unit

let may_switch_test_chain w active_chains spawn_child block =
  let nv = Worker.state w in
  let may_create_child block test_protocol expiration forking_block_hash =
    let block_header = Store.Block.header block in
    let genesis_hash = Context.compute_testchain_genesis forking_block_hash in
    let testchain_id = Context.compute_testchain_chain_id genesis_hash in
    (match nv.child with
    | None -> Lwt.return_false
    | Some (child, _) ->
        let child_chain_store = child.parameters.chain_store in
        let child_genesis = Store.Chain.genesis child_chain_store in
        Lwt.return (Block_hash.equal child_genesis.block genesis_hash))
    >>= fun activated ->
    let expired = expiration < block_header.shell.timestamp in
    if expired && activated then
      shutdown_child nv active_chains >>= fun () -> return_unit
    else
      let chain_store = nv.parameters.chain_store in
      let allow_forked_chains =
        Store.allow_testchains (Store.Chain.global_store chain_store)
      in
      if (not allow_forked_chains) || activated || expired then return_unit
      else
        (Store.get_chain_store_opt
           (Store.Chain.global_store nv.parameters.chain_store)
           testchain_id
         >>= function
         | Some test_chain_store -> return test_chain_store
         | None ->
             let try_init_test_chain cont =
               Store.Block.read_block chain_store forking_block_hash
               >>=? fun forking_block ->
               let bvp = nv.parameters.block_validator_process in
               Block_validator_process.init_test_chain bvp forking_block
               >>= function
               | Ok genesis_header ->
                   Store.Chain.fork_testchain
                     chain_store
                     ~testchain_id
                     ~forked_block:forking_block
                     ~genesis_hash
                     ~genesis_header
                     ~expiration
                     ~test_protocol
                   >>=? fun testchain ->
                   let testchain_store =
                     Store.Chain.testchain_store testchain
                   in
                   Store.Chain.current_head testchain_store
                   >>= fun new_genesis_block ->
                   Lwt_watcher.notify
                     nv.parameters.global_valid_block_input
                     new_genesis_block ;
                   Lwt_watcher.notify nv.valid_block_input new_genesis_block ;
                   return testchain_store
               | Error
                   (Block_validator_errors.Missing_test_protocol
                      missing_protocol
                   :: _) ->
                   Block_validator.fetch_and_compile_protocol
                     nv.parameters.block_validator
                     missing_protocol
                   >>=? fun _ -> cont ()
               | Error _ as error -> Lwt.return error
             in
             try_init_test_chain @@ fun () ->
             try_init_test_chain @@ fun () ->
             failwith "Could not retrieve test protocol")
        >>=? fun testchain_store ->
        (* [spawn_child] is a callback to [create_node]. Thus, it takes care of
           global initialization boilerplate (e.g. notifying [global_chains_input],
           adding the chain to the correct tables, ...) *)
        spawn_child
          ~parent:(Store.Chain.chain_id chain_store)
          nv.parameters.start_prevalidator
          nv.parameters.peer_validator_limits
          nv.parameters.prevalidator_limits
          nv.parameters.block_validator
          nv.parameters.global_valid_block_input
          nv.parameters.global_chains_input
          nv.parameters.db
          testchain_store
          nv.parameters.limits
        (* TODO: different limits main/test ? *)
        >>=? fun child ->
        nv.child <- Some child ;
        return_unit
  in
  (Store.Block.testchain_status nv.parameters.chain_store block >>=? function
   | (Not_running, _) ->
       shutdown_child nv active_chains >>= fun () -> return_unit
   | ((Forking _ | Running _), None) -> return_unit (* only for snapshots *)
   | ( (Forking {protocol; expiration; _} | Running {protocol; expiration; _}),
       Some forking_block_hash ) ->
       may_create_child block protocol expiration forking_block_hash)
  >>= function
  | Ok () -> Lwt.return_unit
  | Error err ->
      Worker.record_event w (Could_not_switch_testchain err) ;
      Lwt.return_unit

let broadcast_head w ~previous block =
  let nv = Worker.state w in
  if not nv.bootstrapped then Lwt.return_unit
  else
    (Store.Block.read_predecessor_opt nv.parameters.chain_store block
     >>= function
     | None -> Lwt.return_true
     | Some predecessor -> Lwt.return (Store.Block.equal predecessor previous))
    >>= fun successor ->
    if successor then (
      Distributed_db.Advertise.current_head nv.chain_db block ;
      Lwt.return_unit)
    else Distributed_db.Advertise.current_branch nv.chain_db

let safe_get_prevalidator_filter hash =
  match Prevalidator_filters.find hash with
  | Some filter -> return filter
  | None -> (
      match Registered_protocol.get hash with
      | None ->
          (* FIXME. *)
          (* This should not happen: it should be handled in the validator. *)
          failwith
            "chain_validator: missing protocol '%a' for the current block."
            Protocol_hash.pp_short
            hash
      | Some protocol ->
          Chain_validator_event.(emit prevalidator_filter_not_found) hash
          >>= fun () ->
          let (module Proto) = protocol in
          let module Filter = Prevalidator_filters.No_filter (Proto) in
          return (module Filter : Prevalidator_filters.FILTER))

let instantiate_prevalidator nv block (limits, chain_db) =
  ( Store.Block.protocol_hash nv.parameters.chain_store block
  >>=? fun new_protocol ->
    safe_get_prevalidator_filter new_protocol >>=? fun (module Filter) ->
    Prevalidator.create limits (module Filter) chain_db )
  >>= function
  | Error errs ->
      Chain_validator_event.(emit prevalidator_reinstantiation_failure) errs
      >>= fun () ->
      nv.prevalidator <- None ;
      Lwt.return_unit
  | Ok prevalidator ->
      nv.prevalidator <- Some prevalidator ;
      Lwt.return_unit

let may_flush_or_update_prevalidator nv ~prev ~block =
  match nv.prevalidator with
  | None -> return_unit
  | Some old_prevalidator ->
      let prev_proto_level = Store.Block.proto_level prev in
      let new_proto_level = Store.Block.proto_level block in
      if Compare.Int.(prev_proto_level < new_proto_level) then
        (* TODO inject in the new prevalidator the operation
           from the previous one. *)
        let Prevalidator.{limits; chain_db} =
          Prevalidator.parameters old_prevalidator
        in
        instantiate_prevalidator nv block (limits, chain_db) >>= fun () ->
        Prevalidator.shutdown old_prevalidator >>= fun () -> return_unit
      else
        Store.Chain.live_blocks nv.parameters.chain_store
        >>= fun (live_blocks, live_operations) ->
        Prevalidator.flush
          old_prevalidator
          (Store.Block.hash block)
          live_blocks
          live_operations

let may_instantiate_prevalidator nv ~head =
  if nv.parameters.start_prevalidator && nv.bootstrapped then
    instantiate_prevalidator
      nv
      head
      (nv.parameters.prevalidator_limits, nv.chain_db)
  else Lwt.return_unit

let on_validation_request w start_testchain active_chains spawn_child block =
  let nv = Worker.state w in
  let chain_store = nv.parameters.chain_store in
  Store.Chain.current_head chain_store >>= fun head ->
  let head_header = Store.Block.header head
  and head_hash = Store.Block.hash head
  and block_header = Store.Block.header block in
  (match nv.prevalidator with
  | None -> Lwt.return head_header.shell.fitness
  | Some pv -> Prevalidator.fitness pv)
  >>= fun context_fitness ->
  let head_fitness = head_header.shell.fitness in
  let new_fitness = block_header.shell.fitness in
  let accepted_head =
    if Fitness.(context_fitness = head_fitness) then
      Fitness.(new_fitness > head_fitness)
    else Fitness.(new_fitness >= context_fitness)
  in
  if not accepted_head then return Event.Ignored_head
  else
    Store.Chain.set_head chain_store block >>=? function
    | None ->
        (* None means that the given head is below a new_head and
           therefore it must not be broadcasted *)
        return Event.Ignored_head
    | Some previous ->
        broadcast_head w ~previous block >>= fun () ->
        may_update_protocol_level chain_store ~prev:previous ~block
        >>=? fun () ->
        may_flush_or_update_prevalidator nv ~prev:previous ~block >>=? fun () ->
        (if start_testchain then
         may_switch_test_chain w active_chains spawn_child block
        else Lwt.return_unit)
        >>= fun () ->
        Lwt_watcher.notify nv.new_head_input block ;
        if Block_hash.equal head_hash block_header.shell.predecessor then
          return Event.Head_increment
        else return Event.Branch_switch

let on_notify_branch w peer_id locator =
  let (block, _) = (locator : Block_locator.t :> _ * _) in
  check_and_update_synchronisation_state w (block, peer_id) >>= fun () ->
  with_activated_peer_validator w peer_id (fun pv ->
      Peer_validator.notify_branch pv locator ;
      return_unit)

let on_notify_head w peer_id header mempool =
  let nv = Worker.state w in
  check_and_update_synchronisation_state w (header, peer_id) >>= fun () ->
  with_activated_peer_validator w peer_id (fun pv ->
      Peer_validator.notify_head pv header ;
      return_unit)
  >>=? fun () ->
  match nv.prevalidator with
  | Some prevalidator ->
      Prevalidator.notify_operations prevalidator peer_id mempool >>= fun () ->
      return_unit
  | None -> return_unit

let on_disconnection w peer_id =
  let nv = Worker.state w in
  match P2p_peer.Error_table.find nv.active_peers peer_id with
  | None -> return_unit
  | Some pv ->
      pv >>=? fun pv ->
      Peer_validator.shutdown pv >>= fun () -> return_unit

let on_request (type a) w start_testchain active_chains spawn_child
    (req : a Request.t) : a tzresult Lwt.t =
  match req with
  | Request.Validated block ->
      on_validation_request w start_testchain active_chains spawn_child block
  | Request.Notify_branch (peer_id, locator) ->
      on_notify_branch w peer_id locator
  | Request.Notify_head (peer_id, header, mempool) ->
      on_notify_head w peer_id header mempool
  | Request.Disconnection peer_id -> on_disconnection w peer_id

let on_completion (type a) w (req : a Request.t) (update : a) request_status =
  match req with
  | Request.Validated block ->
      let fitness = Store.Block.fitness block in
      let request = Request.Hash (Store.Block.hash block) in
      let level = Store.Block.level block in
      let timestamp = Store.Block.timestamp block in
      Worker.record_event
        w
        (Processed_block
           {request; request_status; update; fitness; level; timestamp}) ;
      Lwt.return_unit
  | Request.Notify_head (peer_id, _, _) ->
      Worker.record_event w (Event.Notify_head peer_id) ;
      Lwt.return_unit
  | Request.Notify_branch (peer_id, _) ->
      Worker.record_event w (Event.Notify_branch peer_id) ;
      Lwt.return_unit
  | Request.Disconnection peer_id ->
      Worker.record_event w (Event.Disconnection peer_id) ;
      Lwt.return_unit

let on_close w =
  let nv = Worker.state w in
  Distributed_db.deactivate nv.chain_db >>= fun () ->
  let pvs =
    P2p_peer.Error_table.fold_promises
      (fun _ pv acc ->
        (pv >>= function
         | Error _ -> Lwt.return_unit
         | Ok pv -> Peer_validator.shutdown pv)
        :: acc)
      nv.active_peers
      []
  in
  Lwt.join
    (Option.iter_s Prevalidator.shutdown nv.prevalidator
     :: Option.iter_s (fun (_, shutdown) -> shutdown ()) nv.child :: pvs)

let may_load_protocols parameters =
  let chain_store = parameters.chain_store in
  let global_store = Store.Chain.global_store chain_store in
  Store.Chain.all_protocol_levels chain_store >>= fun indexed_protocols ->
  let open Store_types in
  Protocol_levels.iter_es
    (fun _proto_level {Protocol_levels.protocol; _} ->
      if Registered_protocol.mem protocol then return_unit
      else
        match Store.Protocol.mem global_store protocol with
        | false -> return_unit
        | true ->
            (* Only compile protocols that are on-disk *)
            Chain_validator_event.(emit loading_protocol protocol) >>= fun () ->
            trace
              (Validation_errors.Cannot_load_protocol protocol)
              ( Block_validator.fetch_and_compile_protocol
                  parameters.block_validator
                  protocol
              >>=? fun _ -> return_unit ))
    indexed_protocols

let on_launch w _ parameters =
  may_load_protocols parameters >>=? fun () ->
  let valid_block_input = Lwt_watcher.create_input () in
  let new_head_input = Lwt_watcher.create_input () in
  let (bootstrapped_waiter, bootstrapped_wakener) = Lwt.wait () in
  let synchronisation_state =
    Synchronisation_heuristic.create
      ~threshold:parameters.limits.synchronisation.threshold
      ~latency:parameters.limits.synchronisation.latency
  in
  let bootstrapped =
    match Synchronisation_heuristic.get_status synchronisation_state with
    | Synchronised _ -> true
    | Not_synchronised -> false
  in
  let notify_branch peer_id locator =
    Worker.Queue.push_request_now w (Notify_branch (peer_id, locator))
  in
  let notify_head peer_id header ops =
    Worker.Queue.push_request_now w (Notify_head (peer_id, header, ops))
  in
  let disconnection peer_id =
    Worker.Queue.push_request_now w (Disconnection peer_id)
  in
  let chain_db =
    Distributed_db.activate
      parameters.db
      parameters.chain_store
      {notify_branch; notify_head; disconnection}
  in
  let nv =
    {
      parameters;
      chain_db;
      valid_block_input;
      new_head_input;
      bootstrapped_wakener;
      bootstrapped_waiter;
      bootstrapped;
      synchronisation_state;
      current_status = None;
      active_peers = P2p_peer.Error_table.create 50;
      (* TODO use [2 * max_connection] *)
      child = None;
      prevalidator = None (* the prevalidator may be instantiated next *);
    }
  in
  (* Start the prevalidator when the chain becomes bootstrapped *)
  Lwt.on_success bootstrapped_waiter (fun () ->
      (* ignore errors *)
      Lwt.dont_wait
        (fun () ->
          Store.Chain.current_head parameters.chain_store >>= fun head ->
          may_instantiate_prevalidator nv ~head)
        (fun exc -> ignore exc)) ;
  if nv.bootstrapped then Lwt.wakeup_later nv.bootstrapped_wakener () ;
  return nv

let rec create ~start_testchain ~active_chains ?parent ~block_validator_process
    start_prevalidator (peer_validator_limits : Peer_validator.limits)
    (prevalidator_limits : Prevalidator.limits) block_validator
    global_valid_block_input global_chains_input db chain_store limits =
  let spawn_child ~parent enable_prevalidator peer_validator_limits
      prevalidator_limits block_validator global_valid_block_input
      global_chains_input ddb chain_store limits =
    create
      ~start_testchain
      ~active_chains
      ~parent
      ~block_validator_process
      enable_prevalidator
      peer_validator_limits
      prevalidator_limits
      block_validator
      global_valid_block_input
      global_chains_input
      ddb
      chain_store
      limits
    >>=? fun w -> return (Worker.state w, fun () -> Worker.shutdown w)
  in
  let module Handlers = struct
    type self = t

    let on_launch = on_launch

    let on_request w = on_request w start_testchain active_chains spawn_child

    let on_close = on_close

    let on_error w r st errs =
      Worker.log_event w (Request_failure (r, st, errs)) >>= fun () ->
      match r with
      | Hash _ ->
          (* If an error happens here, it means that the request
             [Validated] failed. For this request, the payload
             associated to the request was validated and therefore is
             safe. The handler for such request does some I/Os, and
             therefore a failure could be "No space left on device" for
             example. If there is an error at this level, it certainly
             requires a manual operation from the maintener of the
             node. *)
          Lwt.return_error errs
      | PeerId _ ->
          (* We do not crash the worker here mainly for one reason:
             Such request comes from a remote peer. The payload for
             this request may contain unsafe data. The current policy
             with tzresult is not clear and there might be a non
             serious error raised as a [tzresult] to say it was a bad
             data. If if is the case, we do not want to crash the
             worker.

             With the current state of the code, it is possible that
             this branch is not reachable. This would be possible to
             see it if we relax the interface of [tezos-worker] to use
             [('a, 'b) result] instead of [tzresult] and if each
             request uses its own error type. *)
          return_unit

    let on_completion = on_completion

    let on_no_request _ = return_unit
  end in
  let parameters =
    {
      parent;
      peer_validator_limits;
      start_prevalidator;
      prevalidator_limits;
      block_validator;
      block_validator_process;
      global_valid_block_input;
      global_chains_input;
      db;
      chain_store;
      limits;
    }
  in
  Worker.launch
    table
    prevalidator_limits.worker_limits
    (Store.Chain.chain_id chain_store)
    parameters
    (module Handlers)
  >>=? fun w ->
  Chain_id.Table.add active_chains (Store.Chain.chain_id chain_store) w ;
  Lwt_watcher.notify global_chains_input (Store.Chain.chain_id chain_store, true) ;
  return w

(** Current block computation *)

let create ~start_prevalidator ~start_testchain ~active_chains
    ~block_validator_process peer_validator_limits prevalidator_limits
    block_validator global_valid_block_input global_chains_input global_db state
    limits =
  (* hide the optional ?parent *)
  create
    ~start_testchain
    ~active_chains
    ~block_validator_process
    start_prevalidator
    peer_validator_limits
    prevalidator_limits
    block_validator
    global_valid_block_input
    global_chains_input
    global_db
    state
    limits

let chain_id w =
  let {parameters = {chain_store; _}; _} = Worker.state w in
  Store.Chain.chain_id chain_store

let chain_store w =
  let {parameters = {chain_store; _}; _} = Worker.state w in
  chain_store

let prevalidator w =
  let {prevalidator; _} = Worker.state w in
  prevalidator

let chain_db w =
  let {chain_db; _} = Worker.state w in
  chain_db

let child w =
  Option.bind
    (Worker.state w).child
    (fun ({parameters = {chain_store; _}; _}, _) ->
      List.assoc
        ~equal:Chain_id.equal
        (Store.Chain.chain_id chain_store)
        (Worker.list table))

let assert_fitness_increases ?(force = false) w distant_header =
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.chain_db in
  Store.Chain.current_head chain_store >>= fun current_head ->
  fail_when
    ((not force)
    && Fitness.compare
         distant_header.Block_header.shell.fitness
         (Store.Block.fitness current_head)
       <= 0)
    (failure "Fitness too low")

let assert_checkpoint w ((hash, _) as block_descr) =
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.chain_db in
  Store.Chain.is_acceptable_block chain_store block_descr >>= fun acceptable ->
  fail_unless acceptable (Validation_errors.Checkpoint_error (hash, None))

let validate_block w ?force hash block operations =
  let nv = Worker.state w in
  let hash' = Block_header.hash block in
  assert (Block_hash.equal hash hash') ;
  assert_fitness_increases ?force w block >>=? fun () ->
  assert_checkpoint w (hash, block.Block_header.shell.level) >>=? fun () ->
  Block_validator.validate
    ~canceler:(Worker.canceler w)
    ~notify_new_block:(notify_new_block w)
    nv.parameters.block_validator
    nv.chain_db
    hash
    block
    operations

let bootstrapped w =
  let {bootstrapped_waiter; _} = Worker.state w in
  Lwt.protected bootstrapped_waiter

let is_bootstrapped w = (Worker.state w).bootstrapped

let force_bootstrapped w b =
  let state = Worker.state w in
  if b then Worker.record_event w Event.Bootstrapped ;
  state.bootstrapped <- b

let valid_block_watcher w =
  let {valid_block_input; _} = Worker.state w in
  Lwt_watcher.create_stream valid_block_input

let new_head_watcher w =
  let {new_head_input; _} = Worker.state w in
  Lwt_watcher.create_stream new_head_input

let status = Worker.status

let information = Worker.information

let running_workers () = Worker.list table

let pending_requests t = Worker.Queue.pending_requests t

let pending_requests_length t = Worker.Queue.pending_requests_length t

let current_request t = Worker.current_request t

let last_events = Worker.last_events

let ddb_information t =
  let state = Worker.state t in
  let ddb = state.chain_db in
  Distributed_db.information ddb

let sync_status w =
  let nv = Worker.state w in
  Synchronisation_heuristic.get_status nv.synchronisation_state
