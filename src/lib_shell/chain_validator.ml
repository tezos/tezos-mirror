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
    | Validated : {
        peer : P2p_peer_id.t option;
        (* The peer who sent the block if it was not injected locally. *)
        block : Store.Block.t;
      }
        -> Event.update t
    | Notify_branch : P2p_peer.Id.t * Block_locator.t -> unit t
    | Notify_head :
        P2p_peer.Id.t * Block_hash.t * Block_header.t * Mempool.t
        -> unit t
    | Disconnection : P2p_peer.Id.t -> unit t

  let view (type a) (req : a t) : view =
    match req with
    | Validated {block; _} -> Hash (Store.Block.hash block)
    | Notify_branch (peer_id, _) -> PeerId peer_id
    | Notify_head (peer_id, _, _, _) -> PeerId peer_id
    | Disconnection peer_id -> PeerId peer_id
end

type synchronisation_limits = {latency : int; threshold : int}

type limits = {synchronisation : synchronisation_limits}

module Types = struct
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
    metrics : Shell_metrics.Chain_validator.t;
  }

  type state = {
    parameters : parameters;
    chain_db : Distributed_db.chain_db;
    (* This state should be updated everytime a block is validated or
       when we receive a message [Current_head] or [Current_branch]
       with a known head. Because the chain validator does not handle
       directly these messages, this is done through callbacks. *)
    synchronisation_state : Synchronisation_heuristic.Bootstrapping.t;
    valid_block_input : Store.Block.t Lwt_watcher.input;
    new_head_input : Store.Block.t Lwt_watcher.input;
    mutable child : (state * (unit -> unit Lwt.t (* shutdown *))) option;
    prevalidator : Prevalidator.t option ref;
    active_peers :
      (Peer_validator.t, Error_monad.tztrace) P2p_peer.Error_table.t;
  }

  let is_bootstrapped (state : state) =
    Synchronisation_heuristic.Bootstrapping.is_bootstrapped
      state.synchronisation_state
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
  let open Lwt_syntax in
  Option.iter_s
    (fun ({parameters = {chain_store; global_chains_input; _}; _}, shutdown) ->
      let test_chain_id = Store.Chain.chain_id chain_store in
      Lwt_watcher.notify global_chains_input (test_chain_id, false) ;
      Chain_id.Table.remove active_chains test_chain_id ;
      let* r = Store.Chain.shutdown_testchain nv.parameters.chain_store in
      match r with
      | Error _err ->
          (* FIXME *)
          Lwt.return_unit
      | Ok () ->
          let* () = shutdown () in
          nv.child <- None ;
          Lwt.return_unit)
    nv.child

(* Update the synchronisation state and if it is relevant, set the
   bootstrapped flag to true. Assume:

   - [peer_id] is not us.

   - [block] is known as valid. *)
let update_synchronisation_state w block peer_id =
  let nv = Worker.state w in
  Synchronisation_heuristic.Bootstrapping.update
    nv.synchronisation_state
    (block.Block_header.shell.timestamp, peer_id)

(* The synchronisation state is updated only for blocks known as
   valid. Assume:

   - [peer_id] is not us *)
let check_and_update_synchronisation_state w (hash, block) peer_id : unit Lwt.t
    =
  let open Lwt_syntax in
  let nv = Worker.state w in
  let* known_valid =
    Store.Block.is_known_valid nv.parameters.chain_store hash
  in
  if known_valid then update_synchronisation_state w block peer_id
  else Lwt.return_unit

(* Called for every validated block. *)
let notify_new_block w peer block =
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
  Worker.Queue.push_request_now w (Validated {peer; block})

let with_activated_peer_validator w peer_id f =
  let open Lwt_result_syntax in
  let nv = Worker.state w in
  let* pv =
    P2p_peer.Error_table.find_or_make nv.active_peers peer_id (fun () ->
        let*! () = Worker.log_event w (Connection peer_id) in
        Peer_validator.create
          ~notify_new_block:(notify_new_block w (Some peer_id))
          ~notify_termination:(fun _pv ->
            P2p_peer.Error_table.remove nv.active_peers peer_id)
          nv.parameters.peer_validator_limits
          nv.parameters.block_validator
          nv.chain_db
          peer_id)
  in
  match Peer_validator.status pv with
  | Worker_types.Running _ -> f pv
  | Worker_types.Closing (_, _)
  | Worker_types.Closed (_, _, _)
  | Worker_types.Launching _ ->
      return_unit

let may_update_protocol_level chain_store ~block =
  let open Lwt_result_syntax in
  let* pred = Store.Block.read_predecessor chain_store block in
  let prev_proto_level = Store.Block.proto_level pred in
  let new_proto_level = Store.Block.proto_level block in
  if Compare.Int.(prev_proto_level < new_proto_level) then
    let* context = Store.Block.context chain_store block in
    let*! new_protocol = Context.get_protocol context in
    Store.Chain.may_update_protocol_level
      chain_store
      ~pred
      ~protocol_level:new_proto_level
      (block, new_protocol)
  else return_unit

let may_switch_test_chain w active_chains spawn_child block =
  let open Lwt_result_syntax in
  let nv = Worker.state w in
  let may_create_child block test_protocol expiration forking_block_hash =
    let block_header = Store.Block.header block in
    let genesis_hash = Context.compute_testchain_genesis forking_block_hash in
    let testchain_id = Context.compute_testchain_chain_id genesis_hash in
    let*! activated =
      match nv.child with
      | None -> Lwt.return_false
      | Some (child, _) ->
          let child_chain_store = child.parameters.chain_store in
          let child_genesis = Store.Chain.genesis child_chain_store in
          Lwt.return (Block_hash.equal child_genesis.block genesis_hash)
    in
    let expired = expiration < block_header.shell.timestamp in
    if expired && activated then
      let*! () = shutdown_child nv active_chains in
      return_unit
    else
      let chain_store = nv.parameters.chain_store in
      let allow_forked_chains =
        Store.allow_testchains (Store.Chain.global_store chain_store)
      in
      if (not allow_forked_chains) || activated || expired then return_unit
      else
        let* testchain_store =
          let*! o =
            Store.get_chain_store_opt
              (Store.Chain.global_store nv.parameters.chain_store)
              testchain_id
          in
          match o with
          | Some test_chain_store -> return test_chain_store
          | None ->
              let try_init_test_chain cont =
                let* forking_block =
                  Store.Block.read_block chain_store forking_block_hash
                in
                let bvp = nv.parameters.block_validator_process in
                let*! r =
                  Block_validator_process.init_test_chain bvp forking_block
                in
                match r with
                | Ok genesis_header ->
                    let* testchain =
                      Store.Chain.fork_testchain
                        chain_store
                        ~testchain_id
                        ~forked_block:forking_block
                        ~genesis_hash
                        ~genesis_header
                        ~expiration
                        ~test_protocol
                    in
                    let testchain_store =
                      Store.Chain.testchain_store testchain
                    in
                    let*! new_genesis_block =
                      Store.Chain.current_head testchain_store
                    in
                    Lwt_watcher.notify
                      nv.parameters.global_valid_block_input
                      new_genesis_block ;
                    Lwt_watcher.notify nv.valid_block_input new_genesis_block ;
                    return testchain_store
                | Error
                    (Block_validator_errors.Missing_test_protocol
                       missing_protocol
                    :: _) ->
                    let* _ =
                      Block_validator.fetch_and_compile_protocol
                        nv.parameters.block_validator
                        missing_protocol
                    in
                    cont ()
                | Error _ as error -> Lwt.return error
              in
              try_init_test_chain @@ fun () ->
              try_init_test_chain @@ fun () ->
              failwith "Could not retrieve test protocol"
        in
        (* [spawn_child] is a callback to [create_node]. Thus, it takes care of
           global initialization boilerplate (e.g. notifying [global_chains_input],
           adding the chain to the correct tables, ...) *)
        let* child =
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
        in
        nv.child <- Some child ;
        return_unit
  in
  let*! r =
    let* v = Store.Block.testchain_status nv.parameters.chain_store block in
    match v with
    | Not_running, _ ->
        let*! () = shutdown_child nv active_chains in
        return_unit
    | (Forking _ | Running _), None -> return_unit (* only for snapshots *)
    | ( (Forking {protocol; expiration; _} | Running {protocol; expiration; _}),
        Some forking_block_hash ) ->
        may_create_child block protocol expiration forking_block_hash
  in
  match r with
  | Ok () -> Lwt.return_unit
  | Error err ->
      Worker.record_event w (Could_not_switch_testchain err) ;
      Lwt.return_unit

let broadcast_head w ~previous block =
  let open Lwt_syntax in
  let nv = Worker.state w in
  if not (is_bootstrapped nv) then Lwt.return_unit
  else
    let* successor =
      let* o =
        Store.Block.read_predecessor_opt nv.parameters.chain_store block
      in
      match o with
      | None -> Lwt.return_true
      | Some predecessor -> Lwt.return (Store.Block.equal predecessor previous)
    in
    if successor then (
      Distributed_db.Advertise.current_head nv.chain_db block ;
      Lwt.return_unit)
    else Distributed_db.Advertise.current_branch nv.chain_db

let safe_get_prevalidator_filter hash =
  let open Lwt_syntax in
  match Prevalidator_filters.find hash with
  | Some filter -> return_ok filter
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
          let* () =
            Chain_validator_event.(emit prevalidator_filter_not_found) hash
          in
          let (module Proto) = protocol in
          let module Filter = Prevalidator_filters.No_filter (Proto) in
          return_ok (module Filter : Prevalidator_filters.FILTER))

let instantiate_prevalidator parameters set_prevalidator block chain_db =
  let open Lwt_syntax in
  let* r =
    let open Lwt_result_syntax in
    let* new_protocol =
      Store.Block.protocol_hash parameters.chain_store block
    in
    let* (module Filter) = safe_get_prevalidator_filter new_protocol in
    Prevalidator.create parameters.prevalidator_limits (module Filter) chain_db
  in
  match r with
  | Error errs ->
      let* () =
        Chain_validator_event.(emit prevalidator_reinstantiation_failure) errs
      in
      set_prevalidator None ;
      Lwt.return_unit
  | Ok prevalidator ->
      set_prevalidator (Some prevalidator) ;
      Lwt.return_unit

let may_flush_or_update_prevalidator parameters event prevalidator chain_db
    ~prev ~block =
  let open Lwt_syntax in
  match !prevalidator with
  | None -> return_ok_unit
  | Some old_prevalidator ->
      let prev_proto_level = Store.Block.proto_level prev in
      let new_proto_level = Store.Block.proto_level block in
      if Compare.Int.(prev_proto_level < new_proto_level) then
        (* TODO inject in the new prevalidator the operation
           from the previous one. *)
        let* () =
          instantiate_prevalidator
            parameters
            (fun new_prevalidator -> prevalidator := new_prevalidator)
            block
            chain_db
        in
        let* () = Prevalidator.shutdown old_prevalidator in
        return_ok_unit
      else
        let* live_blocks, live_operations =
          Store.Chain.live_blocks parameters.chain_store
        in
        Prevalidator.flush
          old_prevalidator
          event
          (Store.Block.hash block)
          live_blocks
          live_operations

let on_validation_request w peer start_testchain active_chains spawn_child block
    =
  let open Lwt_result_syntax in
  let*! () =
    Option.iter_s
      (update_synchronisation_state w (Store.Block.header block))
      peer
  in
  let nv = Worker.state w in
  let chain_store = nv.parameters.chain_store in
  let*! head = Store.Chain.current_head chain_store in
  let head_header = Store.Block.header head
  and head_hash = Store.Block.hash head
  and block_header = Store.Block.header block in
  let head_fitness = head_header.shell.fitness in
  let new_fitness = block_header.shell.fitness in
  let accepted_head = Fitness.(new_fitness > head_fitness) in
  if not accepted_head then return Event.Ignored_head
  else
    let* o = Store.Chain.set_head chain_store block in
    match o with
    | None ->
        (* None means that the given head is below a new_head and
           therefore it must not be broadcasted *)
        return Event.Ignored_head
    | Some previous ->
        let*! () = broadcast_head w ~previous block in
        let* () = may_update_protocol_level chain_store ~block in
        let*! () =
          if start_testchain then
            may_switch_test_chain w active_chains spawn_child block
          else Lwt.return_unit
        in
        Lwt_watcher.notify nv.new_head_input block ;
        let is_branch_switch =
          Block_hash.equal head_hash block_header.shell.predecessor
        in
        let event =
          if is_branch_switch then Event.Head_increment else Event.Branch_switch
        in
        let* () =
          if is_branch_switch then
            Store.Chain.may_update_ancestor_protocol_level
              chain_store
              ~head:block
          else return_unit
        in
        let+ () =
          may_flush_or_update_prevalidator
            nv.parameters
            event
            nv.prevalidator
            nv.chain_db
            ~prev:previous
            ~block
        in
        event

let on_notify_branch w peer_id locator =
  let open Lwt_syntax in
  let {Block_locator.head_hash; head_header; _} = locator in
  let* () =
    check_and_update_synchronisation_state w (head_hash, head_header) peer_id
  in
  with_activated_peer_validator w peer_id (fun pv ->
      Peer_validator.notify_branch pv locator ;
      return_ok_unit)

let on_notify_head w peer_id (hash, header) mempool =
  let open Lwt_result_syntax in
  let nv = Worker.state w in
  let*! () = check_and_update_synchronisation_state w (hash, header) peer_id in
  let* () =
    with_activated_peer_validator w peer_id (fun pv ->
        Peer_validator.notify_head pv hash header ;
        return_unit)
  in
  match !(nv.prevalidator) with
  | Some prevalidator ->
      let*! () = Prevalidator.notify_operations prevalidator peer_id mempool in
      return_unit
  | None -> return_unit

let on_disconnection w peer_id =
  let open Lwt_result_syntax in
  let nv = Worker.state w in
  match P2p_peer.Error_table.find nv.active_peers peer_id with
  | None -> return_unit
  | Some pv ->
      let* pv = pv in
      let*! () = Peer_validator.shutdown pv in
      return_unit

let on_request (type a) w start_testchain active_chains spawn_child
    (req : a Request.t) : a tzresult Lwt.t =
  match req with
  | Request.Validated {peer; block} ->
      on_validation_request
        w
        peer
        start_testchain
        active_chains
        spawn_child
        block
  | Request.Notify_branch (peer_id, locator) ->
      on_notify_branch w peer_id locator
  | Request.Notify_head (peer_id, hash, header, mempool) ->
      on_notify_head w peer_id (hash, header) mempool
  | Request.Disconnection peer_id -> on_disconnection w peer_id

let collect_proto ~metrics (chain_store, block) =
  let open Lwt_syntax in
  let* metadata_opt = Store.Block.get_block_metadata_opt chain_store block in
  match metadata_opt with
  | None -> Lwt.return_unit
  | Some metadata ->
      let open Shell_metrics.Proto_plugin in
      let protocol_metadata = Block_repr.block_metadata metadata in
      Lwt.catch
        (fun () ->
          (* Return Noop if the protocol does not exist, and
             UndefinedMetric if the plugin for the protocol
             does not exist *)
          let* (module ProtoMetrics) =
            let* protocol = Store.Block.protocol_hash_exn chain_store block in
            safe_get_prevalidator_proto_metrics protocol
          in
          let fitness = Store.Block.fitness block in
          let* () =
            ProtoMetrics.update_metrics
              ~protocol_metadata
              fitness
              (Shell_metrics.Chain_validator.update_proto_metrics_callback
                 ~metrics)
          in
          Lwt.return_unit)
        (fun _ -> Lwt.return_unit)

let on_completion (type a) w (req : a Request.t) (update : a) request_status =
  match req with
  | Request.Validated {block; _} ->
      let fitness = Store.Block.fitness block in
      let request = Request.Hash (Store.Block.hash block) in
      let level = Store.Block.level block in
      let timestamp = Store.Block.timestamp block in
      let () =
        let nv = Worker.state w in
        let () =
          Shell_metrics.Worker.update
            nv.parameters.metrics.validation_worker_metrics
            request_status
        in
        match update with
        | Event.Ignored_head ->
            Prometheus.Counter.inc_one nv.parameters.metrics.ignored_head_count
        | Event.Branch_switch ->
            Prometheus.Counter.inc_one nv.parameters.metrics.branch_switch_count ;
            Prometheus.Gauge.set
              nv.parameters.metrics.head_level
              (Int32.to_float level)
        | Event.Head_increment ->
            Prometheus.Counter.inc_one
              nv.parameters.metrics.head_increment_count ;
            Prometheus.Gauge.set
              nv.parameters.metrics.head_level
              (Int32.to_float level) ;
            Shell_metrics.Chain_validator.update_proto (fun () ->
                collect_proto
                  ~metrics:nv.parameters.metrics
                  (nv.parameters.chain_store, block))
      in
      Worker.record_event
        w
        (Processed_block
           {request; request_status; update; fitness; level; timestamp}) ;
      Lwt.return_unit
  | Request.Notify_head (peer_id, _, _, _) ->
      Worker.record_event w (Event.Notify_head peer_id) ;
      Lwt.return_unit
  | Request.Notify_branch (peer_id, _) ->
      Worker.record_event w (Event.Notify_branch peer_id) ;
      Lwt.return_unit
  | Request.Disconnection peer_id ->
      Worker.record_event w (Event.Disconnection peer_id) ;
      Lwt.return_unit

let on_close w =
  let open Lwt_syntax in
  let nv = Worker.state w in
  let* () = Distributed_db.deactivate nv.chain_db in
  let pvs =
    P2p_peer.Error_table.fold_promises
      (fun _ pv acc ->
        (let* r = pv in
         match r with
         | Error _ -> Lwt.return_unit
         | Ok pv -> Peer_validator.shutdown pv)
        :: acc)
      nv.active_peers
      []
  in
  Lwt.join
    (Option.iter_s Prevalidator.shutdown !(nv.prevalidator)
    :: Option.iter_s (fun (_, shutdown) -> shutdown ()) nv.child
    :: pvs)

let may_load_protocols parameters =
  let open Lwt_result_syntax in
  let chain_store = parameters.chain_store in
  let global_store = Store.Chain.global_store chain_store in
  let*! indexed_protocols = Store.Chain.all_protocol_levels chain_store in
  let open Store_types in
  Protocol_levels.iter_es
    (fun _proto_level {Protocol_levels.protocol; _} ->
      if Registered_protocol.mem protocol then return_unit
      else
        match Store.Protocol.mem global_store protocol with
        | false -> return_unit
        | true ->
            (* Only compile protocols that are on-disk *)
            let*! () = Chain_validator_event.(emit loading_protocol protocol) in
            trace
              (Validation_errors.Cannot_load_protocol protocol)
              (let* _ =
                 Block_validator.fetch_and_compile_protocol
                   parameters.block_validator
                   protocol
               in
               return_unit))
    indexed_protocols

let on_launch w _ parameters =
  let open Lwt_result_syntax in
  let* () = may_load_protocols parameters in
  let valid_block_input = Lwt_watcher.create_input () in
  let new_head_input = Lwt_watcher.create_input () in
  let notify_branch peer_id locator =
    Worker.Queue.push_request_now w (Notify_branch (peer_id, locator))
  in
  let notify_head peer_id hash header ops =
    Worker.Queue.push_request_now w (Notify_head (peer_id, hash, header, ops))
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
  let prevalidator = ref None in
  let when_status_changes status =
    let*! () = Worker.log_event w (Event.Sync_status status) in
    match status with
    | Synchronisation_heuristic.Synchronised _ ->
        if parameters.start_prevalidator then
          let*! head = Store.Chain.current_head parameters.chain_store in
          instantiate_prevalidator
            parameters
            (fun new_prevalidator -> prevalidator := new_prevalidator)
            head
            chain_db
        else Lwt.return_unit
    | _ -> Lwt.return_unit
  in
  let synchronisation_state =
    Synchronisation_heuristic.Bootstrapping.create
      ~when_bootstrapped_changes:(fun b ->
        if b then Worker.log_event w Event.Bootstrapped else Lwt.return_unit)
      ~when_status_changes
      ~threshold:parameters.limits.synchronisation.threshold
      ~latency:parameters.limits.synchronisation.latency
      ()
  in
  let nv =
    {
      parameters;
      chain_db;
      valid_block_input;
      new_head_input;
      synchronisation_state;
      active_peers = P2p_peer.Error_table.create 50;
      (* TODO use [2 * max_connection] *)
      child = None;
      prevalidator (* the prevalidator may be instantiated next *);
    }
  in
  (* The synchronisation heuristic must be activated after the
     [chain_db] since when [limits.synchronisation.threshold <= 0] ,
     it will initialise the prevalidator which requires the
     [chain_db]. *)
  let*! () =
    Synchronisation_heuristic.Bootstrapping.activate synchronisation_state
  in
  return nv

let metrics = Shell_metrics.Chain_validator.init Name.base

let rec create ~start_testchain ~active_chains ?parent ~block_validator_process
    start_prevalidator (peer_validator_limits : Peer_validator.limits)
    (prevalidator_limits : Prevalidator.limits) block_validator
    global_valid_block_input global_chains_input db chain_store limits =
  let open Lwt_result_syntax in
  let spawn_child ~parent enable_prevalidator peer_validator_limits
      prevalidator_limits block_validator global_valid_block_input
      global_chains_input ddb chain_store limits =
    let* w =
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
    in
    return (Worker.state w, fun () -> Worker.shutdown w)
  in
  let module Handlers = struct
    type self = t

    let on_launch = on_launch

    let on_request w = on_request w start_testchain active_chains spawn_child

    let on_close = on_close

    let on_error w r st errs =
      let*! () = Worker.log_event w (Request_failure (r, st, errs)) in
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
  let chain_id = Store.Chain.chain_id chain_store in
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
      metrics = metrics chain_id;
    }
  in
  let* w = Worker.launch table chain_id parameters (module Handlers) in
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
  !prevalidator

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
  let open Lwt_syntax in
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.chain_db in
  let* current_head = Store.Chain.current_head chain_store in
  fail_when
    ((not force)
    && Fitness.compare
         distant_header.Block_header.shell.fitness
         (Store.Block.fitness current_head)
       <= 0)
    (error_of_fmt "Fitness too low")

let assert_checkpoint w ((hash, _) as block_descr) =
  let open Lwt_syntax in
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.chain_db in
  let* acceptable = Store.Chain.is_acceptable_block chain_store block_descr in
  fail_unless acceptable (Validation_errors.Checkpoint_error (hash, None))

let validate_block w ?force hash block operations =
  let open Lwt_result_syntax in
  let nv = Worker.state w in
  let* () = assert_fitness_increases ?force w block in
  let* () = assert_checkpoint w (hash, block.Block_header.shell.level) in
  let*! v =
    Block_validator.validate
      ~canceler:(Worker.canceler w)
      ~notify_new_block:(notify_new_block w None)
      ~precheck_and_notify:true
      nv.parameters.block_validator
      nv.chain_db
      hash
      block
      operations
  in
  match v with
  | Valid -> return_unit
  | Invalid errs | Invalid_after_precheck errs -> Lwt.return_error errs

let bootstrapped w =
  let state = Worker.state w in
  Synchronisation_heuristic.Bootstrapping.bootstrapped
    state.synchronisation_state

let is_bootstrapped w = Types.is_bootstrapped (Worker.state w)

let reconfigure_event_logging w config =
  Block_validator_process.reconfigure_event_logging
    (Worker.state w).parameters.block_validator_process
    config

let force_bootstrapped w b =
  let state = Worker.state w in
  Synchronisation_heuristic.Bootstrapping.force_bootstrapped
    state.synchronisation_state
    b

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

let ddb_information t =
  let state = Worker.state t in
  let ddb = state.chain_db in
  Distributed_db.information ddb

let sync_status w =
  let nv = Worker.state w in
  Synchronisation_heuristic.Bootstrapping.get_status nv.synchronisation_state
