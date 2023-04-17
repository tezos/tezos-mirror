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

(* FIXME ignore/postpone fetching/validating of block in the future... *)

open Peer_validator_worker_state

module Name = struct
  type t = Chain_id.t * P2p_peer.Id.t

  let encoding = Data_encoding.tup2 Chain_id.encoding P2p_peer.Id.encoding

  let base = ["validator"; "peer"]

  let pp ppf (chain, peer) =
    Format.fprintf ppf "%a:%a" Chain_id.pp_short chain P2p_peer.Id.pp_short peer

  let equal (c1, p1) (c2, p2) = Chain_id.equal c1 c2 && P2p_peer.Id.equal p1 p2
end

module Request = struct
  include Request

  type (_, _) t =
    | New_head : Block_hash.t * Block_header.t -> (unit, error trace) t
    | New_branch : Block_locator.t * Block_locator.seed -> (unit, error trace) t

  let view (type a b) (req : (a, b) t) : view =
    match req with
    | New_head (hash, _) -> New_head hash
    | New_branch (locator, seed) ->
        (* the seed is associated to each locator
           w.r.t. the peer_id of the sender *)
        New_branch
          (locator.head_hash, Block_locator.estimated_length seed locator)
end

module Types = struct
  type parameters = {
    chain_db : Distributed_db.chain_db;
    block_validator : Block_validator.t;
    (* callback to chain_validator *)
    notify_new_block : Block_validator.new_block -> unit;
    notify_termination : unit -> unit;
    limits : Shell_limits.peer_validator_limits;
  }

  type state = {
    peer_id : P2p_peer.Id.t;
    parameters : parameters;
    mutable pipeline : Bootstrap_pipeline.t option;
    mutable last_validated_head : Block_header.t;
    mutable last_advertised_head : Block_header.t;
  }

  let pipeline_length = function
    | None -> Bootstrap_pipeline.length_zero
    | Some p -> Bootstrap_pipeline.length p
end

module Events = Peer_validator_events
module Worker = Worker.MakeSingle (Name) (Request) (Types)
open Types

type t = Worker.dropbox Worker.t

let metrics = Shell_metrics.Peer_validator.init Name.base

let bootstrap_new_branch w unknown_prefix =
  let open Lwt_result_syntax in
  let pv = Worker.state w in
  let sender_id = Distributed_db.my_peer_id pv.parameters.chain_db in
  (* sender and receiver are inverted here because they are from
     the point of view of the node sending the locator *)
  let seed = {Block_locator.sender_id = pv.peer_id; receiver_id = sender_id} in
  let len = Block_locator.estimated_length seed unknown_prefix in
  let*! () = Events.(emit validating_new_branch) (pv.peer_id, len) in
  let pipeline =
    Bootstrap_pipeline.create
      ~notify_new_block:pv.parameters.notify_new_block
      ~block_header_timeout:pv.parameters.limits.block_header_timeout
      ~block_operations_timeout:pv.parameters.limits.block_operations_timeout
      pv.parameters.block_validator
      pv.peer_id
      pv.parameters.chain_db
      unknown_prefix
  in
  pv.pipeline <- Some pipeline ;
  let* () =
    protect
      ~canceler:(Worker.canceler w)
      ~on_error:(fun error ->
        (* if the peer_validator is killed, let's cancel the pipeline *)
        pv.pipeline <- None ;
        let*! () = Bootstrap_pipeline.cancel pipeline in
        fail error)
      (fun () -> Bootstrap_pipeline.wait pipeline)
  in
  pv.pipeline <- None ;
  let*! () =
    Events.(emit new_branch_validated) (pv.peer_id, unknown_prefix.head_hash)
  in
  return_unit

let only_if_fitness_increases w distant_header hash cont =
  let open Lwt_syntax in
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.parameters.chain_db in
  let* known_valid = Store.Block.is_known_valid chain_store hash in
  if known_valid then (
    pv.last_validated_head <- distant_header ;
    cont `Known_valid)
  else
    let* current_head = Store.Chain.current_head chain_store in
    if
      Fitness.compare
        distant_header.Block_header.shell.fitness
        (Store.Block.fitness current_head)
      <= 0
    then (
      let* () = Events.(emit ignoring_head) (pv.peer_id, hash) in
      (* Don't download a branch that cannot beat the current head. *)
      let meta =
        Distributed_db.get_peer_metadata pv.parameters.chain_db pv.peer_id
      in
      Peer_metadata.incr meta Old_heads ;
      cont `Lower_fitness)
    else cont `Ok

let validate_new_head w hash (header : Block_header.t) =
  let open Lwt_result_syntax in
  let pv = Worker.state w in
  let block_received = (pv.peer_id, hash) in
  let*! () = Events.(emit fetching_operations_for_head) block_received in
  let* operations =
    List.map_ep
      (fun i ->
        protect ~canceler:(Worker.canceler w) (fun () ->
            Distributed_db.Operations.fetch
              ~timeout:pv.parameters.limits.block_operations_timeout
              pv.parameters.chain_db
              ~peer:pv.peer_id
              (hash, i)
              header.shell.operations_hash))
      (0 -- (header.shell.validation_passes - 1))
  in
  (* We redo a check for the fitness here because while waiting for the
     operations, a new head better than this block might be validated. *)
  only_if_fitness_increases w header hash @@ function
  | `Known_valid | `Lower_fitness ->
      (* If the block is known valid or if the fitness does not increase
         we need to clear the fetched operation of the block from the ddb *)
      List.iter
        (fun i ->
          Distributed_db.Operations.clear_or_cancel
            pv.parameters.chain_db
            (hash, i))
        (0 -- (header.shell.validation_passes - 1)) ;
      return_unit
  | `Ok -> (
      let*! () = Events.(emit requesting_new_head_validation) block_received in
      let*! v =
        Block_validator.validate
          ~notify_new_block:pv.parameters.notify_new_block
          ~precheck_and_notify:true
          pv.parameters.block_validator
          pv.parameters.chain_db
          hash
          header
          operations
      in
      match v with
      | Invalid errs ->
          (* This will convert into a kickban when treated by [on_error] --
             or, at least, by a worker termination which will close the
             connection. *)
          Lwt.return_error errs
      | Invalid_after_precheck _errs ->
          let*! () =
            Events.(emit ignoring_prechecked_invalid_block) block_received
          in
          (* We do not kickban the peer if the block received was
             successfully prechecked but invalid -- this means that he
             could have propagated a precheckable block before terminating
             its validation *)
          return_unit
      | Valid ->
          let*! () = Events.(emit new_head_validation_end) block_received in
          let meta =
            Distributed_db.get_peer_metadata pv.parameters.chain_db pv.peer_id
          in
          Peer_metadata.incr meta Valid_blocks ;
          return_unit)

let assert_acceptable_head w hash (header : Block_header.t) =
  let open Lwt_result_syntax in
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.parameters.chain_db in
  let*! acceptable =
    Store.Chain.is_acceptable_block chain_store (hash, header.shell.level)
  in
  fail_unless
    acceptable
    (Validation_errors.Checkpoint_error (hash, Some pv.peer_id))

let may_validate_new_head w hash (header : Block_header.t) =
  let open Lwt_result_syntax in
  let pv = Worker.state w in
  let chain_store = Distributed_db.chain_store pv.parameters.chain_db in
  let*! valid_block = Store.Block.is_known_valid chain_store hash in
  let*! invalid_block = Store.Block.is_known_invalid chain_store hash in
  let*! valid_predecessor =
    Store.Block.is_known_valid chain_store header.shell.predecessor
  in
  let*! invalid_predecessor =
    Store.Block.is_known_invalid chain_store header.shell.predecessor
  in
  let block_received = (pv.peer_id, hash) in
  if valid_block then
    let*! () =
      Events.(emit ignoring_previously_validated_block) block_received
    in
    return_unit
  else if invalid_block then
    let*! () = Events.(emit ignoring_invalid_block) block_received in
    tzfail Validation_errors.Known_invalid
  else if invalid_predecessor then
    let*! () = Events.(emit ignoring_invalid_block) block_received in
    let* _ =
      Distributed_db.commit_invalid_block
        pv.parameters.chain_db
        hash
        header
        [Validation_errors.Known_invalid]
    in
    tzfail Validation_errors.Known_invalid
  else if not valid_predecessor then (
    let*! () = Events.(emit missing_new_head_predecessor) block_received in
    Distributed_db.Request.current_branch pv.parameters.chain_db pv.peer_id ;
    return_unit)
  else
    only_if_fitness_increases w header hash @@ function
    | `Known_valid | `Lower_fitness -> return_unit
    | `Ok ->
        let* () = assert_acceptable_head w hash header in
        validate_new_head w hash header

let may_validate_new_branch w locator =
  let open Lwt_result_syntax in
  (* Make sure this is still ok w.r.t @phink fix *)
  let pv = Worker.state w in
  let {Block_locator.head_header = distant_header; head_hash = distant_hash; _}
      =
    locator
  in
  only_if_fitness_increases w distant_header distant_hash @@ function
  | `Known_valid | `Lower_fitness -> return_unit
  | `Ok -> (
      let* () = assert_acceptable_head w distant_hash distant_header in
      let chain_store = Distributed_db.chain_store pv.parameters.chain_db in
      (* TODO: should we consider level as well ? Rolling could have
         difficulties boostrapping. *)
      let block_received = (pv.peer_id, distant_hash) in
      let*! v =
        Block_locator.unknown_prefix
          ~is_known:(Store.Block.validity chain_store)
          locator
      in
      match v with
      | Known_valid, {history = []; _} -> return_unit
      | Known_valid, {history = [x]; head_header; head_hash}
        when Block_hash.equal x head_header.shell.predecessor ->
          validate_new_head w head_hash head_header
      | Known_valid, prefix_locator -> bootstrap_new_branch w prefix_locator
      | Unknown, _ ->
          (* May happen when:
             - A locator from another chain is received;
             - A rolling peer is too far ahead;
             - In rolling mode when the step is too wide. *)
          let*! () =
            Events.(emit ignoring_branch_without_common_ancestor) block_received
          in
          tzfail Validation_errors.Unknown_ancestor
      | Known_invalid, _ ->
          let*! () =
            Events.(emit ignoring_branch_with_invalid_locator) block_received
          in
          tzfail (Validation_errors.Invalid_locator (pv.peer_id, locator)))

let on_no_request w =
  let open Lwt_syntax in
  let pv = Worker.state w in
  Prometheus.Counter.inc_one metrics.on_no_request ;
  let timespan = pv.parameters.limits.new_head_request_timeout in
  let* () = Events.(emit no_new_head_from_peer) (pv.peer_id, timespan) in
  Distributed_db.Request.current_head_from_peer
    pv.parameters.chain_db
    pv.peer_id ;
  Lwt.return_unit

let on_request (type a b) w (req : (a, b) Request.t) : (a, b) result Lwt.t =
  let open Lwt_syntax in
  let pv = Worker.state w in
  match req with
  | Request.New_head (hash, header) ->
      let* () = Events.(emit processing_new_head) (pv.peer_id, hash) in
      may_validate_new_head w hash header
  | Request.New_branch (locator, _seed) ->
      (* TODO penalize empty locator... ?? *)
      let* () =
        Events.(emit processing_new_branch) (pv.peer_id, locator.head_hash)
      in
      may_validate_new_branch w locator

let on_completion (type a request_error) _w (r : (a, request_error) Request.t) _
    st =
  (match r with
  | Request.New_head _ -> Prometheus.Counter.inc_one metrics.new_head_completed
  | Request.New_branch _ ->
      Prometheus.Counter.inc_one metrics.new_branch_completed) ;

  Events.(emit request_completed) (Request.view r, st)

let on_error (type a b) w st (request : (a, b) Request.t) (err : b) :
    unit tzresult Lwt.t =
  let open Lwt_syntax in
  let pv = Worker.state w in
  let on_error_trace err =
    let request_view = Request.view request in
    match err with
    | (( Validation_errors.Invalid_locator _
       | Block_validator_errors.Invalid_block _ ) as e)
      :: _ ->
        let* () = Distributed_db.greylist pv.parameters.chain_db pv.peer_id in
        let* () =
          Events.(emit terminating_worker)
            (pv.peer_id, "invalid data received: kickban")
        in
        (match e with
        | Validation_errors.Invalid_locator _ ->
            Prometheus.Counter.inc_one metrics.invalid_locator
        | Block_validator_errors.Invalid_block _ ->
            Prometheus.Counter.inc_one metrics.invalid_block
        | _ -> (* Cannot happen but OCaml type checker disagrees. *) ()) ;
        Worker.trigger_shutdown w ;
        let* () = Events.(emit request_error) (request_view, st, err) in
        Lwt.return_error err
    | Block_validator_errors.System_error _ :: _ ->
        Prometheus.Counter.inc_one metrics.system_error ;
        let* () = Events.(emit request_error) (request_view, st, err) in
        return_ok_unit
    | Block_validator_errors.Unavailable_protocol {protocol; _} :: _ -> (
        Prometheus.Counter.inc_one metrics.unavailable_protocol ;
        let* fetched_and_compiled =
          Block_validator.fetch_and_compile_protocol
            pv.parameters.block_validator
            ~peer:pv.peer_id
            ~timeout:pv.parameters.limits.protocol_timeout
            protocol
        in
        match fetched_and_compiled with
        | Ok _ ->
            Distributed_db.Request.current_head_from_peer
              pv.parameters.chain_db
              pv.peer_id ;
            return_ok_unit
        | Error _ ->
            (* TODO: https://gitlab.com/tezos/tezos/-/issues/3061
               should we punish here ? *)
            let* () =
              Events.(emit terminating_worker)
                ( pv.peer_id,
                  Format.asprintf
                    "missing protocol: %a"
                    Protocol_hash.pp
                    protocol )
            in
            let* () = Events.(emit request_error) (request_view, st, err) in
            Lwt.return_error err)
    | (( Validation_errors.Unknown_ancestor
       | Validation_errors.Too_short_locator _ ) as e)
      :: _ ->
        (match e with
        | Validation_errors.Unknown_ancestor ->
            Prometheus.Counter.inc_one metrics.unknown_ancestor
        | Validation_errors.Too_short_locator _ ->
            Prometheus.Counter.inc_one metrics.too_short_locator
        | _ -> (* Cannot happen but OCaml type checker disagrees. *) ()) ;
        let* () =
          Events.(emit terminating_worker)
            ( pv.peer_id,
              Format.asprintf "unknown ancestor or too short locator: kick" )
        in
        let* () = Events.(emit request_error) (request_view, st, err) in
        Worker.trigger_shutdown w ;
        return_ok_unit
    | Distributed_db.Operations.Canceled _ :: _ -> (
        (* Given two nodes A and B (remote). This may happen if A
           prechecks a block, sends it to B. B prechecks a block, sends
           it to A. A tries to fetch operations of the block to B, in
           the meantime, A validates the block and cancels the fetching.
        *)
        match request_view with
        | New_head hash -> (
            let chain_store =
              Distributed_db.chain_store pv.parameters.chain_db
            in
            let* b = Store.Block.is_known_valid chain_store hash in
            match b with
            | true ->
                Prometheus.Counter.inc_one
                  metrics.operations_fetching_canceled_new_known_valid_head ;
                return_ok_unit
            | false ->
                Prometheus.Counter.inc_one
                  metrics.operations_fetching_canceled_new_unknown_head ;
                Lwt.return_error err)
        | _ ->
            Prometheus.Counter.inc_one
              metrics.operations_fetching_canceled_new_branch ;
            Lwt.return_error err)
    | _ ->
        Prometheus.Counter.inc_one metrics.unknown_error ;
        let* () = Events.(emit request_error) (request_view, st, err) in
        Lwt.return_error err
  in
  match request with
  | New_head _ -> on_error_trace err
  | New_branch _ -> on_error_trace err

let on_close w =
  let pv = Worker.state w in
  (* We request the the P2P layer to disconnect from this peer, but we
     do not wait for the disconnection to be effective (the underlying
     socket is closed). This is because the P2P layer does not offer
     the guarantee that the disconnection will be quick.

     This is not a problem, and allows the reconnection with this peer
     using a different socket. *)
  let (_ : unit Lwt.t) =
    Distributed_db.disconnect pv.parameters.chain_db pv.peer_id
  in
  pv.parameters.notify_termination () ;
  Lwt.return_unit

type launch_error = |

let on_launch _ name parameters : (_, launch_error) result Lwt.t =
  let open Lwt_syntax in
  let chain_store = Distributed_db.chain_store parameters.chain_db in
  let* genesis = Store.Chain.genesis_block chain_store in
  (* TODO : why do we have genesis and not current_head here ?? *)
  let rec pv =
    {
      peer_id = snd name;
      parameters = {parameters with notify_new_block};
      pipeline = None;
      last_validated_head = Store.Block.header genesis;
      last_advertised_head = Store.Block.header genesis;
    }
  and notify_new_block ({block; _} as new_block) =
    pv.last_validated_head <- Store.Block.header block ;
    parameters.notify_new_block new_block
  in
  Prometheus.Counter.inc_one metrics.connections ;
  Lwt.return (Ok pv)

let table =
  let merge w (Worker.Any_request neu) old =
    let pv = Worker.state w in
    match neu with
    | Request.New_branch (locator, _) ->
        pv.last_advertised_head <- locator.Block_locator.head_header ;
        Some (Worker.Any_request neu)
    | Request.New_head (_, header) -> (
        pv.last_advertised_head <- header ;
        (* TODO penalize decreasing fitness *)
        match old with
        | Some (Worker.Any_request (Request.New_branch _) as old) ->
            Some old (* ignore *)
        | Some (Worker.Any_request (Request.New_head _)) ->
            Some (Any_request neu)
        | None -> Some (Any_request neu))
  in
  Worker.create_table (Dropbox {merge})

let create ?(notify_new_block = fun _ -> ()) ?(notify_termination = fun _ -> ())
    limits block_validator chain_db peer_id =
  let name =
    (Store.Chain.chain_id (Distributed_db.chain_store chain_db), peer_id)
  in
  let parameters =
    {chain_db; notify_termination; block_validator; notify_new_block; limits}
  in
  let module Handlers = struct
    type self = t

    type nonrec launch_error = launch_error

    let on_launch = on_launch

    let on_request = on_request

    let on_close = on_close

    let on_error = on_error

    let on_completion = on_completion

    let on_no_request = on_no_request
  end in
  let open Lwt_syntax in
  let* (Ok worker) =
    Worker.launch
      table
      ~timeout:limits.new_head_request_timeout
      name
      parameters
      (module Handlers)
  in
  Lwt.return worker

let notify_branch w locator =
  let pv = Worker.state w in
  let sender_id = Distributed_db.my_peer_id pv.parameters.chain_db in
  (* sender and receiver are inverted here because they are from
     the point of view of the node sending the locator *)
  let seed = {Block_locator.sender_id = pv.peer_id; receiver_id = sender_id} in
  Worker.Dropbox.put_request w (New_branch (locator, seed))

let notify_head w hash header =
  Worker.Dropbox.put_request w (New_head (hash, header))

let shutdown w = Worker.shutdown w

let peer_id w =
  let pv = Worker.state w in
  pv.peer_id

let status = Worker.status

let information = Worker.information

let running_workers () = Worker.list table

let current_request t = Worker.current_request t

let pipeline_length w =
  let state = Worker.state w in
  Types.pipeline_length state.pipeline

module Internal_for_tests = struct
  let validate_new_head (t : t) block_hash block_header =
    let open Lwt_result_syntax in
    let* () = validate_new_head t block_hash block_header in
    return_unit
end
