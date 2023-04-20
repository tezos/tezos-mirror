(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

open Block_validator_worker_state
open Block_validator_errors

type validation_result =
  | Already_committed
  | Outdated_block
  | Validated
  | Validation_error of error trace
  | Preapplied of (Block_header.shell_header * error Preapply_result.t list)
  | Preapplication_error of error trace
  | Validation_error_after_precheck of error trace
  | Precheck_failed of error trace

type new_block = {
  block : Store.Block.t;
  resulting_context_hash : Context_hash.t;
}

module Block_hash_ring =
  Aches.Vache.Map (Aches.Vache.FIFO_Precise) (Aches.Vache.Strong) (Block_hash)

module Name = struct
  type t = unit

  let encoding = Data_encoding.empty

  let base = ["validator"; "block"]

  let pp _ () = ()

  let equal () () = true
end

module Types = struct
  type state = {
    protocol_validator : Protocol_validator.t;
    validation_process : Block_validator_process.t;
    limits : Shell_limits.block_validator_limits;
    start_testchain : bool;
    invalid_blocks_after_precheck : error trace Block_hash_ring.t;
  }

  type parameters =
    Shell_limits.block_validator_limits
    * bool
    * Distributed_db.t
    * Block_validator_process.t
end

module Request = struct
  include Request

  type validation_request = {
    chain_db : Distributed_db.chain_db;
    notify_new_block : new_block -> unit;
    canceler : Lwt_canceler.t option;
    peer : P2p_peer.Id.t option;
    hash : Block_hash.t;
    header : Block_header.t;
    operations : Operation.t list list;
    precheck_and_notify : bool;
  }

  type preapplication_request = {
    chain_store : Store.chain_store;
    canceler : Lwt_canceler.t option;
    predecessor : Store.Block.t;
    timestamp : Time.Protocol.t;
    protocol_data : bytes;
    operations : Operation.t list list;
  }

  type ('a, 'b) t =
    | Request_validation :
        validation_request
        -> (validation_result, error trace) t
    | Request_preapplication :
        preapplication_request
        -> (validation_result, error trace) t

  let validation_view {chain_db; peer; hash; _} =
    let chain_store = Distributed_db.chain_store chain_db in
    let chain_id = Store.Chain.chain_id chain_store in
    {chain_id; block = hash; peer}

  let preapplication_view {chain_store; predecessor; _} =
    let chain_id = Store.Chain.chain_id chain_store in
    let level = Int32.succ (Store.Block.level predecessor) in
    {chain_id; level}

  let view : type a b. (a, b) t -> view = function
    | Request_validation r -> Validation (validation_view r)
    | Request_preapplication r -> Preapplication (preapplication_view r)
end

module Events = Block_validator_events
module Worker = Worker.MakeSingle (Name) (Request) (Types)

type t = Worker.infinite Worker.queue Worker.t

let check_chain_liveness chain_db hash (header : Block_header.t) =
  let open Lwt_result_syntax in
  let chain_store = Distributed_db.chain_store chain_db in
  match Store.Chain.expiration chain_store with
  | Some eol when Time.Protocol.(eol <= header.shell.timestamp) ->
      let error =
        Expired_chain
          {
            chain_id = Store.Chain.chain_id chain_store;
            expiration = eol;
            timestamp = header.shell.timestamp;
          }
      in
      tzfail (invalid_block hash error)
  | None | Some _ -> return_unit

let precheck_block bvp chain_db chain_store ~predecessor block_header block_hash
    operations =
  let open Lwt_result_syntax in
  let*! () = Events.(emit prechecking_block) block_hash in
  let* () =
    Block_validator_process.precheck_block
      bvp
      chain_store
      ~predecessor
      block_header
      block_hash
      operations
  in
  let*! () = Events.(emit prechecked_block) block_hash in
  (* Add the block and operations to the cache of the ddb to make them
     available to our peers *)
  Distributed_db.inject_prechecked_block
    chain_db
    block_hash
    block_header
    operations

let on_validation_request w
    {
      Request.chain_db;
      notify_new_block;
      canceler;
      peer;
      hash;
      header;
      operations;
      precheck_and_notify;
    } =
  let open Lwt_result_syntax in
  let bv = Worker.state w in
  let chain_store = Distributed_db.chain_store chain_db in
  let*! b = Store.Block.is_known_valid chain_store hash in
  match b with
  | true -> return Already_committed
  | false -> (
      let*! r =
        match
          Block_hash_ring.find_opt bv.invalid_blocks_after_precheck hash
        with
        | Some errs ->
            (* If the block is invalid but has been previously
               successfuly prechecked, we directly return with the cached
               errors. This way, multiple propagation won't happen. *)
            return (Validation_error_after_precheck errs)
        | None -> (
            let*! o = Store.Block.read_invalid_block_opt chain_store hash in
            match o with
            | Some {errors; _} -> return (Validation_error errors)
            | None -> (
                let*! checkpoint = Store.Chain.checkpoint chain_store in
                (* Safety and late workers in partial mode. *)
                if Compare.Int32.(header.shell.level < snd checkpoint) then
                  return Outdated_block
                else
                  let* pred =
                    Store.Block.read_block chain_store header.shell.predecessor
                  in
                  let with_retry_to_load_protocol f =
                    let*! r = f () in
                    match r with
                    (* [Unavailable_protocol] is expected to be the
                       first error in the trace *)
                    | Error (Unavailable_protocol {protocol; _} :: _) ->
                        let* _ =
                          Protocol_validator.fetch_and_compile_protocol
                            bv.protocol_validator
                            ?peer
                            ~timeout:bv.limits.protocol_timeout
                            protocol
                        in
                        f ()
                    | _ -> Lwt.return r
                  in
                  let*! r =
                    protect ~canceler:(Worker.canceler w) (fun () ->
                        protect ?canceler (fun () ->
                            with_retry_to_load_protocol (fun () ->
                                precheck_block
                                  bv.validation_process
                                  chain_db
                                  chain_store
                                  ~predecessor:pred
                                  header
                                  hash
                                  operations)))
                  in
                  match r with
                  | Error errs -> return (Precheck_failed errs)
                  | Ok () -> (
                      if precheck_and_notify then
                        (* Headers which have been preapplied can be advertised
                           before being fully applied. *)
                        Distributed_db.Advertise.prechecked_head chain_db header ;
                      let* result =
                        protect ~canceler:(Worker.canceler w) (fun () ->
                            protect ?canceler (fun () ->
                                let*! () =
                                  Events.(emit validating_block) hash
                                in
                                with_retry_to_load_protocol (fun () ->
                                    Block_validator_process.apply_block
                                      ~should_precheck:false
                                      bv.validation_process
                                      chain_store
                                      ~predecessor:pred
                                      header
                                      operations)))
                      in
                      Shell_metrics.Block_validator
                      .set_operation_per_pass_collector
                        (fun () ->
                          List.map
                            (fun v -> Int.to_float (List.length v))
                            operations) ;
                      let* o =
                        Distributed_db.commit_block
                          chain_db
                          hash
                          header
                          operations
                          result
                      in
                      match o with
                      | Some block ->
                          notify_new_block
                            {
                              block;
                              resulting_context_hash =
                                result.validation_store.resulting_context_hash;
                            } ;
                          return Validated
                      | None -> return Already_committed)))
      in
      match r with
      | Ok r -> return r
      | Error errs ->
          let* () =
            if
              (not precheck_and_notify)
              && List.exists
                   (function Invalid_block _ -> true | _ -> false)
                   errs
            then
              protect ~canceler:(Worker.canceler w) (fun () ->
                  Distributed_db.commit_invalid_block chain_db hash header errs)
            else return_unit
          in
          if precheck_and_notify then (
            Block_hash_ring.replace bv.invalid_blocks_after_precheck hash errs ;
            return (Validation_error_after_precheck errs))
          else return (Validation_error errs))

let on_preapplication_request w
    {
      Request.chain_store;
      canceler;
      predecessor;
      timestamp;
      protocol_data;
      operations;
    } =
  let open Lwt_syntax in
  let bv = Worker.state w in
  let* r =
    protect ~canceler:(Worker.canceler w) (fun () ->
        protect ?canceler (fun () ->
            Block_validator_process.preapply_block
              bv.validation_process
              chain_store
              ~predecessor
              ~protocol_data
              ~timestamp
              operations))
  in
  match r with
  | Ok res -> return_ok (Preapplied res)
  | Error errs -> return_ok (Preapplication_error errs)

let metrics = Shell_metrics.Block_validator.init Name.base

let on_request :
    type r request_error.
    t -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
 fun w r ->
  Prometheus.Counter.inc_one metrics.worker_counters.worker_request_count ;
  match r with
  | Request.Request_validation r -> on_validation_request w r
  | Request.Request_preapplication r -> on_preapplication_request w r

type launch_error = |

let on_launch _ _ (limits, start_testchain, db, validation_process) :
    (_, launch_error) result Lwt.t =
  let protocol_validator = Protocol_validator.create db in
  let invalid_blocks_after_precheck = Block_hash_ring.create 50 in
  Lwt.return_ok
    {
      Types.protocol_validator;
      validation_process;
      limits;
      start_testchain;
      invalid_blocks_after_precheck;
    }

let on_error (type a b) (_w : t) st (r : (a, b) Request.t) (errs : b) =
  let open Lwt_syntax in
  Prometheus.Counter.inc_one metrics.worker_counters.worker_error_count ;
  match r with
  | Request_validation v ->
      let view = Request.validation_view v in
      let* () =
        match errs with
        | [Canceled] ->
            (* Ignore requests cancelation *)
            Lwt.return_unit
        | errs -> Events.(emit validation_failure) (view.block, st, errs)
      in
      (* Keep the worker alive. *)
      return_ok_unit
  | Request_preapplication v ->
      let view = Request.preapplication_view v in
      let* () = Events.(emit preapplication_failure) (view.level, st, errs) in
      (* Keep the worker alive. *)
      return_ok_unit

let on_completion :
    type a b.
    t -> (a, b) Request.t -> a -> Worker_types.request_status -> unit Lwt.t =
 fun _w request v st ->
  let open Lwt_syntax in
  Prometheus.Counter.inc_one metrics.worker_counters.worker_completion_count ;
  match (request, v) with
  | Request.Request_validation {hash; _}, Already_committed ->
      Prometheus.Counter.inc_one metrics.already_commited_blocks_count ;
      let* () = Events.(emit previously_validated) hash in
      Lwt.return_unit
  | Request.Request_validation {hash; _}, Outdated_block ->
      Prometheus.Counter.inc_one metrics.outdated_blocks_count ;
      let* () = Events.(emit previously_validated) hash in
      Lwt.return_unit
  | Request.Request_validation _, Validated -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.validated_blocks_count ;
      match Request.view request with
      | Validation v -> Events.(emit validation_success) (v.block, st)
      | _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Validation_error errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.validation_errors_count ;
      match Request.view request with
      | Validation v -> (
          match errs with
          | [Canceled] ->
              (* Ignore requests cancellation *)
              Lwt.return_unit
          | errs -> Events.(emit validation_failure) (v.block, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_preapplication _, Preapplied _ -> (
      Prometheus.Counter.inc_one metrics.preapplied_blocks_count ;
      match Request.view request with
      | Preapplication v -> Events.(emit preapplication_success) (v.level, st)
      | _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_preapplication _, Preapplication_error errs -> (
      Prometheus.Counter.inc_one metrics.preapplication_errors_count ;
      match Request.view request with
      | Preapplication v ->
          Events.(emit preapplication_failure) (v.level, st, errs)
      | _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Validation_error_after_precheck errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.validation_errors_after_precheck_count ;
      match Request.view request with
      | Validation v ->
          Events.(emit validation_failure_after_precheck) (v.block, st, errs)
      | _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Precheck_failed errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.precheck_failed_count ;
      match Request.view request with
      | Validation v -> (
          match errs with
          | [Canceled] ->
              (* Ignore requests cancellation *)
              Lwt.return_unit
          | errs -> Events.(emit precheck_failure) (v.block, st, errs))
      | _ -> (* assert false *) Lwt.return_unit)
  | _ -> (* assert false *) Lwt.return_unit

let on_close w =
  let bv = Worker.state w in
  Block_validator_process.close bv.validation_process

let table = Worker.create_table Queue

let create limits db validation_process ~start_testchain =
  let module Handlers = struct
    type self = t

    type nonrec launch_error = launch_error

    let on_launch = on_launch

    let on_request = on_request

    let on_close = on_close

    let on_error = on_error

    let on_completion = on_completion

    let on_no_request _ = Lwt.return_unit
  end in
  let open Lwt_syntax in
  let* (Ok worker) =
    Worker.launch
      table
      ()
      (limits, start_testchain, db, validation_process)
      (module Handlers)
  in
  Lwt.return worker

let shutdown = Worker.shutdown

type block_validity =
  | Valid
  | Invalid_after_precheck of error trace
  | Invalid of error trace

let validate w ?canceler ?peer ?(notify_new_block = fun _ -> ())
    ?(precheck_and_notify = false) chain_db hash (header : Block_header.t)
    operations =
  let open Lwt_syntax in
  let chain_store = Distributed_db.chain_store chain_db in
  let* b = Store.Block.is_known_valid chain_store hash in
  match b with
  | true ->
      let* () = Events.(emit previously_validated) hash in
      return Valid
  | false -> (
      let* r =
        let open Lwt_result_syntax in
        let hashes = List.map (List.map Operation.hash) operations in
        let computed_hash =
          Operation_list_list_hash.compute
            (List.map Operation_list_hash.compute hashes)
        in
        let* () =
          fail_when
            (Operation_list_list_hash.compare
               computed_hash
               header.shell.operations_hash
            <> 0)
            (Inconsistent_operations_hash
               {
                 block = hash;
                 expected = header.shell.operations_hash;
                 found = computed_hash;
               })
          |> Lwt_result.map_error (fun e -> Worker.Request_error e)
        in
        let* () =
          check_chain_liveness chain_db hash header
          |> Lwt_result.map_error (fun e -> Worker.Request_error e)
        in
        Worker.Queue.push_request_and_wait
          w
          (Request_validation
             {
               chain_db;
               notify_new_block;
               canceler;
               peer;
               hash;
               header;
               operations;
               precheck_and_notify;
             })
      in
      match r with
      | Ok (Validated | Already_committed | Outdated_block) -> return Valid
      | Ok (Validation_error_after_precheck errs) ->
          return (Invalid_after_precheck errs)
      | Ok (Precheck_failed errs)
      | Ok (Validation_error errs)
      | Error (Request_error errs) ->
          return (Invalid errs)
      | Error (Closed None) -> return (Invalid [Worker_types.Terminated])
      | Error (Closed (Some errs)) -> return (Invalid errs)
      | Error (Any exn) -> return (Invalid [Exn exn])
      | _ ->
          (* preapplication cases *)
          assert false)

let preapply w ?canceler chain_store ~predecessor ~timestamp ~protocol_data
    operations =
  let open Lwt_syntax in
  let* r =
    Worker.Queue.push_request_and_wait
      w
      (Request_preapplication
         {
           chain_store;
           canceler;
           predecessor;
           timestamp;
           protocol_data;
           operations;
         })
  in
  match r with
  | Ok (Preapplied res) -> return_ok res
  | Ok (Preapplication_error errs) -> Lwt.return_error errs
  | Error (Request_error errs) -> Lwt.return_error errs
  | Error (Closed None) -> Lwt.return_error [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> Lwt.return_error errs
  | Error (Any exn) -> Lwt.return_error [Exn exn]
  | _ ->
      (* validation cases *)
      assert false

let context_garbage_collection w index context_hash ~gc_lockfile_path =
  let bv = Worker.state w in
  Block_validator_process.context_garbage_collection
    bv.validation_process
    index
    context_hash
    ~gc_lockfile_path

let context_split w index =
  let bv = Worker.state w in
  Block_validator_process.context_split bv.validation_process index

let fetch_and_compile_protocol w =
  let bv = Worker.state w in
  Protocol_validator.fetch_and_compile_protocol bv.protocol_validator

let status = Worker.status

let running_worker () =
  match Worker.list table with
  | [(_, single)] -> single
  | [] -> raise Not_found
  | _ :: _ :: _ ->
      (* NOTE: names of workers must be unique, [Name.t = unit] which has only
         one inhabitant. *)
      assert false

let pending_requests t = Worker.Queue.pending_requests t

let current_request t = Worker.current_request t
