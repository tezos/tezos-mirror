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

module Profiler = (val Profiler.wrap Shell_profiling.block_validator_profiler)

type validation_result =
  | Already_committed
  | Already_known_invalid of error trace
  | Validated_and_applied
  | Preapplied of (Block_header.shell_header * error Preapply_result.t list)
  | Preapplication_error of error trace
  | Application_error_after_validation of error trace
  | Validation_failed of error trace
  | Commit_block_failed of error trace

type validate_block_result = Validated | Validation_error of error trace

type apply_block_result =
  | Applied of Block_validation.result
  | Application_error of error trace

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
    inapplicable_blocks_after_validation : error trace Block_hash_ring.t;
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
    advertise_after_validation : bool;
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

let check_operations_merkle_root hash header operations =
  let open Result_syntax in
  let fail_unless b e = if b then return_unit else tzfail e in
  let computed_hash =
    let hashes = List.map (List.map Operation.hash) operations in
    Operation_list_list_hash.compute
      (List.map Operation_list_hash.compute hashes)
  in
  fail_unless
    (Operation_list_list_hash.equal
       computed_hash
       header.Block_header.shell.operations_hash)
    (Inconsistent_operations_hash
       {
         block = hash;
         expected = header.shell.operations_hash;
         found = computed_hash;
       })

(* [with_retry_to_load_protocol bv peer f] tries to call [f], if it fails with an
   [Unavailable_protocol] error, it fetches the protocol from the [peer] and retries
   to call [f] *)
let with_retry_to_load_protocol (bv : Types.state) ~peer f =
  let open Lwt_syntax in
  let* r = f () in
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

let validate_block worker ?canceler bv peer chain_db chain_store ~predecessor
    block_header block_hash operations bv_operations =
  let open Lwt_result_syntax in
  let*! () =
    let block_header = block_header.Block_header.shell in
    Events.(emit validating_block)
      ( block_hash,
        block_header.level,
        block_header.predecessor,
        block_header.fitness,
        block_header.timestamp )
  in
  let*! r =
    protect ~canceler:(Worker.canceler worker) (fun () ->
        protect ?canceler (fun () ->
            with_retry_to_load_protocol bv ~peer (fun () ->
                let* () =
                  Block_validator_process.validate_block
                    bv.validation_process
                    chain_store
                    ~predecessor
                    block_header
                    block_hash
                    bv_operations
                in
                let*! () = Events.(emit validated_block) block_hash in
                (* Add the block and operations to the cache of the ddb to make them
                   available to our peers *)
                Distributed_db.inject_validated_block
                  chain_db
                  block_hash
                  block_header
                  operations)))
  in
  match r with
  | Error errs -> Lwt.return (Validation_error errs)
  | Ok () -> Lwt.return Validated

let errors_contains_context_error errors =
  let rex =
    (* Matching all the candidate to a context error:
       - "[Ir]min|[Bb]rassaia": for any error from irmin or brassaia components
       - "unknown inode key": to catch the so called inode error *)
    Re.compile (Re.Perl.re "[Ii]rmin|[Bb]rassaia|unknown inode key")
  in
  let is_context_error error =
    let error_s = Format.asprintf "%a" Error_monad.pp error in
    match Re.exec rex error_s with exception Not_found -> false | _ -> true
  in
  List.exists is_context_error errors

let apply_block worker ?canceler bv peer chain_store ~predecessor block_header
    block_hash bv_operations =
  let open Lwt_result_syntax in
  let rec apply_block ~retry =
    let*! () = Events.(emit applying_block) block_hash in
    let*! r =
      protect ~canceler:(Worker.canceler worker) (fun () ->
          protect ?canceler (fun () ->
              with_retry_to_load_protocol bv ~peer (fun () ->
                  Block_validator_process.apply_block
                    ~should_validate:false
                    bv.validation_process
                    chain_store
                    ~predecessor
                    block_header
                    bv_operations)))
    in
    match r with
    | Error errs ->
        (* This is a mitigation for context errors. If the application of the
           block fails, we retry once. The external validator is shut down and
           will restart on the new application request sent. This is done to
           force a full reload of the context.

           If the re-application fails, the errors will be raised and the node
           will shutdown gracefully. *)
        if retry && errors_contains_context_error errs then
          match Block_validator_process.kind bv.validation_process with
          | Block_validator_process.External_process -> (
              let*! () =
                Events.(emit context_error_at_block_application)
                  (block_hash, errs)
              in
              let*! r = Block_validator_process.restart bv.validation_process in
              match r with
              | Ok () ->
                  let*! () = Events.(emit retry_block_application) block_hash in
                  apply_block ~retry:false
              | Error errs ->
                  (* If the validator cannot be restarted we are doomed. *)
                  Lwt.return (Application_error errs))
          | Single_process ->
              (* If the node is configured in single process mode we cannot
                 mitigate. The application error is directly raised and the node
                 will be shut down gracefully. *)
              Lwt.return (Application_error errs)
        else Lwt.return (Application_error errs)
    | Ok application_result -> Lwt.return (Applied application_result)
  in
  apply_block ~retry:true

let commit_and_notify_block notify_new_block chain_db hash header operations
    application_result =
  let open Lwt_result_syntax in
  let*! r =
    Distributed_db.commit_block
      chain_db
      hash
      header
      operations
      application_result
  in
  match r with
  | Ok (Some block) ->
      notify_new_block
        {
          block;
          resulting_context_hash =
            application_result.validation_store.resulting_context_hash;
        } ;
      return Validated_and_applied
  | Ok None -> return Already_committed
  | Error errs -> return (Commit_block_failed errs)

(* Commit the block as invalid in the store if an [Invalid_block] error is found
   in the error trace. *)
let may_commit_invalid_block worker chain_db hash header errs =
  let open Lwt_result_syntax in
  if List.exists (function Invalid_block _ -> true | _ -> false) errs then
    let*! r =
      protect ~canceler:(Worker.canceler worker) (fun () ->
          Distributed_db.commit_invalid_block chain_db hash header errs)
    in
    match r with
    | Ok () -> return (Validation_failed errs)
    | Error errs -> return (Commit_block_failed errs)
  else return (Validation_failed errs)

let[@warning "-32"] profiling_validation_prefix hash info =
  Format.sprintf
    "on_validation_request : %s"
    (Block_hash.to_short_b58check hash)
  :: info

let on_validation_request w
    {
      Request.chain_db;
      notify_new_block;
      canceler;
      peer;
      hash;
      header;
      operations;
      advertise_after_validation;
    } =
  let open Lwt_result_syntax in
  let bv = Worker.state w in
  let chain_store = Distributed_db.chain_store chain_db in
  let*! b = Store.Block.is_known_valid chain_store hash in
  match b with
  | true ->
      return
        Already_committed
      [@profiler.mark
        {verbosity = Notice; metadata = [("prometheus", "already commited")]}
          (profiling_validation_prefix hash ["already commited"])]
  | false -> (
      (* This check might be redundant as operation paths are already
         checked when each pass is received from the network. However,
         removing it would prevent checking locally
         injected/reconstructed blocks which might be problematic. *)
      let*? () =
        (check_operations_merkle_root
           hash
           header
           operations
         [@profiler.span_f
           {
             verbosity = Notice;
             metadata = [("prometheus", "check_operations_merkle_root")];
           }
             (profiling_validation_prefix hash ["check_operations_merkle_root"])])
      in
      match
        Block_hash_ring.find_opt bv.inapplicable_blocks_after_validation hash
      with
      | Some errs ->
          (* If the block is inapplicable but has been previously
             successfuly validated, we directly return with the cached
             errors. This way, multiple propagation won't happen. *)
          return (Application_error_after_validation errs)
      | None -> (
          let*! o = Store.Block.read_invalid_block_opt chain_store hash in
          match o with
          | Some {errors; _} -> return (Already_known_invalid errors)
          | None -> (
              let* pred =
                (Store.Block.read_block
                   chain_store
                   header.shell.predecessor
                 [@profiler.span_s
                   {
                     verbosity = Notice;
                     metadata = [("prometheus", "read predecessor")];
                   }
                     (profiling_validation_prefix hash ["read predecessor"])])
              in
              let*! mempool = Store.Chain.mempool chain_store in
              let bv_operations =
                List.map
                  (List.map
                     (Block_validation.mk_operation
                        ~known_valid_operation_set:mempool.known_valid))
                  operations
              in
              let*! r =
                (validate_block
                   w
                   ?canceler
                   bv
                   peer
                   chain_db
                   chain_store
                   ~predecessor:pred
                   header
                   hash
                   operations
                   bv_operations
                 [@profiler.span_s
                   {
                     verbosity = Notice;
                     metadata = [("prometheus", "validate_block")];
                   }
                     (profiling_validation_prefix hash ["validate_block"])])
              in
              match r with
              | Validation_error errs ->
                  may_commit_invalid_block w chain_db hash header errs
              | Validated -> (
                  let () =
                    if advertise_after_validation then
                      (* Headers which have been preapplied can be advertised
                         before being fully applied. *)
                      Distributed_db.Advertise.validated_head chain_db header
                  in
                  let*! r =
                    (apply_block
                       w
                       ?canceler
                       bv
                       peer
                       chain_store
                       ~predecessor:pred
                       header
                       hash
                       bv_operations
                     [@profiler.span_s
                       {
                         verbosity = Notice;
                         metadata = [("prometheus", "apply_block")];
                       }
                         (profiling_validation_prefix hash ["apply_block"])])
                  in
                  match r with
                  | Application_error errs ->
                      if
                        List.exists
                          (function
                            | Exn Lwt.Canceled | Canceled -> false | _ -> true)
                          errs
                      then
                        (* If an error occurs at application that is not a
                           cancellation of the request, the block_hash is
                           registered in [inapplicable_blocks_after_validation]
                           to avoid validating and advertising it again in the
                           future *)
                        Block_hash_ring.replace
                          bv.inapplicable_blocks_after_validation
                          hash
                          errs ;
                      return (Application_error_after_validation errs)
                  | Applied application_result ->
                      Shell_metrics.Block_validator
                      .set_operation_per_pass_collector
                        (fun () ->
                          List.map
                            (fun v -> Int.to_float (List.length v))
                            operations) ;
                      commit_and_notify_block
                        notify_new_block
                        chain_db
                        hash
                        header
                        operations
                        application_result
                      [@profiler.span_s
                        {
                          verbosity = Notice;
                          metadata = [("prometheus", "commit_and_notify_block")];
                        }
                          (profiling_validation_prefix
                             hash
                             ["commit_and_notify_block"])]))))

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
            let* mempool = Store.Chain.mempool chain_store in
            let operations =
              List.map
                (List.map
                   (Block_validation.mk_operation
                      ~known_valid_operation_set:mempool.known_valid))
                operations
            in
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

let on_request : type r request_error.
    t -> (r, request_error) Request.t -> (r, request_error) result Lwt.t =
 fun w r ->
  Prometheus.Counter.inc_one metrics.worker_counters.worker_request_count ;
  match r with
  | Request.Request_validation r ->
      on_validation_request
        w
        r
      [@profiler.span_s
        {
          verbosity = Notice;
          metadata = [("prometheus", "on_validation_request")];
        }
          [
            Format.sprintf
              "on_validation_request : %s"
              (Block_hash.to_short_b58check r.hash);
          ]]
  | Request.Request_preapplication r ->
      on_preapplication_request
        w
        r
      [@profiler.span_s
        {
          verbosity = Notice;
          metadata = [("prometheus", "on_preapplication_request")];
        }
          ["on_preapplication_request"]]

type launch_error = |

let on_launch _ _ (limits, start_testchain, db, validation_process) :
    (_, launch_error) result Lwt.t =
  let protocol_validator = Protocol_validator.create db in
  let inapplicable_blocks_after_validation = Block_hash_ring.create 50 in
  Lwt.return_ok
    {
      Types.protocol_validator;
      validation_process;
      limits;
      start_testchain;
      inapplicable_blocks_after_validation;
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
            Events.(emit validation_canceled) (view.block, st)
        | errs -> Events.(emit validation_failure) (view.block, st, errs)
      in
      (* Keep the worker alive. *)
      return_ok_unit
  | Request_preapplication v ->
      let view = Request.preapplication_view v in
      let* () = Events.(emit preapplication_failure) (view.level, st, errs) in
      (* Keep the worker alive. *)
      return_ok_unit

(* This failsafe aims to look for a context error that is known to be
   critical and, if found, stop the node gracefully. *)
let check_and_quit_on_context_errors errors =
  let open Lwt_syntax in
  if errors_contains_context_error errors then
    let* () = Events.(emit stopping_node_missing_context_key ()) in
    let* _ = Lwt_exit.exit_and_wait 1 in
    return_unit
  else return_unit

let on_completion : type a b.
    t -> (a, b) Request.t -> a -> Worker_types.request_status -> unit Lwt.t =
 fun _w request v st ->
  let open Lwt_syntax in
  Prometheus.Counter.inc_one metrics.worker_counters.worker_completion_count ;
  match (request, v) with
  | Request.Request_validation {hash; _}, Already_committed ->
      Prometheus.Counter.inc_one metrics.already_commited_blocks_count ;
      let* () = Events.(emit previously_validated_and_applied) hash in
      Lwt.return_unit
  | Request.Request_validation {hash; _}, Already_known_invalid errs ->
      Prometheus.Counter.inc_one metrics.already_known_invalid_blocks_count ;
      let* () = Events.(emit previously_invalid_block) (hash, errs) in
      Lwt.return_unit
  | Request.Request_validation _, Validated_and_applied -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.validated_blocks_count ;
      match Request.view request with
      | Validation v ->
          Events.(emit validation_and_application_success) (v.block, st)
      | Preapplication _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_preapplication _, Preapplied _ -> (
      Prometheus.Counter.inc_one metrics.preapplied_blocks_count ;
      match Request.view request with
      | Preapplication v -> Events.(emit preapplication_success) (v.level, st)
      | Validation _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_preapplication _, Preapplication_error errs -> (
      Prometheus.Counter.inc_one metrics.preapplication_errors_count ;
      match Request.view request with
      | Preapplication v ->
          let* () = Events.(emit preapplication_failure) (v.level, st, errs) in
          let* () = check_and_quit_on_context_errors errs in
          return_unit
      | Validation _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Application_error_after_validation errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one
        metrics.application_errors_after_validation_count ;
      match Request.view request with
      | Validation v -> (
          match errs with
          | [Canceled] ->
              (* Ignore requests cancellation *)
              Events.(emit validation_canceled) (v.block, st)
          | errs ->
              let* () =
                Events.(emit application_failure_after_validation)
                  (v.block, st, errs)
              in
              let* () = check_and_quit_on_context_errors errs in
              return_unit)
      | Preapplication _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Validation_failed errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.validation_failed_count ;
      match Request.view request with
      | Validation v -> (
          match errs with
          | [Canceled] ->
              (* Ignore requests cancellation *)
              Events.(emit validation_canceled) (v.block, st)
          | errs ->
              let* () = Events.(emit validation_failure) (v.block, st, errs) in
              let* () = check_and_quit_on_context_errors errs in
              return_unit)
      | Preapplication _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, Commit_block_failed errs -> (
      Shell_metrics.Worker.update_timestamps metrics.worker_timestamps st ;
      Prometheus.Counter.inc_one metrics.commit_block_failed_count ;
      match Request.view request with
      | Validation v -> (
          match errs with
          | [Canceled] ->
              (* Ignore requests cancellation *)
              Events.(emit validation_canceled) (v.block, st)
          | errs ->
              let* () =
                Events.(emit commit_block_failure) (v.block, st, errs)
              in
              let* () = check_and_quit_on_context_errors errs in
              return_unit)
      | Preapplication _ -> (* assert false *) Lwt.return_unit)
  | Request.Request_validation _, (Preapplied _ | Preapplication_error _)
  | ( Request.Request_preapplication _,
      ( Already_committed | Already_known_invalid _ | Validated_and_applied
      | Application_error_after_validation _ | Validation_failed _
      | Commit_block_failed _ ) ) ->
      (* assert false *) Lwt.return_unit

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

    let on_error w status req err =
      Lwt_result.bind (on_error w status req err) (fun () ->
          Lwt_result.return `Continue)

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
  | Inapplicable_after_validation of error trace
  | Invalid of error trace

let validate_and_apply w ?canceler ?peer ?(notify_new_block = fun _ -> ())
    ~advertise_after_validation chain_db hash (header : Block_header.t)
    operations =
  let open Lwt_syntax in
  let chain_store = Distributed_db.chain_store chain_db in
  let* b = Store.Block.is_known_valid chain_store hash in
  match b with
  | true ->
      let* () = Events.(emit previously_validated_and_applied) hash in
      return Valid
  | false -> (
      let* r =
        let open Lwt_result_syntax in
        let* () =
          (check_chain_liveness chain_db hash header
          |> Lwt_result.map_error (fun e -> Worker.Request_error e))
          [@profiler.span_s
            {
              verbosity = Notice;
              metadata = [("prometheus", "check_chain_liveness")];
            }
              (profiling_validation_prefix hash ["check_chain_liveness"])]
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
               advertise_after_validation;
             })
      in
      match r with
      | Ok (Validated_and_applied | Already_committed) -> return Valid
      | Ok (Application_error_after_validation errs)
      | Ok (Commit_block_failed errs) ->
          return (Inapplicable_after_validation errs)
      | Ok (Validation_failed errs)
      | Ok (Already_known_invalid errs)
      | Error (Request_error errs) ->
          return (Invalid errs)
      | Error (Closed None) -> return (Invalid [Worker_types.Terminated])
      | Error (Closed (Some errs)) -> return (Invalid errs)
      | Error (Any exn) -> return (Invalid [Exn exn])
      | Ok (Preapplied _) | Ok (Preapplication_error _) ->
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
  | Ok
      ( Already_committed | Already_known_invalid _ | Validated_and_applied
      | Application_error_after_validation _ | Validation_failed _
      | Commit_block_failed _ ) ->
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

let fetch_and_compile_protocol w ?peer ?timeout =
  let bv = Worker.state w in
  let timeout = Option.value timeout ~default:bv.limits.protocol_timeout in
  Protocol_validator.fetch_and_compile_protocol
    bv.protocol_validator
    ?peer
    ~timeout

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
