(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type limits = {
  protocol_timeout : Time.System.Span.t;
  worker_limits : Worker_types.limits;
}

type result =
  | Already_commited
  | Outdated_block
  | Validated
  | Validation_error of error trace

module Name = struct
  type t = unit

  let encoding = Data_encoding.empty

  let base = ["validator"; "block"]

  let pp _ () = ()

  let equal () () = true
end

module Types = struct
  include Worker_state

  type state = {
    protocol_validator : Protocol_validator.t;
    validation_process : Block_validator_process.t;
    limits : limits;
    start_testchain : bool;
  }

  type parameters =
    limits * bool * Distributed_db.t * Block_validator_process.t

  let view _state _parameters = ()
end

module Request = struct
  include Request

  type 'a t =
    | Request_validation : {
        chain_db : Distributed_db.chain_db;
        notify_new_block : State.Block.t -> unit;
        canceler : Lwt_canceler.t option;
        peer : P2p_peer.Id.t option;
        hash : Block_hash.t;
        header : Block_header.t;
        operations : Operation.t list list;
      }
        -> result t

  let view : type a. a t -> view =
   fun (Request_validation {chain_db; peer; hash; _}) ->
    let chain_id = chain_db |> Distributed_db.chain_state |> State.Chain.id in
    {chain_id; block = hash; peer}
end

module Logger =
  Worker_logger.Make (Event) (Request)
    (struct
      let worker_name = "node_block_validator"
    end)

module Worker = Worker.Make (Name) (Event) (Request) (Types) (Logger)

type t = Worker.infinite Worker.queue Worker.t

let check_chain_liveness chain_db hash (header : Block_header.t) =
  let chain_state = Distributed_db.chain_state chain_db in
  match State.Chain.expiration chain_state with
  | Some eol when Time.Protocol.(eol <= header.shell.timestamp) ->
      fail @@ invalid_block hash
      @@ Expired_chain
           {
             chain_id = State.Chain.id chain_state;
             expiration = eol;
             timestamp = header.shell.timestamp;
           }
  | None | Some _ ->
      return_unit

let is_already_validated chain_state hash = State.Block.known chain_state hash

let on_request : type r. t -> r Request.t -> r tzresult Lwt.t =
 fun w
     (Request.Request_validation
       {chain_db; notify_new_block; canceler; peer; hash; header; operations}) ->
  let bv = Worker.state w in
  let chain_state = Distributed_db.chain_state chain_db in
  is_already_validated chain_state hash
  >>= (function
        | true ->
            return Already_commited
        | false -> (
            State.Block.read_invalid chain_state hash
            >>= function
            | Some {errors; _} ->
                return (Validation_error errors)
            | None -> (
                State.Chain.checkpoint chain_state
                >>= fun checkpoint ->
                (* Safety and late workers in partial mode. *)
                if Compare.Int32.(header.shell.level < checkpoint.shell.level)
                then return Outdated_block
                else
                  Worker.log_event w (Validating_block hash)
                  >>= fun () ->
                  State.Block.read chain_state header.shell.predecessor
                  >>=? fun pred ->
                  Worker.protect w (fun () ->
                      protect ?canceler (fun () ->
                          Block_validator_process.apply_block
                            bv.validation_process
                            ~predecessor:pred
                            header
                            operations
                          >>= function
                          | Ok x ->
                              return x
                          (* [Unavailable_protocol] is expected to be the
                             first error in the trace *)
                          | Error (Unavailable_protocol {protocol; _} :: _) ->
                              Protocol_validator.fetch_and_compile_protocol
                                bv.protocol_validator
                                ?peer
                                ~timeout:bv.limits.protocol_timeout
                                protocol
                              >>=? fun _ ->
                              (* Retry validating after fetching the protocol *)
                              Block_validator_process.apply_block
                                bv.validation_process
                                ~predecessor:pred
                                header
                                operations
                          | Error _ as x ->
                              Lwt.return x))
                  >>=? fun { validation_store;
                             block_metadata;
                             ops_metadata;
                             block_metadata_hash;
                             ops_metadata_hashes;
                             forking_testchain } ->
                  let validation_store =
                    ( {
                        context_hash = validation_store.context_hash;
                        message = validation_store.message;
                        max_operations_ttl =
                          validation_store.max_operations_ttl;
                        last_allowed_fork_level =
                          validation_store.last_allowed_fork_level;
                      }
                      : Block_validation.validation_store )
                  in
                  Distributed_db.commit_block
                    chain_db
                    hash
                    header
                    block_metadata
                    operations
                    ops_metadata
                    block_metadata_hash
                    ops_metadata_hashes
                    validation_store
                    ~forking_testchain
                  >>=? function
                  | Some block ->
                      notify_new_block block ; return Validated
                  | None ->
                      return Already_commited ) ))
  >>= function
  | Ok r ->
      return r
  | Error err ->
      if List.exists (function Invalid_block _ -> true | _ -> false) err then (
        Worker.protect w (fun () ->
            Distributed_db.commit_invalid_block chain_db hash header err)
        >>=? fun committed ->
        assert committed ;
        return (Validation_error err) )
      else return (Validation_error err)

let on_launch _ _ (limits, start_testchain, db, validation_process) =
  let protocol_validator = Protocol_validator.create db in
  return
    {Types.protocol_validator; validation_process; limits; start_testchain}

let on_error w r st errs =
  Worker.log_event w (Validation_failure (r, st, errs))
  >>= fun () ->
  (* Keep the worker alive. *)
  return_unit

let on_completion :
    type a. t -> a Request.t -> a -> Worker_types.request_status -> unit Lwt.t
    =
 fun w (Request.Request_validation {hash; _} as r) v st ->
  match v with
  | Already_commited | Outdated_block ->
      Worker.log_event w (Previously_validated hash)
      >>= fun () -> Lwt.return_unit
  | Validated ->
      Worker.log_event w (Validation_success (Request.view r, st))
  | Validation_error errs ->
      Worker.log_event w (Event.Validation_failure (Request.view r, st, errs))

let on_close w =
  let bv = Worker.state w in
  Block_validator_process.close bv.validation_process

let table = Worker.create_table Queue

let create limits db validation_process ~start_testchain =
  let module Handlers = struct
    type self = t

    let on_launch = on_launch

    let on_request = on_request

    let on_close = on_close

    let on_error = on_error

    let on_completion = on_completion

    let on_no_request _ = return_unit
  end in
  Worker.launch
    table
    limits.worker_limits
    ()
    (limits, start_testchain, db, validation_process)
    (module Handlers)

let shutdown = Worker.shutdown

let validate w ?canceler ?peer ?(notify_new_block = fun _ -> ()) chain_db hash
    (header : Block_header.t) operations =
  let chain_state = Distributed_db.chain_state chain_db in
  is_already_validated chain_state hash
  >>= function
  | true ->
      Worker.log_event w (Previously_validated hash) >>= fun () -> return_unit
  | false -> (
      let hashes = List.map (List.map Operation.hash) operations in
      let computed_hash =
        Operation_list_list_hash.compute
          (List.map Operation_list_hash.compute hashes)
      in
      fail_when
        ( Operation_list_list_hash.compare
            computed_hash
            header.shell.operations_hash
        <> 0 )
        (Inconsistent_operations_hash
           {
             block = hash;
             expected = header.shell.operations_hash;
             found = computed_hash;
           })
      >>=? fun () ->
      check_chain_liveness chain_db hash header
      >>=? fun () ->
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
           })
      >>= function
      | Ok (Validated | Already_commited | Outdated_block) ->
          return_unit
      | Ok (Validation_error errs) | Error errs ->
          Lwt.return_error errs )

let fetch_and_compile_protocol w =
  let bv = Worker.state w in
  Protocol_validator.fetch_and_compile_protocol bv.protocol_validator

let status = Worker.status

let running_worker () =
  match Worker.list table with
  | [(_, single)] ->
      single
  | [] ->
      raise Not_found
  | _ :: _ :: _ ->
      (* NOTE: names of workers must be unique, [Name.t = unit] which has only
         one inhabitant. *)
      assert false

let pending_requests t = Worker.Queue.pending_requests t

let current_request t = Worker.current_request t

let last_events = Worker.last_events
