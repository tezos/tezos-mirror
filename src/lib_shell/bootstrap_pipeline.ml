(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Validation_errors

type t = {
  canceler : Lwt_canceler.t;
  block_header_timeout : Time.System.Span.t;
  block_operations_timeout : Time.System.Span.t;
  mutable headers_fetch_worker : unit Lwt.t;
  mutable operations_fetch_worker : unit Lwt.t;
  mutable validation_worker : unit Lwt.t;
  peer_id : P2p_peer.Id.t;
  chain_db : Distributed_db.chain_db;
  locator : Block_locator.t;
  block_validator : Block_validator.t;
  notify_new_block : State.Block.t -> unit;
  fetched_headers : (Block_hash.t * Block_header.t) list Lwt_pipe.t;
  fetched_blocks :
    (Block_hash.t * Block_header.t * Operation.t list list tzresult Lwt.t)
    Lwt_pipe.t;
  (* HACK, a worker should be able to return the 'error'. *)
  mutable errors : Error_monad.error list;
}

let assert_acceptable_header pipeline hash (header : Block_header.t) =
  let chain_state = Distributed_db.chain_state pipeline.chain_db in
  let time_now = Systime_os.now () in
  fail_unless
    ( Time.Protocol.compare
        (Time.Protocol.add (Time.System.to_protocol (Systime_os.now ())) 15L)
        header.shell.timestamp
    >= 0 )
    (Future_block_header
       {block = hash; time = time_now; block_time = header.shell.timestamp})
  >>=? fun () ->
  State.Chain.checkpoint chain_state
  >>= fun checkpoint ->
  fail_when
    ( Int32.equal header.shell.level checkpoint.shell.level
    && not (Block_header.equal checkpoint header) )
    (Checkpoint_error (hash, Some pipeline.peer_id))
  >>=? fun () ->
  Chain.head chain_state
  >>= fun head ->
  let checkpoint_reached =
    (State.Block.header head).shell.level >= checkpoint.shell.level
  in
  if checkpoint_reached then
    (* If reached the checkpoint, every block before the checkpoint
       must be part of the chain. *)
    if header.shell.level <= checkpoint.shell.level then
      Chain.mem chain_state hash
      >>= fun in_chain ->
      fail_unless in_chain (Checkpoint_error (hash, Some pipeline.peer_id))
    else return_unit
  else return_unit

let fetch_step pipeline (step : Block_locator.step) =
  Bootstrap_pipeline_event.(emit fetching_step_from_peer)
    (step.block, step.predecessor, step.step, pipeline.peer_id)
  >>= fun () ->
  let rec fetch_loop acc hash cpt =
    Lwt_unix.yield ()
    >>= fun () ->
    ( if step.step > 2000 && step.step <> cpt && (step.step - cpt) mod 1000 = 0
    then
      Bootstrap_pipeline_event.(emit still_fetching_large_step_from_peer)
        (step.step - cpt, step.step, pipeline.peer_id)
    else Lwt.return_unit )
    >>= fun () ->
    if cpt < 0 then
      Bootstrap_pipeline_event.(emit step_too_long) pipeline.peer_id
      >>= fun () -> fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
    else if Block_hash.equal hash step.predecessor then
      if step.strict_step && cpt <> 0 then
        Bootstrap_pipeline_event.(emit step_too_short) pipeline.peer_id
        >>= fun () ->
        fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
      else return acc
    else
      let chain_state = Distributed_db.chain_state pipeline.chain_db in
      Chain.mem chain_state hash
      >>= fun in_chain ->
      if in_chain then return acc
      else
        Bootstrap_pipeline_event.(emit fetching_block_header_from_peer)
          (hash, pipeline.peer_id)
        >>= fun () ->
        protect ~canceler:pipeline.canceler (fun () ->
            Distributed_db.Block_header.fetch
              ~timeout:pipeline.block_header_timeout
              pipeline.chain_db
              ~peer:pipeline.peer_id
              hash
              ())
        >>=? fun header ->
        assert_acceptable_header pipeline hash header
        >>=? fun () ->
        Bootstrap_pipeline_event.(emit fetching_block_header_from_peer)
          (hash, pipeline.peer_id)
        >>= fun () ->
        fetch_loop ((hash, header) :: acc) header.shell.predecessor (cpt - 1)
  in
  fetch_loop [] step.block step.step

let headers_fetch_worker_loop pipeline =
  (let sender_id = Distributed_db.my_peer_id pipeline.chain_db in
   (* sender and receiver are inverted here because they are from
       the point of view of the node sending the locator *)
   let seed =
     {Block_locator.sender_id = pipeline.peer_id; receiver_id = sender_id}
   in
   let chain_state = Distributed_db.chain_state pipeline.chain_db in
   let state = State.Chain.global_state chain_state in
   State.history_mode state
   >>= fun history_mode ->
   ( match history_mode with
   | History_mode.Archive ->
       Lwt.return_none
   | Full | Rolling ->
       let chain_state = Distributed_db.chain_state pipeline.chain_db in
       State.Chain.save_point chain_state >>= Lwt.return_some )
   >>= fun save_point ->
   (* In Full and Rolling mode, we do not want to receive blocks
         that are past our save point's level, otherwise we would
         start validating them again. *)
   let steps =
     match save_point with
     | None ->
         Block_locator.to_steps seed pipeline.locator
     | Some (save_point_level, save_point) ->
         let (head, _) = (pipeline.locator : Block_locator.t :> _ * _) in
         let head_level = head.shell.level in
         let truncate_limit = Int32.(sub head_level save_point_level) in
         Block_locator.to_steps_truncate
           ~limit:(Int32.to_int truncate_limit)
           ~save_point
           seed
           pipeline.locator
   in
   match steps with
   | [] ->
       fail (Too_short_locator (sender_id, pipeline.locator))
   | {Block_locator.predecessor; _} :: _ ->
       State.Block.known chain_state predecessor
       >>= fun predecessor_known ->
       (* Check that the locator is anchored in a block locally known *)
       fail_unless
         predecessor_known
         (Too_short_locator (sender_id, pipeline.locator))
       >>=? fun () ->
       let rec process_headers headers =
         let (batch, remaining_headers) = List.split_n 20 headers in
         protect ~canceler:pipeline.canceler (fun () ->
             Lwt_pipe.push pipeline.fetched_headers batch
             >>= fun () -> return_unit)
         >>=? fun () ->
         match remaining_headers with
         | [] ->
             return_unit
         | _ ->
             process_headers remaining_headers
       in
       let rec pipe ?pred = function
         | [] ->
             return_unit
         | first :: (second :: _ as rest) ->
             let fetch =
               match pred with
               | None ->
                   fetch_step pipeline first
               | Some fetch ->
                   fetch
             in
             let pred = fetch_step pipeline second in
             fetch
             >>=? fun headers ->
             process_headers headers >>=? fun () -> pipe ~pred rest
         | [last] ->
             let fetch =
               match pred with
               | None ->
                   fetch_step pipeline last
               | Some fetch ->
                   fetch
             in
             fetch >>=? process_headers
       in
       pipe steps)
  >>= function
  | Ok () ->
      Bootstrap_pipeline_event.(emit fetching_all_steps_from_peer)
        pipeline.peer_id
      >>= fun () ->
      Lwt_pipe.close pipeline.fetched_headers ;
      Lwt.return_unit
  | Error (Exn Lwt.Canceled :: _)
  | Error (Canceled :: _)
  | Error (Exn Lwt_pipe.Closed :: _) ->
      Lwt.return_unit
  | Error (Distributed_db.Block_header.Timeout bh :: _) ->
      Bootstrap_pipeline_event.(emit header_request_timeout)
        (bh, pipeline.peer_id)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error (Future_block_header {block; block_time; time} :: _) ->
      Bootstrap_pipeline_event.(emit locator_contains_future_block)
        (block, pipeline.peer_id, time, block_time)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error (Too_short_locator _ :: _ as err) ->
      pipeline.errors <- pipeline.errors @ err ;
      Bootstrap_pipeline_event.(emit locator_too_short) ()
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      Bootstrap_pipeline_event.(emit unexpected_error_while_fetching_headers)
        ()
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler

let rec operations_fetch_worker_loop pipeline =
  Lwt_unix.yield ()
  >>= (fun () ->
        protect ~canceler:pipeline.canceler (fun () ->
            Lwt_pipe.pop pipeline.fetched_headers >>= return)
        >>=? fun batch ->
        map_p
          (fun (hash, header) ->
            Bootstrap_pipeline_event.(emit fetching_operations)
              (hash, pipeline.peer_id)
            >>= fun () ->
            let operations =
              map_p
                (fun i ->
                  protect ~canceler:pipeline.canceler (fun () ->
                      Distributed_db.Operations.fetch
                        ~timeout:pipeline.block_operations_timeout
                        pipeline.chain_db
                        ~peer:pipeline.peer_id
                        (hash, i)
                        header.Block_header.shell.operations_hash
                      >>= fun res -> Lwt.return res))
                (0 -- (header.shell.validation_passes - 1))
              >>=? fun operations ->
              Bootstrap_pipeline_event.(emit fetched_operations)
                (hash, pipeline.peer_id)
              >>= fun () -> return operations
            in
            return (hash, header, operations))
          batch
        >>=? fun operationss ->
        iter_s
          (fun (hash, header, operations) ->
            protect ~canceler:pipeline.canceler (fun () ->
                Lwt_pipe.push pipeline.fetched_blocks (hash, header, operations)
                >>= fun () -> return_unit))
          operationss)
  >>= function
  | Ok () ->
      operations_fetch_worker_loop pipeline
  | Error (Exn Lwt.Canceled :: _)
  | Error (Canceled :: _)
  | Error (Exn Lwt_pipe.Closed :: _) ->
      Lwt_pipe.close pipeline.fetched_blocks ;
      Lwt.return_unit
  | Error (Distributed_db.Operations.Timeout (bh, n) :: _) ->
      Bootstrap_pipeline_event.(emit request_operations_timeout)
        (bh, n, pipeline.peer_id)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      Bootstrap_pipeline_event.(emit unexpected_error_while_fetching_headers)
        ()
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler

let rec validation_worker_loop pipeline =
  Lwt_unix.yield ()
  >>= (fun () ->
        protect ~canceler:pipeline.canceler (fun () ->
            Lwt_pipe.pop pipeline.fetched_blocks >>= return)
        >>=? fun (hash, header, operations) ->
        Bootstrap_pipeline_event.(emit requesting_validation)
          (hash, pipeline.peer_id)
        >>= fun () ->
        operations
        >>=? fun operations ->
        protect ~canceler:pipeline.canceler (fun () ->
            Block_validator.validate
              ~canceler:pipeline.canceler
              ~notify_new_block:pipeline.notify_new_block
              pipeline.block_validator
              pipeline.chain_db
              hash
              header
              operations)
        >>=? fun _block ->
        Bootstrap_pipeline_event.(emit validated_block) (hash, pipeline.peer_id)
        >>= fun () -> return_unit)
  >>= function
  | Ok () ->
      validation_worker_loop pipeline
  | Error ((Exn Lwt.Canceled | Canceled | Exn Lwt_pipe.Closed) :: _) ->
      Lwt.return_unit
  | Error
      ( ( Block_validator_errors.Invalid_block _
        | Block_validator_errors.Unavailable_protocol _
        | Block_validator_errors.System_error _
        | Timeout )
        :: _ as err ) ->
      (* Propagate the error to the peer validator. *)
      pipeline.errors <- pipeline.errors @ err ;
      Lwt_canceler.cancel pipeline.canceler
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      Bootstrap_pipeline_event.(emit unexpected_error_while_fetching_headers)
        ()
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler

let create ?(notify_new_block = fun _ -> ()) ~block_header_timeout
    ~block_operations_timeout block_validator peer_id chain_db locator =
  let canceler = Lwt_canceler.create () in
  let fetched_headers = Lwt_pipe.create ~size:(1024, fun _ -> 1) () in
  let fetched_blocks = Lwt_pipe.create ~size:(128, fun _ -> 1) () in
  let pipeline =
    {
      canceler;
      block_header_timeout;
      block_operations_timeout;
      headers_fetch_worker = Lwt.return_unit;
      operations_fetch_worker = Lwt.return_unit;
      validation_worker = Lwt.return_unit;
      notify_new_block;
      peer_id;
      chain_db;
      locator;
      block_validator;
      fetched_headers;
      fetched_blocks;
      errors = [];
    }
  in
  Lwt_canceler.on_cancel pipeline.canceler (fun () ->
      Lwt_pipe.close fetched_blocks ;
      Lwt_pipe.close fetched_headers ;
      (* TODO proper cleanup of ressources... *)
      Lwt.return_unit) ;
  let (head, _) = (pipeline.locator : Block_locator.t :> _ * _) in
  let hash = Block_header.hash head in
  pipeline.headers_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf
         "bootstrap_pipeline-headers_fetch.%a.%a"
         P2p_peer.Id.pp_short
         peer_id
         Block_hash.pp_short
         hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> headers_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline.operations_fetch_worker <-
    Lwt_utils.worker
      (Format.asprintf
         "bootstrap_pipeline-operations_fetch.%a.%a"
         P2p_peer.Id.pp_short
         peer_id
         Block_hash.pp_short
         hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> operations_fetch_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline.validation_worker <-
    Lwt_utils.worker
      (Format.asprintf
         "bootstrap_pipeline-validation.%a.%a"
         P2p_peer.Id.pp_short
         peer_id
         Block_hash.pp_short
         hash)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> validation_worker_loop pipeline)
      ~cancel:(fun () -> Lwt_canceler.cancel pipeline.canceler) ;
  pipeline

let wait_workers pipeline =
  pipeline.headers_fetch_worker
  >>= fun () ->
  pipeline.operations_fetch_worker >>= fun () -> pipeline.validation_worker

let wait pipeline =
  wait_workers pipeline
  >>= fun () ->
  match pipeline.errors with
  | [] ->
      return_unit
  | errors ->
      Lwt.return_error errors

let cancel pipeline =
  Lwt_canceler.cancel pipeline.canceler >>= fun () -> wait_workers pipeline

let length pipeline =
  Peer_validator_worker_state.Worker_state.
    {
      fetched_header_length = Lwt_pipe.length pipeline.fetched_headers;
      fetched_block_length = Lwt_pipe.length pipeline.fetched_blocks;
    }

let length_zero =
  Peer_validator_worker_state.Worker_state.
    {fetched_header_length = 0; fetched_block_length = 0}
