(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

include Internal_event.Legacy_logging.Make_semantic (struct
  let name = "node.validator.bootstrap_pipeline"
end)

let node_time_tag =
  Tag.def ~doc:"local time at this node" "node_time" Time.System.pp_hum

let block_time_tag =
  Tag.def
    ~doc:"claimed creation time of block"
    "block_time"
    (fun fmt prot_time -> Time.System.(pp_hum fmt (of_protocol_exn prot_time)))

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

let operations_index_tag =
  Tag.def ~doc:"Operations index" "operations_index" Format.pp_print_int

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
  ( if step.step > 2000 then
    lwt_log_notice
      Tag.DSL.(
        fun f ->
          f
            "fetching a large bootstrap step (%a headers) from peer %a, this \
             may take a while."
          -% t event "fetching_large_step_from_peer"
          -% a (Tag.def ~doc:"" "length" Format.pp_print_int) step.step
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
  else
    lwt_log_info
      Tag.DSL.(
        fun f ->
          f "fetching step %a -> %a (%a) from peer %a."
          -% t event "fetching_step_from_peer"
          -% a Block_hash.Logging.tag step.block
          -% a Block_hash.Logging.predecessor_tag step.predecessor
          -% a (Tag.def ~doc:"" "" Block_locator.pp_step) step
          -% a P2p_peer.Id.Logging.tag pipeline.peer_id) )
  >>= fun () ->
  let rec fetch_loop acc hash cpt =
    Lwt_unix.yield ()
    >>= fun () ->
    ( if step.step > 2000 && step.step <> cpt && (step.step - cpt) mod 1000 = 0
    then
      lwt_log_notice
        Tag.DSL.(
          fun f ->
            f "fetched %a/%a headers from peer %a, and continuing."
            -% t event "still_fetching_large_step_from_peer"
            -% a
                 (Tag.def ~doc:"" "fetched" Format.pp_print_int)
                 (step.step - cpt)
            -% a (Tag.def ~doc:"" "length" Format.pp_print_int) step.step
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
    else Lwt.return_unit )
    >>= fun () ->
    if cpt < 0 then
      lwt_log_info
        Tag.DSL.(
          fun f ->
            f "invalid step from peer %a (too long)."
            -% t event "step_too_long"
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
      >>= fun () -> fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
    else if Block_hash.equal hash step.predecessor then
      if step.strict_step && cpt <> 0 then
        lwt_log_info
          Tag.DSL.(
            fun f ->
              f "invalid step from peer %a (too short)."
              -% t event "step_too_short"
              -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
        >>= fun () ->
        fail (Invalid_locator (pipeline.peer_id, pipeline.locator))
      else return acc
    else
      let chain_state = Distributed_db.chain_state pipeline.chain_db in
      Chain.mem chain_state hash
      >>= fun in_chain ->
      if in_chain then return acc
      else
        lwt_log_info
          Tag.DSL.(
            fun f ->
              f "fetching block header %a from peer %a."
              -% t event "fetching_block_header_from_peer"
              -% a Block_hash.Logging.tag hash
              -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
        lwt_log_info
          Tag.DSL.(
            fun f ->
              f "fetched block header %a from peer %a."
              -% t event "fetched_block_header_from_peer"
              -% a Block_hash.Logging.tag hash
              -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
      lwt_log_info
        Tag.DSL.(
          fun f ->
            f "fetched all steps from peer %a."
            -% t event "fetched_all_steps_from_peer"
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
      >>= fun () ->
      Lwt_pipe.close pipeline.fetched_headers ;
      Lwt.return_unit
  | Error (Exn Lwt.Canceled :: _)
  | Error (Canceled :: _)
  | Error (Exn Lwt_pipe.Closed :: _) ->
      Lwt.return_unit
  | Error (Distributed_db.Block_header.Timeout bh :: _) ->
      lwt_log_info
        Tag.DSL.(
          fun f ->
            f "request for header %a from peer %a timed out."
            -% t event "header_request_timeout"
            -% a Block_hash.Logging.tag bh
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error (Future_block_header {block; block_time; time} :: _) ->
      lwt_log_notice
        Tag.DSL.(
          fun f ->
            f
              "Block locator %a from peer %a contains future blocks. local \
               time: %a, block time: %a"
            -% t event "locator_contains_future_blocks"
            -% a Block_hash.Logging.tag block
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id
            -% a node_time_tag time
            -% a block_time_tag block_time)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error (Too_short_locator _ :: _ as err) ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_info
        Tag.DSL.(
          fun f ->
            f "Too short locator received" -% t event "too_short_locator")
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error
        Tag.DSL.(
          fun f ->
            f "@[Unexpected error (headers fetch):@ %a@]"
            -% t event "unexpected_error" -% a errs_tag err)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler

let rec operations_fetch_worker_loop pipeline =
  Lwt_unix.yield ()
  >>= (fun () ->
        protect ~canceler:pipeline.canceler (fun () ->
            Lwt_pipe.pop pipeline.fetched_headers >>= return)
        >>=? fun batch ->
        map_p
          (fun (hash, header) ->
            lwt_log_info
              Tag.DSL.(
                fun f ->
                  f "fetching operations of block %a from peer %a."
                  -% t event "fetching_operations"
                  -% a Block_hash.Logging.tag hash
                  -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
              lwt_log_info
                Tag.DSL.(
                  fun f ->
                    f "fetched operations of block %a from peer %a."
                    -% t event "fetched_operations"
                    -% a Block_hash.Logging.tag hash
                    -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
      lwt_log_info
        Tag.DSL.(
          fun f ->
            f "request for operations %a:%d from peer %a timed out."
            -% t event "request_operations_timeout"
            -% a Block_hash.Logging.tag bh
            -% s operations_index_tag n
            -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler
  | Error err ->
      pipeline.errors <- pipeline.errors @ err ;
      lwt_log_error
        Tag.DSL.(
          fun f ->
            f "@[Unexpected error (operations fetch):@ %a@]"
            -% t event "unexpected_error" -% a errs_tag err)
      >>= fun () -> Lwt_canceler.cancel pipeline.canceler

let rec validation_worker_loop pipeline =
  Lwt_unix.yield ()
  >>= (fun () ->
        protect ~canceler:pipeline.canceler (fun () ->
            Lwt_pipe.pop pipeline.fetched_blocks >>= return)
        >>=? fun (hash, header, operations) ->
        lwt_log_info
          Tag.DSL.(
            fun f ->
              f "requesting validation for block %a from peer %a."
              -% t event "requesting_validation"
              -% a Block_hash.Logging.tag hash
              -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
        lwt_log_info
          Tag.DSL.(
            fun f ->
              f "validated block %a from peer %a."
              -% t event "validated_block"
              -% a Block_hash.Logging.tag hash
              -% a P2p_peer.Id.Logging.tag pipeline.peer_id)
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
      lwt_log_error
        Tag.DSL.(
          fun f ->
            f "@[Unexpected error (validator):@ %a@]"
            -% t event "unexpected_error" -% a errs_tag err)
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
