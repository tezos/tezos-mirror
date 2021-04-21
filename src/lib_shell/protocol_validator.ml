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

open Validation_errors
module Event = Protocol_validator_event

type t = {
  db : Distributed_db.t;
  mutable worker : unit Lwt.t;
  request : unit Lwt_condition.t;
  mutable pending :
    ( Protocol.t
    * Registered_protocol.t tzresult Lwt.t
    * Registered_protocol.t tzresult Lwt.u )
    Protocol_hash.Map.t;
  canceler : Lwt_canceler.t;
}

(** Block validation *)

let rec worker_loop bv =
  ( match Protocol_hash.Map.choose bv.pending with
  | None ->
      Lwt_condition.wait bv.request >>= return
  | Some (hash, (protocol, _, wakener)) ->
      bv.pending <- Protocol_hash.Map.remove hash bv.pending ;
      Updater.compile hash protocol
      >>= fun valid ->
      if valid then (
        Distributed_db.commit_protocol bv.db hash protocol
        >>=? fun _ ->
        ( match Registered_protocol.get hash with
        | Some protocol ->
            Lwt.wakeup_later wakener (Ok protocol)
        | None ->
            Lwt.wakeup_later
              wakener
              (error (Invalid_protocol {hash; error = Dynlinking_failed})) ) ;
        return_unit )
      else (
        (* no need to tag 'invalid' protocol on disk, the economic protocol
         prevents us from being spammed with protocol validation. *)
        Lwt.wakeup_later
          wakener
          (error (Invalid_protocol {hash; error = Compilation_failed})) ;
        return_unit ) )
  >>= function
  | Ok () ->
      worker_loop bv
  | Error (Canceled :: _) | Error (Exn Lwt_pipe.Closed :: _) ->
      Event.(emit validator_terminated) ()
  | Error err ->
      Event.(emit unexpected_worker_error) err
      >>= fun () -> Error_monad.cancel_with_exceptions bv.canceler

let create db =
  let canceler = Lwt_canceler.create () in
  let pending = Protocol_hash.Map.empty in
  let request = Lwt_condition.create () in
  let bv = {canceler; pending; request; db; worker = Lwt.return_unit} in
  Lwt_canceler.on_cancel bv.canceler (fun () ->
      Protocol_hash.Map.iter (fun _ (_, r, _) -> Lwt.cancel r) bv.pending ;
      Lwt.return_unit) ;
  bv.worker <-
    Lwt_utils.worker
      "block_validator"
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop bv)
      ~cancel:(fun () -> Error_monad.cancel_with_exceptions bv.canceler) ;
  bv

let shutdown {canceler; worker; _} =
  Error_monad.cancel_with_exceptions canceler >>= fun () -> worker

let validate state hash protocol =
  match Registered_protocol.get hash with
  | Some protocol ->
      Protocol_validator_event.(emit previously_validated_protocol) hash
      >>= fun () -> return protocol
  | None -> (
      Protocol_validator_event.(emit pushing_protocol_validation) hash
      >>= fun () ->
      match Protocol_hash.Map.find hash state.pending with
      | None ->
          let (res, wakener) = Lwt.task () in
          let broadcast = Protocol_hash.Map.cardinal state.pending = 0 in
          state.pending <-
            Protocol_hash.Map.add hash (protocol, res, wakener) state.pending ;
          if broadcast then Lwt_condition.broadcast state.request () ;
          res
      | Some (_, res, _) ->
          res )

let fetch_and_compile_protocol pv ?peer ?timeout hash =
  match Registered_protocol.get hash with
  | Some proto ->
      return proto
  | None ->
      Distributed_db.Protocol.read_opt pv.db hash
      >>= (function
            | Some protocol ->
                return protocol
            | None ->
                Event.(emit fetching_protocol) (hash, peer)
                >>= fun () ->
                Distributed_db.Protocol.fetch pv.db ?peer ?timeout hash ())
      >>=? fun protocol ->
      validate pv hash protocol >>=? fun proto -> return proto

let fetch_and_compile_protocols pv ?peer ?timeout (block : State.Block.t) =
  let protocol_level = State.Block.protocol_level block in
  let chain_state = State.Block.chain_state block in
  State.Block.context block
  >>=? fun context ->
  let protocol =
    Context.get_protocol context
    >>= fun protocol_hash ->
    fetch_and_compile_protocol pv ?peer ?timeout protocol_hash
    >>=? fun _p ->
    let chain_id = State.Chain.id chain_state in
    State.Chain.update_level_indexed_protocol_store
      chain_state
      chain_id
      protocol_level
      protocol_hash
      (State.Block.header block)
    >>= fun () -> return_unit
  and test_protocol =
    Context.get_test_chain context
    >>= function
    | Not_running ->
        return_unit
    | Forking {protocol; _} | Running {protocol; _} ->
        fetch_and_compile_protocol pv ?peer ?timeout protocol
        >>=? fun _ ->
        State.Chain.test chain_state
        >>= (function
              | None ->
                  Lwt.return_unit
              | Some chain_id ->
                  State.Chain.update_level_indexed_protocol_store
                    chain_state
                    chain_id
                    protocol_level
                    protocol
                    (State.Block.header block))
        >>= fun () -> return_unit
  in
  protocol >>=? fun () -> test_protocol
