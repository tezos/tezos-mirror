(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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
    (Protocol.t
    * Registered_protocol.t tzresult Lwt.t
    * Registered_protocol.t tzresult Lwt.u)
    Protocol_hash.Map.t;
  canceler : Lwt_canceler.t;
}

(** Block validation *)

let rec worker_loop bv =
  let open Lwt_result_syntax in
  let*! r =
    match Protocol_hash.Map.choose bv.pending with
    | None ->
        let*! v = Lwt_condition.wait bv.request in
        return v
    | Some (hash, (protocol, _, wakener)) ->
        bv.pending <- Protocol_hash.Map.remove hash bv.pending ;
        let*! valid = Updater.compile hash protocol in
        if valid then (
          let* _ = Distributed_db.commit_protocol bv.db hash protocol in
          (match Registered_protocol.get hash with
          | Some protocol -> Lwt.wakeup_later wakener (Ok protocol)
          | None ->
              Lwt.wakeup_later
                wakener
                (Tzresult_syntax.fail
                   (Invalid_protocol {hash; error = Dynlinking_failed}))) ;
          return_unit)
        else (
          (* no need to tag 'invalid' protocol on disk, the economic protocol
             prevents us from being spammed with protocol validation. *)
          Lwt.wakeup_later
            wakener
            (Tzresult_syntax.fail
               (Invalid_protocol {hash; error = Compilation_failed})) ;
          return_unit)
  in
  match r with
  | Ok () -> worker_loop bv
  | Error (Canceled :: _) | Error (Exn Lwt_pipe.Closed :: _) ->
      Event.(emit validator_terminated) ()
  | Error err ->
      let*! () = Event.(emit unexpected_worker_error) err in
      Error_monad.cancel_with_exceptions bv.canceler

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
  let open Lwt_syntax in
  let* () = Error_monad.cancel_with_exceptions canceler in
  worker

let validate state hash protocol =
  let open Lwt_syntax in
  match Registered_protocol.get hash with
  | Some protocol ->
      let* () =
        Protocol_validator_event.(emit previously_validated_protocol) hash
      in
      return_ok protocol
  | None -> (
      let* () =
        Protocol_validator_event.(emit pushing_protocol_validation) hash
      in
      match Protocol_hash.Map.find hash state.pending with
      | None ->
          let (res, wakener) = Lwt.task () in
          let broadcast = Protocol_hash.Map.cardinal state.pending = 0 in
          state.pending <-
            Protocol_hash.Map.add hash (protocol, res, wakener) state.pending ;
          if broadcast then Lwt_condition.broadcast state.request () ;
          res
      | Some (_, res, _) -> res)

let fetch_and_compile_protocol pv ?peer ?timeout hash =
  let open Lwt_result_syntax in
  match Registered_protocol.get hash with
  | Some proto -> return proto
  | None ->
      let* protocol =
        let*! o = Distributed_db.Protocol.read_opt pv.db hash in
        match o with
        | Some protocol -> return protocol
        | None ->
            let*! () = Event.(emit fetching_protocol) (hash, peer) in
            Distributed_db.Protocol.fetch pv.db ?peer ?timeout hash ()
      in
      let* proto = validate pv hash protocol in
      return proto

let fetch_and_compile_protocols pv ?peer ?timeout (block : Store.Block.t) =
  let open Lwt_result_syntax in
  let protocol_level = Store.Block.proto_level block in
  let hash = Store.Block.hash block in
  let state = Distributed_db.store pv.db in
  let*! chain_stores = Store.all_chain_stores state in
  let*! o =
    List.find_map_s
      (fun chain_store ->
        let*! b = Store.Block.is_known chain_store hash in
        match b with
        | false -> Lwt.return_none
        | true -> Lwt.return_some chain_store)
      chain_stores
  in
  match o with
  | None ->
      failwith
        "protocol_validator.fetch_and_compile_protocols: chain state not found"
  | Some chain_store ->
      let* context = Store.Block.context chain_store block in
      let protocol =
        let*! protocol_hash = Context.get_protocol context in
        let* _p = fetch_and_compile_protocol pv ?peer ?timeout protocol_hash in
        Store.Chain.may_update_protocol_level
          chain_store
          ~protocol_level
          (block, protocol_hash)
      and test_protocol =
        let*! v = Context.get_test_chain context in
        match v with
        | Not_running -> return_unit
        | Forking {protocol; _} | Running {protocol; _} -> (
            let* _ = fetch_and_compile_protocol pv ?peer ?timeout protocol in
            let*! o = Store.Chain.testchain chain_store in
            match o with
            | None -> return_unit
            | Some test_chain ->
                let test_chain_store = Store.Chain.testchain_store test_chain in
                let* () =
                  Store.Chain.may_update_protocol_level
                    test_chain_store
                    ~protocol_level
                    (block, protocol)
                in
                return_unit)
      in
      let* () = protocol in
      test_protocol
