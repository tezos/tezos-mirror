(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** One role of this module is to export read-through [REQUESTER] services.
    These services allow client code to lookup resources identified by their
    hashes. Resources are protocols, operations, block_header, operation_hashes
    and operations. Values are first looked up locally, in [State], or are
    requested to peers.

    An exported [REQUESTER] service is implemented by a [FULL_REQUESTER] and
    [P2p_reader] workers working together. Resources are requested via a
    [FULL_REQUESTER.fetch], which uses a callback send function.
    The [P2p_reader] listen for requested values and notify the
    [FULL_REQUESTER].

                            P2P_READER  <---- receives --- ... Peers
                               |
                             notify
                               |
                               \/
    REQUESTER  ---fetch---->  FULL_REQUESTER  --- request ---> ... Peers
                               |
                               \/
                              STATE
  *)

module Message = Distributed_db_message
module Distributed_db_event = Distributed_db_event.Distributed_db_event

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

type db = {
  p2p : p2p;
  p2p_readers : P2p_reader.t P2p_peer.Table.t;
  disk : Store.t;
  active_chains : P2p_reader.chain_db Chain_id.Table.t;
  protocol_db : Distributed_db_requester.Raw_protocol.t;
}

type chain_db = {global_db : db; reader_chain_db : P2p_reader.chain_db}

type t = db

let store {disk; _} = disk

let chain_store chain_db = chain_db.reader_chain_db.chain_store

let db {global_db; _} = global_db

let information {global_db; reader_chain_db} =
  {
    Chain_validator_worker_state.Distributed_db_state.p2p_readers_length =
      P2p_peer.Table.length global_db.p2p_readers;
    active_chains_length = Chain_id.Table.length global_db.active_chains;
    operation_db =
      Distributed_db_requester.Raw_operation.state_of_t
        reader_chain_db.operation_db;
    operations_db =
      Distributed_db_requester.Raw_operations.state_of_t
        reader_chain_db.operations_db;
    block_header_db =
      Distributed_db_requester.Raw_block_header.state_of_t
        reader_chain_db.block_header_db;
    active_connections_length =
      P2p_peer.Table.length reader_chain_db.active_connections;
    active_peers_length = P2p_peer.Set.cardinal !(reader_chain_db.active_peers);
  }

let my_peer_id chain_db = P2p.peer_id chain_db.global_db.p2p

let get_peer_metadata chain_db = P2p.get_peer_metadata chain_db.global_db.p2p

let active_peer_ids p2p () =
  List.fold_left
    (fun acc conn ->
      let {P2p_connection.Info.peer_id; _} = P2p.connection_info p2p conn in
      P2p_peer.Set.add peer_id acc)
    P2p_peer.Set.empty
    (P2p.connections p2p)

let raw_try_send p2p peer_id msg =
  match P2p.find_connection_by_peer_id p2p peer_id with
  | None -> ()
  | Some conn -> ignore (P2p.try_send p2p conn msg : bool)

let create disk p2p =
  let global_request =
    Distributed_db_requester.
      {p2p; data = (); active = active_peer_ids p2p; send = raw_try_send p2p}
  in
  let protocol_db =
    Distributed_db_requester.Raw_protocol.create global_request disk
  in
  let active_chains = Chain_id.Table.create ~random:true 17 in
  let p2p_readers = P2p_peer.Table.create ~random:true 17 in
  let db = {p2p; p2p_readers; disk; active_chains; protocol_db} in
  db

let activate
    ({p2p; active_chains; protocol_db; disk; p2p_readers; _} as global_db)
    chain_store callback =
  let run_p2p_reader gid =
    let register p2p_reader =
      (match P2p_peer.Table.find p2p_readers gid with
      | None -> ()
      | Some old_p2p_reader ->
          Distributed_db_event.(
            emit__dont_wait__use_with_care multiple_p2p_reader)
            gid ;
          (* It is not expected to have exception raised by this function,
             however to prevent future problems potential exceptions are
             logged. *)
          Lwt.dont_wait
            (fun () -> P2p_reader.shutdown old_p2p_reader)
            (fun exn ->
              match exn with
              | Out_of_memory | Stack_overflow -> raise exn
              | _ ->
                  Distributed_db_event.(
                    emit__dont_wait__use_with_care p2p_reader_shutdown_failed)
                    (gid, Printexc.to_string exn))) ;
      P2p_peer.Table.add p2p_readers gid p2p_reader
    in
    let unregister () = P2p_peer.Table.remove p2p_readers gid in
    P2p_reader.run ~register ~unregister p2p disk protocol_db active_chains gid
  in
  P2p.on_new_connection p2p run_p2p_reader ;
  P2p.iter_connections p2p run_p2p_reader ;
  P2p.activate p2p ;
  let chain_id = Store.Chain.chain_id chain_store in
  let reader_chain_db =
    match Chain_id.Table.find active_chains chain_id with
    | Some local_db -> local_db
    | None ->
        let active_peers = ref P2p_peer.Set.empty in
        let p2p_request =
          Distributed_db_requester.
            {
              p2p;
              data = ();
              active = (fun () -> !active_peers);
              send = raw_try_send p2p;
            }
        in
        (* whenever a new chain is activated, requesters are created for all
           resources managed by this chain *)
        let operation_db =
          Distributed_db_requester.Raw_operation.create p2p_request chain_store
        in
        let block_header_db =
          Distributed_db_requester.Raw_block_header.create
            p2p_request
            chain_store
        in
        let operations_db =
          Distributed_db_requester.Raw_operations.create p2p_request chain_store
        in
        let local_db =
          P2p_reader.
            {
              chain_store;
              operation_db;
              block_header_db;
              operations_db;
              callback;
              active_peers;
              active_connections = P2p_peer.Table.create ~random:true 53;
            }
        in
        let sends =
          P2p.fold_connections p2p ~init:[] ~f:(fun _peer_id conn acc ->
              P2p.send p2p conn (Get_current_branch chain_id) :: acc)
        in
        Error_monad.dont_wait
          (fun () -> Error_monad.Lwt_result_syntax.tzjoin sends)
          (fun trace ->
            Format.eprintf
              "Uncaught error: %a\n%!"
              Error_monad.pp_print_trace
              trace)
          (fun exc ->
            Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc)) ;
        Chain_id.Table.add active_chains chain_id local_db ;
        local_db
  in
  {global_db; reader_chain_db}

let deactivate chain_db =
  let open Lwt_syntax in
  let {active_chains; p2p; _} = chain_db.global_db in
  let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
  Chain_id.Table.remove active_chains chain_id ;
  let sends =
    P2p_peer.Table.iter_ep
      (fun gid conn ->
        chain_db.reader_chain_db.callback.disconnection gid ;
        chain_db.reader_chain_db.active_peers :=
          P2p_peer.Set.remove gid !(chain_db.reader_chain_db.active_peers) ;
        P2p_peer.Table.remove chain_db.reader_chain_db.active_connections gid ;
        P2p.send p2p conn (Deactivate chain_id))
      chain_db.reader_chain_db.active_connections
  in
  Error_monad.dont_wait
    (fun () -> sends)
    (fun trace ->
      Format.eprintf "Uncaught error: %a\n%!" Error_monad.pp_print_trace trace)
    (fun exc ->
      Format.eprintf "Uncaught exception: %s\n%!" (Printexc.to_string exc)) ;
  let* () =
    Distributed_db_requester.Raw_operation.shutdown
      chain_db.reader_chain_db.operation_db
  in
  Distributed_db_requester.Raw_block_header.shutdown
    chain_db.reader_chain_db.block_header_db

let get_chain global_db chain_id =
  let open Option_syntax in
  let+ reader_chain_db = Chain_id.Table.find global_db.active_chains chain_id in
  {global_db; reader_chain_db}

let greylist {global_db = {p2p; _}; _} peer_id =
  Lwt.return (P2p.greylist_peer p2p peer_id)

let disconnect ~reason {global_db = {p2p; _}; _} peer_id =
  match P2p.find_connection_by_peer_id p2p peer_id with
  | None -> Lwt.return_unit
  | Some conn -> P2p.disconnect ~reason p2p conn

let shutdown {p2p_readers; active_chains; _} =
  let open Lwt_syntax in
  let* () =
    P2p_peer.Table.iter_p
      (fun _peer_id reader -> P2p_reader.shutdown reader)
      p2p_readers
  in
  Chain_id.Table.iter_p
    (fun _ reader_chain_db ->
      let* () =
        Distributed_db_requester.Raw_operation.shutdown
          reader_chain_db.P2p_reader.operation_db
      in
      Distributed_db_requester.Raw_block_header.shutdown
        reader_chain_db.P2p_reader.block_header_db)
    active_chains

let clear_block chain_db hash n =
  Distributed_db_requester.Raw_operations.clear_all
    chain_db.reader_chain_db.operations_db
    hash
    n ;
  Distributed_db_requester.Raw_block_header.clear_or_cancel
    chain_db.reader_chain_db.block_header_db
    hash

let commit_block chain_db hash block_header operations result =
  let open Lwt_result_syntax in
  assert (Block_hash.equal hash (Block_header.hash block_header)) ;
  assert (
    Compare.List_length_with.(operations = block_header.shell.validation_passes)) ;
  let* res =
    Store.Block.store_block
      chain_db.reader_chain_db.chain_store
      ~block_header
      ~operations
      result
  in
  clear_block chain_db hash block_header.shell.validation_passes ;
  return res

let commit_invalid_block chain_db hash header errors =
  let open Lwt_result_syntax in
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  let* () =
    Store.Block.mark_invalid
      chain_db.reader_chain_db.chain_store
      hash
      ~level:header.shell.level
      errors
  in
  clear_block chain_db hash header.shell.validation_passes ;
  return_unit

let inject_operation chain_db h op =
  assert (Operation_hash.equal h (Operation.hash op)) ;
  Distributed_db_requester.Raw_operation.inject
    chain_db.reader_chain_db.operation_db
    h
    op

let inject_prechecked_block chain_db hash block_header operations =
  Store.Block.store_validated_block
    chain_db.reader_chain_db.chain_store
    ~hash
    ~block_header
    ~operations

let commit_protocol db h p =
  let open Lwt_syntax in
  let* res = Store.Protocol.store db.disk h p in
  Distributed_db_requester.Raw_protocol.clear_or_cancel db.protocol_db h ;
  return_ok (res <> None)

(** This functor is used to export [Requester.REQUESTER] modules outside of this
    module. Thanks to the [Kind] module, requesters are accessed using
    [chain_db] type, and the [Distributed_db_requester.Raw_*.t] type remains
    private to this module. This also ensures that the
    [Distributed_db_requester.Raw_*.t] has been properly created before it is
    possible to use it *)
module Make
    (Table : Requester.REQUESTER) (Kind : sig
      type t

      val proj : t -> Table.t
    end) =
struct
  type key = Table.key

  type value = Table.value

  let known t k = Table.known (Kind.proj t) k

  type error += Missing_data = Table.Missing_data

  type error += Canceled = Table.Canceled

  type error += Timeout = Table.Timeout

  let read t k = Table.read (Kind.proj t) k

  let read_opt t k = Table.read_opt (Kind.proj t) k

  let inject t k v = Table.inject (Kind.proj t) k v

  let fetch t ?peer ?timeout k p = Table.fetch (Kind.proj t) ?peer ?timeout k p

  let clear_or_cancel t k = Table.clear_or_cancel (Kind.proj t) k
end

module Block_header = struct
  type t = Block_header.t

  include (
    Make
      (Distributed_db_requester.Raw_block_header)
      (struct
        type t = chain_db

        let proj chain = chain.reader_chain_db.block_header_db
      end) :
        Requester.REQUESTER
          with type t := chain_db
           and type key := Block_hash.t
           and type value := Block_header.t
           and type param := unit)
end

module Operations =
  Make
    (Distributed_db_requester.Raw_operations)
    (struct
      type t = chain_db

      let proj chain = chain.reader_chain_db.operations_db
    end)

module Operation = struct
  include Operation

  include (
    Make
      (Distributed_db_requester.Raw_operation)
      (struct
        type t = chain_db

        let proj chain = chain.reader_chain_db.operation_db
      end) :
        Requester.REQUESTER
          with type t := chain_db
           and type key := Operation_hash.t
           and type value := Operation.t
           and type param := unit)
end

module Protocol = struct
  type t = Protocol.t

  include (
    Make
      (Distributed_db_requester.Raw_protocol)
      (struct
        type t = db

        let proj db = db.protocol_db
      end) :
        Requester.REQUESTER
          with type t := db
           and type key := Protocol_hash.t
           and type value := Protocol.t
           and type param := unit)
end

let try_send chain_db peer_id msg =
  match
    P2p_peer.Table.find chain_db.reader_chain_db.active_connections peer_id
  with
  | None -> ()
  | Some conn -> ignore (P2p.try_send chain_db.global_db.p2p conn msg : bool)

module Request = struct
  let current_head_from_peer chain_db peer =
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
    Peer_metadata.incr meta (Sent_request Head) ;
    try_send chain_db peer @@ Get_current_head chain_id

  let current_head_from_all chain_db =
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    ignore
      (P2p.broadcast
         chain_db.global_db.p2p
         chain_db.reader_chain_db.active_connections
         (Get_current_head chain_id))

  let current_branch chain_db peer =
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
    Peer_metadata.incr meta (Sent_request Branch) ;
    try_send chain_db peer @@ Get_current_branch chain_id
end

module Advertise = struct
  let current_head chain_db ?(mempool = Mempool.empty) head =
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    P2p.broadcast
      chain_db.global_db.p2p
      ~alt:
        (let if_conn conn =
           let {Connection_metadata.disable_mempool; _} =
             P2p.connection_remote_metadata chain_db.global_db.p2p conn
           in
           disable_mempool
         and then_msg =
           Message.Current_head
             (chain_id, Store.Block.header head, Mempool.empty)
         in
         (if_conn, then_msg))
      chain_db.reader_chain_db.active_connections
      (Message.Current_head (chain_id, Store.Block.header head, mempool))

  let prechecked_head chain_db ?(mempool = Mempool.empty) header =
    let p2p = chain_db.global_db.p2p in
    let acceptable_version conn =
      let {Network_version.distributed_db_version; _} =
        P2p.negotiated_version p2p conn
      in
      Distributed_db_version.compare
        distributed_db_version
        Distributed_db_version.two
      >= 0
    in
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    let msg = Message.Current_head (chain_id, header, mempool) in
    P2p.broadcast
      chain_db.global_db.p2p
      ~except:(fun conn -> not (acceptable_version conn))
      chain_db.reader_chain_db.active_connections
      msg

  let current_branch chain_db =
    let open Lwt_syntax in
    let chain_id = Store.Chain.chain_id chain_db.reader_chain_db.chain_store in
    let chain_store = chain_store chain_db in
    let sender_id = my_peer_id chain_db in
    let* current_head = Store.Chain.current_head chain_store in
    P2p_peer.Table.iter_p
      (fun receiver_id conn ->
        let seed = {Block_locator.receiver_id; sender_id} in
        let* locator =
          Store.Chain.compute_locator chain_store current_head seed
        in
        let msg = Message.Current_branch (chain_id, locator) in
        ignore (P2p.try_send chain_db.global_db.p2p conn msg) ;
        Lwt.return_unit)
      chain_db.reader_chain_db.active_connections
end
