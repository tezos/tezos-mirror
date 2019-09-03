(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Logging = Internal_event.Legacy_logging.Make (struct
  let name = "node.distributed_db"
end)

module Message = Distributed_db_message

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

type connection =
  (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.connection

type callback = {
  notify_branch : P2p_peer.Id.t -> Block_locator.t -> unit;
  notify_head : P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit;
  disconnection : P2p_peer.Id.t -> unit;
}

type db = {
  p2p : p2p;
  p2p_readers : p2p_reader P2p_peer.Table.t;
  disk : State.t;
  active_chains : chain_db Chain_id.Table.t;
  protocol_db : Distributed_db_requester.Raw_protocol.t;
  block_input : (Block_hash.t * Block_header.t) Lwt_watcher.input;
  operation_input : (Operation_hash.t * Operation.t) Lwt_watcher.input;
}

and chain_db = {
  chain_state : State.Chain.t;
  global_db : db;
  operation_db : Distributed_db_requester.Raw_operation.t;
  block_header_db : Distributed_db_requester.Raw_block_header.t;
  operation_hashes_db : Distributed_db_requester.Raw_operation_hashes.t;
  operations_db : Distributed_db_requester.Raw_operations.t;
  mutable callback : callback;
  active_peers : P2p_peer.Set.t ref;
  active_connections : p2p_reader P2p_peer.Table.t;
}

and p2p_reader = {
  gid : P2p_peer.Id.t;
  conn : connection;
  peer_active_chains : chain_db Chain_id.Table.t;
  canceler : Lwt_canceler.t;
  mutable worker : unit Lwt.t;
}

let noop_callback =
  {
    notify_branch = (fun _gid _locator -> ());
    notify_head = (fun _gid _block _ops -> ());
    disconnection = (fun _gid -> ());
  }

type t = db

let state {disk; _} = disk

let chain_state {chain_state; _} = chain_state

let db {global_db; _} = global_db

let information
    ({ global_db = {p2p_readers; active_chains; _};
       operation_db;
       operations_db;
       block_header_db;
       operation_hashes_db;
       active_connections;
       active_peers;
       _ } :
      chain_db) =
  {
    Chain_validator_worker_state.Distributed_db_state.p2p_readers_length =
      P2p_peer.Table.length p2p_readers;
    active_chains_length = Chain_id.Table.length active_chains;
    operation_db =
      Distributed_db_requester.Raw_operation.state_of_t operation_db;
    operations_db =
      Distributed_db_requester.Raw_operations.state_of_t operations_db;
    block_header_db =
      Distributed_db_requester.Raw_block_header.state_of_t block_header_db;
    operations_hashed_db =
      Distributed_db_requester.Raw_operation_hashes.state_of_t
        operation_hashes_db;
    active_connections_length = P2p_peer.Table.length active_connections;
    active_peers_length = P2p_peer.Set.cardinal !active_peers;
  }

let my_peer_id chain_db = P2p.peer_id chain_db.global_db.p2p

let get_peer_metadata chain_db = P2p.get_peer_metadata chain_db.global_db.p2p

let read_block_header {disk; _} h =
  State.read_block disk h
  >>= function
  | Some b ->
      Lwt.return_some (State.Block.chain_id b, State.Block.header b)
  | None ->
      Lwt.return_none

let find_pending_block_header {peer_active_chains; _} h =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
      match acc with
      | Some _ ->
          acc
      | None
        when Distributed_db_requester.Raw_block_header.pending
               chain_db.block_header_db
               h ->
          Some chain_db
      | None ->
          None)
    peer_active_chains
    None

let find_pending_operations {peer_active_chains; _} h i =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
      match acc with
      | Some _ ->
          acc
      | None
        when Distributed_db_requester.Raw_operations.pending
               chain_db.operations_db
               (h, i) ->
          Some chain_db
      | None ->
          None)
    peer_active_chains
    None

let find_pending_operation_hashes {peer_active_chains; _} h i =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
      match acc with
      | Some _ ->
          acc
      | None
        when Distributed_db_requester.Raw_operation_hashes.pending
               chain_db.operation_hashes_db
               (h, i) ->
          Some chain_db
      | None ->
          None)
    peer_active_chains
    None

let find_pending_operation {peer_active_chains; _} h =
  Chain_id.Table.fold
    (fun _chain_id chain_db acc ->
      match acc with
      | Some _ ->
          acc
      | None
        when Distributed_db_requester.Raw_operation.pending
               chain_db.operation_db
               h ->
          Some chain_db
      | None ->
          None)
    peer_active_chains
    None

let read_operation {active_chains; _} h =
  Chain_id.Table.fold
    (fun chain_id chain_db acc ->
      acc
      >>= function
      | Some _ ->
          acc
      | None -> (
          Distributed_db_requester.Raw_operation.read_opt
            chain_db.operation_db
            h
          >>= function
          | None -> Lwt.return_none | Some bh -> Lwt.return_some (chain_id, bh)
          ))
    active_chains
    Lwt.return_none

module P2p_reader = struct
  let may_activate global_db state chain_id f =
    match Chain_id.Table.find_opt state.peer_active_chains chain_id with
    | Some chain_db ->
        f chain_db
    | None -> (
      match Chain_id.Table.find_opt global_db.active_chains chain_id with
      | Some chain_db ->
          chain_db.active_peers :=
            P2p_peer.Set.add state.gid !(chain_db.active_peers) ;
          P2p_peer.Table.add chain_db.active_connections state.gid state ;
          Chain_id.Table.add state.peer_active_chains chain_id chain_db ;
          f chain_db
      | None ->
          let meta = P2p.get_peer_metadata global_db.p2p state.gid in
          Peer_metadata.incr meta Unactivated_chain ;
          Lwt.return_unit )

  let deactivate state chain_db =
    chain_db.callback.disconnection state.gid ;
    chain_db.active_peers :=
      P2p_peer.Set.remove state.gid !(chain_db.active_peers) ;
    P2p_peer.Table.remove chain_db.active_connections state.gid

  (* check if the chain advertized by a peer is (still) active *)
  let may_handle global_db state chain_id f =
    match Chain_id.Table.find_opt state.peer_active_chains chain_id with
    | None ->
        let meta = P2p.get_peer_metadata global_db.p2p state.gid in
        Peer_metadata.incr meta Inactive_chain ;
        Lwt.return_unit
    | Some chain_db ->
        f chain_db

  let may_handle_global global_db chain_id f =
    match Chain_id.Table.find_opt global_db.active_chains chain_id with
    | None ->
        Lwt.return_unit
    | Some chain_db ->
        f chain_db

  module Handle_msg_Logging =
  Internal_event.Legacy_logging.Make_semantic (struct
    let name = "node.distributed_db.p2p_reader"
  end)

  let soon () =
    let now = Systime_os.now () in
    match Ptime.add_span now (Ptime.Span.of_int_s 15) with
    | Some s ->
        s
    | None ->
        invalid_arg "Distributed_db.handle_msg: end of time"

  let handle_msg global_db state msg =
    let open Message in
    let open Handle_msg_Logging in
    let meta = P2p.get_peer_metadata global_db.p2p state.gid in
    lwt_debug
      Tag.DSL.(
        fun f ->
          f "Read message from %a: %a"
          -% t event "read_message"
          -% a P2p_peer.Id.Logging.tag state.gid
          -% a Message.Logging.tag msg)
    >>= fun () ->
    match msg with
    | Get_current_branch chain_id ->
        Peer_metadata.incr meta @@ Received_request Branch ;
        may_handle_global global_db chain_id
        @@ fun chain_db ->
        if not (Chain_id.Table.mem state.peer_active_chains chain_id) then
          Peer_metadata.update_requests meta Branch
          @@ P2p.try_send global_db.p2p state.conn
          @@ Get_current_branch chain_id ;
        let seed =
          {
            Block_locator.receiver_id = state.gid;
            sender_id = my_peer_id chain_db;
          }
        in
        Chain.locator chain_db.chain_state seed
        >>= fun locator ->
        Peer_metadata.update_responses meta Branch
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_branch (chain_id, locator) ;
        Lwt.return_unit
    | Current_branch (chain_id, locator) ->
        may_activate global_db state chain_id
        @@ fun chain_db ->
        let (head, hist) = (locator :> Block_header.t * Block_hash.t list) in
        Lwt_list.exists_p
          (State.Block.known_invalid chain_db.chain_state)
          (Block_header.hash head :: hist)
        >>= fun known_invalid ->
        if known_invalid then (
          P2p.disconnect global_db.p2p state.conn
          >>= fun () ->
          P2p.greylist_peer global_db.p2p state.gid ;
          Lwt.return_unit )
        else if Time.System.(soon () < of_protocol_exn head.shell.timestamp)
        then (
          Peer_metadata.incr meta Future_block ;
          lwt_log_notice
            Tag.DSL.(
              fun f ->
                f "Received future block %a from peer %a."
                -% t event "received_future_block"
                -% a Block_hash.Logging.tag (Block_header.hash head)
                -% a P2p_peer.Id.Logging.tag state.gid) )
        else (
          chain_db.callback.notify_branch state.gid locator ;
          (* TODO discriminate between received advertisements
             and responses? *)
          Peer_metadata.incr meta @@ Received_advertisement Branch ;
          Lwt.return_unit )
    | Deactivate chain_id ->
        may_handle global_db state chain_id
        @@ fun chain_db ->
        deactivate state chain_db ;
        Chain_id.Table.remove state.peer_active_chains chain_id ;
        Lwt.return_unit
    | Get_current_head chain_id ->
        may_handle global_db state chain_id
        @@ fun chain_db ->
        Peer_metadata.incr meta @@ Received_request Head ;
        let {Connection_metadata.disable_mempool; _} =
          P2p.connection_remote_metadata chain_db.global_db.p2p state.conn
        in
        ( if disable_mempool then
          Chain.head chain_db.chain_state
          >>= fun head -> Lwt.return (State.Block.header head, Mempool.empty)
        else State.Current_mempool.get chain_db.chain_state )
        >>= fun (head, mempool) ->
        (* TODO bound the sent mempool size *)
        Peer_metadata.update_responses meta Head
        @@ P2p.try_send global_db.p2p state.conn
        @@ Current_head (chain_id, head, mempool) ;
        Lwt.return_unit
    | Current_head (chain_id, header, mempool) ->
        may_handle global_db state chain_id
        @@ fun chain_db ->
        let head = Block_header.hash header in
        State.Block.known_invalid chain_db.chain_state head
        >>= fun known_invalid ->
        let {Connection_metadata.disable_mempool; _} =
          P2p.connection_local_metadata chain_db.global_db.p2p state.conn
        in
        let known_invalid =
          known_invalid || (disable_mempool && mempool <> Mempool.empty)
          (* A non-empty mempool was received while mempool is desactivated,
               so the message is ignored.
               This should probably warrant a reduction of the sender's score. *)
        in
        if known_invalid then (
          P2p.disconnect global_db.p2p state.conn
          >>= fun () ->
          P2p.greylist_peer global_db.p2p state.gid ;
          Lwt.return_unit )
        else if Time.System.(soon () < of_protocol_exn header.shell.timestamp)
        then (
          Peer_metadata.incr meta Future_block ;
          lwt_log_notice
            Tag.DSL.(
              fun f ->
                f "Received future block %a from peer %a."
                -% t event "received_future_block"
                -% a Block_hash.Logging.tag head
                -% a P2p_peer.Id.Logging.tag state.gid) )
        else (
          chain_db.callback.notify_head state.gid header mempool ;
          (* TODO discriminate between received advertisements
             and responses? *)
          Peer_metadata.incr meta @@ Received_advertisement Head ;
          Lwt.return_unit )
    | Get_block_headers hashes ->
        Peer_metadata.incr meta @@ Received_request Block_header ;
        Lwt_list.iter_p
          (fun hash ->
            read_block_header global_db hash
            >>= function
            | None ->
                Peer_metadata.incr meta @@ Unadvertised Block ;
                Lwt.return_unit
            | Some (_chain_id, header) ->
                Peer_metadata.update_responses meta Block_header
                @@ P2p.try_send global_db.p2p state.conn
                @@ Block_header header ;
                Lwt.return_unit)
          hashes
    | Block_header block -> (
        let hash = Block_header.hash block in
        match find_pending_block_header state hash with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Distributed_db_requester.Raw_block_header.notify
              chain_db.block_header_db
              state.gid
              hash
              block
            >>= fun () ->
            Peer_metadata.incr meta @@ Received_response Block_header ;
            Lwt.return_unit )
    | Get_operations hashes ->
        Peer_metadata.incr meta @@ Received_request Operations ;
        Lwt_list.iter_p
          (fun hash ->
            read_operation global_db hash
            >>= function
            | None ->
                Peer_metadata.incr meta @@ Unadvertised Operations ;
                Lwt.return_unit
            | Some (_chain_id, op) ->
                Peer_metadata.update_responses meta Operations
                @@ P2p.try_send global_db.p2p state.conn
                @@ Operation op ;
                Lwt.return_unit)
          hashes
    | Operation operation -> (
        let hash = Operation.hash operation in
        match find_pending_operation state hash with
        | None ->
            Peer_metadata.incr meta Unexpected_response ;
            Lwt.return_unit
        | Some chain_db ->
            Distributed_db_requester.Raw_operation.notify
              chain_db.operation_db
              state.gid
              hash
              operation
            >>= fun () ->
            Peer_metadata.incr meta @@ Received_response Operations ;
            Lwt.return_unit )
    | Get_protocols hashes ->
        Peer_metadata.incr meta @@ Received_request Protocols ;
        Lwt_list.iter_p
          (fun hash ->
            State.Protocol.read_opt global_db.disk hash
            >>= function
            | None ->
                Peer_metadata.incr meta @@ Unadvertised Protocol ;
                Lwt.return_unit
            | Some p ->
                Peer_metadata.update_responses meta Protocols
                @@ P2p.try_send global_db.p2p state.conn
                @@ Protocol p ;
                Lwt.return_unit)
          hashes
    | Protocol protocol ->
        let hash = Protocol.hash protocol in
        Distributed_db_requester.Raw_protocol.notify
          global_db.protocol_db
          state.gid
          hash
          protocol
        >>= fun () ->
        Peer_metadata.incr meta @@ Received_response Protocols ;
        Lwt.return_unit
    | Get_operation_hashes_for_blocks blocks ->
        Peer_metadata.incr meta @@ Received_request Operation_hashes_for_block ;
        Lwt_list.iter_p
          (fun (hash, ofs) ->
            State.read_block global_db.disk hash
            >>= function
            | None ->
                Lwt.return_unit
            | Some block ->
                State.Block.operation_hashes block ofs
                >>= fun (hashes, path) ->
                Peer_metadata.update_responses meta Operation_hashes_for_block
                @@ P2p.try_send global_db.p2p state.conn
                @@ Operation_hashes_for_block (hash, ofs, hashes, path) ;
                Lwt.return_unit)
          blocks
    | Operation_hashes_for_block (block, ofs, ops, path) -> (
      match find_pending_operation_hashes state block ofs with
      | None ->
          Peer_metadata.incr meta Unexpected_response ;
          Lwt.return_unit
      | Some chain_db ->
          Distributed_db_requester.Raw_operation_hashes.notify
            chain_db.operation_hashes_db
            state.gid
            (block, ofs)
            (ops, path)
          >>= fun () ->
          Peer_metadata.incr meta
          @@ Received_response Operation_hashes_for_block ;
          Lwt.return_unit )
    | Get_operations_for_blocks blocks ->
        Peer_metadata.incr meta @@ Received_request Operations_for_block ;
        Lwt_list.iter_p
          (fun (hash, ofs) ->
            State.read_block global_db.disk hash
            >>= function
            | None ->
                Lwt.return_unit
            | Some block ->
                State.Block.operations block ofs
                >>= fun (ops, path) ->
                Peer_metadata.update_responses meta Operations_for_block
                @@ P2p.try_send global_db.p2p state.conn
                @@ Operations_for_block (hash, ofs, ops, path) ;
                Lwt.return_unit)
          blocks
    | Operations_for_block (block, ofs, ops, path) -> (
      match find_pending_operations state block ofs with
      | None ->
          Peer_metadata.incr meta Unexpected_response ;
          Lwt.return_unit
      | Some chain_db ->
          Distributed_db_requester.Raw_operations.notify
            chain_db.operations_db
            state.gid
            (block, ofs)
            (ops, path)
          >>= fun () ->
          Peer_metadata.incr meta @@ Received_response Operations_for_block ;
          Lwt.return_unit )

  let rec worker_loop global_db state =
    protect ~canceler:state.canceler (fun () ->
        P2p.recv global_db.p2p state.conn)
    >>= function
    | Ok msg ->
        handle_msg global_db state msg
        >>= fun () -> worker_loop global_db state
    | Error _ ->
        Chain_id.Table.iter
          (fun _ -> deactivate state)
          state.peer_active_chains ;
        P2p_peer.Table.remove global_db.p2p_readers state.gid ;
        Lwt.return_unit

  let run db gid conn =
    let canceler = Lwt_canceler.create () in
    let state =
      {
        conn;
        gid;
        canceler;
        peer_active_chains = Chain_id.Table.create 17;
        worker = Lwt.return_unit;
      }
    in
    Chain_id.Table.iter
      (fun chain_id _chain_db ->
        Lwt.async (fun () ->
            let meta = P2p.get_peer_metadata db.p2p gid in
            Peer_metadata.incr meta (Sent_request Branch) ;
            P2p.send db.p2p conn (Get_current_branch chain_id)))
      db.active_chains ;
    state.worker <-
      Lwt_utils.worker
        (Format.asprintf "db_network_reader.%a" P2p_peer.Id.pp_short gid)
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop db state)
        ~cancel:(fun () -> Lwt_canceler.cancel canceler) ;
    P2p_peer.Table.add db.p2p_readers gid state

  let shutdown s = Lwt_canceler.cancel s.canceler >>= fun () -> s.worker
end

let active_peer_ids p2p () =
  List.fold_left
    (fun acc conn ->
      let {P2p_connection.Info.peer_id; _} = P2p.connection_info p2p conn in
      P2p_peer.Set.add peer_id acc)
    P2p_peer.Set.empty
    (P2p.connections p2p)

let raw_try_send p2p peer_id msg =
  match P2p.find_connection p2p peer_id with
  | None ->
      ()
  | Some conn ->
      ignore (P2p.try_send p2p conn msg : bool)

let create disk p2p =
  let global_request =
    Distributed_db_requester.
      {p2p; data = (); active = active_peer_ids p2p; send = raw_try_send p2p}
  in
  let protocol_db =
    Distributed_db_requester.Raw_protocol.create global_request disk
  in
  let active_chains = Chain_id.Table.create 17 in
  let p2p_readers = P2p_peer.Table.create 17 in
  let block_input = Lwt_watcher.create_input () in
  let operation_input = Lwt_watcher.create_input () in
  let db =
    {
      p2p;
      p2p_readers;
      disk;
      active_chains;
      protocol_db;
      block_input;
      operation_input;
    }
  in
  db

let activate ({p2p; active_chains; _} as global_db) chain_state =
  P2p.on_new_connection p2p (P2p_reader.run global_db) ;
  P2p.iter_connections p2p (P2p_reader.run global_db) ;
  P2p.activate p2p ;
  let chain_id = State.Chain.id chain_state in
  match Chain_id.Table.find_opt active_chains chain_id with
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
      let operation_db =
        Distributed_db_requester.Raw_operation.create
          ~global_input:global_db.operation_input
          p2p_request
          chain_state
      in
      let block_header_db =
        Distributed_db_requester.Raw_block_header.create
          ~global_input:global_db.block_input
          p2p_request
          chain_state
      in
      let operation_hashes_db =
        Distributed_db_requester.Raw_operation_hashes.create
          p2p_request
          chain_state
      in
      let operations_db =
        Distributed_db_requester.Raw_operations.create p2p_request chain_state
      in
      let chain =
        {
          global_db;
          operation_db;
          block_header_db;
          operation_hashes_db;
          operations_db;
          chain_state;
          callback = noop_callback;
          active_peers;
          active_connections = P2p_peer.Table.create 53;
        }
      in
      P2p.iter_connections p2p (fun _peer_id conn ->
          Lwt.async (fun () -> P2p.send p2p conn (Get_current_branch chain_id))) ;
      Chain_id.Table.add active_chains chain_id chain ;
      chain
  | Some chain ->
      chain

let set_callback chain_db callback = chain_db.callback <- callback

let deactivate chain_db =
  let {active_chains; p2p; _} = chain_db.global_db in
  let chain_id = State.Chain.id chain_db.chain_state in
  Chain_id.Table.remove active_chains chain_id ;
  P2p_peer.Table.iter
    (fun _peer_id reader ->
      P2p_reader.deactivate reader chain_db ;
      Lwt.async (fun () -> P2p.send p2p reader.conn (Deactivate chain_id)))
    chain_db.active_connections ;
  Distributed_db_requester.Raw_operation.shutdown chain_db.operation_db
  >>= fun () ->
  Distributed_db_requester.Raw_block_header.shutdown chain_db.block_header_db

let get_chain {active_chains; _} chain_id =
  Chain_id.Table.find_opt active_chains chain_id

let greylist {global_db = {p2p; _}; _} peer_id =
  Lwt.return (P2p.greylist_peer p2p peer_id)

let disconnect {global_db = {p2p; _}; _} peer_id =
  match P2p.find_connection p2p peer_id with
  | None ->
      Lwt.return_unit
  | Some conn ->
      P2p.disconnect p2p conn

let shutdown {p2p_readers; active_chains; _} =
  P2p_peer.Table.fold
    (fun _peer_id reader acc -> P2p_reader.shutdown reader >>= fun () -> acc)
    p2p_readers
    Lwt.return_unit
  >>= fun () ->
  Chain_id.Table.fold
    (fun _ chain_db acc ->
      Distributed_db_requester.Raw_operation.shutdown chain_db.operation_db
      >>= fun () ->
      Distributed_db_requester.Raw_block_header.shutdown
        chain_db.block_header_db
      >>= fun () -> acc)
    active_chains
    Lwt.return_unit

let clear_block chain_db hash n =
  Distributed_db_requester.Raw_operations.clear_all
    chain_db.operations_db
    hash
    n ;
  Distributed_db_requester.Raw_operation_hashes.clear_all
    chain_db.operation_hashes_db
    hash
    n ;
  Distributed_db_requester.Raw_block_header.clear_or_cancel
    chain_db.block_header_db
    hash

let commit_block chain_db hash header header_data operations operations_data
    result ~forking_testchain =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  assert (List.length operations = header.shell.validation_passes) ;
  State.Block.store
    chain_db.chain_state
    header
    header_data
    operations
    operations_data
    result
    ~forking_testchain
  >>=? fun res ->
  clear_block chain_db hash header.shell.validation_passes ;
  return res

let commit_invalid_block chain_db hash header errors =
  assert (Block_hash.equal hash (Block_header.hash header)) ;
  State.Block.store_invalid chain_db.chain_state header errors
  >>=? fun res ->
  clear_block chain_db hash header.shell.validation_passes ;
  return res

let inject_operation chain_db h op =
  assert (Operation_hash.equal h (Operation.hash op)) ;
  Distributed_db_requester.Raw_operation.inject chain_db.operation_db h op

let commit_protocol db h p =
  State.Protocol.store db.disk p
  >>= fun res ->
  Distributed_db_requester.Raw_protocol.clear_or_cancel db.protocol_db h ;
  return (res <> None)

let watch_block_header {block_input; _} = Lwt_watcher.create_stream block_input

let watch_operation {operation_input; _} =
  Lwt_watcher.create_stream operation_input

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

        let proj chain = chain.block_header_db
      end) :
        Requester.REQUESTER
          with type t := chain_db
           and type key := Block_hash.t
           and type value := Block_header.t
           and type param := unit )
end

module Operation_hashes =
  Make
    (Distributed_db_requester.Raw_operation_hashes)
    (struct
      type t = chain_db

      let proj chain = chain.operation_hashes_db
    end)

module Operations =
  Make
    (Distributed_db_requester.Raw_operations)
    (struct
      type t = chain_db

      let proj chain = chain.operations_db
    end)

module Operation = struct
  include Operation

  include (
    Make
      (Distributed_db_requester.Raw_operation)
      (struct
        type t = chain_db

        let proj chain = chain.operation_db
      end) :
        Requester.REQUESTER
          with type t := chain_db
           and type key := Operation_hash.t
           and type value := Operation.t
           and type param := unit )
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
           and type param := unit )
end

let broadcast chain_db msg =
  P2p_peer.Table.iter
    (fun _peer_id state ->
      ignore (P2p.try_send chain_db.global_db.p2p state.conn msg))
    chain_db.active_connections

let try_send chain_db peer_id msg =
  match P2p_peer.Table.find_opt chain_db.active_connections peer_id with
  | None ->
      ()
  | Some conn ->
      ignore (P2p.try_send chain_db.global_db.p2p conn.conn msg : bool)

let send chain_db ?peer msg =
  match peer with
  | Some peer ->
      try_send chain_db peer msg
  | None ->
      broadcast chain_db msg

module Request = struct
  let current_head chain_db ?peer () =
    let chain_id = State.Chain.id chain_db.chain_state in
    ( match peer with
    | Some peer ->
        let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
        Peer_metadata.incr meta (Sent_request Head)
    | None ->
        () ) ;
    send chain_db ?peer @@ Get_current_head chain_id

  let current_branch chain_db ?peer () =
    let chain_id = State.Chain.id chain_db.chain_state in
    ( match peer with
    | Some peer ->
        let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
        Peer_metadata.incr meta (Sent_request Head)
    | None ->
        () ) ;
    send chain_db ?peer @@ Get_current_branch chain_id
end

module Advertise = struct
  let current_head chain_db ?peer ?(mempool = Mempool.empty) head =
    let chain_id = State.Chain.id chain_db.chain_state in
    assert (Chain_id.equal chain_id (State.Block.chain_id head)) ;
    ( match peer with
    | Some peer ->
        let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
        Peer_metadata.incr meta (Sent_advertisement Head)
    | None ->
        () ) ;
    let msg_mempool =
      Message.Current_head (chain_id, State.Block.header head, mempool)
    in
    if mempool = Mempool.empty then send chain_db ?peer msg_mempool
    else
      let msg_disable_mempool =
        Message.Current_head (chain_id, State.Block.header head, Mempool.empty)
      in
      let send_mempool state =
        let {Connection_metadata.disable_mempool; _} =
          P2p.connection_remote_metadata chain_db.global_db.p2p state.conn
        in
        let msg =
          if disable_mempool then msg_disable_mempool else msg_mempool
        in
        ignore @@ P2p.try_send chain_db.global_db.p2p state.conn msg
      in
      match peer with
      | Some receiver_id ->
          let state =
            P2p_peer.Table.find chain_db.active_connections receiver_id
          in
          send_mempool state
      | None ->
          List.iter
            (fun (_receiver_id, state) -> send_mempool state)
            (P2p_peer.Table.fold
               (fun k v acc -> (k, v) :: acc)
               chain_db.active_connections
               [])

  let current_branch ?peer chain_db =
    let chain_id = State.Chain.id chain_db.chain_state in
    let chain_state = chain_state chain_db in
    let sender_id = my_peer_id chain_db in
    ( match peer with
    | Some peer ->
        let meta = P2p.get_peer_metadata chain_db.global_db.p2p peer in
        Peer_metadata.incr meta (Sent_advertisement Branch)
    | None ->
        () ) ;
    match peer with
    | Some receiver_id ->
        let seed = {Block_locator.receiver_id; sender_id} in
        Chain.locator chain_state seed
        >>= fun locator ->
        let msg = Message.Current_branch (chain_id, locator) in
        try_send chain_db receiver_id msg ;
        Lwt.return_unit
    | None ->
        Lwt_list.iter_p
          (fun (receiver_id, state) ->
            let seed = {Block_locator.receiver_id; sender_id} in
            Chain.locator chain_state seed
            >>= fun locator ->
            let msg = Message.Current_branch (chain_id, locator) in
            ignore (P2p.try_send chain_db.global_db.p2p state.conn msg) ;
            Lwt.return_unit)
          (P2p_peer.Table.fold
             (fun k v acc -> (k, v) :: acc)
             chain_db.active_connections
             [])
end
