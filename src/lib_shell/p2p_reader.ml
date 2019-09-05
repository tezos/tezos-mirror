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

module Message = Distributed_db_message

type p2p = (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.net

type connection =
  (Message.t, Peer_metadata.t, Connection_metadata.t) P2p.connection

type callback = {
  notify_branch : P2p_peer.Id.t -> Block_locator.t -> unit;
  notify_head : P2p_peer.Id.t -> Block_header.t -> Mempool.t -> unit;
  disconnection : P2p_peer.Id.t -> unit;
}

type chain_db = {
  chain_state : State.Chain.t;
  operation_db : Distributed_db_requester.Raw_operation.t;
  block_header_db : Distributed_db_requester.Raw_block_header.t;
  operation_hashes_db : Distributed_db_requester.Raw_operation_hashes.t;
  operations_db : Distributed_db_requester.Raw_operations.t;
  mutable callback : callback;
  active_peers : P2p_peer.Set.t ref;
  active_connections : connection P2p_peer.Table.t;
}

type t = {
  p2p : p2p;
  gid : P2p_peer.Id.t;
  conn : connection;
  peer_active_chains : chain_db Chain_id.Table.t;
  disk : State.t;
  canceler : Lwt_canceler.t;
  mutable worker : unit Lwt.t;
  protocol_db : Distributed_db_requester.Raw_protocol.t;
  active_chains : chain_db Chain_id.Table.t;
  unregister : unit -> unit;
}

let may_activate state chain_id f =
  match Chain_id.Table.find_opt state.peer_active_chains chain_id with
  | Some chain_db ->
      f chain_db
  | None -> (
    match Chain_id.Table.find_opt state.active_chains chain_id with
    | Some chain_db ->
        chain_db.active_peers :=
          P2p_peer.Set.add state.gid !(chain_db.active_peers) ;
        P2p_peer.Table.add chain_db.active_connections state.gid state.conn ;
        Chain_id.Table.add state.peer_active_chains chain_id chain_db ;
        f chain_db
    | None ->
        let meta = P2p.get_peer_metadata state.p2p state.gid in
        Peer_metadata.incr meta Unactivated_chain ;
        Lwt.return_unit )

(* check if the chain advertized by a peer is (still) active *)
let may_handle state chain_id f =
  match Chain_id.Table.find_opt state.peer_active_chains chain_id with
  | None ->
      let meta = P2p.get_peer_metadata state.p2p state.gid in
      Peer_metadata.incr meta Inactive_chain ;
      Lwt.return_unit
  | Some chain_db ->
      f chain_db

let may_handle_global state chain_id f =
  match Chain_id.Table.find_opt state.active_chains chain_id with
  | None ->
      Lwt.return_unit
  | Some chain_db ->
      f chain_db

module Handle_msg_Logging = Internal_event.Legacy_logging.Make_semantic (struct
  let name = "node.distributed_db.p2p_reader"
end)

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

let read_operation state h =
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
    state.active_chains
    Lwt.return_none

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

let deactivate gid chain_db =
  chain_db.callback.disconnection gid ;
  chain_db.active_peers := P2p_peer.Set.remove gid !(chain_db.active_peers) ;
  P2p_peer.Table.remove chain_db.active_connections gid

let soon () =
  let now = Systime_os.now () in
  match Ptime.add_span now (Ptime.Span.of_int_s 15) with
  | Some s ->
      s
  | None ->
      invalid_arg "Distributed_db.handle_msg: end of time"

let my_peer_id state = P2p.peer_id state.p2p

let handle_msg state msg =
  let open Message in
  let open Handle_msg_Logging in
  let meta = P2p.get_peer_metadata state.p2p state.gid in
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
      may_handle_global state chain_id
      @@ fun chain_db ->
      if not (Chain_id.Table.mem state.peer_active_chains chain_id) then
        Peer_metadata.update_requests meta Branch
        @@ P2p.try_send state.p2p state.conn
        @@ Get_current_branch chain_id ;
      let seed =
        {Block_locator.receiver_id = state.gid; sender_id = my_peer_id state}
      in
      Chain.locator chain_db.chain_state seed
      >>= fun locator ->
      Peer_metadata.update_responses meta Branch
      @@ P2p.try_send state.p2p state.conn
      @@ Current_branch (chain_id, locator) ;
      Lwt.return_unit
  | Current_branch (chain_id, locator) ->
      may_activate state chain_id
      @@ fun chain_db ->
      let (head, hist) = (locator :> Block_header.t * Block_hash.t list) in
      Lwt_list.exists_p
        (State.Block.known_invalid chain_db.chain_state)
        (Block_header.hash head :: hist)
      >>= fun known_invalid ->
      if known_invalid then (
        P2p.disconnect state.p2p state.conn
        >>= fun () ->
        P2p.greylist_peer state.p2p state.gid ;
        Lwt.return_unit )
      else if Time.System.(soon () < of_protocol_exn head.shell.timestamp) then (
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
      may_handle state chain_id
      @@ fun chain_db ->
      deactivate state.gid chain_db ;
      Chain_id.Table.remove state.peer_active_chains chain_id ;
      Lwt.return_unit
  | Get_current_head chain_id ->
      may_handle state chain_id
      @@ fun chain_db ->
      Peer_metadata.incr meta @@ Received_request Head ;
      let {Connection_metadata.disable_mempool; _} =
        P2p.connection_remote_metadata state.p2p state.conn
      in
      ( if disable_mempool then
        Chain.head chain_db.chain_state
        >>= fun head -> Lwt.return (State.Block.header head, Mempool.empty)
      else State.Current_mempool.get chain_db.chain_state )
      >>= fun (head, mempool) ->
      (* TODO bound the sent mempool size *)
      Peer_metadata.update_responses meta Head
      @@ P2p.try_send state.p2p state.conn
      @@ Current_head (chain_id, head, mempool) ;
      Lwt.return_unit
  | Current_head (chain_id, header, mempool) ->
      may_handle state chain_id
      @@ fun chain_db ->
      let head = Block_header.hash header in
      State.Block.known_invalid chain_db.chain_state head
      >>= fun known_invalid ->
      let {Connection_metadata.disable_mempool; _} =
        P2p.connection_local_metadata state.p2p state.conn
      in
      let known_invalid =
        known_invalid || (disable_mempool && mempool <> Mempool.empty)
        (* A non-empty mempool was received while mempool is deactivated,
               so the message is ignored.
               This should probably warrant a reduction of the sender's score. *)
      in
      if known_invalid then (
        P2p.disconnect state.p2p state.conn
        >>= fun () ->
        P2p.greylist_peer state.p2p state.gid ;
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
          read_block_header state hash
          >>= function
          | None ->
              Peer_metadata.incr meta @@ Unadvertised Block ;
              Lwt.return_unit
          | Some (_chain_id, header) ->
              Peer_metadata.update_responses meta Block_header
              @@ P2p.try_send state.p2p state.conn
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
          read_operation state hash
          >>= function
          | None ->
              Peer_metadata.incr meta @@ Unadvertised Operations ;
              Lwt.return_unit
          | Some (_chain_id, op) ->
              Peer_metadata.update_responses meta Operations
              @@ P2p.try_send state.p2p state.conn
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
          State.Protocol.read_opt state.disk hash
          >>= function
          | None ->
              Peer_metadata.incr meta @@ Unadvertised Protocol ;
              Lwt.return_unit
          | Some p ->
              Peer_metadata.update_responses meta Protocols
              @@ P2p.try_send state.p2p state.conn
              @@ Protocol p ;
              Lwt.return_unit)
        hashes
  | Protocol protocol ->
      let hash = Protocol.hash protocol in
      Distributed_db_requester.Raw_protocol.notify
        state.protocol_db
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
          State.read_block state.disk hash
          >>= function
          | None ->
              Lwt.return_unit
          | Some block ->
              State.Block.operation_hashes block ofs
              >>= fun (hashes, path) ->
              Peer_metadata.update_responses meta Operation_hashes_for_block
              @@ P2p.try_send state.p2p state.conn
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
        Peer_metadata.incr meta @@ Received_response Operation_hashes_for_block ;
        Lwt.return_unit )
  | Get_operations_for_blocks blocks ->
      Peer_metadata.incr meta @@ Received_request Operations_for_block ;
      Lwt_list.iter_p
        (fun (hash, ofs) ->
          State.read_block state.disk hash
          >>= function
          | None ->
              Lwt.return_unit
          | Some block ->
              State.Block.operations block ofs
              >>= fun (ops, path) ->
              Peer_metadata.update_responses meta Operations_for_block
              @@ P2p.try_send state.p2p state.conn
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

let rec worker_loop state =
  protect ~canceler:state.canceler (fun () -> P2p.recv state.p2p state.conn)
  >>= function
  | Ok msg ->
      handle_msg state msg >>= fun () -> worker_loop state
  | Error _ ->
      Chain_id.Table.iter
        (fun _ -> deactivate state.gid)
        state.peer_active_chains ;
      state.unregister () ;
      Lwt.return_unit

let run ~register ~unregister p2p disk protocol_db active_chains gid conn =
  let canceler = Lwt_canceler.create () in
  let state =
    {
      active_chains;
      protocol_db;
      p2p;
      disk;
      conn;
      gid;
      canceler;
      peer_active_chains = Chain_id.Table.create 17;
      worker = Lwt.return_unit;
      unregister;
    }
  in
  Chain_id.Table.iter
    (fun chain_id _chain_db ->
      Lwt.async (fun () ->
          let meta = P2p.get_peer_metadata p2p gid in
          Peer_metadata.incr meta (Sent_request Branch) ;
          P2p.send p2p conn (Get_current_branch chain_id)
          >>= fun _ -> Lwt.return_unit))
    active_chains ;
  state.worker <-
    Lwt_utils.worker
      (Format.asprintf "db_network_reader.%a" P2p_peer.Id.pp_short gid)
      ~on_event:Internal_event.Lwt_worker_event.on_event
      ~run:(fun () -> worker_loop state)
      ~cancel:(fun () -> Lwt_canceler.cancel canceler) ;
  register state

let shutdown s = Lwt_canceler.cancel s.canceler >>= fun () -> s.worker
