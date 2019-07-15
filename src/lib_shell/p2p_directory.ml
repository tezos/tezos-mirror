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

let info_of_point_info i =
  let open P2p_point.Info in
  let open P2p_point.State in
  let state = match P2p_point_state.get i with
    | Requested _ -> Requested
    | Accepted { current_peer_id ; _ } -> Accepted current_peer_id
    | Running { current_peer_id ; _ } -> Running current_peer_id
    | Disconnected -> Disconnected in
  P2p_point_state.Info.{
    trusted = trusted i ;
    state ;
    greylisted_until = greylisted_until i ;
    last_failed_connection = last_failed_connection i ;
    last_rejected_connection = last_rejected_connection i ;
    last_established_connection = last_established_connection i ;
    last_disconnection = last_disconnection i ;
    last_seen = last_seen i ;
    last_miss = last_miss i ;
  }

let info_of_peer_info pool i =
  let open P2p_peer.Info in
  let open P2p_peer.State in
  let state, id_point = match P2p_peer_state.get i with
    | Accepted { current_point ; _ } -> Accepted, Some current_point
    | Running { current_point ; _ } -> Running, Some current_point
    | Disconnected -> Disconnected, None in
  let peer_id = P2p_peer_state.Info.peer_id i in
  let score = P2p_pool.Peers.get_score pool peer_id in
  let conn_opt = P2p_pool.Connection.find_by_peer_id pool peer_id in
  let stat =
    match conn_opt with
    | None -> P2p_stat.empty
    | Some conn -> P2p_pool.Connection.stat conn in
  let meta_opt =
    match conn_opt with
    | None -> None
    | Some conn -> Some (P2p_pool.Connection.remote_metadata conn) in
  P2p_peer_state.Info.{
    score ;
    trusted = trusted i ;
    conn_metadata = meta_opt ;
    peer_metadata = peer_metadata i;
    state ;
    id_point ;
    stat ;
    last_failed_connection = last_failed_connection i ;
    last_rejected_connection = last_rejected_connection i ;
    last_established_connection = last_established_connection i ;
    last_disconnection = last_disconnection i ;
    last_seen = last_seen i ;
    last_miss = last_miss i ;
  }

let build_rpc_directory net =

  let dir = RPC_directory.empty in

  (* Network : Global *)

  let dir =
    RPC_directory.register0 dir P2p_services.S.version begin fun () () ->
      return (P2p.announced_version net)
    end in

  let dir =
    (* DEPRECATED: use [version] instead. *)
    RPC_directory.register0 dir P2p_services.S.versions begin fun () () ->
      return [P2p.announced_version net]
    end in

  let dir =
    RPC_directory.register0 dir P2p_services.S.self begin fun () () ->
      match P2p.pool net with
      | None -> failwith "The P2P layer is disabled."
      | Some pool -> return (P2p_pool.config pool).identity.peer_id
    end in

  let dir =
    RPC_directory.register0 dir P2p_services.S.stat begin fun () () ->
      match P2p.pool net with
      | None -> return P2p_stat.empty
      | Some pool -> return (P2p_pool.pool_stat pool)
    end in

  let dir =
    RPC_directory.gen_register0 dir P2p_services.S.events begin fun () () ->
      let stream, stopper =
        match P2p.pool net with
        | None -> Lwt_watcher.create_fake_stream ()
        | Some pool -> P2p_pool.watch pool in
      let shutdown () = Lwt_watcher.shutdown stopper in
      let next () = Lwt_stream.get stream in
      RPC_answer.return_stream { next ; shutdown }
    end in

  let dir =
    RPC_directory.register1 dir P2p_services.S.connect begin fun point q () ->
      match P2p.pool net with
      | None -> failwith "The P2P layer is disabled."
      | Some pool ->
          P2p_pool.connect ~timeout:q#timeout pool point >>=? fun _conn ->
          return_unit
    end in

  (* Network : Connection *)

  let dir =
    RPC_directory.opt_register1 dir P2p_services.Connections.S.info
      begin fun peer_id () () ->
        return @@
        Option.apply (P2p.pool net) ~f: begin fun pool ->
          Option.map ~f:P2p_pool.Connection.info
            (P2p_pool.Connection.find_by_peer_id pool peer_id)
        end
      end in

  let dir =
    RPC_directory.lwt_register1 dir P2p_services.Connections.S.kick
      begin fun peer_id q () ->
        match P2p.pool net with
        | None -> Lwt.return_unit
        | Some pool ->
            match P2p_pool.Connection.find_by_peer_id pool peer_id with
            | None -> Lwt.return_unit
            | Some conn -> P2p_pool.disconnect ~wait:q#wait conn
      end in

  let dir =
    RPC_directory.register0 dir P2p_services.Connections.S.list
      begin fun () () ->
        match P2p.pool net with
        | None -> return_nil
        | Some pool ->
            return @@
            P2p_pool.Connection.fold
              pool ~init:[]
              ~f:begin fun _peer_id c acc ->
                P2p_pool.Connection.info c :: acc
              end
      end in

  (* Network : Peer_id *)

  let dir =
    RPC_directory.register0 dir P2p_services.Peers.S.list
      begin fun q () ->
        match P2p.pool net with
        | None -> return_nil
        | Some pool ->
            return @@
            P2p_pool.Peers.fold_known pool
              ~init:[]
              ~f:begin fun peer_id i a ->
                let info = info_of_peer_info pool i in
                match q#filters with
                | [] -> (peer_id, info) :: a
                | filters when P2p_peer.State.filter filters info.state ->
                    (peer_id, info) :: a
                | _ -> a
              end
      end in

  let dir =
    RPC_directory.opt_register1 dir P2p_services.Peers.S.info
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> return_none
        | Some pool ->
            return @@
            Option.map ~f:(info_of_peer_info pool)
              (P2p_pool.Peers.info pool peer_id)
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Peers.S.events
      begin fun peer_id q () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            match P2p_pool.Peers.info pool peer_id with
            | None -> RPC_answer.return []
            | Some gi ->
                let rev = false and max = max_int in
                let evts =
                  P2p_peer_state.Info.fold gi ~init:[]
                    ~f:(fun a e -> e :: a) in
                let evts = (if rev then List.rev_sub else List.sub) evts max in
                if not q#monitor then
                  RPC_answer.return evts
                else
                  let stream, stopper = P2p_peer_state.Info.watch gi in
                  let shutdown () = Lwt_watcher.shutdown stopper in
                  let first_request = ref true in
                  let next () =
                    if not !first_request then begin
                      Lwt_stream.get stream >|= Option.map ~f:(fun i -> [i])
                    end else begin
                      first_request := false ;
                      Lwt.return_some evts
                    end in
                  RPC_answer.return_stream { next ; shutdown }
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Peers.S.ban
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Peers.untrust pool peer_id ;
            P2p_pool.Peers.ban pool peer_id ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Peers.S.unban
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Peers.unban pool peer_id ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Peers.S.trust
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Peers.trust pool peer_id ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Peers.S.untrust
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Peers.untrust pool peer_id ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.register1 dir P2p_services.Peers.S.banned
      begin fun peer_id () () ->
        match P2p.pool net with
        | None -> return_false
        | Some pool when (P2p_pool.Peers.get_trusted pool peer_id) ->
            return_false
        | Some pool ->
            return (P2p_pool.Peers.banned pool peer_id)
      end in

  (* Network : Point *)

  let dir =
    RPC_directory.register0 dir P2p_services.Points.S.list
      begin fun q () ->
        match P2p.pool net with
        | None -> return_nil
        | Some pool ->
            return @@
            P2p_pool.Points.fold_known
              pool ~init:[]
              ~f:begin fun point i a ->
                let info = info_of_point_info i in
                match q#filters with
                | [] -> (point, info) :: a
                | filters when P2p_point.State.filter filters info.state ->
                    (point, info) :: a
                | _ -> a
              end
      end in

  let dir =
    RPC_directory.opt_register1 dir P2p_services.Points.S.info
      begin fun point () () ->
        match P2p.pool net with
        | None -> return_none
        | Some pool ->
            return @@
            Option.map
              (P2p_pool.Points.info pool point)
              ~f:info_of_point_info
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.events
      begin fun point_id q () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            match P2p_pool.Points.info pool point_id with
            | None -> RPC_answer.return []
            | Some gi ->
                let rev = false and max = max_int in
                let evts =
                  P2p_point_state.Info.fold gi ~init:[]
                    ~f:(fun a e -> e :: a) in
                let evts = (if rev then List.rev_sub else List.sub) evts max in
                if not q#monitor then
                  RPC_answer.return evts
                else
                  let stream, stopper = P2p_point_state.Info.watch gi in
                  let shutdown () = Lwt_watcher.shutdown stopper in
                  let first_request = ref true in
                  let next () =
                    if not !first_request then begin
                      Lwt_stream.get stream >|= Option.map ~f:(fun i -> [i])
                    end else begin
                      first_request := false ;
                      Lwt.return_some evts
                    end in
                  RPC_answer.return_stream { next ; shutdown }
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.ban
      begin fun point () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Points.untrust pool point;
            P2p_pool.Points.ban pool point;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.unban
      begin fun point () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Points.unban pool point;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.trust
      begin fun point () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Points.trust pool point ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.untrust
      begin fun point () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool ->
            P2p_pool.Points.untrust pool point ;
            RPC_answer.return_unit
      end in

  let dir =
    RPC_directory.gen_register1 dir P2p_services.Points.S.banned
      begin fun point () () ->
        match P2p.pool net with
        | None -> RPC_answer.not_found
        | Some pool when (P2p_pool.Points.get_trusted pool point) ->
            RPC_answer.return false
        | Some pool ->
            RPC_answer.return (P2p_pool.Points.banned pool point)
      end in

  (* Network : Greylist *)

  let dir =
    RPC_directory.register dir P2p_services.ACL.S.clear
      begin fun () () () ->
        match P2p.pool net with
        | None -> return_unit
        | Some pool ->
            P2p_pool.acl_clear pool ;
            return_unit
      end in

  dir
