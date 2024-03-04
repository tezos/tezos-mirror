(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

let info_of_point_info = P2p_point_state.info_of_point_info

let info_of_peer_info = P2p_pool.Peers.info_of_peer_info

let build_rpc_directory net =
  let open Lwt_result_syntax in
  let dir = Tezos_rpc.Directory.empty in
  (* Network : Global *)
  let dir =
    Tezos_rpc.Directory.register0 dir P2p_services.S.self (fun () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool -> return (P2p_pool.config pool).identity.peer_id)
  in
  let dir =
    Tezos_rpc.Directory.register0 dir P2p_services.S.stat (fun () () ->
        match P2p.connect_handler net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some connect_handler ->
            return (P2p_connect_handler.stat connect_handler))
  in
  let dir =
    Tezos_rpc.Directory.gen_register0 dir P2p_services.S.events (fun () () ->
        let stream, stopper = P2p.watcher net in
        let shutdown () = Lwt_watcher.shutdown stopper in
        let next () = Lwt_stream.get stream in
        Tezos_rpc.Answer.return_stream {next; shutdown})
  in
  let dir =
    Tezos_rpc.Directory.register1 dir P2p_services.S.connect (fun point q () ->
        match P2p.connect_handler net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some connect_handler ->
            let* _conn =
              P2p_connect_handler.connect
                ~timeout:q#timeout
                connect_handler
                point
            in
            return_unit)
  in
  (* Network : Connection *)
  let dir =
    Tezos_rpc.Directory.opt_register1
      dir
      P2p_services.Connections.S.info
      (fun peer_id () () ->
        return
          (let open Option_syntax in
          let* pool = P2p.pool net in
          let+ conn = P2p_pool.Connection.find_by_peer_id pool peer_id in
          P2p_conn.info conn))
  in
  let dir =
    Tezos_rpc.Directory.lwt_register1
      dir
      P2p_services.Connections.S.kick
      (fun peer_id q () ->
        match P2p.pool net with
        | None -> Lwt.return_unit
        | Some pool -> (
            match P2p_pool.Connection.find_by_peer_id pool peer_id with
            | None -> Lwt.return_unit
            | Some conn ->
                P2p_conn.disconnect ~wait:q#wait ~reason:Explicit_RPC conn))
  in
  let dir =
    Tezos_rpc.Directory.register0
      dir
      P2p_services.Connections.S.list
      (fun () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
            @@ P2p_pool.Connection.fold pool ~init:[] ~f:(fun _peer_id c acc ->
                   P2p_conn.info c :: acc))
  in
  (* Network : Peer_id *)
  let dir =
    Tezos_rpc.Directory.register0 dir P2p_services.Peers.S.list (fun q () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
            @@ P2p_pool.Peers.fold_known pool ~init:[] ~f:(fun peer_id i a ->
                   let info = info_of_peer_info pool i in
                   match q#filters with
                   | [] -> (peer_id, info) :: a
                   | filters when P2p_peer.State.filter filters info.state ->
                       (peer_id, info) :: a
                   | _ -> a))
  in
  let dir =
    Tezos_rpc.Directory.opt_register1
      dir
      P2p_services.Peers.S.info
      (fun peer_id () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
            @@ Option.map
                 (info_of_peer_info pool)
                 (P2p_pool.Peers.info pool peer_id))
  in
  let dir =
    Tezos_rpc.Directory.gen_register1
      dir
      P2p_services.Peers.S.events
      (fun peer_id q () ->
        let open Lwt_syntax in
        match P2p.pool net with
        | None -> Tezos_rpc.Answer.fail [P2p_errors.P2p_layer_disabled]
        | Some pool -> (
            match P2p_pool.Peers.info pool peer_id with
            | None -> Tezos_rpc.Answer.return []
            | Some gi ->
                let evts = P2p_peer_state.Info.events gi in
                if not q#monitor then Tezos_rpc.Answer.return evts
                else
                  let stream, stopper = P2p_peer_state.Info.watch gi in
                  let shutdown () = Lwt_watcher.shutdown stopper in
                  let first_request = ref true in
                  let next () =
                    if not !first_request then
                      let+ o = Lwt_stream.get stream in
                      Option.map (fun i -> [i]) o
                    else (
                      first_request := false ;
                      Lwt.return_some evts)
                  in
                  Tezos_rpc.Answer.return_stream {next; shutdown}))
  in
  let dir =
    Tezos_rpc.Directory.opt_register1
      dir
      P2p_services.Peers.S.patch
      (fun peer_id () acl ->
        let open Lwt_result_syntax in
        match P2p.pool net with
        | None -> return_none
        | Some pool ->
            let*! () =
              match acl with
              | None -> Lwt.return_unit
              | Some `Ban ->
                  (* ban *)
                  P2p_pool.Peers.untrust pool peer_id ;
                  P2p_pool.Peers.ban pool peer_id
              | Some `Trust ->
                  (* trust *)
                  P2p_pool.Peers.trust pool peer_id ;
                  Lwt.return_unit
              | Some `Open ->
                  (* unban, untrust *)
                  P2p_pool.Peers.unban pool peer_id ;
                  P2p_pool.Peers.untrust pool peer_id ;
                  Lwt.return_unit
            in
            return
            @@ Option.map
                 (info_of_peer_info pool)
                 (P2p_pool.Peers.info pool peer_id))
  in
  let dir =
    Tezos_rpc.Directory.register1
      dir
      P2p_services.Peers.S.banned
      (fun peer_id () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool when P2p_pool.Peers.get_trusted pool peer_id -> return_false
        | Some pool -> return (P2p_pool.Peers.banned pool peer_id))
  in
  let dir =
    Tezos_rpc.Directory.register0
      dir
      P2p_services.ACL.S.get_greylisted_peers
      (fun () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool -> return (P2p_pool.Peers.get_greylisted_list pool))
  in
  (* Network : Point *)
  let dir =
    Tezos_rpc.Directory.register0 dir P2p_services.Points.S.list (fun q () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
            @@ P2p_pool.Points.fold_known pool ~init:[] ~f:(fun point i a ->
                   let info = info_of_point_info i in
                   match q#filters with
                   | [] -> (point, info) :: a
                   | filters when P2p_point.State.filter filters info.state ->
                       (point, info) :: a
                   | _ -> a))
  in
  let dir =
    Tezos_rpc.Directory.opt_register1
      dir
      P2p_services.Points.S.info
      (fun point () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
            @@ Option.map info_of_point_info (P2p_pool.Points.info pool point))
  in
  let dir =
    Tezos_rpc.Directory.opt_register1
      dir
      P2p_services.Points.S.patch
      (fun point () (acl, peer_id) ->
        let open Lwt_result_syntax in
        match P2p.pool net with
        | None -> return_none
        | Some pool ->
            let*! () =
              match peer_id with
              | None -> Lwt.return_unit
              | Some peer_id -> P2p_pool.set_expected_peer_id pool point peer_id
            in
            let*! () =
              match acl with
              | None -> Lwt.return_unit
              | Some `Ban ->
                  (* ban and untrust *)
                  P2p_pool.Points.untrust pool point ;
                  P2p_pool.Points.ban pool point
              | Some `Trust ->
                  (* trust ( and implicitely unban ) *)
                  P2p_pool.Points.trust pool point ;
                  Lwt.return_unit
              | Some `Open ->
                  (* unban and untrust *)
                  P2p_pool.Points.unban pool point ;
                  P2p_pool.Points.untrust pool point ;
                  Lwt.return_unit
            in
            return
            @@ Option.map info_of_point_info (P2p_pool.Points.info pool point))
  in
  let dir =
    Tezos_rpc.Directory.gen_register1
      dir
      P2p_services.Points.S.events
      (fun point_id q () ->
        let open Lwt_syntax in
        match P2p.pool net with
        | None -> Tezos_rpc.Answer.fail [P2p_errors.P2p_layer_disabled]
        | Some pool -> (
            match P2p_pool.Points.info pool point_id with
            | None -> Tezos_rpc.Answer.return []
            | Some gi ->
                let evts = P2p_point_state.Info.events gi in
                if not q#monitor then Tezos_rpc.Answer.return evts
                else
                  let stream, stopper = P2p_point_state.Info.watch gi in
                  let shutdown () = Lwt_watcher.shutdown stopper in
                  let first_request = ref true in
                  let next () =
                    if not !first_request then
                      let+ o = Lwt_stream.get stream in
                      Option.map (fun i -> [i]) o
                    else (
                      first_request := false ;
                      Lwt.return_some evts)
                  in
                  Tezos_rpc.Answer.return_stream {next; shutdown}))
  in
  let dir =
    Tezos_rpc.Directory.gen_register1
      dir
      P2p_services.Points.S.banned
      (fun point () () ->
        match P2p.pool net with
        | None -> Tezos_rpc.Answer.fail [P2p_errors.P2p_layer_disabled]
        | Some pool when P2p_pool.Points.get_trusted pool point ->
            Tezos_rpc.Answer.return false
        | Some pool ->
            Tezos_rpc.Answer.return (P2p_pool.Points.banned pool point))
  in
  let dir =
    Tezos_rpc.Directory.register0
      dir
      P2p_services.ACL.S.get_greylisted_ips
      (fun () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            return
              {
                P2p_services.ACL.ips = P2p_pool.Points.get_greylisted_list pool;
                not_reliable_since =
                  P2p_pool.Points.greylisted_list_not_reliable_since pool;
              })
  in
  (* Network : Greylist *)
  let dir =
    Tezos_rpc.Directory.register0
      dir
      P2p_services.ACL.S.clear_delete
      (fun () () ->
        match P2p.pool net with
        | None -> tzfail P2p_errors.P2p_layer_disabled
        | Some pool ->
            P2p_pool.acl_clear pool ;
            return_unit)
  in
  dir
