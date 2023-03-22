(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Testing
    -------
    Component:    P2P
    Invocation:   dune exec src/lib_p2p/test/main.exe
    Dependencies: src/lib_p2p/test/process.ml
    Subject:      Testing of the Pool
                  Each test launches nodes in separate process, each node
                  has its own pool and is given a function to be executed.
*)

include Internal_event.Legacy_logging.Make (struct
  let name = "test.p2p.connection-pool"
end)

type error += Connect | Write | Read

(** Detaches [!client] nodes. Each of them will send a [Ping] to each
    other node, then await for reading one from each other node.
*)
module Simple = struct
  let rec connect ~timeout connect_handler pool point =
    let open Lwt_syntax in
    let* () = lwt_debug "Connect to %a" P2p_point.Id.pp point in
    let* r = P2p_connect_handler.connect connect_handler point ~timeout in
    match r with
    | Error (Tezos_p2p_services.P2p_errors.Connected :: _) -> (
        match P2p_pool.Connection.find_by_point pool point with
        | Some conn -> return_ok conn
        | None -> failwith "Woops...")
    | Error
        ((( Tezos_p2p_services.P2p_errors.Connection_refused
          | Tezos_p2p_services.P2p_errors.Pending_connection
          | Tezos_p2p_services.P2p_errors.Rejected_socket_connection
          | Tezos_p2p_services.P2p_errors.Rejected_by_nack _ | Canceled
          | Timeout | Tezos_p2p_services.P2p_errors.Rejected _ ) as head_err)
        :: _) ->
        let* () =
          lwt_debug
            "Connection to %a failed (%a)@."
            P2p_point.Id.pp
            point
            (fun ppf err ->
              match err with
              | Tezos_p2p_services.P2p_errors.Connection_refused ->
                  Format.fprintf ppf "connection refused"
              | Tezos_p2p_services.P2p_errors.Pending_connection ->
                  Format.fprintf ppf "pending connection"
              | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                  Format.fprintf ppf "rejected"
              | Tezos_p2p_services.P2p_errors.Rejected_by_nack
                  {alternative_points = Some alternative_points; _} ->
                  Format.fprintf
                    ppf
                    "rejected (nack_v1, peer list: @[<h>%a@])"
                    P2p_point.Id.pp_list
                    alternative_points
              | Tezos_p2p_services.P2p_errors.Rejected_by_nack
                  {alternative_points = None; _} ->
                  Format.fprintf ppf "rejected (nack_v0)"
              | Canceled -> Format.fprintf ppf "canceled"
              | Timeout -> Format.fprintf ppf "timeout"
              | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                  Format.fprintf
                    ppf
                    "rejected (%a) motive:%a"
                    P2p_peer.Id.pp
                    peer
                    P2p_rejection.pp
                    motive
              | _ -> assert false)
            head_err
        in
        let* () = Lwt_unix.sleep (0.5 +. Random.float 2.) in
        connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res -> Lwt.return res

  let connect_all ~timeout connect_handler pool points =
    List.map_ep (connect ~timeout connect_handler pool) points

  let write_all conns msg =
    List.iter_ep (fun conn -> trace Write @@ P2p_conn.write_sync conn msg) conns

  let read_all conns =
    let open Lwt_result_syntax in
    List.iter_ep
      (fun conn ->
        let* msg = trace Read @@ P2p_conn.read conn in
        match msg with Node.Ping -> return_unit | _ -> assert false
        (* in this test only Ping messages are used. *))
      conns

  let close_all conns = List.iter_p P2p_conn.disconnect conns

  let node (node : Node.t) =
    let open Lwt_result_syntax in
    let* conns =
      connect_all
        ~timeout:(Time.System.Span.of_seconds_exn 2.)
        node.connect_handler
        node.pool
        node.points
    in
    let*! () = lwt_debug "Bootstrap OK@." in
    let* () = Node.sync node in
    let* () = write_all conns Node.Ping in
    let*! () = lwt_debug "Sent all messages.@." in
    let* () = Node.sync node in
    let* () = read_all conns in
    let*! () = lwt_debug "Read all messages.@." in
    let* () = Node.sync node in
    let*! () = close_all conns in
    let*! () = lwt_debug "All connections successfully closed.@." in
    return_unit

  let run points = Node.detach_nodes (fun _ -> node) points
end

(** Detaches a number of nodes (which is [!clients]). Each of them will
    connect to each other node at random points in time, to send a
    ping, await for a message, then disconnect. This process is
    repeated [repeat] times.
*)
module Random_connections = struct
  let rec connect_random connect_handler pool total rem point n =
    let open Lwt_result_syntax in
    let*! () = Lwt_unix.sleep (0.2 +. Random.float 1.0) in
    let* conn =
      trace Connect
      @@ Simple.connect
           ~timeout:(Time.System.Span.of_seconds_exn 2.)
           connect_handler
           pool
           point
    in
    let*! _ = trace Write @@ P2p_conn.write conn Node.Ping in
    let* _ = trace Read @@ P2p_conn.read conn in
    let*! () = Lwt_unix.sleep (0.2 +. Random.float 1.0) in
    let*! () = P2p_conn.disconnect conn in
    let*! () =
      decr rem ;
      if !rem mod total = 0 then lwt_debug "Remaining: %d.@." (!rem / total)
      else Lwt.return_unit
    in
    if n > 1 then connect_random connect_handler pool total rem point (pred n)
    else return_unit

  let connect_random_all connect_handler pool points n =
    let total = List.length points in
    let rem = ref (n * total) in
    List.iter_ep
      (fun point -> connect_random connect_handler pool total rem point n)
      points

  let node repeat (node : Node.t) =
    let open Lwt_result_syntax in
    let*! () = lwt_debug "Begin random connections.@." in
    let* () =
      connect_random_all node.connect_handler node.pool node.points repeat
    in
    let*! () = lwt_debug "Random connections OK.@." in
    return_unit

  let run points repeat = Node.detach_nodes (fun _ -> node repeat) points
end

(** Detaches a number of nodes. Each will connect to all other nodes
    (peers) of the pool, sync, write garbled data, then await for
    reading a message. Their process will finish whenever all
    connections are closed. It is asserted that each closed connection
    comes with an error.
*)
module Garbled = struct
  let is_connection_closed = function
    | Error
        ((Write | Read) :: Tezos_p2p_services.P2p_errors.Connection_closed :: _)
      ->
        true
    | Ok _ -> false
    | Error err ->
        debug "Unexpected error: %a@." pp_print_trace err ;
        false

  let write_bad_all conns =
    let bad_msg = Bytes.of_string (String.make 16 '\000') in
    List.iter_ep
      (fun conn ->
        trace Write @@ P2p_conn.Internal_for_tests.raw_write_sync conn bad_msg)
      conns

  let node (node : Node.t) =
    let open Lwt_result_syntax in
    let* conns =
      Simple.connect_all
        ~timeout:(Time.System.Span.of_seconds_exn 2.)
        node.connect_handler
        node.pool
        node.points
    in
    let* () = Node.sync node in
    let*! err =
      let* () = write_bad_all conns in
      Simple.read_all conns
    in
    if is_connection_closed err then return_unit
    else
      let pos (file, lnum, cnum, _) = (file, lnum, cnum) in
      fail_with_exn (Assert_failure (pos __POS__))

  let run points = Node.detach_nodes (fun _ -> node) points
end

module Overcrowded = struct
  type error += Advertisement_failure of P2p_point.Id.t list

  let () =
    register_error_kind
      `Permanent
      ~id:"test_p2p_pool.Overcrowded.advertisement_failure"
      ~title:"Advertisement Failure"
      ~description:"The given list of points should be known, but are not."
      ~pp:(fun ppf lst ->
        Format.fprintf
          ppf
          "The given list of points should be known, but are not : %a"
          P2p_point.Id.pp_list
          lst)
      Data_encoding.(obj1 (req "value" (list P2p_point.Id.encoding)))
      (function Advertisement_failure l -> Some l | _ -> None)
      (fun l -> Advertisement_failure l)

  let rec connect ?iter_count ~timeout connect_handler pool point =
    let open Lwt_syntax in
    let* () =
      lwt_debug
        "Connect%a to %a@."
        (fun ppf iter_count ->
          Option.iter (Format.fprintf ppf " to peer %d") iter_count)
        iter_count
        P2p_point.Id.pp
        point
    in
    let* r = P2p_connect_handler.connect connect_handler point ~timeout in
    match r with
    | Error [Tezos_p2p_services.P2p_errors.Connected] -> (
        match P2p_pool.Connection.find_by_point pool point with
        | Some conn -> return_ok conn
        | None -> failwith "Woops...")
    | Error
        [
          (( Tezos_p2p_services.P2p_errors.Connection_refused
           | Tezos_p2p_services.P2p_errors.Pending_connection
           | Tezos_p2p_services.P2p_errors.Rejected_socket_connection | Canceled
           | Timeout | Tezos_p2p_services.P2p_errors.Rejected _ ) as err);
        ] ->
        let* () =
          lwt_debug
            "Connection to%a %a failed (%a)@."
            (fun ppf iter_count ->
              Option.iter (Format.fprintf ppf " peer %d") iter_count)
            iter_count
            P2p_point.Id.pp
            point
            (fun ppf err ->
              match err with
              | Tezos_p2p_services.P2p_errors.Connection_refused ->
                  Format.fprintf ppf "connection refused"
              | Tezos_p2p_services.P2p_errors.Pending_connection ->
                  Format.fprintf ppf "pending connection"
              | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                  Format.fprintf ppf "rejected"
              | Canceled -> Format.fprintf ppf "canceled"
              | Timeout -> Format.fprintf ppf "timeout"
              | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                  Format.fprintf
                    ppf
                    "rejected (%a) motive:%a"
                    P2p_peer.Id.pp
                    peer
                    P2p_rejection.pp
                    motive
              | _ -> assert false)
            err
        in
        let* () = Lwt_unix.sleep (0.5 +. Random.float 2.) in
        connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res -> Lwt.return res

  (** Node code of nodes that will connect to the target,
      and either get a list of pairs or have an established connection.
  *)
  let client_connect connect_handler pool legacy trusted_points all_points =
    let open Lwt_result_syntax in
    debug
      "@[<v 2>client connects to %a in the universe @[%a@]@]@."
      P2p_point.Id.pp_list
      trusted_points
      P2p_point.Id.pp_list
      all_points ;
    let port =
      Option.value
        ~default:0
        (P2p_connect_handler.config connect_handler).listening_port
    in
    let target =
      WithExceptions.Option.get ~loc:__LOC__ @@ List.hd trusted_points
    in
    let*! r =
      connect
        ~iter_count:0
        ~timeout:(Time.System.Span.of_seconds_exn 2.)
        connect_handler
        pool
        target
    in
    match r with
    | Ok conn ->
        let*! () =
          lwt_debug
            "Not good: connection accepted while it should be rejected (local: \
             %d, remote: %d).@."
            port
            (snd target)
        in
        let*! () = P2p_conn.disconnect conn in
        Error_monad.failwith
          "Overcrowded error: connection should be rejected (local: %d, \
           remote: %d).@."
          port
          (snd target)
    | Error
        [
          Tezos_p2p_services.P2p_errors.Rejected_by_nack
            {alternative_points = None; _};
        ] as err ->
        if legacy then
          let*! () =
            lwt_debug
              "Good: client is rejected without point list (local: %d, remote: \
               %d)@."
              port
              (snd target)
          in
          return_unit
        else
          let*! () =
            lwt_debug
              "Not good: client is rejected without point list (local: %d, \
               remote: %d)@."
              port
              (snd target)
          in
          Lwt.return err
    | Error
        [
          Tezos_p2p_services.P2p_errors.Rejected_by_nack
            {alternative_points = Some alternative_points; _};
        ] ->
        let*! () =
          lwt_debug
            "Good: client is rejected with point list (local: %d, remote: %d) \
             @[%a@]@."
            port
            (snd target)
            P2p_point.Id.pp_list
            alternative_points
        in
        return_unit
    | Error _ as res -> Lwt.return res

  let client_knowledge pool all_points =
    let unknowns, known =
      P2p_pool.Points.fold_known
        pool
        ~init:(all_points, [])
        ~f:(fun id _ (unknown_points, known) ->
          let unknown_points =
            List.filter
              (fun unk_id -> not (P2p_point.Id.equal id unk_id))
              unknown_points
          in
          (unknown_points, id :: known))
    in
    (unknowns, known)

  let client_check pool all_points legacy =
    let unknowns, _known = client_knowledge pool all_points in
    let advert_succeed = unknowns = [] in
    if legacy || advert_succeed then
      debug
        "Good: Advertisement%s worked as intended.@."
        (if legacy then " legacy" else "")
    else
      debug
        "@[<v 2>Not Good: advertisement failure. legacy %b. unknowns :  @[%a@]\n\
         \t knowns : @[%a@].@."
        legacy
        P2p_point.Id.pp_list
        unknowns
        P2p_point.Id.pp_list
        _known ;
    fail_unless
      (if legacy then not advert_succeed else advert_succeed)
      (Advertisement_failure unknowns)

  let client legacy (node : Node.t) =
    let open Lwt_result_syntax in
    if Compare.List_length_with.(node.points > 50) then (
      log_error
        "This test only works for less clients than the advertisement list \
         length (50)@." ;
      assert false) ;
    (*   *)
    (* first connection: let advertise us as public nodes *)
    let* () =
      client_connect
        node.connect_handler
        node.pool
        legacy
        node.trusted_points
        node.points
    in
    let* () = Node.sync node in
    (* sync 2 *)
    let* () =
      client_connect
        node.connect_handler
        node.pool
        legacy
        node.trusted_points
        node.points
    in
    let* () = Node.sync node in
    (* sync 3 *)
    let* () = client_check node.pool node.points legacy in
    let* () = Node.sync node in
    (* sync 4 *)
    let*! () = lwt_debug "client closing.@." in
    return_unit

  (** Code of the target that should be overcrowded by all the clients. *)
  let target (node : Node.t) =
    let open Lwt_result_syntax in
    let unknowns_knowns () =
      P2p_pool.Points.fold_known
        node.pool
        ~init:(node.points, [])
        ~f:(fun id _ (unknown_points, knowns) ->
          let unknown_points =
            List.filter
              (fun unk_id -> not (P2p_point.Id.equal id unk_id))
              unknown_points
          in
          (unknown_points, id :: knowns))
    in
    let unknowns, knowns = unknowns_knowns () in
    let log, stopper = Lwt_watcher.create_stream node.watcher in
    let*! () =
      lwt_debug "trusted : %a" P2p_point.Id.pp_list node.trusted_points
    in
    let*! () = lwt_debug "unknown : %a" P2p_point.Id.pp_list unknowns in
    let*! () = lwt_debug "known : %a" P2p_point.Id.pp_list knowns in
    let _pool_log =
      Lwt_stream.iter (debug "p2p event %a" P2p_connection.P2p_event.pp) log
    in
    let*! () = lwt_debug "Target waiting@." in
    let* () = Node.sync node in
    (* sync 2 *)
    let* () = Node.sync node in
    (* sync 3 *)
    let* () = Node.sync node in
    (* sync 4 *)
    Lwt_watcher.shutdown stopper ;
    let*! () = lwt_debug "Target closing.@." in
    return_unit

  let node i = if i = 0 then target else client false

  let node_mixed i = if i = 0 then target else client (i mod 2 = 1)

  let trusted i points =
    if i = 0 then points
    else [WithExceptions.Option.get ~loc:__LOC__ @@ List.hd points]

  (** Detaches a number of nodes: one of them is the target (its
      max_incoming_connections is set to zero), and all the rest are
      clients (knowing only the target). The target will be overcrowded
      by the other clients connecting to it. All clients should have
      their pool populated with the list of points. Specifically for
      this test function, each client use p2p v.1 and check that they
      eventually know all other nodes thanks to the node reply.
  *)
  let run points =
    (* setting connections to -1 ensure there will be no random
       acceptance of connections *)
    let prefix = function 0 -> "Target_" | _ -> "Client_"
    and min_connections = function 0 -> -1 | _ -> 1
    and max_connections = function 0 -> -1 | _ -> 1
    and max_incoming_connections = function _ -> List.length points in
    Node.detach_nodes
      ~prefix
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      node
      points
      ~trusted

  (** Same as previously but by mixing different protocol version
      tags. Half the clients do as in run and half of the clients use
      p2p v.0 and check that they didn't receive new nodes (meaning
      that [target] actually sent a Nack_v_0.
  *)
  let run_mixed_versions points =
    let prefix = function
      | 0 -> "Target_"
      | i -> if i mod 2 = 1 then "Client_v0_" else "Client_v1_"
    and min_connections = function 0 -> -1 | _ -> 1
    and max_connections = function 0 -> -1 | _ -> 1
    and max_incoming_connections = function _ -> List.length points
    and p2p_versions i =
      if i mod 2 = 1 then [P2p_version.zero]
      else [P2p_version.zero; P2p_version.one]
    in
    Node.detach_nodes
      ~prefix
      ~p2p_versions
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      node_mixed
      points
      ~trusted
end

(** Detaches a number of nodes (one target, the rest being clients) on
    a different network. It is asserted that the connection must be
    rejected due to the fact that the target is not on a common
    network.
*)
module No_common_network = struct
  let rec connect ?iter_count ~timeout connect_handler pool point =
    let open Lwt_syntax in
    let* () =
      lwt_debug
        "Connect%a to @[%a@]@."
        (fun ppf iter_count ->
          Option.iter (Format.fprintf ppf " to peer %d") iter_count)
        iter_count
        P2p_point.Id.pp
        point
    in
    let* r = P2p_connect_handler.connect connect_handler point ~timeout in
    match r with
    | Error [Tezos_p2p_services.P2p_errors.Connected] -> (
        match P2p_pool.Connection.find_by_point pool point with
        | Some conn -> return_ok conn
        | None -> failwith "Woops...")
    | Error
        [
          (( Tezos_p2p_services.P2p_errors.Connection_refused
           | Tezos_p2p_services.P2p_errors.Pending_connection
           | Tezos_p2p_services.P2p_errors.Rejected_socket_connection | Canceled
           | Timeout | Tezos_p2p_services.P2p_errors.Rejected _ ) as err);
        ] ->
        let* () =
          lwt_debug
            "Connection to%a %a failed (%a)@."
            (fun ppf iter_count ->
              Option.iter (Format.fprintf ppf " peer %d") iter_count)
            iter_count
            P2p_point.Id.pp
            point
            (fun ppf err ->
              match err with
              | Tezos_p2p_services.P2p_errors.Connection_refused ->
                  Format.fprintf ppf "connection refused"
              | Tezos_p2p_services.P2p_errors.Pending_connection ->
                  Format.fprintf ppf "pending connection"
              | Tezos_p2p_services.P2p_errors.Rejected_socket_connection ->
                  Format.fprintf ppf "rejected"
              | Canceled -> Format.fprintf ppf "canceled"
              | Timeout -> Format.fprintf ppf "timeout"
              | Tezos_p2p_services.P2p_errors.Rejected {peer; motive} ->
                  Format.fprintf
                    ppf
                    "rejected (%a) motive:%a"
                    P2p_peer.Id.pp
                    peer
                    P2p_rejection.pp
                    motive
              | _ -> assert false)
            err
        in
        let* () = Lwt_unix.sleep (0.5 +. Random.float 2.) in
        connect ~timeout connect_handler pool point
    | (Ok _ | Error _) as res -> Lwt.return res

  (** Node code of nodes that will connect to the target,
      and either get a list of pairs or have an established connection.
  *)
  let client_connect connect_handler pool trusted_points all_points =
    let open Lwt_result_syntax in
    debug
      "@[<v 2>client connects to %a in the universe @[%a@]@]@."
      P2p_point.Id.pp_list
      trusted_points
      P2p_point.Id.pp_list
      all_points ;
    let*! r =
      connect
        ~iter_count:0
        ~timeout:(Time.System.Span.of_seconds_exn 2.)
        connect_handler
        pool
        (WithExceptions.Option.get ~loc:__LOC__ @@ List.hd trusted_points)
    in
    match r with
    | Ok conn ->
        let*! () =
          lwt_debug
            "Not good: connection accepted while it should be rejected.@."
        in
        let*! () = P2p_conn.disconnect conn in
        return_unit
    | Error
        [Tezos_p2p_services.P2p_errors.Rejected_no_common_protocol {announced}]
      ->
        let*! () =
          lwt_debug
            "Good: Connection cannot be established,no common network with \
             @[%a@].@."
            Network_version.pp
            announced
        in
        return_unit
    | Error _ as res -> Lwt.return res

  let client (node : Node.t) =
    let open Lwt_result_syntax in
    let* () =
      client_connect
        node.connect_handler
        node.pool
        node.trusted_points
        node.points
    in
    let* () = Node.sync node in
    (* sync 2 *)
    let*! () = lwt_debug "client closing.@." in
    return_unit

  (** Code of the target that should be overcrowded by all the clients. *)
  let target (node : Node.t) =
    let open Lwt_result_syntax in
    let*! () = lwt_debug "Target waiting.@." in
    let* () = Node.sync node in
    (* sync 2 *)
    let*! () = lwt_debug "Target closing.@." in
    return_unit

  let node i = if i = 0 then target else client

  let trusted i points =
    if i = 0 then points
    else [WithExceptions.Option.get ~loc:__LOC__ @@ List.hd points]

  (** Running the target and the clients.
      All clients should have their pool populated with the list of points.

  *)
  let run points =
    let point_count = List.length points in
    let min_connections = function _ -> 0
    and max_connections = function 0 -> point_count | _ -> 1
    and max_incoming_connections = function _ -> point_count
    and p2p_versions = function
      | 0 -> [P2p_version.zero]
      | _ -> [P2p_version.one]
    in
    Node.detach_nodes
      ~timeout:10.
      ~min_connections
      ~max_connections
      ~max_incoming_connections
      ~p2p_versions
      node
      points
      ~trusted
end

let () = Random.self_init ()

let init_logs =
  let lwt_log_sink =
    Lwt_log_sink_unix.create_cfg
      ~rules:"test.p2p.connection-pool -> info; p2p.connection-pool -> info"
      ()
  in
  lazy (Tezos_base_unix.Internal_event_unix.init ~lwt_log_sink ())

let points = ref []

let wrap ?addr ?port ?(clients = 10) n f =
  let gen_points addr = points := Node.gen_points ?port clients addr in
  let addr =
    Option.value ~default:(Ipaddr.V6.of_string_exn "::ffff:127.0.0.1") addr
  in
  gen_points addr ;
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      let open Lwt_syntax in
      let* () = Lazy.force init_logs in
      let rec aux n f =
        let* r = f () in
        match r with
        | Ok () -> Lwt.return_unit
        | Error (Exn (Unix.Unix_error ((EADDRINUSE | EADDRNOTAVAIL), _, _)) :: _)
          ->
            warn "Conflict on ports, retry the test." ;
            gen_points addr ;
            aux n f
        | Error error ->
            Format.kasprintf Stdlib.failwith "%a" pp_print_trace error
      in
      aux n f)

let main () =
  let repeat_connections = 5 in
  Lwt_main.run
  @@ Alcotest_lwt.run
       "tezos-p2p"
       [
         ( "p2p-connection-pool",
           [
             wrap "simple" (fun _ -> Simple.run !points);
             wrap "random" (fun _ ->
                 Random_connections.run !points repeat_connections);
             wrap "garbled" (fun _ -> Garbled.run !points);
             wrap "overcrowded" (fun _ -> Overcrowded.run !points);
             wrap "overcrowded-mixed" (fun _ ->
                 Overcrowded.run_mixed_versions !points);
             wrap "no-common-network-protocol" (fun _ ->
                 No_common_network.run !points);
           ] );
       ]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()
