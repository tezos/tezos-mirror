(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* Testing
   -------
   Component:    lib_p2p
   Invocation:   dune exec src/lib_p2p/test/main.exe
   Subject:      Check maintenance mechanism.
*)

module Event = struct
  include Internal_event.Simple

  let section = ["test"; "p2p"; "maintenance"]

  let x_active_connections =
    declare_1
      ~section
      ~name:"x_active_connections"
      ~msg:"{connections_count} connections are actives."
      ~level:Info
      ("connections_count", Data_encoding.int31)

  let maintenance_start =
    declare_0
      ~section
      ~name:"maintenance_start"
      ~msg:"maintenance has started."
      ~level:Debug
      ()

  let maintenance_stop =
    declare_0
      ~section
      ~name:"maintenance_stop"
      ~msg:"maintenance finished."
      ~level:Debug
      ()

  let maintenance_debug =
    declare_1
      ~section
      ~name:"maintenance_debug"
      ~msg:"maintenance {debug}."
      ~level:Debug
      ("debug", Data_encoding.string)

  let maintenance_event =
    declare_1
      ~section
      ~name:"maintenance_event"
      ~msg:"maintenance event {event}."
      ~level:Debug
      ~pp1:P2p_connection.P2p_event.pp
      ("event", P2p_connection.P2p_event.encoding)
end

let filteri f l =
  List.mapi (fun i x -> (i, x)) l
  |> List.filter (fun (i, x) -> f i x)
  |> List.split |> snd

let partitioni f l =
  let l1, l2 =
    List.partition (fun (i, x) -> f i x) (List.mapi (fun i x -> (i, x)) l)
  in
  let remove_i l = snd (List.split l) in
  (remove_i l1, remove_i l2)

let rec connect ~timeout connect_handler pool point =
  let open Lwt_syntax in
  let* () =
    Event.(emit maintenance_debug)
      (Format.asprintf "Connect to %a" P2p_point.Id.pp point)
  in
  let* r = P2p_connect_handler.connect connect_handler point ~timeout in
  match r with
  | Error (Tezos_p2p_services.P2p_errors.Connected :: _) -> (
      match P2p_pool.Connection.find_by_point pool point with
      | Some conn -> return_ok conn
      | None ->
          failwith
            "Something went wrong while connecting to %a"
            P2p_point.Id.pp
            point)
  | Error
      ((( Tezos_p2p_services.P2p_errors.Connection_refused
        | Tezos_p2p_services.P2p_errors.Pending_connection
        | Tezos_p2p_services.P2p_errors.Rejected_socket_connection
        | Tezos_p2p_services.P2p_errors.Rejected_by_nack _ | Canceled | Timeout
        | Tezos_p2p_services.P2p_errors.Rejected _ ) as head_err)
      :: _) ->
      let* () =
        Event.(emit maintenance_debug)
          (Format.asprintf
             "Connection to %a failed (%a) Retry@."
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
             head_err)
      in
      let* () = Lwt_unix.sleep (0.5 +. Random.float 2.) in
      connect ~timeout connect_handler pool point
  | (Ok _ | Error _) as res -> Lwt.return res

module Triggers = struct
  module Too_many_connections = struct
    (* Test.
       Test that the maintenance is triggered when there are too many active
       connections.
       1 - [target] maintenance is configured to not be trigger by the timer.
       2 - [target] maintenance establishes connections to [client]s and finish
           its initial maintain.
       3 - [target] establishes more connections to reach [max_connections - 1]
           active connections.
       4 - [client_overconnect] try to establish a connection to [target] until
           it success. Since, the number of connection is high, the connection
           can be rejected.
       5 - Checks that the connect handler triggers the maintenance and the
           maintenance starts. *)
    let target expected_connections max_connections overconnect_points
        (node : Node.t) =
      let open Lwt_result_syntax in
      (* As there is no way to deactivate the maintenance timer while keeping
         the maintenance active, we rely on the CI timeout being lesser than the
         maintenance_idle_time that we set.
      *)
      let maintenance_idle_time = Time.System.Span.of_seconds_exn 36000. in
      let time_between_looking_for_peers =
        Time.System.Span.of_seconds_exn 10.
      in
      let maintenance_watcher = Lwt_watcher.create_input () in
      let maintenance_log, maintenance_stopper =
        Lwt_watcher.create_stream maintenance_watcher
      in
      let maintenance =
        let maintenance_config =
          {
            P2p_maintenance.maintenance_idle_time;
            private_mode = false;
            min_connections = 0;
            max_connections;
            expected_connections;
            time_between_looking_for_peers;
          }
        in
        P2p_maintenance.create
          maintenance_config
          node.pool
          node.connect_handler
          node.trigger
          ~log:(Lwt_watcher.notify maintenance_watcher)
      in
      (* Activate the maintenance and wait until it becomes idle. *)
      P2p_maintenance.activate maintenance ;
      let*! () = Event.(emit maintenance_start) () in
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e -> e = P2p_connection.P2p_event.Maintenance_ended)
                 e)
          ~arg:maintenance_log
          ()
      in
      let active_connections = P2p_pool.active_connections node.pool in
      let*! () = Event.(emit maintenance_stop) () in
      let*! () = Event.(emit x_active_connections) active_connections in
      (* Establish new client connection to get to max_connections - 1. *)
      let nb_connections_to_add = max_connections - active_connections - 1 in
      (*assert (nb_connections_to_add <= List.length overconnect_points) ;*)
      let*! () =
        Event.(emit maintenance_debug)
          (Format.asprintf
             "nb_connections_to_add = %d - %d -1 = %d"
             max_connections
             active_connections
             nb_connections_to_add)
      in
      let* _ =
        List.map_ep
          (fun point ->
            connect
              ~timeout:(Time.System.Span.of_seconds_exn 10.)
              node.connect_handler
              node.pool
              point)
          (filteri (fun i _ -> i <= nb_connections_to_add) overconnect_points)
      in
      let active_connections = P2p_pool.active_connections node.pool in
      (*assert (active_connections = max_connections - 1) ;*)
      let*! () = Event.(emit x_active_connections) active_connections in
      (* We have max_connections - 1. Now we try to connect one mode client,
         and wait for a Too_many_connections event *)
      let* () = Node.sync node in
      (* Check that the maintenance has been triggered by the scheduler *)
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            let*! () =
              Event.(emit maintenance_debug) "waiting Too_many_connections"
            in
            let*! () =
              match e with
              | None -> Lwt.return_unit
              | Some e -> Event.(emit maintenance_event) e
            in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e ->
                   e = P2p_connection.P2p_event.Too_many_connections)
                 e)
          ~arg:maintenance_log
          ()
      in
      (* wait for the maintanance to start after the trigger *)
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e ->
                   e = P2p_connection.P2p_event.Maintenance_started)
                 e)
          ~arg:maintenance_log
          ()
      in
      let*! () = Event.(emit maintenance_start) () in
      (* End of the test *)
      let* () = Node.sync node in
      (* the number of connection is not back to max_connections - 1 *)
      let active_connections = P2p_pool.active_connections node.pool in
      (*assert (active_connections = max_connections - 1) ;*)
      let*! () = Event.(emit x_active_connections) active_connections in
      Lwt_watcher.shutdown maintenance_stopper ;
      let*! () = P2p_maintenance.shutdown maintenance in
      return_unit

    let client (node : Node.t) =
      let open Lwt_result_syntax in
      (* Try to connect one mode client *)
      let* () = Node.sync node in
      (* End of the test *)
      Node.sync node

    let client_overconnect (node : Node.t) =
      let open Lwt_result_syntax in
      let rec overconnect = function
        | [] -> Lwt.fail Alcotest.Test_error
        | point :: _ as points -> (
            let*! res =
              connect
                ~timeout:(Time.System.Span.of_seconds_exn 10.)
                node.connect_handler
                node.pool
                point
            in
            match res with
            | Error
                [
                  P2p_errors.Rejected_by_nack
                    {motive = P2p_rejection.Too_many_connections; _};
                ] ->
                overconnect points
            | Error _ -> Lwt.fail Alcotest.Test_error
            | Ok c -> Lwt.return c)
      in
      (* Try to connect one more client *)
      let* () = Node.sync node in
      let*! _conn = overconnect node.points in
      (* End of the test *)
      Node.sync node

    let node expected_connections max_connections overconnect_points = function
      | 0 -> target expected_connections max_connections overconnect_points
      | 1 -> client_overconnect
      | _ -> client

    let prefix = function 0 -> "Target_" | 1 -> "Overco_" | _ -> "Client_"

    let trusted initial_points i points =
      if i = 0 then initial_points else points

    (* expected_connections = 24
     * List.length points = 42
     * max_connections = 36
     *)
    let run points =
      let expected_connections = 24 in
      let max_connections i =
        if i = 0 then 3 * expected_connections / 2 else List.length points
      in
      (* min connection set to zero to avoid triggering the
         maintenance at node start *)
      let min_connections _ = 0 in
      let initial_points, overconnect_points =
        let client_points = filteri (fun i _ -> i > 1) points in
        (* we create a list of points. some of them are going to be the initial
         * points, and other used to trigger too_many_connections. We discriminate
         * this using the index of the list *)
        partitioni (fun i _ -> i < expected_connections) client_points
      in
      Node.detach_nodes
        ~prefix
        ~max_connections
        ~min_connections
        (node expected_connections (max_connections 0) overconnect_points)
        points
        ~trusted:(trusted initial_points)
  end

  module Too_few_connections = struct
    (* Test.
       Test that the maintenance is triggered when there are too few active
       connections.
       1 - [target] maintenance is configured to not be trigger by the timer.
       2 - [target] maintenance establishes connections to [client]s and finish
           its initial maintain.
       3 - [target] closes its connections to reach [min_connections] active
           connections.
       5 - Checks that the connect handler triggers the maintenance and the
           maintenance starts. *)
    let target expected_connections min_connections (node : Node.t) =
      let open Lwt_result_syntax in
      (* As there is no way to deactivate the maintenance timer while keeping
         the maintenance active, we rely on the CI timeout being lesser than the
         maintenance_idle_time that we set.
      *)
      let maintenance_idle_time = Time.System.Span.of_seconds_exn 36000. in
      let time_between_looking_for_peers =
        Time.System.Span.of_seconds_exn 10.
      in
      let maintenance_watcher = Lwt_watcher.create_input () in
      let maintenance_log, maintenance_stopper =
        Lwt_watcher.create_stream maintenance_watcher
      in
      let maintenance =
        let maintenance_config =
          {
            P2p_maintenance.maintenance_idle_time;
            private_mode = false;
            min_connections;
            max_connections = 400;
            expected_connections;
            time_between_looking_for_peers;
          }
        in
        P2p_maintenance.create
          maintenance_config
          node.pool
          node.connect_handler
          node.trigger
          ~log:(Lwt_watcher.notify maintenance_watcher)
      in
      (* Active the maintenance and wait until it become idle. *)
      P2p_maintenance.activate maintenance ;
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e -> e = P2p_connection.P2p_event.Maintenance_ended)
                 e)
          ~arg:maintenance_log
          ()
      in
      let active_connections = P2p_pool.active_connections node.pool in
      let*! () = Event.(emit x_active_connections) active_connections in
      (* Close connections to each min_connections. *)
      let nb_connections_to_close = active_connections - min_connections + 1 in
      let*! () =
        Lwt_list.iteri_p
          (fun i (_, conn) ->
            if i < nb_connections_to_close then
              P2p_conn.disconnect ~wait:true conn
            else Lwt.return_unit)
          (P2p_pool.Connection.list node.pool)
      in
      let active_connections = P2p_pool.active_connections node.pool in
      let*! () = Event.(emit x_active_connections) active_connections in
      (* Check that the maintenance has been triggered. *)
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e ->
                   e = P2p_connection.P2p_event.Too_few_connections)
                 e)
          ~arg:maintenance_log
          ()
      in
      let* () =
        P2p_test_utils.wait_pred_s
          ~pred:(fun log ->
            let*! e = Lwt_stream.get log in
            Lwt.return
            @@ Option.fold
                 ~none:false
                 ~some:(fun e ->
                   e = P2p_connection.P2p_event.Maintenance_started)
                 e)
          ~arg:maintenance_log
          ()
      in
      (* End of the test *)
      let* () = Node.sync node in
      Lwt_watcher.shutdown maintenance_stopper ;
      let*! () = P2p_maintenance.shutdown maintenance in
      return_unit

    let client (node : Node.t) =
      (* End of the test *)
      Node.sync node

    let node expected_connections min_connections i =
      if i = 0 then target expected_connections min_connections else client

    let prefix i = if i = 0 then "Target_" else "Client_"

    (*let trusted initial_points i points =*)
    (*if i = 0 then initial_points else points*)

    let run points =
      let expected_connections = 36 in
      let min_connections i = if i = 0 then expected_connections / 2 else 0 in
      Node.detach_nodes
        ~prefix
        ~min_connections
        (node expected_connections (min_connections 0))
        points
  end

  module Timed = struct
    (* TODO: checks that the maintenance maintains every
       [P2p_maintenance.config.maintenance_idle_time] seconds. *)
    let _run _nodes = Lwt_result_syntax.return_unit
  end
end

let gen_points ?(clients = 42) addr = Node.gen_points clients addr

let wrap addr n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      let open Lwt_syntax in
      let rec aux n f =
        let* r = f (gen_points addr) in
        match r with
        | Ok () -> Lwt.return_unit
        | Error (Exn (Unix.Unix_error ((EADDRINUSE | EADDRNOTAVAIL), _, _)) :: _)
          ->
            aux n f
        | Error error ->
            Format.kasprintf Stdlib.failwith "%a" pp_print_trace error
      in
      aux n f)

let main () =
  let lwt_log_sink =
    Lwt_log_sink_unix.create_cfg
      ~rules:
        "test.p2p.maintenance -> debug; p2p.maintenance -> debug; \
         p2p.connect_handler -> debug; test.p2p.node -> debug"
      ()
  in
  let () =
    Lwt_main.run (Tezos_base_unix.Internal_event_unix.init ~lwt_log_sink ())
  in
  let addr = Ipaddr.V6.of_string_exn "::ffff:127.0.0.1" in
  Lwt_main.run
  @@ Alcotest_lwt.run
       "tezos-p2p"
       [
         ( "p2p-maintenance",
           [
             wrap addr "triggers.too-few-connections" (fun points ->
                 Triggers.Too_few_connections.run points);
             wrap addr "triggers.too-many-connections" (fun points ->
                 Triggers.Too_many_connections.run points);
           ] );
       ]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()
