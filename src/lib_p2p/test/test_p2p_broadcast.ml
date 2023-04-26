(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Invocation:   dune exec src/lib_p2p/test/test_p2p_broadcast.exe
    Dependencies: src/lib_p2p/test/process.ml
    Subject:      Testing of the Broadcast
                  Each test launches nodes in separate process, each node
                  has its own pool and is given a function to be executed.
*)

include Internal_event.Legacy_logging.Make (struct
  let name = "test.p2p.broadcast"
end)

type error += Read | Wrong_message_received | Wrong_message_count

let () = Random.self_init ()

(* options that are hardcoded here *)
let repeat = 5

let nb_oph = 1000

let no_check = false

let init_logs =
  let log_cfg =
    Some
      (Tezos_base_unix.Logs_simple_config.create_cfg
         ~rules:"test.p2p.connection-pool -> info; p2p.connection-pool -> info"
         ())
  in
  lazy (Tezos_base_unix.Internal_event_unix.init ?log_cfg ())

(** Detaches [!client] nodes. Each of them will send a [BigPing] to each
    other node, then await for reading one from each other node.
*)
module Simple = struct
  let message () =
    List.map
      (fun _ ->
        List.map
          (fun _ -> Operation_hash.of_bytes_exn (Bytes.create 32))
          (1 -- nb_oph))
      (if no_check then 0 -- 1 else 0 -- repeat)

  let message_is_ok m1 m2 : (unit, error trace) result =
    let open Result_syntax in
    if no_check then return_unit
    else
      List.fold_left2_e
        ~when_different_lengths:(Error_monad.TzTrace.make Wrong_message_count)
        (fun () (m1 : Operation_hash.t) (m2 : Operation_hash.t) ->
          if Operation_hash.equal m1 m2 then Ok ()
          else tzfail Wrong_message_received)
        ()
        m1
        m2

  let peer_table = P2p_peer.Table.create 100

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
    | Ok res ->
        P2p_peer.Table.add peer_table (P2p_conn.peer_id res) res ;
        Lwt.return (Ok res)
    | Error _ as res -> Lwt.return res

  let connect_all ~timeout connect_handler pool points =
    List.map_ep (connect ~timeout connect_handler pool) points

  let write_all msg = P2p.Internal_for_tests.broadcast_conns peer_table msg

  let read conn ref_msg =
    let open Lwt_result_syntax in
    let* msg = trace Read @@ P2p_conn.read conn in
    match msg with
    | Node.BigPing l -> Lwt.return @@ message_is_ok l ref_msg
    | Ping -> tzfail Wrong_message_received

  let read_all pool ref_msg =
    let open Lwt_result_syntax in
    P2p_pool.Connection.fold
      ~f:(fun _ conn acc ->
        let* () = acc in
        read conn ref_msg)
      ~init:return_unit
      pool

  let close_all pool =
    let open Lwt_syntax in
    P2p_pool.Connection.fold
      ~f:(fun _ conn acc ->
        let* () = P2p_conn.disconnect conn in
        acc)
      ~init:Lwt.return_unit
      pool

  let mean list =
    let count = ref 0 in
    let sum = ref 0. in
    List.iter
      (fun value ->
        incr count ;
        sum := !sum +. value)
      list ;
    !sum /. float !count

  let median list =
    let sorted = List.sort Float.compare list |> Array.of_list in
    let count = Array.length sorted in
    if count > 0 then
      if count mod 2 = 0 then
        let i = count / 2 in
        (sorted.(i - 1) +. sorted.(i)) /. 2.
      else sorted.(count / 2)
    else invalid_arg "Long_test.median: empty list"

  let stddev list =
    let list_mean = mean list in
    let squared_diffs =
      List.map
        (fun x ->
          let d = x -. list_mean in
          d *. d)
        list
    in
    sqrt (mean squared_diffs)

  let broadcaster msgs (node : Node.t) =
    let open Lwt_result_syntax in
    let* _conns =
      connect_all
        ~timeout:(Time.System.Span.of_seconds_exn 2.)
        node.connect_handler
        node.pool
        node.points
    in
    let*! () = lwt_debug "Bootstrap OK@." in
    let* () = Node.sync node in
    let rec loop n acc gacc msg =
      if n <= 0 then return (acc, gacc)
      else
        match msg with
        | [] -> return (acc, gacc)
        | ref_msg :: next ->
            let start_global_time = Ptime_clock.now () in
            let start_time = Ptime_clock.now () in
            write_all (Node.BigPing ref_msg) ;
            let end_time = Ptime_clock.now () in
            let span = Ptime.diff end_time start_time in
            let*! () =
              lwt_debug "Broadcast message in %a.@." Ptime.Span.pp span
            in
            let* () = Node.sync node in
            let*! () = lwt_debug "Wait others.@." in
            let* () = Node.sync node in
            let end_global_time = Ptime_clock.now () in
            let gspan = Ptime.diff end_global_time start_global_time in
            loop
              (n - 1)
              (span :: acc)
              (gspan :: gacc)
              (if no_check then next @ [ref_msg] else next)
    in
    let* times, gtimes = loop repeat [] [] msgs in
    let print_stat times name =
      let ftimes = List.map Ptime.Span.to_float_s times in
      lwt_debug
        "%s; %f; %f; %f; %f; %f"
        name
        (List.fold_left Float.max Float.min_float ftimes)
        (List.fold_left Float.min Float.max_float ftimes)
        (mean ftimes)
        (median ftimes)
        (stddev ftimes)
    in
    let*! () = close_all node.pool in
    let*! () = lwt_debug "All connections successfully closed.@." in
    let*! () = lwt_debug "type; max; min; avg; median; std_dev" in
    let*! () = print_stat times "broadcasting" in
    let*! () = print_stat gtimes "global" in

    return_unit

  let node msgs (node : Node.t) =
    let open Lwt_result_syntax in
    let*! () = lwt_debug "Bootstrap OK@." in
    let* () = Node.sync node in
    let rec loop n msg =
      if n <= 0 then return_unit
      else
        match msg with
        | [] -> return_unit
        | ref_msg :: next ->
            let*! () = lwt_debug "Wait broadcaster.@." in
            let* () = Node.sync node in
            let* _msgs = read_all node.pool ref_msg in
            let*! () = lwt_debug "Read message.@." in
            let* () = Node.sync node in
            loop (n - 1) (if no_check then next @ [ref_msg] else next)
    in
    loop repeat msgs

  let run points =
    (* Messages are precomputed for every iteration and shared between
       processes to allow checking their content *)
    debug "Running broadcast test on %d points.@." (List.length points) ;
    let msgs = message () in
    Node.detach_nodes
      (fun i -> if i = 0 then broadcaster msgs else node msgs)
      points
end

let wrap addr n f =
  Alcotest_lwt.test_case n `Quick (fun _ () ->
      let open Lwt_syntax in
      let* () = Lazy.force init_logs in
      let rec aux n f =
        let points = Node.gen_points 3 addr in
        let* r = f points in
        match r with
        | Ok () -> Lwt.return_unit
        | Error (Exn (Unix.Unix_error ((EADDRINUSE | EADDRNOTAVAIL), _, _)) :: _)
          ->
            warn "Conflict on ports, retry the test." ;
            aux n f
        | Error error ->
            Format.kasprintf Stdlib.failwith "%a" pp_print_trace error
      in
      aux n f)

let main () =
  let addr = Node.default_ipv6_addr in
  Lwt_main.run
  @@ Alcotest_lwt.run
       ~__FILE__
       "tezos-p2p-broadcast"
       [("p2p-broadcast", [wrap addr "simple" Simple.run])]

let () =
  Sys.catch_break true ;
  try main () with _ -> ()

let () = Tezt.Test.run ()
