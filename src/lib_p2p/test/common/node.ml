(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This module creates simple p2p nodes. The others parts of the shell are
   disabled.
   The function [detach_nodes] creates a network of nodes.
   The message [Ping] is the unique none p2p message usde by nodes.
   The function [sync] is used to create a barrier for all the nodes of a
   network. *)

module Event = struct
  include Internal_event.Simple

  let section = ["test"; "p2p"; "node"]

  let process_timeout =
    declare_0
      ~section
      ~name:"process_timeout"
      ~msg:"Process timeout"
      ~level:Debug
      ()

  let node_ready =
    declare_1
      ~section
      ~name:"node_ready"
      ~msg:"Node ready (port: {port})@."
      ~level:Info
      ("port", Data_encoding.uint16)

  let sync_iteration =
    declare_1
      ~section
      ~name:"sync_iteration"
      ~msg:"Sync iteration {iteration}."
      ~level:Debug
      ("iteration", Data_encoding.int31)

  let shutting_down =
    declare_0
      ~section
      ~name:"shutting_down"
      ~msg:"Shuting done..."
      ~level:Info
      ()

  let bye = declare_0 ~section ~name:"bye" ~msg:"Bye." ~level:Info ()
end

type message = Ping | BigPing of Operation_hash.t list

let msg_config : message P2p_params.message_config =
  let open Data_encoding in
  let case ?max_length ~tag ~title encoding unwrap wrap =
    P2p_params.Encoding {tag; title; encoding; wrap; unwrap; max_length}
  in
  {
    encoding =
      [
        case
          ~tag:0x10
          ~title:"Ping"
          empty
          (function Ping -> Some () | _ -> None)
          (fun () -> Ping);
        case
          ~tag:0x11
          ~title:"BigPing"
          (list Operation_hash.encoding)
          (function BigPing l -> Some l | _ -> None)
          (fun l -> BigPing l);
      ];
    chain_name = Distributed_db_version.Name.of_string "SANDBOXED_TEZOS";
    distributed_db_versions = Distributed_db_version.[zero; one];
  }

type metadata = Metadata

let metadata_encoding =
  Data_encoding.conv (fun _ -> ()) (fun _ -> Metadata) Data_encoding.empty

let peer_meta_config : metadata P2p_params.peer_meta_config =
  {
    peer_meta_encoding = metadata_encoding;
    peer_meta_initial = (fun _ -> Metadata);
    score = (fun Metadata -> 0.);
  }

let conn_meta_config : metadata P2p_params.conn_meta_config =
  {
    conn_meta_encoding = metadata_encoding;
    conn_meta_value = (fun () -> Metadata);
    private_node = (fun _ -> false);
  }

type t = {
  iteration : int ref;
  channel : (unit, unit) Process.Channel.t;
  connect_handler : (message, metadata, metadata) P2p_connect_handler.t;
  pool : (message, metadata, metadata) Tezos_p2p.P2p_pool.t;
  watcher : P2p_connection.P2p_event.t Lwt_watcher.input;
  trigger : P2p_trigger.t;
  points : P2p_point.Id.t list;
  trusted_points : P2p_point.Id.t list;
}

(** Syncing inside the detached process *)
let sync (node : t) =
  let open Lwt_result_syntax in
  incr node.iteration ;
  let*! () = Event.(emit sync_iteration) !(node.iteration) in
  let* () = Process.Channel.push node.channel () in
  Process.Channel.pop node.channel

(** Syncing from the main process everyone until one node fails to sync  *)
let rec sync_nodes nodes =
  let open Lwt_result_syntax in
  let* () = List.iter_ep (fun p -> Process.receive p) nodes in
  let* () = List.iter_ep (fun p -> Process.send p ()) nodes in
  sync_nodes nodes

let sync_nodes nodes =
  let open Lwt_result_syntax in
  let*! r = sync_nodes nodes in
  match r with
  | Ok () | Error (Exn End_of_file :: _) -> return_unit
  | Error _ as err -> Lwt.return err

(**Detach a process with a p2p_pool and a welcome worker.  *)
let detach_node ?(prefix = "") ?timeout ?(min_connections : int option)
    ?max_connections ?max_incoming_connections ?p2p_versions
    ?(msg_config = msg_config) canceler f trusted_points all_points addr port =
  let trusted_points =
    List.filter
      (fun p -> not (P2p_point.Id.equal (addr, port) p))
      trusted_points
  in
  let proof_of_work_target = Tezos_crypto.Crypto_box.make_pow_target 0. in
  let identity = P2p_identity.generate_with_pow_target_0 () in
  let private_mode = false in
  let nb_points = List.length trusted_points in
  let unopt = Option.value ~default:nb_points in
  let connect_handler_cfg =
    P2p_connect_handler.
      {
        identity;
        proof_of_work_target;
        listening_port = Some port;
        advertised_port = Some port;
        private_mode;
        reconnection_config = Point_reconnection_config.default;
        min_connections = unopt min_connections;
        max_connections = unopt max_connections;
        max_incoming_connections = unopt max_incoming_connections;
        connection_timeout = Time.System.Span.of_seconds_exn 10.;
        authentication_timeout = Time.System.Span.of_seconds_exn 2.;
        incoming_app_message_queue_size = None;
        incoming_message_queue_size = None;
        outgoing_message_queue_size = None;
        binary_chunks_size = None;
        disable_peer_discovery = false;
      }
  in
  let pool_config =
    P2p_pool.
      {
        identity;
        trusted_points = List.map (fun p -> (p, None)) trusted_points;
        peers_file = "/dev/null";
        private_mode;
        max_known_points = None;
        max_known_peer_ids = None;
        peer_greylist_size = 10;
        ip_greylist_size_in_kilobytes = 1024;
        ip_greylist_cleanup_delay = Time.System.Span.of_seconds_exn 60.;
      }
  in
  Process.detach
    ~prefix:
      (Format.asprintf
         "%s%a:%d: "
         prefix
         P2p_peer.Id.pp_short
         identity.peer_id
         port)
    ~canceler
    (fun channel ->
      let open Lwt_result_syntax in
      let timer ti =
        let*! () = Lwt_unix.sleep ti in
        Event.(emit process_timeout) ()
      in
      with_timeout
        ~canceler
        (Option.fold_f ~some:timer ~none:Lwt_utils.never_ending timeout)
        (fun _canceler ->
          let iteration = ref 0 in
          let sched = P2p_io_scheduler.create ~read_buffer_size:(1 lsl 12) () in
          let trigger = P2p_trigger.create () in
          let watcher = Lwt_watcher.create_input () in
          let log event = Lwt_watcher.notify watcher event in
          let*! pool =
            P2p_pool.create pool_config peer_meta_config ~log trigger
          in
          let answerer = lazy (P2p_protocol.create_private ()) in
          let connect_handler =
            P2p_connect_handler.create
              ?p2p_versions
              connect_handler_cfg
              pool
              msg_config
              conn_meta_config
              sched
              trigger
              ~log
              ~answerer
          in
          let*! _ =
            Lwt_list.map_p
              (fun point ->
                P2p_pool.Points.info pool point
                |> Option.iter (fun info ->
                       P2p_point_state.set_private info false) ;
                Lwt.return_unit)
              trusted_points
          in
          let*! welcome =
            P2p_welcome.create ~backlog:10 connect_handler ~addr port
          in
          let* welcome =
            match welcome with
            | Ok w -> Lwt.return @@ Ok w
            | Error _ ->
                let*! () = Lwt_unix.sleep 2. in
                P2p_welcome.create ~backlog:10 connect_handler ~addr port
          in
          P2p_welcome.activate welcome ;
          let*! () = Event.(emit node_ready) port in
          let node =
            {
              iteration;
              channel;
              connect_handler;
              pool;
              watcher;
              trigger;
              trusted_points;
              points = all_points;
            }
          in
          let* () = sync node in
          (* Sync interaction 1 *)
          let* () = f node in
          let*! () = Event.(emit shutting_down) () in
          let*! () = P2p_welcome.shutdown welcome in
          (* Here P2p_pool.tear_down_connections is called instead of
             P2p_pool.destroy because there is not data-dir and it is required
             to save the known peers list. *)
          let*! () = P2p_pool.tear_down_connections pool in
          let*! () = P2p_io_scheduler.shutdown sched in
          let*! () = Event.(emit bye) () in
          return_unit))

let select_nth_point n points =
  if n < 0 then
    raise (Invalid_argument "P2p.Test.Node.select_nth_point: negative input") ;
  let rec loop n acc = function
    | [] -> invalid_arg "Utils.select"
    | x :: xs when n <= 0 -> (x, List.rev_append acc xs)
    | x :: xs -> loop (pred n) (x :: acc) xs
  in
  loop n [] points

let default_ipv6_addr = Ipaddr.V6.localhost

let gen_points npoints ?port addr =
  match port with
  | Some port ->
      let ports = port -- (port + npoints - 1) in
      List.map (fun port -> (addr, port)) ports
  | None ->
      let uaddr = Ipaddr_unix.V6.to_inet_addr addr in
      let rec loop i ports =
        if i <= 0 then ports
        else
          try
            let main_socket = Unix.(socket PF_INET6 SOCK_STREAM 0) in
            try
              Unix.setsockopt main_socket Unix.SO_REUSEPORT true ;
              Unix.setsockopt main_socket Unix.SO_REUSEADDR true ;
              Unix.set_close_on_exec main_socket ;
              Unix.bind
                main_socket
                (ADDR_INET
                   ( uaddr,
                     match port with
                     | None -> 0
                     | Some port -> port + (npoints - i) )) ;
              Unix.listen main_socket 50 ;
              let port =
                match Unix.getsockname main_socket with
                | ADDR_UNIX _ -> assert false
                | ADDR_INET (_, port) -> port
              in
              Unix.close main_socket ;
              loop (i - 1) (port :: ports)
            with
            | Unix.Unix_error ((Unix.EADDRINUSE | Unix.EADDRNOTAVAIL), _, _)
              when port = None ->
                Unix.close main_socket ;
                loop i ports
            | Unix.Unix_error (e, _, _) ->
                Unix.close main_socket ;
                Format.kasprintf Stdlib.failwith "%s" (Unix.error_message e)
          with Unix.Unix_error (e, _, _) ->
            Format.kasprintf Stdlib.failwith "%s" (Unix.error_message e)
      in

      let ports = loop npoints [] in
      List.map (fun port -> (addr, port)) ports

(**Detach one process per id in [points], each with a p2p_pool and a
   welcome worker.

   Most arguments are the same as for [detach_node] but they are
  function that specify the value of the argument for a given position
  in the list of points, allowing to specify the characteristics of
  each detached node.

  *)
let detach_nodes ?timeout ?prefix ?min_connections ?max_connections
    ?max_incoming_connections ?p2p_versions ?msg_config
    ?(trusted = fun _ points -> points) run_node points =
  let open Lwt_result_syntax in
  let canceler = Lwt_canceler.create () in
  let*! nodes =
    List.mapi_s
      (fun n _ ->
        let prefix = Option.map (fun f -> f n) prefix in
        let p2p_versions = Option.map (fun f -> f n) p2p_versions in
        let msg_config = Option.map (fun f -> f n) msg_config in
        let min_connections = Option.map (fun f -> f n) min_connections in
        let max_connections = Option.map (fun f -> f n) max_connections in
        let max_incoming_connections =
          Option.map (fun f -> f n) max_incoming_connections
        in
        let (addr, port), other_points = select_nth_point n points in
        detach_node
          ?prefix
          ?p2p_versions
          ?timeout
          ?min_connections
          ?max_connections
          ?max_incoming_connections
          ?msg_config
          canceler
          (run_node n)
          (trusted n points)
          other_points
          addr
          port)
      points
  in
  let*? nodes = Error_monad.Result_syntax.tzall nodes in
  Lwt.ignore_result (sync_nodes nodes) ;
  Process.wait_all nodes
