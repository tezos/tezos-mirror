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

type t = {
  connection_timeout : Time.System.Span.t;
  authentication_timeout : Time.System.Span.t;
  greylist_timeout : Time.System.Span.t;
  maintenance_idle_time : Time.System.Span.t option;
  min_connections : int;
  expected_connections : int;
  max_connections : int;
  backlog : int;
  max_incoming_connections : int;
  max_download_speed : int option;
  max_upload_speed : int option;
  read_buffer_size : int;
  read_queue_size : int option;
  write_queue_size : int option;
  incoming_app_message_queue_size : int option;
  incoming_message_queue_size : int option;
  outgoing_message_queue_size : int option;
  max_known_peer_ids : (int * int) option;
  max_known_points : (int * int) option;
  peer_greylist_size : int;
  ip_greylist_size_in_kilobytes : int;
  ip_greylist_cleanup_delay : Time.System.Span.t;
  swap_linger : Time.System.Span.t option;
  binary_chunks_size : int option;
}

let default =
  let greylist_timeout = Time.System.Span.of_seconds_exn 86400. (* one day *) in
  {
    connection_timeout = Time.System.Span.of_seconds_exn 10.;
    authentication_timeout = Time.System.Span.of_seconds_exn 5.;
    greylist_timeout;
    maintenance_idle_time =
      Some (Time.System.Span.of_seconds_exn 120.) (* two minutes *);
    min_connections = 10;
    expected_connections = 50;
    max_connections = 100;
    backlog = 20;
    max_incoming_connections = 20;
    max_download_speed = None;
    max_upload_speed = None;
    read_buffer_size = 1 lsl 14;
    read_queue_size = None;
    write_queue_size = None;
    incoming_app_message_queue_size = None;
    incoming_message_queue_size = None;
    outgoing_message_queue_size = None;
    max_known_points = Some (400, 300);
    max_known_peer_ids = Some (400, 300);
    peer_greylist_size = 1023 (* historical value *);
    ip_greylist_size_in_kilobytes =
      2 * 1024 (* two megabytes has shown good properties in simulation *);
    ip_greylist_cleanup_delay = greylist_timeout;
    swap_linger = Some (Time.System.Span.of_seconds_exn 30.);
    binary_chunks_size = None;
  }

let encoding : t Data_encoding.t =
  let open Data_encoding in
  conv
    (fun {
           connection_timeout;
           authentication_timeout;
           greylist_timeout;
           maintenance_idle_time;
           min_connections;
           expected_connections;
           max_connections;
           backlog;
           max_incoming_connections;
           max_download_speed;
           max_upload_speed;
           read_buffer_size;
           read_queue_size;
           write_queue_size;
           incoming_app_message_queue_size;
           incoming_message_queue_size;
           outgoing_message_queue_size;
           max_known_points;
           max_known_peer_ids;
           peer_greylist_size;
           ip_greylist_size_in_kilobytes;
           ip_greylist_cleanup_delay;
           swap_linger;
           binary_chunks_size;
         } ->
      ( ( ( connection_timeout,
            authentication_timeout,
            min_connections,
            expected_connections,
            max_connections,
            backlog,
            max_incoming_connections,
            max_download_speed,
            max_upload_speed,
            swap_linger ),
          ( binary_chunks_size,
            read_buffer_size,
            read_queue_size,
            write_queue_size,
            incoming_app_message_queue_size,
            incoming_message_queue_size,
            outgoing_message_queue_size,
            max_known_points ) ),
        ( max_known_peer_ids,
          peer_greylist_size,
          ip_greylist_size_in_kilobytes,
          ip_greylist_cleanup_delay,
          greylist_timeout,
          maintenance_idle_time ) ))
    (fun ( ( ( connection_timeout,
               authentication_timeout,
               min_connections,
               expected_connections,
               max_connections,
               backlog,
               max_incoming_connections,
               max_download_speed,
               max_upload_speed,
               swap_linger ),
             ( binary_chunks_size,
               read_buffer_size,
               read_queue_size,
               write_queue_size,
               incoming_app_message_queue_size,
               incoming_message_queue_size,
               outgoing_message_queue_size,
               max_known_points ) ),
           ( max_known_peer_ids,
             peer_greylist_size,
             ip_greylist_size_in_kilobytes,
             ip_greylist_cleanup_delay,
             greylist_timeout,
             maintenance_idle_time ) ) ->
      {
        connection_timeout;
        authentication_timeout;
        greylist_timeout;
        maintenance_idle_time;
        min_connections;
        expected_connections;
        max_connections;
        backlog;
        max_incoming_connections;
        max_download_speed;
        max_upload_speed;
        read_buffer_size;
        read_queue_size;
        write_queue_size;
        incoming_app_message_queue_size;
        incoming_message_queue_size;
        outgoing_message_queue_size;
        max_known_points;
        max_known_peer_ids;
        peer_greylist_size;
        ip_greylist_size_in_kilobytes;
        ip_greylist_cleanup_delay;
        swap_linger;
        binary_chunks_size;
      })
    (merge_objs
       (merge_objs
          (obj10
             (dft
                "connection-timeout"
                ~description:
                  "Delay acceptable when initiating a connection to a new \
                   peer, in seconds."
                Time.System.Span.encoding
                default.authentication_timeout)
             (dft
                "authentication-timeout"
                ~description:
                  "Delay granted to a peer to perform authentication, in \
                   seconds."
                Time.System.Span.encoding
                default.authentication_timeout)
             (dft
                "min-connections"
                ~description:
                  "Strict minimum number of connections (triggers an urgent \
                   maintenance)."
                uint16
                default.min_connections)
             (dft
                "expected-connections"
                ~description:
                  "Targeted number of connections to reach when bootstrapping \
                   / maintaining."
                uint16
                default.expected_connections)
             (dft
                "max-connections"
                ~description:
                  "Maximum number of connections (exceeding peers are \
                   disconnected)."
                uint16
                default.max_connections)
             (dft
                "backlog"
                ~description:
                  "Number above which pending incoming connections are \
                   immediately rejected."
                uint8
                default.backlog)
             (dft
                "max-incoming-connections"
                ~description:
                  "Number above which pending incoming connections are \
                   immediately rejected."
                uint8
                default.max_incoming_connections)
             (opt
                "max-download-speed"
                ~description:"Max download speeds in KiB/s."
                int31)
             (opt
                "max-upload-speed"
                ~description:"Max upload speeds in KiB/s."
                int31)
             (dft
                "swap-linger"
                (option Time.System.Span.encoding)
                default.swap_linger))
          (obj8
             (opt "binary-chunks-size" uint8)
             (dft
                "read-buffer-size"
                ~description:"Size of the buffer passed to read(2)."
                int31
                default.read_buffer_size)
             (opt "read-queue-size" int31)
             (opt "write-queue-size" int31)
             (opt "incoming-app-message-queue-size" int31)
             (opt "incoming-message-queue-size" int31)
             (opt "outgoing-message-queue-size" int31)
             (opt
                "max_known_points"
                ~description:
                  "The max and target size for the known address table."
                (tup2 uint16 uint16))))
       (obj6
          (opt
             "max_known_peer_ids"
             ~description:"The max and target size for the known peers table."
             (tup2 uint16 uint16))
          (dft
             "peer_greylist_size"
             ~description:"The number of peer_ids kept in the peer_id greylist."
             uint16
             default.peer_greylist_size)
          (dft
             "ip_greylist_size_in_kilobytes"
             ~description:"The size of the IP address greylist (in kilobytes)."
             uint16
             default.ip_greylist_size_in_kilobytes)
          (dft
             "ip_greylist_cleanup_delay"
             ~description:"The time an IP address is kept in the greylist."
             Time.System.Span.encoding
             default.ip_greylist_cleanup_delay)
          (dft
             "greylist-timeout"
             ~description:"GC delay for the greylists tables, in seconds."
             Time.System.Span.encoding
             default.greylist_timeout)
          (dft
             "maintenance-idle-time"
             ~description:
               "How long to wait at most, in seconds, before running a \
                maintenance loop. If null -- decoding to None -- is provided \
                then the maintenance is disabled."
             (option Time.System.Span.encoding)
             default.maintenance_idle_time)))
