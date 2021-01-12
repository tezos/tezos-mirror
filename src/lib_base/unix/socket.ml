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

open Error_monad

type addr =
  | Unix of string
  | Tcp of string * string * Unix.getaddrinfo_option list

let handle_literal_ipv6 host =
  (* To strip '[' and ']' when a literal IPv6 is provided *)
  match Ipaddr.of_string host with
  | Error (`Msg _) ->
      host
  | Ok ipaddr ->
      Ipaddr.to_string ipaddr

let connect ?(timeout = !Lwt_utils_unix.default_net_timeout) = function
  | Unix path ->
      let addr = Lwt_unix.ADDR_UNIX path in
      let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec sock ;
      Lwt_unix.connect sock addr >>= fun () -> return sock
  | Tcp (host, service, opts) -> (
      let host = handle_literal_ipv6 host in
      Lwt_unix.getaddrinfo host service opts
      >>= function
      | [] ->
          failwith "could not resolve host '%s'" host
      | addrs ->
          let rec try_connect acc = function
            | [] ->
                Lwt.return
                  (Error
                     (failure "could not connect to '%s'" host :: List.rev acc))
            | {Unix.ai_family; ai_socktype; ai_protocol; ai_addr; _} :: addrs
              -> (
                let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
                Lwt_unix.set_close_on_exec sock ;
                protect
                  ~on_error:(fun e ->
                    Lwt_unix.close sock >>= fun () -> Lwt.return_error e)
                  (fun () ->
                    Lwt_unix.with_timeout
                      (Ptime.Span.to_float_s timeout)
                      (fun () ->
                        Lwt_unix.connect sock ai_addr >>= fun () -> return sock))
                >>= function
                | Ok sock ->
                    return sock
                | Error e ->
                    try_connect (e @ acc) addrs )
          in
          try_connect [] addrs )

let with_connection ?timeout addr f =
  connect ?timeout addr
  >>=? fun conn ->
  protect
    (fun () ->
      f conn
      >>=? fun a -> Lwt_utils_unix.safe_close conn >>=? fun () -> return a)
    ~on_error:(fun e ->
      Lwt_utils_unix.safe_close conn >>=? fun () -> Lwt.return (Error e))

let bind ?(backlog = 10) = function
  | Unix path ->
      let addr = Lwt_unix.ADDR_UNIX path in
      let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec sock ;
      Lwt_unix.bind sock addr
      >>= fun () ->
      Lwt_unix.listen sock backlog ;
      return [sock]
  | Tcp (host, service, opts) -> (
      Lwt_unix.getaddrinfo
        (handle_literal_ipv6 host)
        service
        (AI_PASSIVE :: opts)
      >>= function
      | [] ->
          failwith "could not resolve host '%s'" host
      | addrs ->
          let do_bind {Unix.ai_family; ai_socktype; ai_protocol; ai_addr; _} =
            let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
            Lwt_unix.set_close_on_exec sock ;
            Lwt_unix.setsockopt sock SO_REUSEADDR true ;
            Lwt_unix.bind sock ai_addr
            >>= fun () ->
            Lwt_unix.listen sock backlog ;
            return sock
          in
          Tezos_lwt_result_stdlib.Lwtreslib.List.map_es do_bind addrs )

(* To get the encoding/decoding errors into scope. *)
open Data_encoding_wrapper

let message_len_size = 2

let send fd encoding message =
  let encoded_message_len = Data_encoding.Binary.length encoding message in
  fail_unless
    (encoded_message_len < 1 lsl (message_len_size * 8))
    Unexpected_size_of_encoded_value
  >>=? fun () ->
  (* len is the length of int16 plus the length of the message we want to send *)
  let len = message_len_size + encoded_message_len in
  let buf = Bytes.create len in
  match
    Data_encoding.Binary.write
      encoding
      message
      buf
      message_len_size
      encoded_message_len
  with
  | Error we ->
      fail (Encoding_error we)
  | Ok last ->
      fail_unless (last = len) Unexpected_size_of_encoded_value
      >>=? fun () ->
      (* we set the beginning of the buf with the length of what is next *)
      Tezos_stdlib.TzEndian.set_int16 buf 0 encoded_message_len ;
      protect (fun () -> Lwt_utils_unix.write_bytes fd buf >|= ok)

let recv ?timeout fd encoding =
  let header_buf = Bytes.create message_len_size in
  protect (fun () ->
      Lwt_utils_unix.read_bytes ?timeout ~len:message_len_size fd header_buf
      >|= ok)
  >>=? fun () ->
  let len = Tezos_stdlib.TzEndian.get_uint16 header_buf 0 in
  let buf = Bytes.create len in
  protect (fun () -> Lwt_utils_unix.read_bytes ?timeout ~len fd buf >|= ok)
  >>=? fun () ->
  match Data_encoding.Binary.read encoding buf 0 len with
  | Error re ->
      fail (Decoding_error re)
  | Ok (read_len, message) ->
      if read_len <> len then fail (Decoding_error Extra_bytes)
      else return message
