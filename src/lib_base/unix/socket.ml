(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

open Error_monad

type error += Handshake_failure

let () =
  register_error_kind
    `Temporary
    ~id:"handshake_failure"
    ~title:"Handshake failure"
    ~description:"Handshake failed"
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Hanshake failed: cannot validate magic bytes")
    Data_encoding.empty
    (function Handshake_failure -> Some () | _ -> None)
    (fun () -> Handshake_failure)

type addr =
  | Unix of string
  | Tcp of string * string * Unix.getaddrinfo_option list

let handle_literal_ipv6 host =
  (* To strip '[' and ']' when a literal IPv6 is provided *)
  match Ipaddr.of_string host with
  | Error (`Msg _) -> host
  | Ok ipaddr -> Ipaddr.to_string ipaddr

let connect ?(timeout = !Lwt_utils_unix.default_net_timeout) =
  let open Lwt_result_syntax in
  function
  | Unix path ->
      let addr = Lwt_unix.ADDR_UNIX path in
      let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec sock ;
      let*! () = Lwt_unix.connect sock addr in
      return sock
  | Tcp (host, service, opts) -> (
      let host = handle_literal_ipv6 host in
      let*! addrs = Lwt_unix.getaddrinfo host service opts in
      match addrs with
      | [] -> failwith "could not resolve host '%s'" host
      | addrs ->
          let rec try_connect acc = function
            | [] ->
                Lwt.return
                  (Error
                     (let err = error_of_fmt "could not connect to '%s'" host in
                      match acc with
                      | None -> TzTrace.make err
                      | Some tr -> TzTrace.cons err tr))
            | {Unix.ai_family; ai_socktype; ai_protocol; ai_addr; _} :: addrs
              -> (
                let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
                Lwt_unix.set_close_on_exec sock ;
                let*! r =
                  protect
                    ~on_error:(fun e ->
                      let*! () = Lwt_unix.close sock in
                      Lwt.return_error e)
                    (fun () ->
                      Lwt_unix.with_timeout
                        (Ptime.Span.to_float_s timeout)
                        (fun () ->
                          let*! () = Lwt_unix.connect sock ai_addr in
                          return sock))
                in
                match r with
                | Ok sock -> return sock
                | Error (e : error trace) ->
                    let acc =
                      match acc with
                      | None -> Some e
                      | Some tr -> Some (TzTrace.conp e tr)
                    in
                    try_connect acc addrs)
          in
          try_connect None addrs)

let with_connection ?timeout addr f =
  let open Lwt_result_syntax in
  let* conn = connect ?timeout addr in
  protect
    (fun () ->
      let* a = f conn in
      let* () = Lwt_utils_unix.safe_close conn in
      return a)
    ~on_error:(fun e ->
      let* () = Lwt_utils_unix.safe_close conn in
      Lwt.return (Error e))

let bind ?(backlog = 10) =
  let open Lwt_syntax in
  function
  | Unix path ->
      let addr = Lwt_unix.ADDR_UNIX path in
      let sock = Lwt_unix.socket PF_UNIX SOCK_STREAM 0 in
      Lwt_unix.set_close_on_exec sock ;
      let* () = Lwt_unix.bind sock addr in
      Lwt_unix.listen sock backlog ;
      return_ok [sock]
  | Tcp (host, service, opts) -> (
      let* addrs =
        Lwt_unix.getaddrinfo
          (handle_literal_ipv6 host)
          service
          (AI_PASSIVE :: opts)
      in
      match addrs with
      | [] -> failwith "could not resolve host '%s'" host
      | addrs ->
          let do_bind {Unix.ai_family; ai_socktype; ai_protocol; ai_addr; _} =
            let sock = Lwt_unix.socket ai_family ai_socktype ai_protocol in
            Lwt_unix.set_close_on_exec sock ;
            Lwt_unix.setsockopt sock SO_REUSEADDR true ;
            let* () = Lwt_unix.bind sock ai_addr in
            Lwt_unix.listen sock backlog ;
            return_ok sock
          in
          Tezos_error_monad.TzLwtreslib.List.map_es do_bind addrs)

(* To get the encoding/decoding errors into scope. *)
open Data_encoding_wrapper

(* length information is encoded as an [int16] which has a size of [2] bytes *)
let size_of_length_of_message_payload = 2

(* some messages may be too long for their length to be encoded *)
let maximum_length_of_message_payload =
  (* or [0b1111_1111_1111_1111] *)
  1 lsl (size_of_length_of_message_payload * 8)

let send fd encoding message =
  let open Lwt_result_syntax in
  let length_of_message_payload =
    Data_encoding.Binary.length encoding message
  in
  assert (length_of_message_payload >= 0) ;
  let* () =
    fail_unless
      (length_of_message_payload < maximum_length_of_message_payload)
      Unexpected_size_of_encoded_value
  in
  let total_length_of_message =
    size_of_length_of_message_payload + length_of_message_payload
  in
  let message_serialisation_buffer = Bytes.create total_length_of_message in
  let serialisation_state =
    Data_encoding.Binary.make_writer_state
      message_serialisation_buffer
      ~offset:size_of_length_of_message_payload
      ~allowed_bytes:length_of_message_payload
  in
  (* By construction, the length of the serialisation buffer is the state's
     offset + the state's allowed_length. As a result, we are within the range
     of valid parameter for [make_writer_state]. *)
  assert (Option.is_some serialisation_state) ;
  let serialisation_state = Stdlib.Option.get serialisation_state in
  match Data_encoding.Binary.write encoding message serialisation_state with
  | Error we -> tzfail (Encoding_error we)
  | Ok last ->
      let* () =
        fail_unless
          (last = total_length_of_message)
          Unexpected_size_of_encoded_value
      in
      (* we set the beginning of the buf with the length of what is next *)
      Tezos_stdlib.TzEndian.set_int16
        message_serialisation_buffer
        0
        length_of_message_payload ;
      protect (fun () ->
          Lwt_result.ok
          @@ Lwt_utils_unix.write_bytes fd message_serialisation_buffer)

let recv ?timeout fd encoding =
  let open Lwt_result_syntax in
  let header_buf = Bytes.create size_of_length_of_message_payload in
  let* () =
    protect (fun () ->
        Lwt_result.ok
        @@ Lwt_utils_unix.read_bytes_with_timeout
             ?timeout
             ~len:size_of_length_of_message_payload
             fd
             header_buf)
  in
  let len = Tezos_stdlib.TzEndian.get_uint16 header_buf 0 in
  let buf = Bytes.create len in
  let* () =
    protect (fun () ->
        Lwt_result.ok
        @@ Lwt_utils_unix.read_bytes_with_timeout ?timeout ~len fd buf)
  in
  let buf = Bytes.unsafe_to_string buf in
  match Data_encoding.Binary.read encoding buf 0 len with
  | Error re -> tzfail (Decoding_error re)
  | Ok (read_len, message) ->
      if read_len <> len then tzfail (Decoding_error Extra_bytes)
      else return message

let handshake socket magic_bytes =
  let open Lwt_result_syntax in
  let* () = send socket Data_encoding.Variable.bytes magic_bytes in
  let* magic = recv socket Data_encoding.Variable.bytes in
  fail_unless (Bytes.equal magic magic_bytes) Handshake_failure

let get_temporary_socket_dir () =
  match Sys.getenv_opt "XDG_RUNTIME_DIR" with
  | Some xdg_runtime_dir when xdg_runtime_dir <> "" -> xdg_runtime_dir
  | Some _ | None -> Filename.get_temp_dir_name ()
