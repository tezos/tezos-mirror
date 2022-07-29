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

module Events = P2p_events.Discovery

type pool = Pool : ('msg, 'meta, 'meta_conn) P2p_pool.t -> pool

module Message = struct
  let encoding =
    Data_encoding.(tup3 (Fixed.string 10) P2p_peer.Id.encoding int16)

  let length =
    WithExceptions.Option.get ~loc:__LOC__
    @@ Data_encoding.Binary.fixed_length encoding

  let key = "DISCOMAGIC"

  let make peer_id port =
    Data_encoding.Binary.to_bytes_exn encoding (key, peer_id, port)
end

module Answer = struct
  type t = {
    my_peer_id : P2p_peer.Id.t;
    pool : pool;
    discovery_port : int;
    canceler : Lwt_canceler.t;
    trust_discovered_peers : bool;
    mutable worker : unit Lwt.t;
  }

  let create_socket st =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        let socket = Lwt_unix.socket PF_INET SOCK_DGRAM 0 in
        Lwt_unix.set_close_on_exec socket ;
        Lwt_canceler.on_cancel st.canceler (fun () ->
            let* r = Lwt_utils_unix.safe_close socket in
            Result.iter_error
              (Format.eprintf "Uncaught error: %a\n%!" pp_print_trace)
              r ;
            return_unit) ;
        Lwt_unix.setsockopt socket SO_BROADCAST true ;
        Lwt_unix.setsockopt socket SO_REUSEADDR true ;
        let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, st.discovery_port) in
        let* () = Lwt_unix.bind socket addr in
        return socket)
      (fun exn ->
        let* () = Events.(emit create_socket_error) () in
        Lwt.fail exn)

  let loop st =
    let open Lwt_result_syntax in
    let* socket =
      protect ~canceler:st.canceler (fun () ->
          Lwt_result.ok @@ create_socket st)
    in
    (* Infinite loop, should never exit. *)
    let rec aux () =
      let buf = Bytes.create Message.length in
      let* rd =
        protect ~canceler:st.canceler (fun () ->
            let*! content = Lwt_unix.recvfrom socket buf 0 Message.length [] in
            let*! () = Events.(emit message_received) () in
            return content)
      in
      match rd with
      | len, Lwt_unix.ADDR_INET (remote_addr, _)
        when Compare.Int.equal len Message.length -> (
          match Data_encoding.Binary.of_bytes_opt Message.encoding buf with
          | Some (key, remote_peer_id, remote_port)
            when Compare.String.equal key Message.key
                 && not (P2p_peer.Id.equal remote_peer_id st.my_peer_id) -> (
              let s_addr = Unix.string_of_inet_addr remote_addr in
              match P2p_addr.of_string_opt s_addr with
              | None ->
                  let*! () = Events.(emit parse_error) s_addr in
                  aux ()
              | Some addr ->
                  let (Pool pool) = st.pool in
                  let*! () = Events.(emit register_new) (addr, remote_port) in
                  P2p_pool.register_new_point
                    ~trusted:st.trust_discovered_peers
                    pool
                    (addr, remote_port)
                  |> ignore ;
                  aux ())
          | _ -> aux ())
      | _ -> aux ()
    in
    aux ()

  let worker_loop st =
    let open Lwt_syntax in
    let* r = loop st in
    match r with
    | Error (Canceled :: _) -> return_unit
    | Error err ->
        let* () = Events.(emit unexpected_error) ("answer", err) in
        Error_monad.cancel_with_exceptions st.canceler
    | Ok () ->
        let* () = Events.(emit unexpected_exit) () in
        Error_monad.cancel_with_exceptions st.canceler

  let create my_peer_id pool ~trust_discovered_peers ~discovery_port =
    {
      canceler = Lwt_canceler.create ();
      my_peer_id;
      discovery_port;
      trust_discovered_peers;
      pool = Pool pool;
      worker = Lwt.return_unit;
    }

  let activate st =
    st.worker <-
      Lwt_utils.worker
        "discovery_answer"
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler)
end

(* ************************************************************ *)
(* Sender  *)

module Sender = struct
  type t = {
    canceler : Lwt_canceler.t;
    my_peer_id : P2p_peer.Id.t;
    listening_port : int;
    discovery_port : int;
    discovery_addr : Ipaddr.V4.t;
    pool : pool;
    restart_discovery : unit Lwt_condition.t;
    mutable worker : unit Lwt.t;
  }

  module Config = struct
    type t = {delay : float; loop : int}

    let initial = {delay = 0.1; loop = 0}

    let increase_delay config = {config with delay = 2.0 *. config.delay}

    let max_loop = 10
  end

  let broadcast_message st =
    let open Lwt_syntax in
    let msg = Message.make st.my_peer_id st.listening_port in
    Lwt.catch
      (fun () ->
        let socket = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
        Lwt_canceler.on_cancel st.canceler (fun () ->
            let* r = Lwt_utils_unix.safe_close socket in
            Result.iter_error
              (Format.eprintf "Uncaught error: %a\n%!" pp_print_trace)
              r ;
            return_unit) ;
        Lwt_unix.setsockopt socket Lwt_unix.SO_BROADCAST true ;
        let broadcast_ipv4 = Ipaddr_unix.V4.to_inet_addr st.discovery_addr in
        let addr = Lwt_unix.ADDR_INET (broadcast_ipv4, st.discovery_port) in
        let* () = Lwt_unix.connect socket addr in
        let* () = Events.(emit broadcast_message) () in
        let* _len = Lwt_unix.sendto socket msg 0 Message.length [] addr in
        let* r = Lwt_utils_unix.safe_close socket in
        Result.iter_error
          (fun trace ->
            Format.eprintf "Uncaught error: %a\n%!" pp_print_trace trace)
          r ;
        return_unit)
      (fun _exn -> Events.(emit broadcast_error) ())

  let rec worker_loop sender_config st =
    let open Lwt_syntax in
    let* r =
      Lwt_result.bind
        (protect ~canceler:st.canceler (fun () ->
             Lwt_result.ok @@ broadcast_message st))
      @@ fun () ->
      protect ~canceler:st.canceler (fun () ->
          Lwt_result.ok
          @@ Lwt.pick
               [
                 (let* () = Lwt_condition.wait st.restart_discovery in
                  return Config.initial);
                 (let* () = Lwt_unix.sleep sender_config.Config.delay in
                  return
                    {sender_config with Config.loop = succ sender_config.loop});
               ])
    in
    match r with
    | Ok config when config.Config.loop = Config.max_loop ->
        let new_sender_config = {config with Config.loop = pred config.loop} in
        worker_loop new_sender_config st
    | Ok config ->
        let new_sender_config = Config.increase_delay config in
        worker_loop new_sender_config st
    | Error (Canceled :: _) -> return_unit
    | Error err ->
        let* () = Events.(emit unexpected_error) ("sender", err) in
        Error_monad.cancel_with_exceptions st.canceler

  let create my_peer_id pool ~listening_port ~discovery_port ~discovery_addr =
    {
      canceler = Lwt_canceler.create ();
      my_peer_id;
      listening_port;
      discovery_port;
      discovery_addr;
      restart_discovery = Lwt_condition.create ();
      pool = Pool pool;
      worker = Lwt.return_unit;
    }

  let activate st =
    st.worker <-
      Lwt_utils.worker
        "discovery_sender"
        ~on_event:Internal_event.Lwt_worker_event.on_event
        ~run:(fun () -> worker_loop Config.initial st)
        ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler)
end

(* ********************************************************************** *)

type t = {answer : Answer.t; sender : Sender.t}

let create ~listening_port ~discovery_port ~discovery_addr
    ~trust_discovered_peers pool my_peer_id =
  let answer =
    Answer.create my_peer_id pool ~discovery_port ~trust_discovered_peers
  in
  let sender =
    Sender.create
      my_peer_id
      pool
      ~listening_port
      ~discovery_port
      ~discovery_addr
  in
  {answer; sender}

let activate {answer; sender} =
  Answer.activate answer ;
  Sender.activate sender

let wakeup t = Lwt_condition.signal t.sender.restart_discovery ()

let shutdown t =
  Lwt.join
    [
      Error_monad.cancel_with_exceptions t.answer.canceler;
      Error_monad.cancel_with_exceptions t.sender.canceler;
    ]
