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
  type inner_state = {
    my_peer_id : P2p_peer.Id.t;
    pool : pool;
    discovery_port : int;
    trust_discovered_peers : bool;
    socket : Lwt_unix.file_descr;
  }

  module Name = P2p_workers.Unique_name_maker (struct
    let base = ["p2p_discovery"; "answer"]
  end)

  module Types = struct
    type state = inner_state

    type parameters = {
      my_peer_id : P2p_peer.Id.t;
      pool : pool;
      discovery_port : int;
      trust_discovered_peers : bool;
    }
  end

  module Worker = P2p_workers.Make (Name) (P2p_workers.Loop_request) (Types)

  type t = Worker.activated_worker

  let create_socket discovery_port canceler =
    let open Lwt_syntax in
    Lwt.catch
      (fun () ->
        let socket = Lwt_unix.socket PF_INET SOCK_DGRAM 0 in
        Lwt_unix.set_close_on_exec socket ;
        Lwt_canceler.on_cancel canceler (fun () ->
            let* r = Lwt_utils_unix.safe_close socket in
            Result.iter_error
              (Format.eprintf "Uncaught error: %a\n%!" pp_print_trace)
              r ;
            return_unit) ;
        Lwt_unix.setsockopt socket SO_BROADCAST true ;
        Lwt_unix.setsockopt socket SO_REUSEADDR true ;
        let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_any, discovery_port) in
        let* () = Lwt_unix.bind socket addr in
        return socket)
      (fun exn ->
        let* () = Events.(emit create_socket_error) () in
        Lwt.fail exn)

  let loop worker =
    let open Lwt_result_syntax in
    let st = Worker.state worker in
    let buf = Bytes.create Message.length in
    let* rd =
      protect (fun () ->
          let*! content = Lwt_unix.recvfrom st.socket buf 0 Message.length [] in
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
                return_unit
            | Some addr ->
                let (Pool pool) = st.pool in
                let*! () = Events.(emit register_new) (addr, remote_port) in
                P2p_pool.register_new_point
                  ~trusted:st.trust_discovered_peers
                  pool
                  (addr, remote_port)
                |> ignore ;
                return_unit)
        | _ -> return_unit)
    | _ -> return_unit

  module Handlers = struct
    type self = Worker.callback Worker.t

    type launch_error = tztrace

    let on_launch :
        self ->
        Name.t ->
        Types.parameters ->
        (Types.state, launch_error) result Lwt.t =
     fun self
         _name
         {my_peer_id; discovery_port; trust_discovered_peers; pool} ->
      let open Lwt_result_syntax in
      let* socket =
        protect (fun () ->
            Lwt_result.ok @@ create_socket discovery_port (Worker.canceler self))
      in
      Lwt.return_ok
        {my_peer_id; discovery_port; trust_discovered_peers; pool; socket}

    let on_request : type response error.
        self ->
        (response, error) P2p_workers.Loop_request.t ->
        (response, error) result Lwt.t =
     fun self Loop -> loop self

    let on_no_request _self = Lwt.return_unit

    let on_close _self = Lwt.return_unit

    let on_error : type response error.
        self ->
        _ ->
        (response, error) P2p_workers.Loop_request.t ->
        error ->
        [`Continue | `Shutdown] tzresult Lwt.t =
     fun _self _ Loop error ->
      let open Lwt_result_syntax in
      match error with
      | Canceled :: _ -> return `Shutdown
      | err ->
          let*! () = Events.(emit unexpected_error) ("answer", err) in
          return `Shutdown

    let on_completion _self _request _result _status = Lwt.return_unit
  end

  let create my_peer_id pool ~trust_discovered_peers ~discovery_port =
    Worker.create
      ()
      {my_peer_id; discovery_port; trust_discovered_peers; pool = Pool pool}
      (module Handlers)
end

(* ************************************************************ *)
(* Sender  *)

module Sender = struct
  module Config = struct
    type t = {delay : float; loop : int}

    let initial = {delay = 0.1; loop = 0}

    let increase_delay config = {config with delay = 2.0 *. config.delay}

    let max_loop = 10
  end

  type inner_state = {
    mutable config : Config.t;
    my_peer_id : P2p_peer.Id.t;
    listening_port : int;
    discovery_port : int;
    discovery_addr : Ipaddr.V4.t;
    pool : pool;
    restart_discovery : unit Lwt_condition.t;
  }

  module Name = P2p_workers.Unique_name_maker (struct
    let base = ["p2p_discovery"; "sender"]
  end)

  module Types = struct
    type state = inner_state

    type parameters = {
      my_peer_id : P2p_peer.Id.t;
      listening_port : int;
      discovery_port : int;
      discovery_addr : Ipaddr.V4.t;
      pool : pool;
    }
  end

  module Request = struct
    type ('response, 'error) t = Loop : (Config.t, tztrace) t

    type view = View : ('response, 'error) t -> view

    let view r = View r

    let encoding =
      let open Data_encoding in
      conv (fun (View Loop) -> ()) (fun () -> View Loop) unit

    let pp ppf (View Loop) = Format.fprintf ppf "loop"

    let default_callback_value = View Loop
  end

  module Worker = P2p_workers.Make (Name) (Request) (Types)

  type t = Worker.activated_worker

  let broadcast_message worker =
    let open Lwt_syntax in
    let st = Worker.state worker in
    let msg = Message.make st.my_peer_id st.listening_port in
    Lwt.catch
      (fun () ->
        let socket = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
        Lwt_canceler.on_cancel (Worker.canceler worker) (fun () ->
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

  let loop worker =
    let open Lwt_result_syntax in
    let st = Worker.state worker in
    let sender_config = st.config in
    Lwt_result.bind
      (protect (fun () -> Lwt_result.ok @@ broadcast_message worker))
    @@ fun () ->
    protect (fun () ->
        Lwt.pick
          [
            (let*! () = Lwt_condition.wait st.restart_discovery in
             return Config.initial);
            (let*! () = Lwt_unix.sleep sender_config.Config.delay in
             return {sender_config with Config.loop = succ sender_config.loop});
          ])

  let loop_completion config st =
    if config.Config.loop = Config.max_loop then
      st.config <- {config with Config.loop = pred config.loop}
    else st.config <- Config.increase_delay config ;
    Lwt.return_unit

  let loop_error err =
    let open Lwt_syntax in
    match err with
    | Canceled :: _ -> return_unit
    | err -> Events.(emit unexpected_error) ("sender", err)

  module Handlers = struct
    type self = Worker.callback Worker.t

    type launch_error = tztrace

    let on_launch :
        self ->
        Name.t ->
        Types.parameters ->
        (Types.state, launch_error) result Lwt.t =
     fun _self
         _name
         {my_peer_id; listening_port; discovery_port; discovery_addr; pool} ->
      Lwt.return_ok
        {
          config = Config.initial;
          my_peer_id;
          listening_port;
          discovery_port;
          discovery_addr;
          restart_discovery = Lwt_condition.create ();
          pool;
        }

    let on_request : type response error.
        self -> (response, error) Request.t -> (response, error) result Lwt.t =
     fun self Loop -> loop self

    let on_no_request _self = Lwt.return_unit

    let on_close _self = Lwt.return_unit

    let on_error : type response error.
        self ->
        _ ->
        (response, error) Request.t ->
        error ->
        [`Continue | `Shutdown] tzresult Lwt.t =
     fun _self _ Loop error ->
      let open Lwt_result_syntax in
      let*! () = loop_error error in
      return `Shutdown

    let on_completion : type resp err.
        self -> (resp, err) Request.t -> resp -> _ -> unit Lwt.t =
     fun self Loop config _status -> loop_completion config (Worker.state self)
  end

  let create my_peer_id pool ~listening_port ~discovery_port ~discovery_addr =
    Worker.create
      ()
      {
        my_peer_id;
        listening_port;
        discovery_port;
        discovery_addr;
        pool = Pool pool;
      }
      (module Handlers)
end

(* ********************************************************************** *)

type t = {answer : Answer.t; sender : Sender.t}

let create ~listening_port ~discovery_port ~discovery_addr
    ~trust_discovered_peers pool my_peer_id =
  let open Lwt_result_syntax in
  let* answer =
    Answer.create my_peer_id pool ~discovery_port ~trust_discovered_peers
  in
  let* sender =
    Sender.create
      my_peer_id
      pool
      ~listening_port
      ~discovery_port
      ~discovery_addr
  in
  return {answer; sender}

let activate {answer; sender} =
  Answer.Worker.activate answer ;
  Sender.Worker.activate sender

let wakeup t =
  Lwt_condition.signal
    (Sender.Worker.state t.sender.worker_state).restart_discovery
    ()

let shutdown t =
  Lwt.join [Answer.Worker.shutdown t.answer; Sender.Worker.shutdown t.sender]
