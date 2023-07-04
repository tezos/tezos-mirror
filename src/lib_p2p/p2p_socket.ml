(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/4603

   test `close ~wait:true`.
*)
module Events = P2p_events.P2p_socket

module Crypto = struct
  (* maximal size of the buffer *)
  let bufsize = (1 lsl 16) - 1

  let header_length = 2

  (* The size of extra data added by encryption. *)
  let tag_length = Tezos_crypto.Crypto_box.tag_length

  (* The number of bytes added by encryption + header *)
  let extrabytes = header_length + tag_length

  let max_content_length = bufsize - extrabytes

  type data = {
    channel_key : Tezos_crypto.Crypto_box.channel_key;
    mutable local_nonce : Tezos_crypto.Crypto_box.nonce;
    mutable remote_nonce : Tezos_crypto.Crypto_box.nonce;
  }

  (* We do the following assumptions on the NaCl library.  Note that
     we also make the assumption, here, that the NaCl library allows
     in-place boxing and unboxing, since we use the same buffer for
     input and output. *)
  let () = assert (tag_length >= header_length)

  let create_data ~incoming ~sent_msg ~recv_msg ~sk ~pk =
    let channel_key = Tezos_crypto.Crypto_box.precompute sk pk in
    let local_nonce, remote_nonce =
      Tezos_crypto.Crypto_box.generate_nonces ~incoming ~sent_msg ~recv_msg
    in
    {channel_key; local_nonce; remote_nonce}

  (* msg is overwritten and should not be used after this invocation *)
  let write_chunk ?canceler fd cryptobox_data msg =
    let open Lwt_result_syntax in
    let msg_length = Bytes.length msg in
    let* () =
      fail_unless
        (msg_length <= max_content_length)
        P2p_errors.Invalid_message_size
    in
    let encrypted_length = tag_length + msg_length in
    let payload_length = header_length + encrypted_length in
    let tag = Bytes.create tag_length in
    let local_nonce = cryptobox_data.local_nonce in
    cryptobox_data.local_nonce <-
      Tezos_crypto.Crypto_box.increment_nonce local_nonce ;
    Tezos_crypto.Crypto_box.fast_box_noalloc
      cryptobox_data.channel_key
      local_nonce
      tag
      msg ;
    let payload = Bytes.create payload_length in
    TzEndian.set_uint16 payload 0 encrypted_length ;
    Bytes.blit tag 0 payload header_length tag_length ;
    Bytes.blit msg 0 payload extrabytes msg_length ;
    P2p_io_scheduler.write ?canceler fd payload

  (** Read and decypher data from the fd.

      Returned buffer's size is lesser than [bufsize] - [tag_length].
  *)
  let read_chunk ?canceler fd cryptobox_data =
    let open Lwt_result_syntax in
    let open P2p_buffer_reader in
    let header_buf = Bytes.create header_length in
    let* () = read_full ?canceler fd @@ mk_buffer_safe header_buf in
    let encrypted_length = TzEndian.get_uint16 header_buf 0 in
    let* () =
      fail_unless
        (encrypted_length >= tag_length)
        P2p_errors.Invalid_incoming_ciphertext_size
    in
    let tag = Bytes.create tag_length in
    let* () = read_full ?canceler fd @@ mk_buffer_safe tag in
    (* [msg_length] is [>= 0], as guaranteed by the [fail_unless] guard above. *)
    let msg_length = encrypted_length - tag_length in
    let msg = Bytes.create msg_length in
    let* () = read_full ?canceler fd @@ mk_buffer_safe msg in
    let remote_nonce = cryptobox_data.remote_nonce in
    cryptobox_data.remote_nonce <-
      Tezos_crypto.Crypto_box.increment_nonce remote_nonce ;
    match
      Tezos_crypto.Crypto_box.fast_box_open_noalloc
        cryptobox_data.channel_key
        remote_nonce
        tag
        msg
    with
    | false -> tzfail P2p_errors.Decipher_error
    | true -> return msg
end

(* Note: there is an inconsistency here, since we display an error in
   bytes, whereas the option is set in kbytes. Also, since the default
   size is 64kB-1, it is actually impossible to set the default
   size using the option (the max is 63 kB). *)
let check_binary_chunks_size size =
  let value = size - Crypto.extrabytes in
  error_unless
    (value > 0 && value <= Crypto.max_content_length)
    (P2p_errors.Invalid_chunks_size
       {value = size; min = Crypto.extrabytes + 1; max = Crypto.bufsize})

module Connection_message = struct
  type t = {
    port : int option;
    public_key : Tezos_crypto.Crypto_box.public_key;
    proof_of_work_stamp : Tezos_crypto.Crypto_box.nonce;
    message_nonce : Tezos_crypto.Crypto_box.nonce;
    version : Network_version.t;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {port; public_key; proof_of_work_stamp; message_nonce; version} ->
        let port = match port with None -> 0 | Some port -> port in
        (port, public_key, proof_of_work_stamp, message_nonce, version))
      (fun (port, public_key, proof_of_work_stamp, message_nonce, version) ->
        let port = if port = 0 then None else Some port in
        {port; public_key; proof_of_work_stamp; message_nonce; version})
      (obj5
         (req "port" uint16)
         (req "pubkey" Tezos_crypto.Crypto_box.public_key_encoding)
         (req "proof_of_work_stamp" Tezos_crypto.Crypto_box.nonce_encoding)
         (req "message_nonce" Tezos_crypto.Crypto_box.nonce_encoding)
         (req "version" Network_version.encoding))

  let create identity advertised_port announced_version =
    let local_nonce_seed = Tezos_crypto.Crypto_box.random_nonce () in
    {
      public_key = identity.P2p_identity.public_key;
      proof_of_work_stamp = identity.proof_of_work_stamp;
      message_nonce = local_nonce_seed;
      port = advertised_port;
      version = announced_version;
    }

  let write ~canceler fd message =
    let open Lwt_result_syntax in
    let encoded_message_len = Data_encoding.Binary.length encoding message in
    let* () =
      fail_unless
        (encoded_message_len < 1 lsl (Crypto.header_length * 8))
        Tezos_base.Data_encoding_wrapper.Unexpected_size_of_decoded_buffer
    in
    let len = Crypto.header_length + encoded_message_len in
    let buf = Bytes.create len in
    let state =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Data_encoding.Binary.make_writer_state
           buf
           ~offset:Crypto.header_length
           ~allowed_bytes:encoded_message_len
    in
    match Data_encoding.Binary.write encoding message state with
    | Error we -> tzfail (Tezos_base.Data_encoding_wrapper.Encoding_error we)
    | Ok last ->
        let* () =
          fail_unless
            (last = len)
            Tezos_base.Data_encoding_wrapper.Unexpected_size_of_encoded_value
        in
        TzEndian.set_int16 buf 0 encoded_message_len ;
        let* () = P2p_io_scheduler.write ~canceler fd buf in
        (* We return the raw message as it is used later to compute
           the nonces *)
        return buf

  let read ~canceler fd =
    let open Lwt_result_syntax in
    let open P2p_buffer_reader in
    let header_buf = Bytes.create Crypto.header_length in
    let* () = read_full ~canceler fd @@ mk_buffer_safe header_buf in
    let len = TzEndian.get_uint16 header_buf 0 in
    let pos = Crypto.header_length in
    let buf = Bytes.create (pos + len) in
    TzEndian.set_uint16 buf 0 len ;
    let* () =
      read_full ~canceler fd
      @@
      match mk_buffer ~length_to_copy:len ~pos buf with
      | Error _ ->
          (* This call to [mk_buffer] is safe (it can't [Error] out)
             but we cannot use [mk_buffer_safe], because we need to specify
             ~len and ~pos. *)
          assert false
      | Ok buffer -> buffer
    in
    let buf = Bytes.unsafe_to_string buf in
    match Data_encoding.Binary.read encoding buf pos len with
    | Error re -> tzfail (P2p_errors.Decoding_error re)
    | Ok (next_pos, message) ->
        if next_pos <> pos + len then
          tzfail (P2p_errors.Decoding_error Data_encoding.Binary.Extra_bytes)
        else
          (* TODO: https://gitlab.com/tezos/tezos/-/issues/4604

             make the below bytes-to-string copy-conversion unnecessary.
             This requires making the consumer of the [buf] value
             ([Crypto_box.generate_nonces]) able to work with strings directly. *)
          let buf = Bytes.of_string buf in
          return (message, buf)
end

module Metadata = struct
  let write ~canceler metadata_config fd cryptobox_data message =
    let open Lwt_result_syntax in
    let encoded_message_len =
      Data_encoding.Binary.length
        metadata_config.P2p_params.conn_meta_encoding
        message
    in
    let buf = Bytes.create encoded_message_len in
    let state =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Data_encoding.Binary.make_writer_state
           buf
           ~offset:0
           ~allowed_bytes:encoded_message_len
    in
    match
      Data_encoding.Binary.write
        metadata_config.conn_meta_encoding
        message
        state
    with
    | Error we -> tzfail (Tezos_base.Data_encoding_wrapper.Encoding_error we)
    | Ok last ->
        let* () =
          fail_unless
            (last = encoded_message_len)
            Tezos_base.Data_encoding_wrapper.Unexpected_size_of_encoded_value
        in
        Crypto.write_chunk ~canceler fd cryptobox_data buf

  let read ~canceler metadata_config fd cryptobox_data =
    let open Lwt_result_syntax in
    let* buf = Crypto.read_chunk ~canceler fd cryptobox_data in
    let buf = Bytes.unsafe_to_string buf in
    let length = String.length buf in
    let encoding = metadata_config.P2p_params.conn_meta_encoding in
    match Data_encoding.Binary.read encoding buf 0 length with
    | Error re -> tzfail (P2p_errors.Decoding_error re)
    | Ok (read_len, message) ->
        if read_len <> length then
          tzfail (P2p_errors.Decoding_error Data_encoding.Binary.Extra_bytes)
        else return message
end

module Ack = struct
  type t =
    | Ack
    | Nack_v_0
    | Nack of {
        motive : P2p_rejection.t;
        potential_peers_to_connect : P2p_point.Id.t list;
      }

  let encoding =
    let open Data_encoding in
    let ack_encoding = obj1 (req "ack" empty) in
    let nack_v_0_encoding = obj1 (req "nack_v_0" empty) in
    let nack_encoding =
      obj2
        (req "nack_motive" P2p_rejection.encoding)
        (req
           "nack_list"
           (Data_encoding.list ~max_length:100 P2p_point.Id.encoding))
    in
    let ack_case tag =
      case
        tag
        ack_encoding
        ~title:"Ack"
        (function Ack -> Some () | _ -> None)
        (fun () -> Ack)
    in
    let nack_case tag =
      case
        tag
        nack_encoding
        ~title:"Nack"
        (function
          | Nack {motive; potential_peers_to_connect} ->
              Some (motive, potential_peers_to_connect)
          | _ -> None)
        (fun (motive, lst) -> Nack {motive; potential_peers_to_connect = lst})
    in
    let nack_v_0_case tag =
      case
        tag
        nack_v_0_encoding
        ~title:"Nack_v_0"
        (function Nack_v_0 -> Some () | _ -> None)
        (fun () -> Nack_v_0)
    in
    union [ack_case (Tag 0); nack_v_0_case (Tag 255); nack_case (Tag 1)]

  let write ?canceler fd cryptobox_data message =
    let open Lwt_result_syntax in
    let encoded_message_len = Data_encoding.Binary.length encoding message in
    let buf = Bytes.create encoded_message_len in
    let state =
      WithExceptions.Option.get ~loc:__LOC__
      @@ Data_encoding.Binary.make_writer_state
           buf
           ~offset:0
           ~allowed_bytes:encoded_message_len
    in
    match Data_encoding.Binary.write encoding message state with
    | Error we -> tzfail (Tezos_base.Data_encoding_wrapper.Encoding_error we)
    | Ok last ->
        let* () =
          fail_unless
            (last = encoded_message_len)
            Tezos_base.Data_encoding_wrapper.Unexpected_size_of_encoded_value
        in
        Crypto.write_chunk ?canceler fd cryptobox_data buf

  let read ?canceler fd cryptobox_data =
    let open Lwt_result_syntax in
    let* buf = Crypto.read_chunk ?canceler fd cryptobox_data in
    let buf = Bytes.unsafe_to_string buf in
    let length = String.length buf in
    match Data_encoding.Binary.read encoding buf 0 length with
    | Error re -> tzfail (P2p_errors.Decoding_error re)
    | Ok (read_len, message) ->
        if read_len <> length then
          tzfail (P2p_errors.Decoding_error Data_encoding.Binary.Extra_bytes)
        else return message
end

type 'meta authenticated_connection = {
  scheduled_conn : P2p_io_scheduler.connection;
  info : 'meta P2p_connection.Info.t;
  cryptobox_data : Crypto.data;
}

let nack {scheduled_conn; cryptobox_data; info} motive
    potential_peers_to_connect =
  let open Lwt_syntax in
  let* nack =
    if
      P2p_version.feature_available
        P2p_version.Nack_with_list
        info.announced_version.p2p_version
    then
      let* () =
        Events.(emit nack_point_with_list)
          (info.id_point, potential_peers_to_connect)
      in
      Lwt.return (Ack.Nack {motive; potential_peers_to_connect})
    else
      let* () = Events.(emit nack_point_no_point) info.id_point in
      Lwt.return Ack.Nack_v_0
  in
  let* (_ : unit tzresult) = Ack.write scheduled_conn cryptobox_data nack in
  let* (_ : unit tzresult) = P2p_io_scheduler.close scheduled_conn in
  Lwt.return_unit

(* First step: write and read credentials, makes no difference
   whether we're trying to connect to a peer or checking an incoming
   connection, both parties must first introduce themselves. *)
let authenticate ~canceler ~proof_of_work_target ~incoming scheduled_conn
    ((remote_addr, remote_socket_port) as point) ?advertised_port identity
    announced_version metadata_config =
  let open Lwt_result_syntax in
  let*! () = Events.(emit sending_authentication) point in
  let msg_to_send =
    Connection_message.create identity advertised_port announced_version
  in
  let* sent_msg =
    Connection_message.write ~canceler scheduled_conn msg_to_send
  in
  let* msg, recv_msg =
    Connection_message.read
      ~canceler
      (P2p_io_scheduler.to_readable scheduled_conn)
  in
  let remote_listening_port =
    if incoming then msg.port else Some remote_socket_port
  in
  let id_point = (remote_addr, remote_listening_port) in
  let remote_peer_id = Tezos_crypto.Crypto_box.hash msg.public_key in
  let* () =
    fail_unless
      (Tezos_crypto.Crypto_box.check_proof_of_work
         msg.public_key
         msg.proof_of_work_stamp
         proof_of_work_target)
      (P2p_errors.Not_enough_proof_of_work remote_peer_id)
  in
  let cryptobox_data =
    Crypto.create_data
      ~incoming
      ~recv_msg
      ~sent_msg
      ~sk:identity.P2p_identity.secret_key
      ~pk:msg.public_key
  in
  let local_metadata = metadata_config.P2p_params.conn_meta_value () in
  let* () =
    Metadata.write
      ~canceler
      metadata_config
      scheduled_conn
      cryptobox_data
      local_metadata
  in
  let* remote_metadata =
    Metadata.read
      ~canceler
      metadata_config
      (P2p_io_scheduler.to_readable scheduled_conn)
      cryptobox_data
  in
  (* Check that this connection does not lead to the node itself after reading
     first encrypted message. *)
  let* () =
    fail_unless
      (remote_peer_id <> identity.P2p_identity.peer_id)
      (P2p_errors.Myself id_point)
  in
  let info =
    {
      P2p_connection.Info.peer_id = remote_peer_id;
      announced_version = msg.version;
      incoming;
      id_point;
      remote_socket_port;
      private_node = metadata_config.private_node remote_metadata;
      local_metadata;
      remote_metadata;
    }
  in
  return (info, {scheduled_conn; info; cryptobox_data})

module Reader = struct
  type ('msg, 'meta) t = {
    canceler : Lwt_canceler.t;
    conn : 'meta authenticated_connection;
    encoding : 'msg Data_encoding.t;
    messages : (int * 'msg) tzresult Lwt_pipe.Maybe_bounded.t;
    mutable worker : unit Lwt.t;
  }

  let read_message st init =
    let rec loop status =
      let open Lwt_result_syntax in
      let*! () = Lwt.pause () in
      let open Data_encoding.Binary in
      match status with
      | Success {result; size; stream} -> return (result, size, stream)
      | Error err ->
          let*! () = Events.(emit read_error) () in
          tzfail (P2p_errors.Decoding_error err)
      | Await decode_next_buf ->
          let* buf =
            Crypto.read_chunk
              ~canceler:st.canceler
              (P2p_io_scheduler.to_readable st.conn.scheduled_conn)
              st.conn.cryptobox_data
          in
          let*! () =
            Events.(emit read_event) (Bytes.length buf, st.conn.info.peer_id)
          in
          loop (decode_next_buf buf)
    in
    loop (Data_encoding.Binary.read_stream ?init st.encoding)

  let rec worker_loop st stream =
    let open Lwt_syntax in
    let* r =
      let open Lwt_result_syntax in
      let* msg, size, stream = read_message st stream in
      protect ~canceler:st.canceler (fun () ->
          let*! () = Lwt_pipe.Maybe_bounded.push st.messages (Ok (size, msg)) in
          return_some stream)
    in
    match r with
    | Ok (Some stream) -> worker_loop st (Some stream)
    | Ok None -> Error_monad.cancel_with_exceptions st.canceler
    | Error (Canceled :: _) | Error (Exn Lwt_pipe.Closed :: _) ->
        Events.(emit connection_closed) st.conn.info.peer_id
    | Error _ as err ->
        if Lwt_pipe.Maybe_bounded.is_closed st.messages then ()
        else
          (* best-effort push to the messages, we ignore failures *)
          (ignore : bool -> unit)
          @@ Lwt_pipe.Maybe_bounded.push_now st.messages err ;
        Error_monad.cancel_with_exceptions st.canceler

  let run ?size conn encoding canceler =
    let compute_size = function
      | Ok (size, _) ->
          (Sys.word_size / 8 * 11) + size + Lwt_pipe.Maybe_bounded.push_overhead
      | Error _ -> 0
      (* we push Error only when we close the socket,
                        we don't fear memory leaks in that case... *)
    in
    let bound = Option.map (fun max -> (max, compute_size)) size in
    let st =
      {
        canceler;
        conn;
        encoding;
        messages = Lwt_pipe.Maybe_bounded.create ?bound ();
        worker = Lwt.return_unit;
      }
    in
    Lwt_canceler.on_cancel st.canceler (fun () ->
        Lwt_pipe.Maybe_bounded.close st.messages ;
        Lwt.return_unit) ;
    st.worker <-
      Lwt_utils.worker
        "reader"
        ~on_event:Internal_event.Lwt_worker_logger.on_event
        ~run:(fun () -> worker_loop st None)
        ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler) ;
    st

  let shutdown st = Error_monad.cancel_with_exceptions st.canceler
end

type 'msg encoded_message = bytes list

let copy_encoded_message = List.map Bytes.copy

module Writer = struct
  type ('msg, 'meta) t = {
    canceler : Lwt_canceler.t;
    conn : 'meta authenticated_connection;
    encoding : 'msg Data_encoding.t;
    messages :
      (Bytes.t list * unit tzresult Lwt.u option) Lwt_pipe.Maybe_bounded.t;
    mutable worker : unit Lwt.t;
    binary_chunks_size : int; (* in bytes *)
  }

  let send_message st buf =
    let open Lwt_result_syntax in
    let rec loop = function
      | [] -> return_unit
      | buf :: l ->
          let* () =
            Crypto.write_chunk
              ~canceler:st.canceler
              st.conn.scheduled_conn
              st.conn.cryptobox_data
              buf
          in
          let*! () =
            Events.(emit write_event) (Bytes.length buf, st.conn.info.peer_id)
          in
          loop l
    in
    loop buf

  let encode_message (st : ('msg, _) t) msg =
    match Data_encoding.Binary.to_bytes st.encoding msg with
    | Error we ->
        Result_syntax.tzfail
          (Tezos_base.Data_encoding_wrapper.Encoding_error we)
    | Ok bytes -> Ok (Utils.cut st.binary_chunks_size bytes)

  let rec worker_loop st =
    let open Lwt_syntax in
    let* () = Lwt.pause () in
    let* r =
      protect ~canceler:st.canceler (fun () ->
          Lwt_result.ok @@ Lwt_pipe.Maybe_bounded.pop st.messages)
    in
    match r with
    | Error (Canceled :: _) | Error (Exn Lwt_pipe.Closed :: _) ->
        Events.(emit connection_closed) st.conn.info.peer_id
    | Error err ->
        let* () = Events.(emit write_error) (err, st.conn.info.peer_id) in
        Error_monad.cancel_with_exceptions st.canceler
    | Ok (buf, wakener) -> (
        let* res = send_message st buf in
        match res with
        | Ok () ->
            Option.iter (fun u -> Lwt.wakeup_later u res) wakener ;
            worker_loop st
        | Error err -> (
            Option.iter
              (fun u ->
                Lwt.wakeup_later
                  u
                  (Result_syntax.tzfail P2p_errors.Connection_closed))
              wakener ;
            match err with
            | (Canceled | Exn Lwt_pipe.Closed) :: _ ->
                Events.(emit connection_closed) st.conn.info.peer_id
            | P2p_errors.Connection_closed :: _ ->
                let* () =
                  Events.(emit connection_closed) st.conn.info.peer_id
                in
                Error_monad.cancel_with_exceptions st.canceler
            | err ->
                let* () =
                  Events.(emit write_error) (err, st.conn.info.peer_id)
                in
                Error_monad.cancel_with_exceptions st.canceler))

  let run ?size ?binary_chunks_size conn encoding canceler =
    let binary_chunks_size =
      match binary_chunks_size with
      | None -> Crypto.max_content_length
      | Some size ->
          let size = size - Crypto.extrabytes in
          assert (size > 0) ;
          assert (size <= Crypto.max_content_length) ;
          size
    in
    let compute_size =
      let buf_list_size =
        List.fold_left
          (fun sz buf -> sz + Bytes.length buf + (2 * Sys.word_size))
          0
      in
      function
      | buf_l, None ->
          Sys.word_size + buf_list_size buf_l
          + Lwt_pipe.Maybe_bounded.push_overhead
      | buf_l, Some _ ->
          (2 * Sys.word_size) + buf_list_size buf_l
          + Lwt_pipe.Maybe_bounded.push_overhead
    in
    let bound = Option.map (fun max -> (max, compute_size)) size in
    let st =
      {
        canceler;
        conn;
        encoding;
        messages = Lwt_pipe.Maybe_bounded.create ?bound ();
        worker = Lwt.return_unit;
        binary_chunks_size;
      }
    in
    Lwt_canceler.on_cancel st.canceler (fun () ->
        Lwt_pipe.Maybe_bounded.close st.messages ;
        let rec loop () =
          match Lwt_pipe.Maybe_bounded.pop_now st.messages with
          | exception Lwt_pipe.Closed -> ()
          | None -> ()
          | Some (_, None) -> loop ()
          | Some (_, Some w) ->
              Lwt.wakeup_later w (error_with_exn Lwt_pipe.Closed) ;
              loop ()
        in
        loop () ;
        Lwt.return_unit) ;
    st.worker <-
      Lwt_utils.worker
        "writer"
        ~on_event:Internal_event.Lwt_worker_logger.on_event
        ~run:(fun () -> worker_loop st)
        ~cancel:(fun () -> Error_monad.cancel_with_exceptions st.canceler) ;
    st

  let shutdown st =
    let open Lwt_syntax in
    let* () = Error_monad.cancel_with_exceptions st.canceler in
    st.worker
end

type ('msg, 'meta) t = {
  conn : 'meta authenticated_connection;
  reader : ('msg, 'meta) Reader.t;
  writer : ('msg, 'meta) Writer.t;
}

let equal {conn = {scheduled_conn = conn2; _}; _}
    {conn = {scheduled_conn = conn1; _}; _} =
  P2p_io_scheduler.id conn1 = P2p_io_scheduler.id conn2

let pp ppf {conn; _} = P2p_connection.Info.pp (fun _ _ -> ()) ppf conn.info

let info {conn; _} = conn.info

let local_metadata {conn; _} = conn.info.local_metadata

let remote_metadata {conn; _} = conn.info.remote_metadata

let private_node {conn; _} = conn.info.private_node

let accept ?incoming_message_queue_size ?outgoing_message_queue_size
    ?binary_chunks_size ~canceler conn encoding =
  let open Lwt_result_syntax in
  let* ack =
    protect
      (fun () ->
        let* () =
          Ack.write ~canceler conn.scheduled_conn conn.cryptobox_data Ack
        in
        Ack.read
          ~canceler
          (P2p_io_scheduler.to_readable conn.scheduled_conn)
          conn.cryptobox_data)
      ~on_error:(fun err ->
        let*! (_ : unit tzresult) =
          P2p_io_scheduler.close conn.scheduled_conn
        in
        match err with
        | [P2p_errors.Connection_closed] ->
            tzfail P2p_errors.Rejected_socket_connection
        | [P2p_errors.Decipher_error] -> tzfail P2p_errors.Invalid_auth
        | err -> Lwt.return_error err)
  in
  match ack with
  | Ack ->
      let canceler = Lwt_canceler.create () in
      let reader =
        Reader.run ?size:incoming_message_queue_size conn encoding canceler
      and writer =
        Writer.run
          ?size:outgoing_message_queue_size
          ?binary_chunks_size
          conn
          encoding
          canceler
      in
      let conn = {conn; reader; writer} in
      Lwt_canceler.on_cancel canceler (fun () ->
          let open Lwt_syntax in
          let* (_ : unit tzresult) =
            P2p_io_scheduler.close conn.conn.scheduled_conn
          in
          Lwt.return_unit) ;
      return conn
  | Nack_v_0 ->
      tzfail
        (P2p_errors.Rejected_by_nack
           {motive = P2p_rejection.No_motive; alternative_points = None})
  | Nack {motive; potential_peers_to_connect} ->
      tzfail
        (P2p_errors.Rejected_by_nack
           {motive; alternative_points = Some potential_peers_to_connect})

let catch_closed_pipe f =
  let open Lwt_result_syntax in
  let*! r =
    Lwt.catch f (function
        | Lwt_pipe.Closed -> tzfail P2p_errors.Connection_closed
        | exn -> fail_with_exn exn)
  in
  match r with
  | Error (Exn Lwt_pipe.Closed :: _) -> tzfail P2p_errors.Connection_closed
  | (Error _ | Ok _) as v -> Lwt.return v

let write {writer; _} msg =
  let open Lwt_result_syntax in
  catch_closed_pipe (fun () ->
      let*? buf = Writer.encode_message writer msg in
      let*! () = Lwt_pipe.Maybe_bounded.push writer.messages (buf, None) in
      return_unit)

let write_sync {writer; _} msg =
  let open Lwt_result_syntax in
  catch_closed_pipe (fun () ->
      let waiter, wakener = Lwt.wait () in
      let*? buf = Writer.encode_message writer msg in
      let*! () =
        Lwt_pipe.Maybe_bounded.push writer.messages (buf, Some wakener)
      in
      waiter)

let encode {writer : ('msg, _) Writer.t; _} msg =
  Writer.encode_message writer msg

let write_encoded_now {writer; _} buf =
  let open Result_syntax in
  try Ok (Lwt_pipe.Maybe_bounded.push_now writer.messages (buf, None))
  with Lwt_pipe.Closed -> tzfail P2p_errors.Connection_closed

let write_now t msg =
  let open Result_syntax in
  let* buf = encode t msg in
  write_encoded_now t buf

let rec split_bytes size bytes =
  if Bytes.length bytes <= size then [bytes]
  else
    Bytes.sub bytes 0 size
    :: split_bytes size (Bytes.sub bytes size (Bytes.length bytes - size))

let raw_write_sync {writer; _} bytes =
  let open Lwt_syntax in
  let bytes = split_bytes writer.binary_chunks_size bytes in
  catch_closed_pipe (fun () ->
      let waiter, wakener = Lwt.wait () in
      let* () =
        Lwt_pipe.Maybe_bounded.push writer.messages (bytes, Some wakener)
      in
      waiter)

let read {reader; _} =
  catch_closed_pipe (fun () -> Lwt_pipe.Maybe_bounded.pop reader.messages)

let read_now {reader; _} =
  try Lwt_pipe.Maybe_bounded.pop_now reader.messages
  with Lwt_pipe.Closed ->
    Some (Result_syntax.tzfail P2p_errors.Connection_closed)

let stat {conn = {scheduled_conn; _}; _} = P2p_io_scheduler.stat scheduled_conn

let close ?(wait = false) st =
  let open Lwt_syntax in
  let* () =
    if not wait then Lwt.return_unit
    else (
      Lwt_pipe.Maybe_bounded.close st.reader.messages ;
      Lwt_pipe.Maybe_bounded.close st.writer.messages ;
      st.writer.worker)
  in
  let* () = Reader.shutdown st.reader in
  let* () = Writer.shutdown st.writer in
  let* (_ : unit tzresult) = P2p_io_scheduler.close st.conn.scheduled_conn in
  Lwt.return_unit

module Internal_for_tests = struct
  module Connection_message = struct
    type t = Connection_message.t

    let get_public_key t = t.Connection_message.public_key

    let write = Connection_message.write

    let read = Connection_message.read
  end

  module Metadata = struct
    let write = Metadata.write
  end

  let raw_write_sync = raw_write_sync

  let mock_authenticated_connection default_metadata =
    let secret_key, public_key, _pkh =
      Tezos_crypto.Crypto_box.random_keypair ()
    in
    let cryptobox_data =
      Crypto.
        {
          channel_key = Tezos_crypto.Crypto_box.precompute secret_key public_key;
          local_nonce = Tezos_crypto.Crypto_box.zero_nonce;
          remote_nonce = Tezos_crypto.Crypto_box.zero_nonce;
        }
    in
    let scheduled_conn =
      let f2d_t = Lwt_main.run (P2p_fd.socket ()) in
      P2p_io_scheduler.register
        (P2p_io_scheduler.create ~read_buffer_size:0 ())
        f2d_t
    in
    let info = P2p_connection.Internal_for_tests.Info.mock default_metadata in
    {scheduled_conn; info; cryptobox_data}

  module Crypto = struct
    type data = Crypto.data

    let create_data = Crypto.create_data
  end

  let make_crashing_encoding () : 'a Data_encoding.t =
    Data_encoding.conv
      (fun _ -> assert false)
      (fun _ -> assert false)
      Data_encoding.unit

  let mock ?(reader = Lwt_pipe.Maybe_bounded.create ())
      ?(writer = Lwt_pipe.Maybe_bounded.create ()) conn =
    let reader =
      Reader.
        {
          canceler = Lwt_canceler.create ();
          conn;
          encoding = make_crashing_encoding ();
          messages = reader;
          worker = Lwt.return_unit;
        }
    in
    let writer =
      Writer.
        {
          canceler = Lwt_canceler.create ();
          conn;
          encoding = make_crashing_encoding ();
          messages = writer;
          worker = Lwt.return_unit;
          binary_chunks_size = 0;
        }
    in
    {conn; reader; writer}
end
