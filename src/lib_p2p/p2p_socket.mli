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

(** Typed and encrypted connections to peers.

    This module defines:
    - primitive functions to implement a session-establishment protocol
      (set up an authentication/encryption symmetric session key,
       check proof of work target, authenticate hosts, exchange meta data),
    - a higher-level, authenticated and encrypted, type of connection.

    It is parametric in two (abstract data) types. ['msg] is the unit of
    communication. ['meta] is a type of message sent in session establishment.

    Connections defined in this module wrap a [P2p_io_scheduler.connection]
    (which is simply a file descriptor on which R/W are regulated.)

    Each connection has an associated internal read (resp. write) queue
    containing messages (of type ['msg]), whose size can be limited by
    providing corresponding arguments to [accept]. *)

(** {1 Types} *)

(** Type of a connection that successfully passed the authentication
    phase, but has not been accepted yet. Parametrized by the type
    of expected parameter in the `ack` message. *)
type 'meta authenticated_connection

(** Type of an accepted connection, parametrized by the type of
    messages exchanged between peers. *)
type ('msg, 'meta) t

(** [equal t1 t2] returns true iff the identities of the underlying
    [P2p_io_scheduler.connection]s are equal. *)
val equal : ('mst, 'meta) t -> ('msg, 'meta) t -> bool

val pp : Format.formatter -> ('msg, 'meta) t -> unit

val info : ('msg, 'meta) t -> 'meta P2p_connection.Info.t

(** [local_metadata t] returns the metadata provided when calling
    [authenticate]. *)
val local_metadata : ('msg, 'meta) t -> 'meta

(** [remote_metadata t] returns the remote metadata, communicated by the
    remote host when the session was established. *)
val remote_metadata : ('msg, 'meta) t -> 'meta

val private_node : ('msg, 'meta) t -> bool

(** {1 Session-establishment functions}

    These should be used together
    to implement the session establishment protocol. Session establishment
    proceeds in three synchronous, symmetric, steps. The first two steps are
    implemented by [authenticate]. The third step is implemented by either [accept]
    or [nack].

    1. Hosts send each other an authentication message. The message contains
       notably a public key, a nonce, and proof of work stamp computed from
       the public key. PoW work is checked, and a session key is established
       (authenticated key exchange). The session key will be used to
       encrypt/authenticate all subsequent messages over this connection.

    2. Hosts send each other a ['meta] message.

    3. Each host send either an [Ack] message ([accept] function) or an [Nack]
       message ([nack] function). If both hosts send an [Ack], the connection
       is established and they can start to read/write ['msg].

    Note that [P2p_errors.Decipher_error] can be raised from all functions
    receiving messages after step 1, when a message can't be decrypted.

    Typically, the calling module will make additional checks after step 2 to
    decide what to do in step 3. For instance, based on network version or
    ['meta] information. *)

(** [authenticate canceler pow incoming conn point ?port identity version meta]
    returns a couple [(info, auth_conn)] tries to set up a session with
    the host connected via [conn].

    Can fail with
    - [P2p_errors.Not_enough_proof_of_work] if PoW target isn't reached
    - [P2p_errors.Myself] if both hosts are the same peer
    - [P2p_errors.Connection_closed] if the remote peer closes the connection *)
val authenticate :
  canceler:Lwt_canceler.t ->
  proof_of_work_target:Tezos_crypto.Crypto_box.pow_target ->
  incoming:bool ->
  P2p_io_scheduler.connection ->
  P2p_point.Id.t ->
  ?advertised_port:int ->
  P2p_identity.t ->
  Network_version.t ->
  'meta P2p_params.conn_meta_config ->
  ('meta P2p_connection.Info.t * 'meta authenticated_connection) tzresult Lwt.t

(** [nack ac motive alts] sends a [Nack] message with the rejection
    [motive] and a list of proposed
    alternative points to the remote peer, notifying it
    that its connection is rejected. It then closes the connection.

    [alts] must contain less than 100 points or you will be greylisted *)
val nack :
  'meta authenticated_connection ->
  P2p_rejection.t ->
  P2p_point.Id.t list ->
  unit Lwt.t

(** [Accepts] sends an [Ack message] to the remote peer and wait for an [Ack]
    from the remote peer to complete session set up. This can fail with errors:
    - [P2p_errors.Rejected_socket_connection] on connection closed
    - [P2p_errors.Rejected_by_nack] if [Nack] is received
    - [P2p_errors.Invalid_auth] thrown if [P2p_error.Decipher_error] *)
val accept :
  ?incoming_message_queue_size:int ->
  ?outgoing_message_queue_size:int ->
  ?binary_chunks_size:int ->
  canceler:Lwt_canceler.t ->
  'meta authenticated_connection ->
  'msg Data_encoding.t ->
  ('msg, 'meta) t tzresult Lwt.t

(** Check for the [?binary_chunks_size] parameter of [accept]. *)
val check_binary_chunks_size : int -> unit tzresult

(** {1 IO functions on connections} *)

(** {2 Output functions} *)

(** [write conn msg] returns when [msg] has successfully been added to
    [conn]'s internal write queue or fails with a corresponding
    error. *)
val write : ('msg, 'meta) t -> 'msg -> unit tzresult Lwt.t

(** [write_now conn msg] is [Ok true] if [msg] has been added to
    [conn]'s internal write queue, [Ok false] if [msg] has been
    dropped, or fails with a corresponding error otherwise. *)
val write_now : ('msg, 'meta) t -> 'msg -> bool tzresult

(** type of an encoded_message parameterized by the type of message
    it can be encoded from *)
type 'msg encoded_message

(** Copies and encoded message in a newly allocated one *)
val copy_encoded_message : 'msg encoded_message -> 'msg encoded_message

(** Encodes a message to be used with [write_encoded_now].*)
val encode : ('msg, 'meta) t -> 'msg -> 'msg encoded_message tzresult

(** Similar to [write_now conn msg] but with a preencoded message.
    [msg] will be overwritten and should not be used after this
    invocation.
    It will fail if called on a closed connection with the
    `P2p_errors.Connection_closed` error.
*)
val write_encoded_now : ('msg, 'meta) t -> 'msg encoded_message -> bool tzresult

(** [write_sync conn msg] returns when [msg] has been successfully
    sent to the remote end of [conn], or fails accordingly. *)
val write_sync : ('msg, 'meta) t -> 'msg -> unit tzresult Lwt.t

(** {2 Input functions} *)

(** [read conn msg] returns when [msg] has successfully been popped
    from [conn]'s internal read queue or fails with a corresponding
    error. *)
val read : ('msg, 'meta) t -> (int * 'msg) tzresult Lwt.t

(** [read_now conn msg] is [Some msg] if [conn]'s internal read queue
    is not empty, [None] if it is empty, or fails with a corresponding
    error otherwise. *)
val read_now : ('msg, 'meta) t -> (int * 'msg) tzresult option

(** [stat conn] is a snapshot of current bandwidth usage for
    [conn]. *)
val stat : ('msg, 'meta) t -> P2p_stat.t

(** Stores a reason for which it will be closed in a near future. *)
val add_closing_reason :
  reason:P2p_disconnection_reason.t -> ('msg, 'meta) t -> unit

val close :
  ?wait:bool ->
  reason:P2p_disconnection_reason.t ->
  ('msg, 'meta) t ->
  unit Lwt.t

(**/**)

module Internal_for_tests : sig
  (** [Crypto] module permits to create cryptographic data required during some
      p2p handshake tests. *)
  module Crypto : sig
    (** Type of cryptographic data required to send and receive ciphered
        messages. *)
    type data

    (** [create_data ~incoming ~sent_msg ~recv_msg ~sk ~pk] creates
        cryptographic data from:
        - [~sent_msg] and [~recv_msg] that are bytes of connection messages
          respectively sent and received during the first step of the
          handshake.
        _ [~sk] that is the secret key from the p2p identity of
          the node.
        - [~pk] that is the public key from the other connect that has been
          send during the first step of p2p handshake in the connection message.

        [~incoming] should be true if the handshake has been initiated by the
        other node else false.
    *)
    val create_data :
      incoming:bool ->
      sent_msg:bytes ->
      recv_msg:bytes ->
      sk:Tezos_crypto.Crypto_box.secret_key ->
      pk:Tezos_crypto.Crypto_box.public_key ->
      data
  end

  (** [Connection_message] module permits to send or receive the first message
      exchanged after the TCP connection has been established. This message is
      sent and received by the both nodes of the connection and are not
      ciphered.
      This module is required for some tests about p2p handshake. *)
  module Connection_message : sig
    (** Type of the message that is sent and received during the first step of
        the p2p handshake. *)
    type t

    (** [get_public_key conn_msg] returns the public key contained in
        [conn_msg]. *)
    val get_public_key : t -> Tezos_crypto.Crypto_box.public_key

    (** [write ~canceler scheduled_conn conn_msg] encodes then sends [conn_msg]
        on [scheduled_conn] and returns the bytes of this message to initiate
        cryptographic data. *)
    val write :
      canceler:Lwt_canceler.t ->
      P2p_io_scheduler.connection ->
      t ->
      (bytes, error trace) result Lwt.t

    (** [read ~canceler buff] reads the received data in [buff] then tries to
        decode it has a connection message. Returns the received [t] and the
        bytes of this message to initiate cryptographic data. *)
    val read :
      canceler:Lwt_canceler.t ->
      P2p_buffer_reader.readable ->
      (t * bytes, tztrace) result Lwt.t
  end

  (** [Metadata] module permits to send the second message of the p2p
      handshake. These message is exchanged cyphered.
      This module is required for some tests about p2p handshake. *)
  module Metadata : sig
    (** [write ~canceler metadata_config scheduled_conn crypto_data metadata]
        encodes with [metadata_config], cyphers with [crypto_data] then sends
        [metadata] on [scheduled_conn]. *)
    val write :
      canceler:Lwt_canceler.t ->
      'a P2p_params.conn_meta_config ->
      P2p_io_scheduler.connection ->
      Crypto.data ->
      'a ->
      (unit, error trace) result Lwt.t
  end

  val raw_write_sync : ('msg, 'meta) t -> Bytes.t -> unit tzresult Lwt.t

  val mock_authenticated_connection : 'meta -> 'meta authenticated_connection

  val mock :
    ?reader:(int * 'msg) tzresult Lwt_pipe.Maybe_bounded.t ->
    ?writer:(bytes list * unit tzresult Lwt.u option) Lwt_pipe.Maybe_bounded.t ->
    'meta authenticated_connection ->
    ('msg, 'meta) t
end
