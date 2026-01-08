(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A worker process running on a dedicated domain that communicate using Eio
    streams of [Bytes.t]. *)
type t

(** [output_channel pw] accesses the output channel of the process_worker [pw].
    This is useful to get data from the process worker. *)
val output_channel : t -> Bytes.t Eio.Stream.t

(** [input_channel pw] accesses the input channel of the process_worker [pw].
    This is useful to send data to the process worker. *)
val input_channel : t -> Bytes.t Eio.Stream.t

(** [read_message input_stream] reads the message [msg] from the Eio.Stream
    [input_stream]. *)
val read_message : Bytes.t Eio.Stream.t -> bytes

(** [write_message output_stream msg] writes the message [msg] to the
    Eio.Stream [output_stream]. *)
val write_message : Bytes.t Eio.Stream.t -> bytes -> unit

(** [run f arg] will run the function [f input_stream output_stream arg], in a
    dedicated domain. The communication with the process is done through
    Eio.Streams. The [f] function is expected to read and write on the provided
    streams. The returned value is a process manager containing the streams to
    be use to communicate with the process.
    As of now, only one reader and one writer are supported. There is not yet a
    locking mechanism and Eio streams are publicly accessible. On parent
    termination on cancellation, the signal is propagated to the process worker
    associated domain.
    *)
val run :
  (Bytes.t Eio.Stream.t -> Bytes.t Eio.Stream.t -> 'a -> unit) -> 'a -> t
